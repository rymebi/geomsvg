
#' Grob関数とmakeContent関数を定義し環境にバインドする
#' 
#' @param n_path_obj_list style, bezier, splineの3要素の名前付きリストを、path要素のid毎に作成した名前付きリスト
#' @param class_name 関数から生成されるgrobのクラス名
#' @param env 
#' @returns env paste0(class_name, 'Grob'): Grob関数、paste0('makeContent.', class_name): 描画時の関数
defineGrob <- function(n_path_obj_list, class_name, env = parent.env(environment())) {
  # パスが空の場合は終了する
  if (length(n_path_obj_list) == 0) {
    warning('input path list is empty')
    return(env)
  }
  
  svgGrob <- function(x = grid::unit(0, 'npc'),
                      y = grid::unit(0, 'npc'),
                      size = grid::unit(1.0, 'npc'),
                      rotation = 0,
                      default.units = 'npc',
                      name = NULL,
                      gp = grid::gpar(),
                      vp = NULL) {
    
    # convert to default unit
    if (!grid::is.unit(x)) x <- grid::unit(x, default.units)
    if (!grid::is.unit(y)) y <- grid::unit(y, default.units)
    if (!grid::is.unit(size)) size <- grid::unit(size, default.units)
    # build a gTree of class "svg"
    grid::gTree(
      x = x, y = y, size = size, rotation = rotation,
      name = name, gp = gp, vp = vp, cl = class_name
    )
  }
  
  # merge multiple xspline dataframes into one
  spline_df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(spline_df) <- c('i', 'x', 'y', 's')
  offset_i <- 0
  for (n_path_obj in n_path_obj_list) {
    spline_df[(nrow(spline_df)+1):(nrow(spline_df)+nrow(n_path_obj$spline)),] <-
      data.frame(
        i = n_path_obj$spline$seg_i + offset_i,
        x = n_path_obj$spline$x,
        y = n_path_obj$spline$y,
        s = n_path_obj$spline$s
      )
    offset_i <- spline_df[nrow(spline_df),'i']
  }
  n_seg_vec <- sapply(unname(n_path_obj_list), function(x) x$bezier[nrow(x$bezier),'seg_i'])
  total_seg <- sum(n_seg_vec)
  # set default color, fill, line width from svg file
  default.col <- rep(
    sapply(unname(n_path_obj_list), function(x) if(is.null(x$style$stroke)) NA else x$style$stroke), times = n_seg_vec)
  default.fill <- rep(
    sapply(unname(n_path_obj_list), function(x) if(is.null(x$style$fill)) NA else x$style$fill), times = n_seg_vec)
  default.lwd <- rep(
    sapply(unname(n_path_obj_list), function(x) if(is.null(x$style[['stroke-width']])) NA else x$style[['stroke-width']]), times = n_seg_vec)
  mergeGpar <- function(name, size, default.value, gp) {
    val <- rep(default.value, size)
    # overwrite with gp$fill, gp$col, ...
    if (!is.null(gp[[name]])) {
      val_gl <- rep_len(rep(gp[[name]], each = total_seg), total_seg * size)
      val <- ifelse(is.na(val_gl), val, val_gl)
    }
    # overwrite with gp$layer1:fill, gp$layer2:fill, ...
    layer_names <- names(n_path_obj_list)
    val_lc <- as.vector(
      do.call( # stack matrix by row
        rbind,
        lapply(seq_along(n_path_obj_list), function(i) {
          v <- gp[[paste0(layer_names[i], ':', name)]]
          # repeat each value for the number of segments in layer
          rep_v <- rep(if(is.null(v)) NA else v, each = n_seg_vec[i])
          # match length and stack
          matrix(
            rep_len(rep_v, n_seg_vec[i]*size),
            nrow = n_seg_vec[i], ncol = size)
        })
      )
    )
    ifelse(is.na(val_lc), val, val_lc)
  }

  # makeContent
  makeContent.svg <- function(g) {
    # convert position and size values absolute units
    x <- grid::convertX(g$x, "mm", valueOnly = TRUE)
    y <- grid::convertY(g$y, "mm", valueOnly = TRUE)
    size <- grid::convertWidth(g$size, "mm", valueOnly = TRUE)
    # no unit for rotation
    rotation <- g$rotation
    
    # prepare color vectors
    # set colors
    col <- mergeGpar('col', length(g$x), default.col, g$gp)
    fill <- mergeGpar('fill', length(g$x), default.fill, g$gp)
    lwd <- mergeGpar('lwd', length(g$x), default.lwd, g$gp)
    gp <- grid::gpar(col = col, fill = fill, lwd = lwd)
    
    # transform input data for drawing xspline grob
    t_spline_df <- do.call(
      rbind,
      lapply(seq_along(g$x), function(i) data.frame(
        id = spline_df$i + (i - 1) * total_seg,
        x = (spline_df$x * cos(rotation[i]) - spline_df$y * sin(rotation[i])) * size[i] + x[i],
        y = (spline_df$x * sin(rotation[i]) + spline_df$y * cos(rotation[i])) * size[i] + y[i],
        s = spline_df$s
      ))
    )
    
    # create a xsplinegrob
    spline_grob <- grid::xsplineGrob(
      x = t_spline_df$x, y = t_spline_df$y, shape = t_spline_df$s, id = t_spline_df$id,
      open = FALSE, default.units = 'mm', gp = gp, vp = g$vp)
    # construct grob tree
    grid::setChildren(g, grid::gList(spline_grob))
  }
  
  assign(paste0(class_name, 'Grob'), svgGrob, envir = env)
  assign(paste0('makeContent.', class_name), makeContent.svg, envir = env)
  env
}

#' Geomクラスとgeom_xxx関数、scale_xxx関数を定義し環境にバインドする
#' 
#' @param n_path_obj_list style, bezier, splineの3要素の名前付きリストを、path要素のid毎に作成した名前付きリスト
#' @param class_name 関数から生成されるgrobのクラス名
#' @param custom_grob Grob関数
#' @param env 
#' @returns env paste0('Geom', class_name): Geomクラス, paste0('geom_', class_name): geom_xxx関数, paste0('scale_', class_name, ':', path_name, ':', '_fill_', [continuous|discrete|manual])
defineGeom <- function(n_path_obj_list, class_name, custom_grob, env = parent.env(environment())) {
  # パスが空の場合は終了する
  if (length(n_path_obj_list) == 0) {
    warning('input path list is empty')
    return(env)
  }
  
  # default aes for each layer in path objects
  default_style_aes <- as.list(setNames(
    apply( # value
      expand.grid(c('stroke', 'fill', 'stroke-width'), names(n_path_obj_list)),
      1,
      function(x, y) n_path_obj_list[[x[2]]][['style']][[x[1]]],
    ),
    apply( # key
      expand.grid(c('colour', 'fill', 'linewidth'), names(n_path_obj_list)),
      1,
      function(x, y) paste0(x[[2]], ":", x[[1]]),
    )
  ))
  
  # function to create gpar
  createGpar <- function(data) {
    gp <- grid::gpar()
    if (!is.null(data$colour)) {
      gp$col <- scales::alpha(data$colour, data$alpha)
      gp$lwd <- data$linewidth
    } else {
      gp$col <- '#00000000'
      gp$lwd <- 0
    }
    if (!is.null(data$fill)) {
      gp$fill <- scales::alpha(data$fill, data$alpha)
    }
    for (n in names(n_path_obj_list)) {
      if (!is.null(data[[paste0(n, ':colour')]])) {
        gp[[paste0(n, ':col')]] <- scales::alpha(data[[paste0(n, ':colour')]], data$alpha)
        if (!is.null(data[[paste0(n, ':linewidth')]])) {
          gp[[paste0(n, ':lwd')]] <- data[[paste0(n, ':linewidth')]]
        }
      }
      if (!is.null(data[[paste0(n, ':fill')]])) {
        gp[[paste0(n, ':fill')]] <- scales::alpha(data[[paste0(n, ':fill')]], data$alpha)
      }
    }
    
    gp
  }
  
  # Geom
  GeomSVG <- ggplot2::ggproto(
    class_name,
    ggplot2::Geom,
    
    # required and default aes
    required_aes = c("x", "y"),
    default_aes = do.call( # size=1, rotation=0, colour=NULL, fill=NULL, path1:colour=col, path1:fill=col, ...
      ggplot2::aes,
      append(
        list(size = 3.5, rotation = 0.0, colour = NULL, linewidth = 0, fill = NULL, alpha = NA),
        default_style_aes
      )
    ),
    
    # check parameters
    setup_params = function(data, params) {
      params
    },
    
    # make grob
    draw_panel = function(data,
                          panel_params,
                          coord,
                          na.rm = FALSE) {
      
      # cleanse and transform data
      data <- ggplot2::remove_missing(
        df = data,
        na.rm = na.rm,
        vars = c("x", "y"),
        name = class_name
      )
      if (is.null(data) || nrow(data) == 0) return(grid::zeroGrob())
      coord <- coord$transform(data, panel_params)
      
      # build the grob
      custom_grob(
        x = coord$x,
        y = coord$y,
        size = grid::unit(coord$size/8, 'cm'),
        rotation = coord$rotation,
        default.units = "native",
        gp = createGpar(coord)
      )
    },
    
    # draw geom in legend
    draw_key = function(data, params, size) {
      custom_grob(x = 0.5, y = 0.5, size = grid::unit(data$size/8, 'cm'), rotation = 0.0,
                  gp = createGpar(data), vp = grid::viewport(clip = 'on'))
    }
  )
  
  # geom_svg
  geom_svg <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomSVG,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    )
  }
  
  # bind geom to env
  assign(paste0('Geom', sub('^(\\w?)', '\\U\\1', class_name, perl = TRUE)), GeomSVG, envir = env)
  assign(paste0('geom_', class_name), geom_svg, envir = env)
  
  # bind scale functions
  bindScaleFn <- function(n, env) {
    # scale for fill
    fill_aes_name <- paste0(n, ':', 'fill')
    fn_tag <- paste0(class_name, ':', fill_aes_name)

    # scale fill continuous
    assign(
      paste0('scale_', fn_tag, '_continuous'),
      function(..., type = ggplot2::scale_fill_gradient)
        ggplot2:::check_scale_type(
          type(..., aesthetics = fill_aes_name),
          paste0('scale_', fill_aes_name, '_continuous'),
          fill_aes_name
        ),
      envir = env
    )
    
    # scale fill discrete
    assign(
      paste0('scale_', fn_tag, '_discrete'),
      function(..., type = ggplot2:::scale_fill_hue)
        if (is.function(type))
          ggplot2:::check_scale_type(
            type(..., aesthetics = fill_aes_name),
            paste0('scale_', fill_aes_name, '_discrete'),
            fill_aes_name,
            scale_is_discrete = TRUE)
        else
          ggplot2:::scale_fill_qualitative(..., type = type),
      envir = env
    )

    # scale fill manual
    assign(
      paste0('scale_', fn_tag, '_manual'),
      function(..., values, breaks = ggplot2::waiver(), na.value = "grey50")
        ggplot2:::manual_scale(fill_aes_name, values, breaks, ..., na.value = na.value),
      envir = env
    )
  }
  
  lapply(names(n_path_obj_list), function(n) bindScaleFn(n, env))
  env
}
