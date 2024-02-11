
#' SVGファイルを読み込みpath要素のd属性の文字列からベジェ曲線、X-Spline曲線のdataframeを、
#' style属性からpathのstyleを抽出する
#' 
#' @param svg_file SVGファイルのパス
#' @returns style, bezier, splineの3要素の名前付きリストを、path要素のid毎に作成した名前付きリスト
read.svg.path <- function(svg_file) {
  svg_doc <- XML::xmlInternalTreeParse(svg_file)
  svg_root <- XML::xmlRoot(svg_doc)
  svg_paths <- XML::getNodeSet(svg_root, paste0('//*[name()="path"]'), namespaces = XML::xmlNamespace(svg_root))

  # オブジェクト登録
  path_obj_list <- list()
  for (svg_path in svg_paths) {
    # idアトリビュートを名前に設定する
    path_id <- XML::xmlGetAttr(svg_path, 'id')
    path_d <- XML::xmlGetAttr(svg_path, 'd')
    # ベジェ曲線を抽出
    bezier <- as.bezier(path_d)
    # スタイルを抽出
    style_list <- XML::xmlGetAttr(svg_path, 'style')
    style_seq <- unlist(lapply(strsplit(style_list, ';')[[1]], strsplit, split = ':'))
    style <- setNames(as.list(style_seq[seq(2, length(style_seq), 2)]), style_seq[seq(1, length(style_seq), 2)])
    path_obj_list[[path_id]] <- list(
      #d = path_d,
      style = style,
      bezier = bezier,
      spline = as.xspline(bezier)
    )
  }
  path_obj_list
}

#' ベジェ曲線、X-Spline曲線の中心を0、長辺を[-0.5, 0.5]の範囲に変換する
#' 
#' @param path_obj_list style, bezier, splineの3要素の名前付きリストを、path要素のid毎に作成した名前付きリスト
#' @returns style, bezier, splineの3要素の名前付きリストを、path要素のid毎に作成した名前付きリスト
normalizeSVGPath <- function(path_obj_list) {
  # 空のリストの場合は空のリストを返す
  if (length(path_obj_list) == 0) {
    return(list())
  }
  
  pos <- do.call(
    rbind,
    lapply(path_obj_list, function(a) {
      s <- a$bezier[,c('sposx', 'sposy')]
      names(s) <- c('x', 'y')
      d <- a$bezier[,c('dposx', 'dposy')]
      names(d) <- c('x', 'y')
      rbind(s, d)
    })
  )
  min_x = min(pos[,1])
  min_y = min(pos[,2])
  width <- max(pos[,1]) - min_x
  height <- max(pos[,2]) - min_y
  size <- pmax(width, height)
  
  n_path_obj_list <- list()
  for (name in names(path_obj_list)) {
    path_obj <- path_obj_list[[name]]
    n_path_obj_list[[name]] <- list(
      #d=path_obj$d,
      style = path_obj$style,
      bezier = data.frame(
        seg_i = path_obj$bezier$seg_i,
        sposx =  (path_obj$bezier$sposx - min_x) / size - 0.5,
        sposy = -(path_obj$bezier$sposy - min_y) / size + 0.5,
        sctlx =  (path_obj$bezier$sctlx - min_x) / size - 0.5,
        sctly = -(path_obj$bezier$sctly - min_y) / size + 0.5,
        dctlx =  (path_obj$bezier$dctlx - min_x) / size - 0.5,
        dctly = -(path_obj$bezier$dctly - min_y) / size + 0.5,
        dposx =  (path_obj$bezier$dposx - min_x) / size - 0.5,
        dposy = -(path_obj$bezier$dposy - min_y) / size + 0.5
      ),
      spline = data.frame(
        seg_i = path_obj$spline$seg_i,
        x =  (path_obj$spline$x - min_x) / size - 0.5,
        y = -(path_obj$spline$y - min_y) / size + 0.5,
        s = path_obj$spline$s
      )
    )
  }
  n_path_obj_list
}

#' SVGのpath要素のd属性の文字列からベジェ曲線の端点と制御点を作成する
#'
#' @param d SVGのpath要素のd属性の文字列
#' @returns 'seg_i', 'sposx', 'sposy', 'sctlx', 'sctly', 'dctlx', 'dctly', 'dposx', 'dposy'のdataframe
as.bezier <- function(d) {
  
  # split command string into list of single command
  m <- gregexpr('([MmLlHhVvCcSsQqTtAaZz]|-?[0-9]+\\.?[0-9]*([eE]-?[0-9]+)?)', d)[[1]]
  m <- data.frame(pos = as.vector(m), len = attr(m, 'match.length'))
  c_vec <- apply(m, 1, function(x) substr(d, x[1], x[1] + x[2] - 1))
  c_pos <- seq(length(c_vec))[unlist(gregexpr('[MmLlHhVvCcSsQqTtAaZz]', c_vec)) > 0]
  c_list <- list()
  for (i in seq(length(c_pos))) {
    end_pos <- if (i < length(c_pos)) c_pos[i + 1] - 1 else length(c_vec)
    c_list[[length(c_list) + 1]] <- c_vec[(c_pos[i]):end_pos]
  }
  
  # create empty output data frame
  df <- data.frame(matrix(nrow = 0, ncol = 9))
  colnames(df) <- c('seg_i', 'sposx', 'sposy', 'sctlx', 'sctly', 'dctlx', 'dctly', 'dposx', 'dposy')

  # variables for current state
  seg_i <- 0
  initpos <- NULL
  curpos <- c(0, 0)
  curctl <- c(0, 0)

  for (i in seq(length(c_list))) {
    command <- c_list[[i]]
    c_char <- command[1]
    v_buff <- if (length(command) > 1) as.numeric(command[2:length(command)]) else c()

    if (c_char == 'M' || c_char == 'm') {
      seg_i <- seg_i + 1
      vdf <- data.frame(matrix(v_buff, ncol = 2, byrow = TRUE))
      colnames(vdf) <- c('x', 'y')
      for (i in seq(nrow(vdf))) {
        delta <- if (c_char == 'M') c(0, 0) else curpos
        tarpos <- delta + c(vdf[i,'x'], vdf[i,'y'])
        
        if (is.null(initpos))
          initpos <- tarpos
        if (i > 1) {
          # draw line from previous point
          df[nrow(df) + 1,] <- c(seg_i, curpos, curpos, tarpos, tarpos)
        }
        curpos <- tarpos
      }
      curctl <- c(0, 0)
      
    } else if (c_char == 'H' || c_char == 'h') {
      for (x in v_buff) {
        if (c_char == 'H')
          tarpos <- c(x, curpos[2])
        else
          tarpos <- c(curpos[1] + x, curpos[2])
        
        df[nrow(df) + 1,] <- c(seg_i, curpos, curpos, tarpos, tarpos)
        curpos <- tarpos
      }
      curctl <- c(0, 0)
      
    } else if (c_char == 'V' || c_char == 'v') {
      for (y in v_buff) {
        if (c_char == 'V')
          tarpos <- c(curpos[1], y)
        else
          tarpos <- c(curpos[1], curpos[2] + y)
        
        df[nrow(df) + 1,] <- c(seg_i, curpos, curpos, tarpos, tarpos)
        curpos <- tarpos
      }
      curctl <- c(0, 0)
      
    } else if (c_char == 'L' || c_char == 'l') {
      vdf <- data.frame(matrix(v_buff, ncol = 2, byrow = TRUE))
      colnames(vdf) <- c('x', 'y')
      for (i in seq(nrow(vdf))) {
        delta <- if (c_char == 'L') c(0, 0) else curpos
        tarpos <- delta + c(vdf[i,'x'], vdf[i,'y'])
        
        df[nrow(df) + 1,] <- c(seg_i, curpos, curpos, tarpos, tarpos)
        curpos <- tarpos
      }
      curctl <- c(0, 0)
      
    } else if (c_char == 'C' || c_char == 'c') {
      vdf <- data.frame(matrix(v_buff, ncol = 6, byrow = TRUE))
      colnames(vdf) <- c('x1', 'y1', 'x2', 'y2', 'x', 'y')
      for (i in seq(nrow(vdf))) {
        delta <- if (c_char == 'C') c(0, 0) else curpos
        tarpos <- delta + c(vdf[i,'x'], vdf[i,'y'])
        ctlpos1 <- delta + c(vdf[i,'x1'], vdf[i,'y1'])
        ctlpos2 <- delta + c(vdf[i,'x2'], vdf[i,'y2'])
        
        df[nrow(df) + 1,] <- c(seg_i, curpos, ctlpos1, ctlpos2, tarpos)
        # set last control point
        curpos <- tarpos
        curctl <- ctlpos2 - tarpos
      }
      
    } else if (c_char == 'S' || c_char == 's') {
      vdf <- data.frame(matrix(v_buff, ncol = 4, byrow = TRUE))
      colnames(vdf) <- c('x2', 'y2', 'x', 'y')
      for (i in seq(nrow(vdf))) {
        delta <- if (c_char == 'S') c(0, 0) else curpos
        tarpos <- delta + c(vdf[i,'x'], vdf[i,'y'])
        ctlpos1 <- curpos - curctl # subtract to reflect
        ctlpos2 <- delta + c(vdf[i,'x2'], vdf[i,'y2'])
        
        df[nrow(df) + 1,] <- c(seg_i, curpos, ctlpos1, ctlpos2, tarpos)
        # set last control point
        curpos <- tarpos
        curctl <- ctlpos2 - tarpos
      }
      
    } else if (c_char == 'Q' || c_char == 'q') {
      vdf <- data.frame(matrix(v_buff, ncol = 4, byrow = TRUE))
      colnames(vdf) <- c('x1', 'y1', 'x', 'y')
      for (i in seq(nrow(vdf))) {
        delta <- if (c_char == 'Q') c(0, 0) else curpos
        tarpos <- delta + c(vdf[i,'x'], vdf[i,'y'])
        q_ctlpos <- delta + c(vdf[i,'x1'], vdf[i,'y1'])
        # find control point of cubic bezier curve from one of quadratic bezier curve
        # using equations: P_quad(1/2) = P_cubic(1/2) ; P_quad(1/4) = P_cubic(1/4)
        ctlpos1 <- 1/3 * (curpos + 2*q_ctlpos)
        ctlpos2 <- 1/3 * (tarpos + 2*q_ctlpos)
        
        df[nrow(df) + 1,] <- c(seg_i, curpos, ctlpos1, ctlpos2, tarpos)
        # set last control point
        curpos <- tarpos
        curctl <- ctlpos2 - tarpos
      }
      
    } else if (c_char == 'T' || c_char == 't') {
      vdf <- data.frame(matrix(v_buff, ncol = 2, byrow = TRUE))
      colnames(vdf) <- c('x', 'y')
      for (i in seq(nrow(vdf))) {
        if (c_char == 'T') {
          tarpos <- c(vdf[i,'x'], vdf[i,'y'])
          # curctl + curpos = actual last control point
          # and calculate control point of last quadratic bezier curve
          last_q_ctlpos <- 1/2*(3*(curctl + curpos) + curpos)
          # then calculate control point of this quadratic bezier curve
          q_ctlpos <- curctl + (curctl - last_q_ctlpos)
        } else {
          tarpos <- curpos + c(vdf[i,'x'], vdf[i,'y'])
          last_q_ctlpos <- 1/2*(3*(curctl + curpos) - curpos)
          q_ctlpos <- curpos + (curpos - last_q_ctlpos)
        }
        ctlpos1 <- curpos - curctl # subtract to reflect
        # finally, calculate control point of cubic bezier curve approximating quad one
        ctlpos2 <- 1/3 * (tarpos + 2*q_ctlpos)
        
        df[nrow(df) + 1,] <- c(seg_i, curpos, ctlpos1, ctlpos2, tarpos)
        # set last control point
        curpos <- tarpos
        curctl <- ctlpos2 - tarpos
      }
      
    } else if (c_char == 'A' || c_char == 'a') {
      # convert from endpoint to center parameterization
      # (Ref: F.6 Elliptical arc implementation notes from W3C recommendation)
      vdf <- data.frame(matrix(v_buff, ncol = 7, byrow = TRUE))
      colnames(vdf) <- c('rx', 'ry', 'x_axis_rotation', 'large_arc_flag', 'sweep_flag', 'x', 'y')
      for (i in seq(nrow(vdf))) {
        delta <- if (c_char == 'A') c(0, 0) else curpos
        tarpos <- delta + c(vdf[i,'x'], vdf[i,'y'])
        
        arc_params <- .convertToCenterParam(curpos[1], curpos[2], tarpos[1], tarpos[2],
                                            vdf[i,'large_arc_flag'], vdf[i,'sweep_flag'],
                                            vdf[i,'rx'], vdf[i,'ry'], vdf[i,'x_axis_rotation'])
        uc_bezier_points <- .createArcBezier(arc_params$theta1, arc_params$theta_delta)
        bezier_points <- .applyTRS(uc_bezier_points,
                                   arc_params$rx, arc_params$ry,
                                   vdf[i,'x_axis_rotation'],
                                   arc_params$cx, arc_params$cy)
        
        df[(nrow(df) + 1):(nrow(df) + nrow(bezier_points)),] <- cbind(seg_i = seg_i, bezier_points)
        
        # set last control point
        curpos <- tarpos
        curctl <- c(df[nrow(df),'dctlx'] - df[nrow(df),'dposx'], df[nrow(df),'dctly'] - df[nrow(df),'dposy'])
      }
      
    } else if (c_char == 'Z' || c_char == 'z') {
      # move to the first point and draw straight line
      df[nrow(df) + 1,] <- c(seg_i, curpos, curpos, initpos, initpos)
      curpos <- initpos
      initpos <- NULL
      curctl <- c(0, 0)
    }
  }
  
  df
}

.convertToCenterParam <- function(x1, y1, x2, y2, fA, fS, rx, ry, phi) {
  # Step1. Compute (x1′, y1′)
  x1r <-  cos(phi) * (x1 - x2) / 2. + sin(phi) * (y1 - y2) / 2.
  y1r <- -sin(phi) * (x1 - x2) / 2. + cos(phi) * (y1 - y2) / 2.
  
  # Check radii and if the ellipse are too small to reach the endpoints,
  # scale radii
  chkval <- x1r^2 / rx^2 + y1r^2 / ry^2
  if (chkval > 0.999) {
    scale <- sqrt(chkval) * 1.0001
    rx <- scale * rx
    ry <- scale * ry
  }
  
  # Step2. Compute (cx′, cy′) (transformed center point)
  sgn <- if (fA != fS) 1 else -1
  coef <- sqrt(
    max(0, (rx^2*ry^2 - rx^2*y1r^2 - ry^2*x1r^2) / (rx^2*y1r^2 + ry^2*x1r^2))
  )
  cxr <- sgn * coef * (rx*y1r/ry)
  cyr <- sgn * coef * (-ry*x1r/rx)
  
  # Step3. Compute (cx, cy) from (cx′, cy′)
  cx <- cos(phi) * cxr - sin(phi) * cyr + (x1 + x2) / 2.
  cy <- sin(phi) * cxr + cos(phi) * cyr + (y1 + y2) / 2.
  
  # Step4. Compute theta1 and theta_delta
  angle <- function(ux, uy, vx, vy) {
    s <- if (ux*vy - uy*vx < 0) -1 else 1
    s * acos((ux*vx + uy*vy) / (sqrt(ux^2 + uy^2) * sqrt(vx^2 + vy^2)))
  }
  theta1 <- angle(1, 0, (x1r - cxr) / rx, (y1r - cyr) / ry)
  theta_delta <- angle((x1r - cxr) / rx, (y1r - cyr) / ry, (-x1r - cxr) / rx, (-y1r - cyr) / ry) %% (2*pi)
  if (fS == 0 && theta_delta > 0) {
    theta_delta <- theta_delta - 2*pi
  } else if (fS == 1 && theta_delta < 0) {
    theta_delta <- theta_delta + 2*pi
  }
  
  list(
    cx = cx,
    cy = cy,
    theta1 = theta1,
    theta_delta = theta_delta,
    rx = rx,
    ry = ry
  )
}

.createArcBezier <- function(theta1, theta_delta) {
  # Approximation of a cubic bezier curve by circular arcs and vice versa
  n_segments <- ceiling(abs(theta_delta) / (pi / 2.))
  bezier_angle <- theta_delta / n_segments
  #k <- 4 / 3 * (1 - cos(bezier_angle / 2)) / sin(bezier_angle / 2) # length between point and control point
  k <- 4 / 3 * sin(bezier_angle / 2) / (1 + cos(bezier_angle / 2)) # for numerical stability
  
  angle1 <- bezier_angle * (seq(n_segments) - 1) + theta1
  angle2 <- bezier_angle * seq(n_segments) + theta1
  data.frame(
    sposx = cos(angle1),
    sposy = sin(angle1),
    sctlx = cos(angle1) - k * sin(angle1),
    sctly = sin(angle1) + k * cos(angle1),
    dctlx = cos(angle2) + k * sin(angle2),
    dctly = sin(angle2) - k * cos(angle2),
    dposx = cos(angle2),
    dposy = sin(angle2)
  )
}

.applyTRS <- function(df, sx, sy, theta, tx, ty) {
  data.frame(
    sposx = sx * df$sposx * cos(theta) - sy * df$sposy * sin(theta) + tx,
    sposy = sx * df$sposx * sin(theta) + sy * df$sposy * cos(theta) + ty,
    sctlx = sx * df$sctlx * cos(theta) - sy * df$sctly * sin(theta) + tx,
    sctly = sx * df$sctlx * sin(theta) + sy * df$sctly * cos(theta) + ty,
    dctlx = sx * df$dctlx * cos(theta) - sy * df$dctly * sin(theta) + tx,
    dctly = sx * df$dctlx * sin(theta) + sy * df$dctly * cos(theta) + ty,
    dposx = sx * df$dposx * cos(theta) - sy * df$dposy * sin(theta) + tx,
    dposy = sx * df$dposx * sin(theta) + sy * df$dposy * cos(theta) + ty
  )
}

#' ベジェ曲線を表すdataframeをX-Spline曲線のパラメータに変換する
#' 
#' @param bezier_df 'seg_i', 'sposx', 'sposy', 'sctlx', 'sctly', 'dctlx', 'dctly', 'dposx', 'dposy'のdataframe
#' @returns 'seg_i', 'x', 'y', 's'のdataframe
#' @references X-Splines : A Spline Model Designed for the End-User
as.xspline <- function(bezier_df) {
  # blending function for x-splines
  blending_f <- function(u, p) u^3 * (10-p + (2*p-15)*u + (6-p)*u^2)
  blending_g <- function(u, p, q) q*u + 2*q*u^2 + (10-12*q-p)*u^3 + (2*p+14*q-15)*u^4 + (6-5*q-p)*u^5
  blending_h <- function(u, q) q*u + 2*q*u^2 - 2*q*u^4 + q*u*5
  # point in bezier curve from t and 4 points
  bezier_c <- function(t, x0, x1, x2, x3) x0*(1-t)^3 + 3*x1*t*(1-t)^2 + 3*x2*t^2*(1-t) + x3*t^3
  
  spline_df <- data.frame(matrix(nrow = 0, ncol = 4))
  colnames(spline_df) <- c('seg_i', 'x', 'y', 's')
  
  df_list <- split(bezier_df, bezier_df$seg_i)
  for (seg_i in names(df_list)) {
    df <- df_list[[seg_i]]
    if (nrow(df) == 0)
      continue
    
    spline_df[nrow(spline_df) + 1,] <-
      data.frame(seg_i = as.numeric(seg_i), x = df[1,'sposx'], y = df[1,'sposy'], s = -1)
    for (c_i in seq(nrow(df))) {
      b_1_x <- bezier_c(1./3, df[c_i,'sposx'], df[c_i,'sctlx'], df[c_i,'dctlx'], df[c_i,'dposx'])
      b_1_y <- bezier_c(1./3, df[c_i,'sposy'], df[c_i,'sctly'], df[c_i,'dctly'], df[c_i,'dposy'])
      b_2_x <- bezier_c(2./3, df[c_i,'sposx'], df[c_i,'sctlx'], df[c_i,'dctlx'], df[c_i,'dposx'])
      b_2_y <- bezier_c(2./3, df[c_i,'sposy'], df[c_i,'sctly'], df[c_i,'dctly'], df[c_i,'dposy'])
      
      blend_0 <- blending_f(1./2, 8.)
      blend_1 <- blending_f(1., 8.)
      A <- matrix(
        c(blend_1, blend_0, 0., 0., # coef for P_1x
          blend_0, blend_1, 0., 0., # coef for P_2x
          0., 0., blend_1, blend_0, # coef for P_1y
          0., 0., blend_0, blend_1),# coef for P_2y
        4, 4, byrow = TRUE)
      b <- c(b_1_x*(2*blend_0 + blend_1) - blend_0*df[c_i,'sposx'],
             b_2_x*(2*blend_0 + blend_1) - blend_0*df[c_i,'dposx'],
             b_1_y*(2*blend_0 + blend_1) - blend_0*df[c_i,'sposy'],
             b_2_y*(2*blend_0 + blend_1) - blend_0*df[c_i,'dposy'])
      sol <- solve(A, b)
      
      spline_df[(nrow(spline_df) + 1):(nrow(spline_df) + 3),] <-
        data.frame(seg_i = as.numeric(seg_i),
                   x = c(sol[[1]], sol[[2]], df[c_i,'dposx']),
                   y = c(sol[[3]], sol[[4]], df[c_i,'dposy']),
                   s = c(1, 1, -1))
    }
  }
  
  spline_df
}

