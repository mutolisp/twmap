#' Plot Species Distribution Map with Vertical Profile
#'
#' This function generates a map displaying species distribution points (x, y) 
#' and their corresponding elevation profiles (y, z). It uses Base R graphics.
#'
#' @param pts A data frame or list with three columns: x (longitude), y (latitude), and z (elevation).
#' @param boundary Character. File path or name of the boundary shapefile (to be loaded as an sf object).
#' @param col Character or vector of characters. Color(s) used for plotting the species points.
#' @param vert_prof A data frame or list containing the main island's elevation profile data 
#'   (typically from the `elevprof` dataset loaded via \code{data()}).
#' @param cex Numeric. Magnification factor for plotting symbols. Default is 0.3.
#' @param pch Numeric. Plotting character used for points. Default is 19 (solid circle).
#'
#' @return The function is used for its side effect (plotting) and returns \code{NULL} invisibly.
#' @export
#'
#' @importFrom graphics abline axis box lines par segments text plot.xy
#' @importFrom grDevices xy.coords
#' @importFrom sf st_read
#' @importFrom methods as
#' @importFrom base pretty
#'
#' Plot Species Distribution Map with Vertical Profile
# ... (Roxygen 註釋不變) ...
#' @importFrom base pretty # <--- 確保這行已經加入
#'
#' Plot Species Distribution Map with Vertical Profile
# ... (Roxygen 註釋不變) ...
#'
distrmap <- function (pts,
                      boundary,
                      col = "black",
                      vert_prof,
                      cex = 0.3,
                      pch = 19) {
  
  basemap <- function (boundary, vert_prof) {
    
    custom_sf <- sf::st_read(boundary, quiet = TRUE)
    bbox <- sf::st_bbox(custom_sf)
    
    x_range <- bbox["xmax"] - bbox["xmin"]
    y_range <- bbox["ymax"] - bbox["ymin"]
    
    # ----------------------------------------------------
    # 1. 動態計算邊界、填充與寬度
    # ----------------------------------------------------
    
    # 1.1 設置填充 (留白)
    x_pad_map <- x_range * 0.05
    y_pad_map <- y_range * 0.05
    
    # ⭐️ 修正 1：大幅增加 y_pad_bottom 的絕對值，確保容納海拔標籤
    y_pad_bottom <- base::max(y_pad_map, y_range * 0.1)
    y_pad_top <- y_pad_map
    
    # 1.2 設定垂直剖面圖的寬度 (X 軸長度)
    vert_prof_width <- x_range * 0.4 
    
    # 1.3 設定垂直剖面圖的起始經度 (vert_bound)
    vert_prof_gap <- x_pad_map * 0.5 
    vert_bound <- bbox["xmax"] + x_pad_map + vert_prof_gap 
    
    # 1.4 重新計算總繪圖區域 xlim 和 ylim
    # ⭐️ 修正 2: 減少右側留白比例 (5%)
    x_pad_label <- vert_prof_width * 0.02
    
    xlim <- c(bbox["xmin"] - x_pad_map, vert_bound + vert_prof_width + x_pad_label)
    ylim <- c(bbox["ymin"] - y_pad_bottom, bbox["ymax"] + y_pad_top)
    
    # 1.5 繪圖參數設定
    ly <- ylim[1]
    uy <- ylim[2]
    
    # 計算海拔極限
    max_elev <- base::max(vert_prof$z, na.rm = TRUE)
    elev_limit <- base::ceiling(max_elev / 1000) * 1000
    
    custom_coords <- sf::st_coordinates(custom_sf)
    
    # ----------------------------------------------------
    # 2. 繪製地圖區塊
    # ----------------------------------------------------
    
    graphics::plot(
      NA, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "", asp = 1
    )
    
    graphics::polygon(
      custom_coords[, "X"], custom_coords[, "Y"], col = "white", border = "#aaaaaa"
    )
    
    # 緯度軸
    grid_lat <- base::pretty(ylim, n = 3)
    grid_lat <- grid_lat[grid_lat >= ylim[1] & grid_lat <= ylim[2]]
    axis_latlab <- base::parse(text = base::paste(grid_lat, "*degree~", "N", sep = ""))
    graphics::axis(side = 2, lwd = 0.8, labels = axis_latlab , 
                   lwd.ticks = 0.5, at = grid_lat, cex = 0.8)
    
    # 經度軸 (只在地圖範圍內顯示)
    grid_long <- base::pretty(c(bbox["xmin"], bbox["xmax"]), n = 3)
    grid_long <- grid_long[grid_long >= xlim[1] & grid_long <= xlim[2]]
    axis_longlab <- base::parse(text = base::paste(grid_long, "*degree~", "E", sep = ""))
    graphics::axis(side = 1, labels = axis_longlab, at = grid_long, 
                   line = NA, lwd = 0.8, cex = 0.8)
    
    # ----------------------------------------------------
    # 3. 繪製垂直剖面區塊
    # ----------------------------------------------------
    
    graphics::par(new = TRUE)
    
    # 繪製剖面圖邊框
    graphics::segments(vert_bound, ly + y_pad_map / 2, vert_bound, uy) 
    graphics::segments(vert_bound, ly + y_pad_map / 2, vert_bound + vert_prof_width, ly + y_pad_map / 2) 
    
    # 動態海拔軸和網格
    num_ticks <- 4
    for (i in 0:num_ticks) {
      current_elev <- (i / num_ticks) * elev_limit
      x_pos <- vert_bound + (current_elev / elev_limit) * vert_prof_width 
      
      if (i > 0) {
        graphics::segments(x_pos, ly + y_pad_map / 2, x_pos, uy, col = 'grey', lty = 1)
      }
      
      # 繪製刻度線和標籤
      graphics::segments(x_pos, ly + y_pad_map / 2, x_pos, ly + y_pad_map / 2)
      ticlab <- base::paste0(current_elev)
      lat_unit <- base::abs(grid_lat[2] - grid_lat[1])
      graphics::text(x_pos, ly, ticlab, cex = 0.7)
    }
    
    label_x_pos <- vert_bound + (vert_prof_width / 2)
    # ⭐️ 修正 3：將 'Elevation (m)' 標籤向下推
    graphics::text(label_x_pos, ly - lat_unit / 10, 'Elevation (m)', cex = 0.8)
    
    # 繪製海拔剖面線
    vert_prof$x_map <- vert_bound + (vert_prof$z / elev_limit) * vert_prof_width 
    vf <- base::cbind(vert_prof$x_map, vert_prof$y) 
    
    graphics::plot.xy(
      grDevices::xy.coords(vf),
      type = 'p', col = 'white', pch = 19, cex = 0.5
    )
    graphics::lines(vf, col = 'darkgrey')
    
    # graphics::axis(
    #   side = 4,
    #   lwd = 0.8,
    #   labels = axis_latlab,
    #   lwd.ticks = 0.5,
    #   at = grid_lat,
    #   cex = 0.8
    # )
    
    graphics::box()
    
    # 返回動態參數給主函式
    return(list(vert_bound = vert_bound, 
                elev_limit = elev_limit,
                vert_prof_width = vert_prof_width))
  }
  
  # --- draw_sp_pts --- (此部分與上個版本相同)
  draw_sp_pts <- function(pts, col, cex, pch, vert_bound, elev_limit, vert_prof_width) { 
    
    if (base::dim(pts)[2] != 3) {
      base::print('The input points should have x (longitude), y (latitude) and z (elevation)')
    } else {
      # 1. 水平分佈點 (在地圖上)
      horizon_pts <- base::cbind(x = pts$x, y = pts$y)
      
      graphics::plot.xy(
        grDevices::xy.coords(horizon_pts),
        type = 'p', col = col, pch = pch, cex = cex
      )
      
      # 2. 垂直分佈點 (在剖面圖上)
      pts$x_map <- vert_bound + (pts$z / elev_limit) * vert_prof_width 
      vert_pts <- base::cbind(x_map = pts$x_map, y = pts$y)
      
      graphics::plot.xy(
        grDevices::xy.coords(vert_pts),
        type = 'p', col = col, pch = pch, cex = cex
      )
    }
  }
  
  # --- 主呼叫 ---
  dynamic_params <- basemap(boundary, vert_prof)
  draw_sp_pts(pts, col, cex, pch, 
              vert_bound = dynamic_params$vert_bound, 
              elev_limit = dynamic_params$elev_limit,
              vert_prof_width = dynamic_params$vert_prof_width)
}
