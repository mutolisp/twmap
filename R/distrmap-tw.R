#' @title Draw Species Distribution Map and Vertical Profile for Taiwan (Simplified)
#'
#' @description This is a simplified wrapper function that draws the species
#' distribution points (\code{pts}) on the Taiwan map and displays the
#' corresponding vertical elevation profile, automatically using the package's
#' built-in \code{twbound} and \code{elevprof} datasets.
#'
#' @param pts A data frame containing species distribution points. Must include
#'   at least three columns: \code{x}, \code{y} (WGS84 or TWD97 horizontal coordinates),
#'   and \code{z} (elevation in meters).
#' @param theme Integer. The map style to draw. \code{1} for colored terrain
#'   (\code{draw.twmap}), \code{3} for black/white border only.
#' @param vert_orient Integer. The orientation for the vertical profile.
#'   \code{1} for longitudinal profile (latitude vs. elevation, default),
#'   \code{2} for lateral profile. Passed to \code{draw.vertmap}.
#' @param ... Additional graphical parameters passed to \code{graphics::points()}.
#'
#' @export
#'
#' @importFrom graphics layout par plot points box
#' @importFrom utils data
#'
#' @examples
#' \dontrun{
#' # 1. 準備模擬的物種分佈數據 (WGS84)
#' set.seed(123)
#' mock_pts <- data.frame(
#'   x = runif(50, 120.5, 121.8), # Longitude
#'   y = runif(50, 22.5, 25.0),   # Latitude
#'   z = sample(seq(10, 3500, by=10), 50) # Elevation
#' )
#' 
#' # 範例 1: 彩色地形圖 (theme=1) + 縱向剖面 (vert_orient=1)
#' distrmap.tw(mock_pts, theme = 1, pch = 16, col = "red")
#' 
#' # 範例 2: 黑白邊界圖 (theme=3) + 橫向剖面 (vert_orient=2)
#' distrmap.tw(mock_pts, theme = 3, vert_orient = 2, pch = 1)
#' }
distrmap.tw <- function(pts, theme = 1, cex = 0.4, pch = 19, col = "black", ...) {
  
  # --- I. 參數檢查與數據準備 ---
  if (!all(c("x", "y", "z") %in% names(pts))) {
    stop("Input 'pts' must be a data frame containing 'x', 'y', and 'z' columns.")
  }
  
  # --- II. 設置圖形佈局 (1 行 2 列) ---
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par))
  
  graphics::layout(matrix(c(1, 2), 1, 2, byrow = TRUE), widths = c(3, 1))
  graphics::par(mar = c(5, 4, 4, 0)) # 地圖 (左邊) 邊距
  
  # --- III. 繪製地圖 ---
  
  if (theme %in% 1:4) {
    # 1. 繪製彩色地形圖 (使用 draw.twmap)
    draw.twmap(theme = theme, lwd = 1, draw_river = FALSE, draw_grid_lines = FALSE)
    
  } else {
    stop("Invalid 'theme' parameter. Use 1 (color), 2 (gray), 3 (dark), or 4 (border).")
  }
  
  # 在地圖上繪製物種點 (水平分佈)
  graphics::points(pts$x, 
                   pts$y,
                   pch = pch,  # <--- 使用新的 pch 參數
                   cex = cex,  # <--- 使用新的 cex 參數
                   col = col)
  
  # --- IV. 繪製垂直剖面圖 ---
  
  graphics::par(mar = c(5, 0, 4, 2)) # 垂直剖面圖 (右邊) 邊距
  
  # 1. 繪製垂直剖面圖的背景線條 (使用 draw.vertmap)
  # 由於 draw.vertmap 內部使用 elevprof，我們需要先載入
  utils::data("elevprof", package = "twmap")
  
  # 繪製垂直剖面背景
  # 我們需要確保 draw.vertmap 使用正確的座標系統，這裡假設 pts$y 是 WGS84 緯度
  draw.vertmap(
    coorsys = 84, # 假設 pts$y 是 WGS84 緯度
    orient = 3,
    mountain = F, # 繪製山形背景
    lwd = 1.5 
  )
  
  # 2. 在垂直剖面上繪製物種點 (垂直分佈)
  # 注意：y軸是緯度/距離，z軸是海拔。
  # 由於 draw.vertmap 的輸出空間是以緯度/距離為 X 軸，海拔為 Y 軸，
  # 我們可以直接繪製 pts$y 對 pts$z。
  graphics::points(pts$z, 
                   pts$y,
                   pch = pch,
                   cex = cex,
                   col = col)
  
}
