This is an extension of drawing Taiwan map in R


## 介紹

這是一個畫台灣的物種分布圖 R package。2011年因為植群圖計劃尾聲需要畫一堆這樣的物種（或是植群型)分布圖，所以謝長富老師、陳子英老師還有五木學長問我能否幫忙批次出圖，所以就寫了個 R 的 package 來畫圖。我也知道 GIS 軟體畫起來更漂亮，可以微調的細節也很多，也有許多替代的軟體或套件可用，但這個 package 的目標是：

1. 寫 paper 或和鄉民筆戰的時候，發文附圖方便
2. 新的實作(```distrmap()```) 用黑白灰色系，拿掉過去在平面圖上的海拔顯示，可以顯目列出分布點
3. 垂直剖面圖(緯度海拔)也新增了實作方法(```lat_elev_profile()```)，未來可以自行輸入 raster 建立該地緯度海拔剖面
4. 簡單的座標轉換 TWD1997/TWD1967/WGS84

因為我有點懶，目前都是碰到問題才修改或新增，大概都還算堪用，有問題歡迎 pull request 或 email 給我，我會試著改正或新增功能。

![範例圖](https://github.com/mutolisp/twmap/blob/master/examples/twmap_distrmap.png)

## 安裝

你可以透過 _devtools_ 安裝

```R
install.packages('devtools')
library(devtools)
devtools::install_github('mutolisp/twmap')

```
## 使用方法
```R
library(twmap)
# twboundary 是要繪製台灣邊界的圖，shapefile，可以到 http://gadm.org 下載 
# 請記得是 WGS84 經緯度座標系統
twboundary = '~/Google Drive/GIS_Layers/admin_area/world/TWN_adm/TWN_adm0.shp'
# dtm，這裡使用的範例是從 http://gis.rchss.sinica.edu.tw/qgis/?p=1619 下載
# ASTER GDEM v2, 解析度為 30m x 30m
# 記得都是 WGS84 經緯度座標系統
vert_prof = lat_elev_profile('~/Documents/GIS_Layers/aster_dtm_v2_twn_wgs84.tif')

# 物種分布點位匯入
spts = read.csv('~/Desktop/test.csv', header=T, sep=',')
# 繪圖並輸出成 pdf 圖
pdf(file='~/Desktop/test.pdf', height=16, width=13, pointsize=18)
  twmap::distrmap(pts=spts, boundary=twboundary, col='black', vert_prof, pch=19, cex=0.1)
dev.off()

```
