This is an extension of drawing Taiwan map in R

`twmap` 是一個 R 語言的台灣地圖與物種分布繪圖工具。本版本（1.2-6）針對 R 套件系統的重大變革（如 `rgdal` 和 `rgeos` 併入 `sf`）進行全面現代化，並重構部分程式碼，確保在最新 R 環境中可穩定運行。

除了解決 dependencies 問題，原本的實作```distrmap()```可以同時畫出物種的平面與垂直分布點，不過需要手動輸入繪製區域的邊界 `.shp` 檔以及 數值地形模型 (DTM) 的 `.tif` 檔。
在這版更新當中，利用原本套件中就有的資料集，把這個實作拆成兩個功能，分別是：

### 預設繪製全臺灣分布圖的```distrmap.tw```

在此實作中，套件會自動輸入台灣的邊界與海拔資料，使用者只要輸入物種分布的 x, y, z 座標，就可以直接在臺灣地圖上呈現物種水平與垂直分布點。
此功能提供四種色彩模式 (theme)，使用者可以自行選取。

```R
data("sample_spts.tw")
distrmap.tw(sample_spts.tw, theme = 1, col = "red")
dev.off()
```

![新範例圖-tw](https://raw.githubusercontent.com/Gavin987/twmap/master/inst/examples/twmap_distrmap.tw_theme1.png)

### 可以自訂輸入區域的```distrmap```

此實作開放使用者自行輸入分布地區邊界 `.shp` 以及 raster `.tif`，搭配物種分布座標，最後可以輸出使用者自行輸入的區域分布圖。

```R
boundary_path <- system.file("extdata", "LanyuBorder.shp", package = "twmap")
profile_data <- lat_elev_profile(system.file("extdata", "LanyuDTM.tif", package = "twmap"))
data("sample_spts.ly") 
distrmap(sample_spts.ly, boundary = boundary_path, vert_prof = profile_data, col = "blue")
```

![新範例圖-自訂範圍](https://raw.githubusercontent.com/Gavin987/twmap/master/inst/examples/twmap_distrmap_new.png)
### 如何安裝
```R
install.packages('devtools')
library(devtools)
devtools::install_github('Gavin987/twmap')
```

---
以下為原始版本之介紹：
## 介紹

這是一個畫台灣的物種分布圖 R package。2011年因為植群圖計劃尾聲需要畫一堆這樣的物種（或是植群型)分布圖，所以謝長富老師、陳子英老師還有五木學長問我能否幫忙批次出圖，所以就寫了個 R 的 package 來畫圖。我也知道 GIS 軟體畫起來更漂亮，可以微調的細節也很多，也有許多替代的軟體或套件可用，但這個 package 的目標是：

1. 寫 paper 或和鄉民筆戰的時候，發文附圖方便
2. 新的實作(```distrmap()```) 用黑白灰色系，拿掉過去在平面圖上的海拔顯示，可以顯目列出分布點
3. 垂直剖面圖(緯度海拔)也新增了實作方法(```lat_elev_profile()```)，未來可以自行輸入 raster 建立該地緯度海拔剖面
4. 簡單的座標轉換 TWD1997/TWD1967/WGS84

因為我有點懶，目前都是碰到問題才修改或新增，大概都還算堪用，有問題歡迎 pull request 或 email 給我，我會試著改正或新增功能。

![原始範例圖](https://raw.githubusercontent.com/mutolisp/twmap/master/examples/twmap_distrmap.png)

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
