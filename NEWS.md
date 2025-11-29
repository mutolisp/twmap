# twmap 1.2-6

This is a major update focusing on modernization, dependency resolution, and significant functional enhancement to distribution mapping. (By lobelia)

## Core Improvements and Compatibility

* **R Modernization:** Thoroughly modernized package to ensure clean installation and full functionality on R (>= 4.5.0).
* **Dependency Resolution:** Resolved critical `lazy loading failed` installation ERROR by removing unnecessary Base R imports and ensuring proper namespace declarations, particularly for the `sf` and `terra` dependencies.

## New and Enhanced Features

* **Enhanced Distribution Mapping (`distrmap`):**
    * The core `distrmap` function is now fully flexible, allowing users to **customize the background map input** for plotting species distribution data.
* **New Taiwan-Specific Function (`distrmap.tw`):**
    * Introduced the new function `distrmap.tw()` for streamlined plotting. Users can now input simple species distribution XYZ coordinates, and the function automatically provides **default Taiwan basemaps** (including flat and cross-section views) for immediate visualization.

# twmap 1.2-5

* New implementation to draw maps (`distrmap` function added/updated). (psilotum, 2015-04-04)

# twmap 1.2-4

* Upgraded package compatibility to R 2.15.1 and fixed several reported bugs. (psilotum, 2012-08-17)

# twmap 1.2-3

* Added `NAMESPACE` file to meet the requirements of R 2.14.0. (psilotum, 2011-12-02)

# twmap 1.2-2

* Added support for vertical map portrait mode (left/right options) to `draw.vertmap()`. (psilotum, 2011-05-31)

# twmap 1.2-1

* Added the `draw.vertmap()` function to visualize elevation profiles. (psilotum, 2011-05-27)

# twmap 1.1-9

* Added the `twcoor.trans()` function, supporting WGS84 <-> TWD67/TWD97 coordinate transformation. (psilotum, 2011-05-27)

# twmap 1.0-0

* Initial commit. (psilotum, 2011-05-25)
