# `ggmitji` <img src="logo.png" align="right" alt="" width="350" />

Useful custom functions made with or for `ggplot2`.

## Functions

* `stat_summary_boxplot()`: draws a boxplot without outliers in a ggplot. To calculate mean, do comparisons and other stuff, all points are taken into account (including outliers). 
* `stat_info_boxplot()`: adds information in a ggplot boxplot about the number observations, mean, median or sd of each group.
* `stat_point_boxplot()`: draws a point for the mean, median, minimum, maximum or quartile values in a boxplot.
* `fill_strips_top()`: allows to color the top strips of a ggplot with facets.
* `fill_strips_right()`: allows to color the right strips of a ggplot with facets.
* `shaded_2d_venn()`: creates a 2D Venn diagram with the desired highlighted areas.
* `shaded_3d_venn()`: creates a 3D Venn diagram with the desired highlighted areas.
* `draw_polygon()`: draws a polygon in several shapes (e.g. square, triangle, L-like...).
* `theme_custom()`: custom theme for ggplot that allows to customize almost everything from a plot
* `theme_ggvenn()`: custom them for ggplot-based Venn diagrams, specially designed for plots made with `ggvenn::ggvenn()`. 

## Requirements

`ggmitji` requires the following R packages:

- `ggplot2` *(all functions)*
- `magrittr` *(`shaded_2d_venn()`, `shaded_3d_venn()`)*
- `polyclip` *(`shaded_2d_venn()`, `shaded_3d_venn()`)*
- `cowplot` *(`fill_strips_top()`, `fill_strips_right()`,`shaded_2d_venn()`, `shaded_3d_venn()`, `draw_polygon()`)*
- `ggforce` *(`draw_polygon()`)*
- `ggpubr` *(`theme_custom()`)*


## Install `ggmitji` 

To install `ggmitji` you have to run the following command in R:

```
# if not installed, install the devtools package from CRAN 
if(!require(devtools)) { install.packages("devtools") }

# install ggmitji from this Github repository 
devtools::install_github("amitjavilaventura/ggmitji")
```

## Contributors

This package has been developed by [Adrià Mitjavila Ventura](https://amitjavilaventura.github.io). Some ideas were taken from internet forums.

If you want to contribute to this package, make a post in the issues section in this repository or fork this repository adding your code and do a pull request.

## Cite

If you use this package, please cite [this repository](https://github.com/amitjavilaventura/ggmitji) and give it a star.

## Versions

* `1.0.0`:
  + Initial package. 

* `1.0.1`:
  + Add new function: `draw_polygon()`. 
  + `shaded_*d_venn()`: add internal function to calculate coordinates of circles instead of relying on `VennDiagram`.
