# `ggmitji` <img src="logo.png" align="right" alt="" width="350" />

Useful custom functions made with or for `ggplot2`.

## Functions

* `stat_summary_boxplot()`: draws a boxplot without outliers in a ggplot. To calculate mean, do comparisons and other stuff, all points are taken into account (including outliers). 
* `stat_info_boxplot()`: adds information in a ggplot boxplot about the number observations, mean, median or sd of each group.
* `stat_point_boxplot()`: draws a point for the mean, median, minimum, maximum or quartile values in a boxplot.
* `stat_line_boxplot()`: same as `stat_point_boxplot()`, but draws a line instead of a point.
* `fill_strips_top()`: allows to color the top strips of a ggplot with facets.
* `fill_strips_right()`: allows to color the right strips of a ggplot with facets.
* `shaded_2d_venn()`: creates a 2D Venn diagram with the desired highlighted areas.
* `shaded_3d_venn()`: creates a 3D Venn diagram with the desired highlighted areas.
* `draw_polygon()`: draws a polygon in several shapes (e.g. square, triangle, L-like...).
* `theme_clean()`: custom theme for ggplots that allows to customize almost everything from a plot.
* `theme_custom()`: custom theme for ggplots that allows to customize almost everything from a plot.
* `theme_ggvenn()`: custom theme for ggplot-based Venn diagrams, specially designed for plots made with `ggvenn::ggvenn()`. 
* `remove_axis()`: remove the axes of a ggplot.
* `remove_x_axis()`: remove the X axis of a ggplot.
* `remove_y_axis()`: remove the Y axis of a ggplot.
* `add_border()`: adds a border around the plot area of a ggplot.
* `add_grid()`: adds the grid in the plot area of a ggplot.
* `ch_panel_bg()`: changes the panel background of a ggplot.
* `ch_plot_bg()`: changes the plot background of a ggplot.
* `rm_strips()`: remove strips of a facetted ggplot.
* `rm_strips_x()`: remove strips from the X axis of a facetted ggplot.
* `rm_strips_y()`: remove strips from the Y axis of a facetted ggplot.
* `ch_strips()`: customize strips from a facetted ggplot.
* `ch_strips_x()`: customize strips from the X axis of a facetted ggplot.
* `ch_strips_y()`: customize strips from the Y axis of a facetted ggplot.

## Requirements

`ggmitji` requires the following R packages:

- `ggplot2` *(all functions)*
- `magrittr` *(`shaded_2d_venn()`, `shaded_3d_venn()`)*, for the pipe (`%>%`).
- `polyclip` *(`shaded_2d_venn()`, `shaded_3d_venn()`)*
- `cowplot` *(`fill_strips_top()`, `fill_strips_right()`)*
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

* `1.1.0`:
  + Add new function: `draw_polygon()`. 
  + `shaded_*d_venn()`: add internal function to calculate coordinates of circles instead of relying on `VennDiagram`.

* `2.0.0`:
  + Add new theme functions: `remove_axis()`, `remove_x_axis()`, `remove_y_axis()`, `add_border()`, `add_grid()`, `ch_panel_bg()`, `ch_plot_bg()`. 
  + `shaded_*d_venn()`: change `cowplot::theme_nothing()` for `ggplot2::theme_void()`.
  + `draw_polygon()`: change `cowplot::theme_nothing()` for `ggplot2::theme_void()`.
  + `draw_polygon()`: add internal function to calculate circle coordinates instead of relying on `ggforce::geom_circle()`.
  + `draw_polygon()`: add "heart" and "diamond" as new shapes.
  
* `2.0.1`:
  + `shaded_*d_venn()`: fix a bug that didn't allow to paint more than one area.

* `3.0.0`:
  + Add new functions to customize the strips of a facetted ggplot: `rm_strips()`, `rm_strips_x()`, `rm_strips_y()`, `ch_strips()`, `ch_strips_x()`, `ch_strips_y()`.
  + `stat_info_boxplot()`: add option to write the sum of the observations.
  
* `3.0.1`:
  + Add new `fill_strips*()` functions.
  
* `4.0.0`:
  + Add new function: `stat_line_boxplot()`.
  
* `4.0.1`:
  + Add new theme: `theme_clean()`.
