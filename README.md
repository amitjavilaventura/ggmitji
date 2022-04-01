# `ggmitji` <img src="ggmitji.png" align="right" alt="" width="350" />

Package with useful custom functions made with or for `ggplot2`.

## Functions

* `stat_summary_boxplot`: draws a boxplot without outliers in a ggplot. 
* `stat_info_boxplot`: adds information in a ggplot boxplot about the number observations, mean, median or sd of each group.
* `fill_top_strips`: allows to color the top strips of a ggplot with facets.
* `shaded_2d_venn`: creates a 2D Venn diagram with the desired highlighted areas.
* `shaded_3d_venn`: creates a 3D Venn diagram with the desired highlighted areas.
* `theme_custom()`: custom theme for ggplot that allows to customize almost everything from a plot
* `theme_ggvenn()`: custom them for ggplot-based Venn diagrams, specially designed for plots made with `ggvenn::ggvenn()`. 

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

* `1.0.0`