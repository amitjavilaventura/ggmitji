# ------------------ #
# fill_strips_right() #
# ------------------ #

#' @title fill_strips_right
#' @author amitjavilaventura
#'
#' @description
#' Takes a ggplot-based plot with facets and colors the right strips. It does not return a ggplot object
#'
#' @seealso `ggplot2::ggplot_gtable()`
#' @seealso `ggplot2::ggplot_build()`
#'
#' @param p ggplot object.
#' @param colots Character. Colors used to color the top strips.
#'
#' @export

fill_strips_right <- function(p, colors){

  library(ggplot2)
  library(cowplot)

  g <- ggplot_gtable(ggplot_build(p))
  strip <- which(grepl('strip-r', g$layout$name))
  fills <- colors
  k <- 1
  for (i in strip) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  plot <- cowplot::ggdraw(g)
  plot
}
