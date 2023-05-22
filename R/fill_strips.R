# ------------- #
# fill_strips() #
# ------------- #

#' @title fill_strips
#' @author amitjavilaventura
#'
#' @description
#' Takes a ggplot-based plot with facets and colors the desired strips. It does not return a ggplot object
#'
#' @seealso `ggplot2::ggplot_gtable()`
#' @seealso `ggplot2::ggplot_build()`
#'
#' @param p ggplot object.
#' @param side Character of length 1 to 4. Sides of the stripes to change the colors. One or more of c("top", "right","bottom","left")
#' @param colors Character. Colors used to color the top strips.
#'
#' @export

fill_strips <- function(p, side = c("top", "right"), colors){

  library(ggplot2)
  library(cowplot)

  sides <- c()
  if("top" %in% side){ sides <- c(sides, "t") }
  if("right" %in% side){ sides <- c(sides, "r") }
  if("bottom" %in% side){ sides <- c(sides, "b") }
  if("left" %in% side){ sides <- c(sides, "l") }

  strips <- paste("strip", sides, sep="-")

  g <- ggplot_gtable(ggplot_build(p))
  strip <- which(grepl(paste(strips, collapse = "|"), g$layout$name))
  fills <- colors
  k <- 1
  for (i in strip) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  return(cowplot::ggdraw(g))
}



# ------------------ #
# fill_stripts_top() #
# ------------------ #

#' @title fill_strips_top
#' @author amitjavilaventura
#'
#' @description
#' Takes a ggplot-based plot with facets and colors the top strips. It does not return a ggplot object
#'
#' @seealso `ggplot2::ggplot_gtable()`
#' @seealso `ggplot2::ggplot_build()`
#'
#' @param p ggplot object.
#' @param colots Character. Colors used to color the top strips.
#'
#' @export

fill_strips_top <- function(p, colors){

  library(ggplot2)
  library(cowplot)

  g <- ggplot_gtable(ggplot_build(p))
  strip <- which(grepl('strip-t', g$layout$name))
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


# ------------------ #
# fill_strips_bottom() #
# ------------------ #

#' @title fill_strips_bottom
#' @author amitjavilaventura
#'
#' @description
#' Takes a ggplot-based plot with facets and colors the bottom strips. It does not return a ggplot object
#'
#' @seealso `ggplot2::ggplot_gtable()`
#' @seealso `ggplot2::ggplot_build()`
#'
#' @param p ggplot object.
#' @param colots Character. Colors used to color the top strips.
#'
#' @export

fill_strips_bottom <- function(p, colors){

  library(ggplot2)
  library(cowplot)

  g <- ggplot_gtable(ggplot_build(p))
  strip <- which(grepl('strip-b', g$layout$name))
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

# ------------------ #
# fill_strips_left() #
# ------------------ #

#' @title fill_strips_left
#' @author amitjavilaventura
#'
#' @description
#' Takes a ggplot-based plot with facets and colors the left strips. It does not return a ggplot object
#'
#' @seealso `ggplot2::ggplot_gtable()`
#' @seealso `ggplot2::ggplot_build()`
#'
#' @param p ggplot object.
#' @param colots Character. Colors used to color the top strips.
#'
#' @export

fill_strips_left <- function(p, colors){

  library(ggplot2)
  library(cowplot)

  g <- ggplot_gtable(ggplot_build(p))
  strip <- which(grepl('strip-l', g$layout$name))
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

