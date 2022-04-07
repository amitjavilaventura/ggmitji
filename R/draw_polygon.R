# ---------------- #
# shaded_3d_venn() #
# ---------------- #

#' @title shaded_2d_venn
#' @author amitjavilaventura
#'
#' @description
#' Creates a blank 2D venn, searches the intersections and then draws them with the desired shaded areas in a ggplot based venn.
#' The two circles are A, B and C, respectively. ABC is the intersection, ABnoC is the intersection of A and B not overlapping with C, AnoBnoC is the area of A not overlapping with B or C, and so on.
#' It is useful to inidcate that the data from a figure comes from an intersection.
#'
#' @param shape Character of length 1. One of c('L', 'square', 'triangle', 'circle').
#' @param color Character of length 1. Color of the line. Default = "Black".
#' @param fill Character of length 1 or NA. Fill color of the polygon. If NA, the color is transparent. Default = NA.
#'
#' @export

draw_polygon <- function(shape,
                         color = "black",
                         fill  = NA) {

  # Load required packages
  require(ggplot2)
  require(cowplot)
  if(shape == "circle"){ require(ggforce) }

  # Check that inputs are OK
  if(!is.character(shape)) { stop("'shape' has to a character vector of length 1.") }
  else if(length(shape) != 1) { stop("'shape' has to a character vector of length 1.") }
  else if (!shape %in% c('L', 'square', 'triangle', 'circle')) { stop("'shape' has to be one of 'L', 'square', 'triangle' or 'circle'.") }
  else if(length(color) != 1 | !is.character(color)) { stop("'color' has to a character vector of length 1 with a valid color name.") }
  else if(length(fill) != 1) { stop("'fill' has to a character vector of length 1 with a valid color name or NA.") }
  else if(!is.character(fill) & !is.na(fill)) { stop("'fill' has to a character vector of length 1 with a valid color name or NA.") }

  # Create X and Y coordinates depending on the shape
  if(shape == "L"){ x = c(0, .5, .5, 1, 1, 0, 0); y = c(0, 0, .5, .5, 1, 1, 0); d = data.frame(x,y) }
  else if(shape == "square"){ x = c(0,1,1,0,0); y = c(0,0,1,1,0); d = data.frame(x,y) }
  else if(shape == "triangle"){ x = c(0,1,.5,0); y =c(0,0,1,0); d = data.frame(x,y) }
  else if(shape == "circle"){ x = c(.5); y =c(.5);d = data.frame(x,y) }

  # Start ggplot depending on the shape
  if(shape != "circle"){ g <- ggplot(d, aes(x,y)) + geom_polygon(fill = fill, color = color) }
  else if(shape == "circle"){ g <- ggplot(d) + ggforce::geom_circle(aes(x0 = x, y0 = y, r = .5), fill = fill, color = color) }

  # Format with cowplot
  g <- g + cowplot::theme_nothing()

  # Return
  return(g)

}


