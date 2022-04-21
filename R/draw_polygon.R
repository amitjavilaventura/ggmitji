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
#' @param shape Character of length 1. One of c('L', 'square', 'triangle', 'circle', "diamond", "heart").
#' @param color Character of length 1. Color of the line. Default = "Black".
#' @param fill Character of length 1 or NA. Fill color of the polygon. If NA, the color is transparent. Default = NA.
#'
#' @export

draw_polygon <- function(shape,
                         color = "black",
                         fill  = NA) {

  # Load required packages
  require(ggplot2)

  # Shape to lower case.
  shape <- tolower(shape)

  # Which are the accepted shapes
  accepted_shapes <- c('l', 'square', 'triangle', 'circle', "diamond", "heart")

  # Check that inputs are OK
  if(!is.character(shape)) { stop("'shape' has to a character vector of length 1.") }
  else if(length(shape) != 1) { stop("'shape' has to a character vector of length 1.") }
  else if (!shape %in% accepted_shapes) { stop("'shape' has to be one of 'L', 'square', 'triangle', 'diamond', 'heart' or 'circle'.") }
  else if(length(color) != 1) { stop("'color' has to a character vector of length 1 with a valid color name, or NA.") }
  else if(length(fill) != 1) { stop("'fill' has to a character vector of length 1 with a valid color name or NA.") }
  else if(!is.character(fill) & !is.na(fill)) { stop("'fill' has to a character vector of length 1 with a valid color name or NA.") }

  # Internal function to draw the circle coordinates
  if(shape == "circle"){
    circle_coords <- function(cc=c(.5,.5), dd=1, np=100){
      r=dd/2
      tt=seq(0,2*pi, length.out=np)
      x=cc[1]+r*cos(tt)
      y=cc[2]+r*sin(tt)
      return(data.frame(x,y))
    }
  }

  # Internal function to draw the heart coordinates
  #  Taken from https://www.r-bloggers.com/2019/01/making-original-bingo-heart-theme/
  if(shape == "heart") {
    heart_coords <- function(cc=c(.5,.5), ss=.1, np=100){
      r=ss*0.5
      tt=seq(-pi,pi, length.out=np)
      x=cc[1]+16*r*(sin(tt))^3
      y=cc[2]+r*.1+r*13*cos(tt)-r*5*cos(2*tt)-r*2*cos(3*tt)-r*cos(4*tt)
      return(data.frame(x,y))
    }
  }

  # Create X and Y coordinates depending on the shape
  if(shape == "l"){ x = c(0, .5, .5, 1, 1, 0, 0); y = c(0, 0, .5, .5, 1, 1, 0); d = data.frame(x,y) }
  else if(shape == "square"){ x = c(0,1,1,0,0); y = c(0,0,1,1,0); d = data.frame(x,y) }
  else if(shape == "triangle"){ x = c(0,1,.5,0); y =c(0,0,1,0); d = data.frame(x,y) }
  else if(shape == "circle"){ d = circle_coords(cc = c(.5, .5), dd = 1) }
  else if(shape == "diamond"){ x = c(.5, .75, .5, .25, .5); y = c(0, .5, 1, .5, 0); d = data.frame(x,y) }
  else if(shape == "heart"){ d = heart_coords(cc = c(.5, .5), ss = .1)  }
  else if(shape == "hexagon"){ x = c(0,.5,1,1,.5,0,0); y = c(.25,0,.25,.75,1,.75,.25); d = data.frame(x,y)  }

  # Start ggplot depending on the shape
  g <- ggplot(d, aes(x,y)) + geom_polygon(fill = fill, color = color, show.legend = F)

  # Format with cowplot
  g <- g + theme_void()

  # Return
  return(g)

}
