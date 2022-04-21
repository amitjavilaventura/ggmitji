# ---------------- #
# shaded_2d_venn() #
# ---------------- #

#' @title shaded_2d_venn
#' @author amitjavilaventura
#'
#' @description
#' Creates a blank 2D venn, searches the intersections and then draws them with the desired shaded areas in a ggplot based venn.
#' The two circles are A and B, respectively. AB is the intersection, AnoB is the area of A not overlapping with B, and noAB is the area of B not overlappin with A.
#' It is useful to inidcate that the data from a figure comes from an intersection.
#'
#' @param highlight Character of length 1 to 3. One of c("AB", "AnoB", "noAB") or 'all'. If 'all', highlights all the areas. Default = c("AB", "AnoB", "noAB").
#' @param color Character of length 1 to 3. The same length as highlight. Colors of the highlighted areas. Default =  c("red", "pink", "blue").
#' @param label Character of length 1 or NULL. Label to draw next to the Venn. If NULL, does not write any label. Default = NULL.
#' @param label.pos Character of length 1. One of c("bottom.right", "right.bottom", "bottom.centre", "centre.bottom", "bottom.center", "center.bottom", "top.centre", "centre.top", "top.center", "center.top", "bottom.left", "left.bottom", "top.right", "right.top", "top.left", "left.top"). Position of the label if 'label' is not NULL. Default = 'bottom.right'
#' @param label.face Character of length 1. Fontface of the label. Default = "italic"
#' @param label.size Numeric of length 1. Size of the label. Default = 13
#' @param label.col Character of length 1. Color of the label. Default = "black"
#' @param line.col Character of length 1. Color of the line of the venn. Default = "black"
#'
#' @export

shaded_2d_venn <- function(highlight = c("AB", "AnoB", "noAB"),
                           color = c("red", "pink", "blue"),
                           label = NULL,
                           label.pos = "bottom.right",
                           label.face = "italic",
                           label.size = 13,
                           label.col = "black",
                           line.col = "black"){

  # Load required packages
  require(polyclip)
  require(ggplot2)
  require(cowplot)

  # Check that the inputs are OK.
  if(!highlight %in% c("AB", "AnoB", "noAB", "all")){ stop("'highlight' must be a character vector with one or many of the intersection names or 'all'. If 'all', the character must be of length 1.") }
  if(length(color) != length(highlight)){ stop("'color' must be a character vector with the same length as 'highlight'.") }
  if(!label.pos %in% c("bottom.right", "right.bottom", "bottom.centre", "centre.bottom", "bottom.center", "center.bottom", "top.centre", "centre.top", "top.center", "center.top", "bottom.left", "left.bottom", "top.right", "right.top", "top.left", "left.top")){
    stop("Incorrect value for 'label.pos'.")
  }

  # Define internal function that will create the points of the coordinates for the circles
  circle_coords <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  # Call the function to draw the circles
  A = circle_coords(center = c(.35, .5), diameter = .6)
  B = circle_coords(center = c(.65, .5), diameter = .6)

  # Find coordinates of intersection
  AB <- polyclip::polyclip(A,B)

  # Find areas of A and B not overlapping with the other
  AnoB <- polyclip::polyclip(A,B, op = "minus")
  noAB <- polyclip::polyclip(B,A, op = "minus")

  # Make a list with coordinates of each areas
  sets <- list("AB" = AB,  "AnoB" = AnoB, "noAB" = noAB)

  # Dataframe with the coordinates of the circle
  data <- data.frame(A=A, B=B)

  # If highlight == "all" set to all clusters
  if(highlight == "all") { highlight <- names(sets); color <- rep(color, 7)}


  # Start ggplot with the blank venn diagram
  venn <- ggplot(data) +
    geom_polygon(mapping = aes(A.x, A.y), color ="black", fill = NA, alpha = 1, size = 0.2) +
    geom_polygon(mapping = aes(B.x, B.y), color ="black", fill = NA, alpha = 1, size = 0.2)

  # Draw the shaded areas with the desired color
  for(i in 1:length(highlight)){
    set <- highlight[i]
    new_data <- data.frame(set=sets[[set]])
    venn <- venn + geom_polygon(data = new_data, aes(set.x,set.y), color = "black", fill = color[i], alpha = 1, size = 0.2, show.legend = F)
  }

  # Remove all the elements in the plot.
  venn <- venn + theme_void()

  # Add label if desired
  if(!is.null(label)){
    if(label.pos %in% c("bottom.right", "right.bottom")) { lab.x = 1; lab.y = .2 }
    else if(label.pos %in% c("bottom.centre", "centre.bottom", "bottom.center", "center.bottom")) { lab.x = .5; lab.y = .15 }
    else if(label.pos %in% c("bottom.left", "left.bottom")) { lab.x = 0; lab.y = .2 }
    else if(label.pos %in% c("top.right", "right.top")) { lab.x = 1; lab.y = .8 }
    else if(label.pos %in% c("top.centre", "centre.top", "top.center", "center.top")) { lab.x = .5; lab.y = .85 }
    else if(label.pos %in% c("top.left", "left.top")) { lab.x = 0; lab.y = .8 }
    venn <- venn + annotate(geom = "text", label = label, x = lab.x, y = lab.y, hjust = .5, size = label.size, fontface = label.face, color = label.col)
  }

  # Return venn
  venn
}
