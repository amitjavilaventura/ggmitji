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
#' @param highlight Character of length 1 to 7. One of c("ABC", "ABnoC", "AnoBC", "noABC", "AnoBnoC", "noABnoC", "noAnoBC") or 'all'. If 'all', highlights all the areas. Default = c("ABC", "ABnoC", "AnoBC", "noABC", "AnoBnoC", "noABnoC", "noAnoBC").
#' @param color Character of length 1 to 7. The same length as highlight. Colors of the highlighted areas. Default =  c("red", "pink", "blue", "yellow", "green", "orange", "purple").
#' @param label Character of length 1 or NULL. Label to draw next to the Venn. If NULL, does not write any label. Default = NULL.
#' @param label.pos Character of length 1. One of c("bottom.right", "right.bottom", "bottom.left", "left.bottom", "top.right", "right.top", "top.left", "left.top"). Position of the label if 'label is not NULL. Default = 'bottom.right'
#' @param label.face Character of length 1. Fontface of the label. Default = "italic"
#' @param label.size Numeric of length 1. Size of the label. Default = 13
#' @param label.col Character of length 1. Color of the label. Default = "black"
#' @param line.col Character of length 1. Color of the line of the Venn. Default = "black"
#'
#' @export

shaded_3d_venn <- function(highlight = c("ABC", "ABnoC", "AnoBC", "noABC", "AnoBnoC", "noABnoC", "noAnoBC"),
                           color = c("red", "pink", "blue", "yellow", "green", "orange", "purple"),
                           label = NULL,
                           label.pos = "bottom.right",
                           label.face = "italic",
                           label.size = 13,
                           label.col = "black",
                           line.col = "black"){

  # load required packages
  require(polyclip)
  require(magrittr)
  require(ggplot2)
  require(cowplot)

  # Check that parameters are OK.
  if(!highlight %in% c("all", "ABC", "ABnoC", "AnoBC", "noABC", "AnoBnoC", "noABnoC", "noAnoBC")){ stop("'highlight' must be a character vector with one or many of the intersection names or 'all'. If 'all', the character must be of length 1.") }
  if(length(color) != length(highlight)){ stop("'color' must be a character vector with the same length as 'highlight'.") }
  if(!label.pos %in% c("bottom.right", "right.bottom", "bottom.left", "left.bottom", "top.right", "right.top", "top.left", "left.top")){ stop("Incorrect value for 'label.pos'.") }

  # Define internal function that will create the points of the coordinates for the circles
  circle_coords <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  # Call the function to draw the circles
  A = circle_coords(center = c(.35, .6), diameter = .65)
  B = circle_coords(center = c(.65, .6), diameter = .65)
  C = circle_coords(center = c(.5, .35), diameter = .65)

  # Find the intersections and other areas
  AB <- polyclip::polyclip(A,B)
  AC <- polyclip::polyclip(A,C)
  BC <- polyclip::polyclip(B,C)

  # Intersection of all
  ABC     <- polyclip::polyclip(A,B) %>% polyclip::polyclip(., C)

  # Intersection of two, subtracting one
  ABnoC   <- polyclip::polyclip(A,B) %>% polyclip::polyclip(., C, op = "minus")
  AnoBC   <- polyclip::polyclip(A,C) %>% polyclip::polyclip(., B, op = "minus")
  noABC   <- polyclip::polyclip(C, B) %>% polyclip::polyclip(., A, op = "minus")

  # Specific of one area
  AnoBnoC <- polyclip::polyclip(A,B, op = "minus") %>% polyclip::polyclip(., C, op = "minus")
  noABnoC <- polyclip::polyclip(B,A, op = "minus") %>% polyclip::polyclip(., C, op = "minus")
  noAnoBC <- polyclip::polyclip(C,A, op = "minus") %>% polyclip::polyclip(., B, op = "minus")

  # Put all intersections in a list
  sets <- list("ABC" = ABC, "ABnoC" = ABnoC, "AnoBC" = AnoBC, "noABC" = noABC, "AnoBnoC" = AnoBnoC, "noABnoC" = noABnoC, "noAnoBC" = noAnoBC)

  # If highlight == "all" set to all clusters
  if(highlight == "all") { highlight <- names(sets); color <- rep(color, 7)}

  # Write a dataframe with the data from each circle
  data <- data.frame(A=A, B=B, C=C)

  # Draw the circles. Line color = black. Fill = NA.
  venn <- ggplot(data) +
    geom_polygon(mapping = aes(A.x, A.y), color = line.col, fill = NA, alpha = 1, size = 0.2) +
    geom_polygon(mapping = aes(B.x, B.y), color = line.col, fill = NA, alpha = 1, size = 0.2) +
    geom_polygon(mapping = aes(C.x, C.y), color = line.col, fill = NA, alpha = 1, size = 0.2)

  # Highlight desired areas by drawing them
  for(i in 1:length(highlight)){
    set <- highlight[i]
    new_data <- data.frame(set=sets[[set]])
    venn <- venn + geom_polygon(data = new_data, aes(set.x,set.y), color = line.col, fill = color[i], alpha = 1, size = 0.2)
  }

  # Remove all the elements from the grid, axes...
  venn <- venn + cowplot::theme_nothing()

  # Add label if desired
  if(!is.null(label)){
    if(label.pos %in% c("bottom.right", "right.bottom")) { lab.x = 1; lab.y = .2 }
    else if(label.pos %in% c("bottom.left", "left.bottom")) { lab.x = 0; lab.y = .2 }
    else if(label.pos %in% c("top.right", "right.top")) { lab.x = 1; lab.y = .9 }
    else if(label.pos %in% c("top.left", "left.top")) { lab.x = 0; lab.y = .9 }
    venn <- venn + annotate(geom = "text", label = label, x = lab.x, y = lab.y, hjust = .5, size = label.size, fontface = label.face, color = label.col)
  }

  # Return venn with shaded areas
  venn
}
