# -------------------- #
# stat_point_boxplot() #
# -------------------- #

#' @title stat_point_boxplot
#' @author amitjavilaventura
#'
#' @description
#' Draws a point in a boxplot to show where the mean, median, quartiles... are.
#'
#' @seealso `ggplot2::stat_summary()`
#'
#' @param stat Character of length 1. One of c('mean', 'median', 'q1', 'q2', 'q3', 'q4', 'min', 'max'). Default = "mean".
#' @param width Numeric of length 1. Passed through poistion_dodge(width=width). Default = .5 (same as in stat_summary_boxplot())
#' @param point_color Character of length 1. Color of the point. Default = "red".
#' @param point_size Numeric of length 1. Size of the point. Default = 2
#'
#' @export

stat_point_boxplot <- function(stat = "mean",
                               width = .5,
                               point_size = 2,
                               point_color = "red"){

  # Check and format the inputs.
  if(is.character(stat)){ stat <- tolower(stat) }
  else if(!stat %in% c('mean', 'median', 'q1', 'q2', 'q3', 'q4', 'min', 'max')){ stop("'statistic' must be one of 'mean', 'median', 'q1', 'q2', 'q3', 'q4', 'min' or 'max'.") }

  # Define internal function to calculate the Y, and define the label.
  calc_point_boxplot <- function(x){

    # Define Y
    if(stat == "min"){ y = fivenum(x)[1] }  ## min value of the boxplot
    else if(stat %in% c("max", "q4")){ y = fivenum(x)[5] }  ## max value of the boxplot
    else if(stat == "q1"){ y = fivenum(x)[2] }  ## q1 value of the boxplot (value 25%)
    else if(stat %in% c("median", "q2")){ y = fivenum(x)[3] }  ## median value of the boxplot (value 50%)
    else if(stat == "q3"){ y = fivenum(x)[4] }  ## q3 value of the boxplot (value 75%)
    else if(stat == "mean"){ y = mean(x) }  ## mean value of the boxplot

    # Create the dataframe with the values
    n <- data.frame(y = y)

    # Return the dataframe
    return(n)

  }

  # Call stat summary with the infor function
  stat_summary(fun.data = calc_point_boxplot, geom = "point", size = point_size, color = point_color, position = position_dodge(width = width))
}



# ------------------- #
# stat_line_boxplot() #
# ------------------- #

#' @title stat_line_boxplot
#' @author amitjavilaventura
#'
#' @description
#' Draws a line in a boxplot to show where the mean, median, quartiles... are.
#'
#' @seealso `ggplot2::stat_summary()`
#'
#' @param stat Character of length 1. One of c('mean', 'median', 'q1', 'q2', 'q3', 'q4', 'min', 'max'). Default = "mean".
#' @param width Numeric of length 1. Passed through poistion_dodge(width=width). Default = .5 (same as in stat_summary_boxplot())
#' @param height Numeric of length 1. Height of the line Default = .1
#' @param color Character of length 1. Color of the point. Default = "red".
#'
#' @export

stat_line_boxplot <- function(stat = "mean",
                              width = .5,
                              height = .1,
                              color = "red"){

  # Check and format the inputs.
  if(is.character(stat)){ stat <- tolower(stat) }
  else if(!stat %in% c('mean', 'median', 'q1', 'q2', 'q3', 'q4', 'min', 'max')){ stop("'statistic' must be one of 'mean', 'median', 'q1', 'q2', 'q3', 'q4', 'min' or 'max'.") }

  # Define internal function to calculate the Y, and define the label.
  calc_line_boxplot <- function(x){

    # Define Y
    if(stat == "min"){ y = fivenum(x)[1] }  ## min value of the boxplot
    else if(stat %in% c("max", "q4")){ y = fivenum(x)[5] }  ## max value of the boxplot
    else if(stat == "q1"){ y = fivenum(x)[2] }  ## q1 value of the boxplot (value 25%)
    else if(stat %in% c("median", "q2")){ y = fivenum(x)[3] }  ## median value of the boxplot (value 50%)
    else if(stat == "q3"){ y = fivenum(x)[4] }  ## q3 value of the boxplot (value 75%)
    else if(stat == "mean"){ y = mean(x) }  ## mean value of the boxplot

    # Create the dataframe with the values
    n <- data.frame(y=y, height=height, width=width)

    # Return the dataframe
    return(n)

  }

  # Call stat summary with the infor function
  stat_summary(fun.data = calc_line_boxplot, geom = "tile", color = color, position = position_dodge(width = width))
}
