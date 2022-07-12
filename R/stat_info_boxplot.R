# ------------------- #
# stat_info_boxplot() #
# ------------------- #

#' @title stat_info_boxplot
#' @author amitjavilaventura
#'
#' @description
#' Adds information in a boxplot about the number, mean, median or standard deviation for each group
#'
#' @seealso `ggplot2::stat_summary()`
#'
#' @param statistic Character of length 1. One of c("n", "mean", "median", "sd", 'sum'). Default = "n".
#' @param label Character of length 1. The label to show with the statistic, e.g. "N=". Default = "".
#' @param y Character or numeric of length 1. If character, one of c("upper", "lower", "min", "max", "q1", "q2", "median", "q3"). If numeric, the Y coordinates of the label. Default = 0.
#' @param width Numeric of length 1. Passed through poistion_dodge(width=width).
#' @param text_color Character of length 1. Color of the label.
#' @param text_size Numeric of length 1. Size of the label.
#'
#' @export

stat_info_boxplot <- function(statistic = "n",
                              label = "",
                              y = 0,
                              text_size = 3,
                              text_color = "black",
                              width = .5){

  # Check and format the inputs.
  if(is.character(y)){ y <- tolower(y) }
  if(is.character(statistic)){ statistic <- tolower(statistic) }
  else if(!statistic %in% c('n', 'mean', 'median', 'sd', 'sum', 'min', 'max')) { stop("'statistic' must be one of 'n', 'mean', 'median', 'sd', 'sum', 'min', 'max'.") }

  # Define internal function to calculate the Y, and define the label.
  calc_info_boxplot <- function(x){

    # Define Y
    if(y == "upper"){ y = mean(fivenum(x)[3:4]) } ## upper part of the box
    else if(y == "lower"){ y = mean(fivenum(x)[2:3]) } ## lower part of the box
    else if(y == "min"){ y = fivenum(x)[1] }  ## min value of the boxplot
    else if(y == "max"){ y = fivenum(x)[5] }  ## max value of the boxplot
    else if(y == "q1"){ y = fivenum(x)[2] }  ## q1 value of the boxplot (value 25%)
    else if(y %in% c("median", "q2")){ y = fivenum(x)[3] }  ## median value of the boxplot (value 50%)
    else if(y == "q3"){ y = fivenum(x)[4] }  ## q3 value of the boxplot (value 75%)
    else if(is.numeric(y)) { y <- y }

    # Remove NAs
    x <- na.omit(x)

    # Define the label with the corresponding statistic
    if(statistic == "n"){ label = paste0(label, length(x), sep = "") }
    else if(statistic == "mean"){ label = paste0(label, round(mean(x), digits = 2), sep = "")}
    else if(statistic == "median"){ label = paste0(label, round(median(x), digits = 2), sep = "")}
    else if(statistic == "sd"){ label = paste0(label, round(sd(x), digits = 2), sep = "")}
    else if(statistic == "sum"){ label = paste0(label, round(sum(x), digits = 2), sep = "")}
    else if(statistic == "min"){ label = paste0(label, round(min(x), digits = 2), sep = "")}
    else if(statistic == "max"){ label = paste0(label, round(max(x), digits = 2), sep = "")}

    # Create the dataframe with the values
    n <- data.frame(y = y, label = label)

    # Return the dataframe
    return(n)

  }

  # Call stat summary with the infor function
  stat_summary(fun.data = calc_info_boxplot, geom = "text", size = text_size, color = text_color, position = position_dodge(width = width))
}
