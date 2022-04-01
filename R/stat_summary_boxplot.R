# ---------------------- #
# stat_summary_boxplot() #
# ---------------------- #

#' @title stat_summary_boxplot
#' @author amitjavilaventura
#'
#' @description
#' Adds a boxplot layer without outliers to a ggplot. It uses the aesthetics of the `ggplot()` call.
#'
#' @seealso `ggplot2::stat_summary()`
#'
#' @param size Numeric of length 1. Passed through poistion_dodge(width=width).
#' @param width Numeric of length 1. Size of the label.
#'
#' @export

stat_summary_boxplot <- function(size = .5, width = .5){

  # Define the function to calculate the data of the boxplot.
  calc_boxplot_stat <- function(x){

    # coef for the outliers
    coef <- 1.5

    # total number
    n <- sum(!is.na(x))

    # calculate quantiles
    stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")

    # interquartile range
    iqr <- diff(stats[c(2, 4)])

    # set whiskers
    outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)

    if (any(outliers)) {
      stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
    }

    return(stats)
  }

  # Call stat_summary with the created function.
  stat_summary(fun.data = calc_boxplot_stat, geom="boxplot", size = size, width = width,  position = position_dodge(width = width), stat = "identity")
}

