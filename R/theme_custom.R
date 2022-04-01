# -------------- #
# theme_custom() #
# -------------- #

#' @title theme_custom
#' @author amitjavilaventura
#'
#' @description
#' Custom theme for ggplot2 that allows to customize almost everything from the plot, the grid and the text.
#' It uses `ggpubr::theme_pubr()` and other parameters passed through `ggplot2::theme()`
#'
#' @seealso `ggpubr::theme_pubr()`
#' @seealso `ggplot2::theme()`
#'
#' @export


theme_custom <- function(legend = "none",
                         margin = T,
                         base_size = 12,
                         border = T,
                         x.text.angle = 0,
                         x.text.hjust = .5,
                         x.text.vjust = .5,
                         title.hjust = .5,
                         title.face = "bold",
                         subtitle.face = "italic",
                         title.family = "sans",
                         subtitle.family = "sans",
                         title.size = 12,
                         subtitle.size = 11,
                         axis.title.size = 10,
                         caption.size = 7,
                         caption.face = "plain",
                         caption.hjust = 1,
                         caption.family = "sans",
                         axis.title.face = "bold",
                         axis.title.familiy = "sans",
                         axis.text.family = "sans",
                         axis.text.size = 10,
                         hgrid.major = 0,
                         hgrid.minor = 0,
                         vgrid.major = 0,
                         vgrid.minor = 0,
                         grid.color = "gray50",
                         grid.type = "dashed"){

  ggpubr::theme_pubr(legend = legend, border = border, margin = margin, base_size = base_size) +
    theme(plot.title = element_text(face = title.face, hjust = title.hjust, family = title.family, size = title.size),
          plot.subtitle = element_text(face = subtitle.face, hjust = title.hjust, family = subtitle.family, size = subtitle.size),
          axis.title = element_text(face = axis.title.face, family = axis.title.familiy, size = axis.title.size),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = x.text.angle, hjust = x.text.hjust, vjust = x.text.vjust),
          axis.text   = element_text(family = axis.text.family, size = axis.text.size),
          plot.caption = element_text(face = caption.face, hjust = caption.hjust, family = caption.family, size = caption.size),
          panel.grid.major.y = element_line(colour = grid.color, size = hgrid.major, linetype = grid.type),
          panel.grid.minor.y = element_line(colour = grid.color, size = hgrid.minor, linetype = grid.type),
          panel.grid.major.x = element_line(colour = grid.color, size = vgrid.major, linetype = grid.type),
          panel.grid.minor.x = element_line(colour = grid.color, size = vgrid.minor, linetype = grid.type))
}
