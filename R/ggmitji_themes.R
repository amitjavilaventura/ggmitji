# -------------- #
# theme_custom() #
# -------------- #

#' @title theme_clean
#' @author amitjavilaventura
#'
#' @description
#' Custom theme for ggplot2 that allows to customize almost everything from the plot, the grid and the text.
#' It uses other parameters passed through `ggplot2::theme()`
#'
#' @seealso `ggplot2::theme()`
#'
#' @export
theme_clean <- function(legend="right",
                        title.face="italic", title.size=10, title.color="black",
                        subtitle.face="plain", subtitle.size=9, subtitle.color="black",
                        caption.face="plain", caption.size=7, caption.color="black",
                        strip.text.face="plain", strip.text.size=8, strip.text.color="black",
                        axis.text.face="plain", axis.text.size=8, axis.text.color="black",
                        x.text.angle=90,
                        clean.x.axis=F, remove.x.axis=F, clean.y.axis=T, remove.y.axis=F,
                        add.h.grid=T, add.v.grid=F){

  if(x.text.angle == 90) { x.text.hjust=1; x.text.vjust=.5 } else if( x.text.angle == 0) { x.text.hjust=.5; x.text.vjust=1 } else { x.text.hjust=1; x.text.vjust=1 }

  t <- theme_classic() +
    theme(legend.position = legend, legend.title = element_blank(),
          plot.title = element_text(hjust=.5, face=title.face, size=title.size, color=title.color),
          plot.subtitle = element_text(hjust=.5, face=subtitle.face, size=subtitle.size, color=subtitle.color),
          strip.background = element_blank(), strip.text = element_text(face=strip.text.face, size=strip.text.size, color=strip.text.color),
          axis.text.x = element_text(angle=x.text.angle, hjust=x.text.hjust, vjust=x.text.vjust),
          axis.text = element_text(face=axis.text.face, size=axis.text.size, color=axis.text.color),
          axis.line = element_line(color="black", linewidth = .2))



  if(clean.x.axis) { t <- t + theme(axis.line.x=element_blank(), axis.ticks.x=element_blank()) }
  if(clean.y.axis) { t <- t + theme(axis.line.y=element_blank(), axis.ticks.y=element_blank()) }
  if(remove.x.axis) { t <- t +  theme(axis.line.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.text.x=element_blank()) }
  if(remove.y.axis) { t <- t +  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank()) }

  if(add.h.grid) { t <- t + theme(panel.grid.major.y = element_line(color="gray50", linewidth=.3, linetype="dashed"))}
  if(add.v.grid) { t <- t + theme(panel.grid.major.x = element_line(color="gray50", linewidth=.3, linetype="dashed"))}

  t
}

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
                         grid.type = "dashed",
                         strip.text.size = 9,
                         strip.text.face = "plain"){

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
          panel.grid.minor.x = element_line(colour = grid.color, size = vgrid.minor, linetype = grid.type),
          strip.text = element_text(size = strip.text.size, face = strip.text.face))
}


# -------------- #
# theme_ggvenn() #
# -------------- #

#' @title theme_ggvenn
#' @author amitjavilaventura
#'
#' @description
#' Custom theme for ggplot2 that allows to customize several things from a plot. It's specially designed for plots generated by `ggvenn::ggvenn()`
#' It uses `ggplot2::theme()`
#'
#' @seealso `ggplot2::theme()`
#' @seealso `ggvenn::ggvenn()`
#'
#' @export

theme_ggvenn <- function(title.hjust = .5,
                         title.face = "plain",
                         subtitle.face = "italic",
                         title.family = "sans",
                         subtitle.family = "sans",
                         caption.face = "plain",
                         caption.hjust = 1,
                         caption.family = "sans"){

  theme(plot.title = element_text(face = title.face, hjust = title.hjust, family = title.family),
        plot.subtitle = element_text(face = subtitle.face, hjust = title.hjust, family = subtitle.family),
        plot.caption = element_text(face = caption.face, hjust = caption.hjust, family = caption.family))
}


# -------------- #
# remove_axis()  #
# -------------- #

#' @title remove_axis
#' @author amitjavilaventura
#'
#' @description
#' Remove axes in gggplots.
#'
#' @seealso `ggplot2::theme()`
#'
#' @export

remove_axis  <- function(){ theme(axis.title = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank()) }

# ---------------- #
# remove_x_axis()  #
# ---------------- #

#' @title remove_x_axis
#' @author amitjavilaventura
#'
#' @description
#' Remove X axis in gggplots.
#'
#' @seealso `ggplot2::theme()`
#'
#' @export

remove_x_axis <- function(){ theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank()) }

# ---------------- #
# remove_y_axis()  #
# ---------------- #

#' @title remove_y_axis
#' @author amitjavilaventura
#'
#' @description
#' Remove Y axis in gggplots.
#'
#' @seealso `ggplot2::theme()`
#'
#' @export

remove_y_axis <- function(){ theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) }

# ---------------- #
# add_border()     #
# ---------------- #

#' @title add_border
#' @author amitjavilaventura
#'
#' @description
#' Draws the panel border and, optionally, adds margin.
#'
#' @seealso `ggplot2::theme()`
#'
#' @export

add_border <- function(color = "black",
                       size = 1,
                       linetype = 1,
                       margin = 4){
  theme(plot.margin = margin(rep(margin,4)),
        panel.border = element_rect(fill = NA, color = color, size = size, linetype = linetype))
}

# ---------------- #
# add_grid()       #
# ---------------- #

#' @title add_grid
#' @author amitjavilaventura
#'
#' @description
#' Draws the panel grids.
#'
#' @seealso `ggplot2::theme()`
#'
#' @export

add_grid <- function(hgrid.major = .5,
                     hgrid.minor = 0,
                     vgrid.major = .5,
                     vgrid.minor = 0,
                     grid.color  = "gray50",
                     grid.type   = "dashed"){

  theme(panel.grid.major.y = element_line(colour = grid.color, size = hgrid.major, linetype = grid.type),
        panel.grid.minor.y = element_line(colour = grid.color, size = hgrid.minor, linetype = grid.type),
        panel.grid.major.x = element_line(colour = grid.color, size = vgrid.major, linetype = grid.type),
        panel.grid.minor.x = element_line(colour = grid.color, size = vgrid.minor, linetype = grid.type))

}

# ---------------- #
# change_bg()      #
# ---------------- #

#' @title change_bg
#' @author amitjavilaventura
#'
#' @description
#' Removes the panel background, including the grids, making it transparent. Alternatively, can be used to change the background color and the border of the panel.
#'
#' @seealso `ggplot2::theme()`
#'
#' @export

ch_panel_bg <- function(fill = NA,
                        color = NULL,
                        size = 1,
                        linetype = 1,
                        grid = F) {
  if(!is.null(color) && !is.na(color)) { t <- theme(panel.background = element_rect(fill = fill, color = color, size = size, linetype = linetype)) }
  else { t <- theme(panel.background = element_rect(fill = fill)) }

  if(!grid) { t <- t + theme(panel.grid = element_blank()) }
  t
}

# ----------------- #
# change_plot_bg()  #
# ----------------- #

#' @title change_plot_bg
#' @author amitjavilaventura
#'
#' @description
#' Removes the plot background, making it transparent. Alternatively, can be used to change the background color and the border of the whole plot.
#'
#' @seealso `ggplot2::theme()`
#'
#' @export

ch_plot_bg <- function(fill = NA,
                       color = NULL,
                       size = 1,
                       linetype = 1,
                       grid = F) {
  if(!is.null(color) && !is.na(color)) { t <- theme(plot.background = element_rect(fill = fill, color = color, size = size, linetype = linetype)) }
  else { t <- theme(plot.background = element_rect(fill = fill)) }
  t
}

# ----------------- #
# rm_strips()       #
# ----------------- #

#' @title rm_strips
#' @author amitjavilaventura
#'
#' @description
#' Removes the strips of plot facets.
#'
#' @seealso `ggplot2::theme()`
#'
#' @export

rm_strips <- function() { theme(strip.background = element_blank(), strip.text = element_blank()) }

# ----------------- #
# rm_strips_x()     #
# ----------------- #

#' @title rm_strips_x
#' @author amitjavilaventura
#'
#' @description
#' Removes the strips of plot facets (only X axis).
#'
#' @seealso `ggplot2::theme()`
#'
#' @export
#' @rdname rm_strips
rm_strips_x <- function() { theme(strip.background.x = element_blank(), strip.text.x = element_blank()) }

# ----------------- #
# rm_strips_y()     #
# ----------------- #

#' @title rm_strips_y
#' @author amitjavilaventura
#'
#' @description
#' Removes the strips of plot facets (only Y axis).
#'
#' @seealso `ggplot2::theme()`
#'
#' @export
#' @rdname rm_strips
rm_strips_y <- function() { theme(strip.background.y = element_blank(), strip.text.y = element_blank()) }

# ----------------- #
# ch_strips()       #
# ----------------- #

#' @title ch_strips
#' @author amitjavilaventura
#'
#' @description
#' Change the theme of the strips
#'
#' @param bg_fill Character of length 1. Color of the background of the strip. Default = "white"
#' @param bg_color Character of length 1. Color of the line of the strip. Default = "black"
#' @param bg_linetype Character of length 1. Line type of the outer line of the strip. Default = "solid"
#' @param text_face Character of length 1. Face of the strip text. Default = "plain"
#' @param text_color Character of length 1. Color of the strip text. Default = "black"
#' @param text_size Numeric of length 1. Size of the strip text. Default = 9
#'
#' @seealso `ggplot2::theme()`
#'
#' @export
ch_strips <- function(bg_fill     = "white",
                      bg_color    = "black",
                      bg_linetype = "solid",
                      text_face   = "plain",
                      text_color  = "black",
                      text_size   = 9) {
  theme(strip.background = element_rect(fill = bg_fill, colour = bg_color, linetype = bg_linetype),
        strip.text       = element_text(face = text_face, colour = text_color, size = text_size))
}

# ----------------- #
# ch_strips_x()     #
# ----------------- #

#' @title ch_strips_x
#' @author amitjavilaventura
#'
#' @description
#' Change the theme of the strips (only X axis strips).
#'
#' @param bg_fill Character of length 1. Color of the background of the strip. Default = "white"
#' @param bg_color Character of length 1. Color of the line of the strip. Default = "black"
#' @param bg_linetype Character of length 1. Line type of the outer line of the strip. Default = "solid"
#' @param text_face Character of length 1. Face of the strip text. Default = "plain"
#' @param text_color Character of length 1. Color of the strip text. Default = "black"
#' @param text_size Numeric of length 1. Size of the strip text. Default = 9
#'
#' @seealso `ggplot2::theme()`
#'
#' @export
#' @rdname ch_strips
ch_strips_x <- function(bg_fill     = "white",
                        bg_color    = "black",
                        bg_linetype = "solid",
                        text_face   = "plain",
                        text_color  = "black",
                        text_size   = 9) {
  theme(strip.background.x = element_rect(fill = bg_fill, colour = bg_color, linetype = bg_linetype),
        strip.text.x       = element_text(face = text_face, colour = text_color, size = text_size))
}

# ----------------- #
# ch_strips_y()     #
# ----------------- #

#' @title ch_strips_y
#' @author amitjavilaventura
#'
#' @description
#' Change the theme of the strips (only Y axis strips).
#'
#' @param bg_fill Character of length 1. Color of the background of the strip. Default = "white"
#' @param bg_color Character of length 1. Color of the line of the strip. Default = "black"
#' @param bg_linetype Character of length 1. Line type of the outer line of the strip. Default = "solid"
#' @param text_face Character of length 1. Face of the strip text. Default = "plain"
#' @param text_color Character of length 1. Color of the strip text. Default = "black"
#' @param text_size Numeric of length 1. Size of the strip text. Default = 9
#'
#' @seealso `ggplot2::theme()`
#'
#' @export
#' @rdname ch_strips
ch_strips_y <- function(bg_fill     = "white",
                        bg_color    = "black",
                        bg_linetype = "solid",
                        text_face   = "plain",
                        text_color  = "black",
                        text_size   = 9) {
  theme(strip.background.y = element_rect(fill = bg_fill, colour = bg_color, linetype = bg_linetype),
        strip.text.y       = element_text(face = text_face, colour = text_color, size = text_size))
}
