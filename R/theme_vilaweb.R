#' vilaweb theme
#'
#' Apply the vilaweb look to a ggplot2-generated visualization

#' @param base_size The size of the base font
#' @param base_family The font used in the legend and title
#' @param y_comma Whether to add comma seperators every 3 digits to a continuous
#' y axis
#' @param subtitle_family The font used in the subtitle
#' @param axis_family The font used in the axes
#' @return A list of options compatible with \code{ggplot()}
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 theme_bw theme element_rect element_line element_blank element_text scale_y_continuous
#' @importFrom grid unit
#' @importFrom extrafont loadfonts
#' @importFrom scales comma
#' @export

theme_vilaweb <-
  function (base_size = 15,
            base_family = "Open Sans",
            y_comma = TRUE,
            subtitle_family = 'Open Sans',
            axis_family = 'Open Sans'){


    color_background = 'white'#  '#FFFFFF'#  '#F8F5E1'#grey(0.99)
    color_grid_major = 'grey'
    color_axis_text =  "#333333"
    color_axis_title = color_axis_text
    color = 'darkgrey'
    color_title = '#000000'
    color_subtitle = '#ff6600'
    base_size1 = base_size
    out <-
      theme_bw(base_size = base_size1) +
      theme(panel.background = element_rect(fill = color_background,
                                            color = color_background)) +
      theme(plot.background = element_rect(fill = color_background,
                                           color = color_background)) +
      theme(panel.border = element_rect(color = color_background)) +
      theme(panel.grid.major = element_line(color = adjustcolor(color_grid_major, alpha.f = 0.25),
                                            size = 0.25)) +
      theme(panel.grid.major = element_line(color = adjustcolor(color_grid_major, alpha.f = 0.4),
                                            size = 0.4)) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(legend.background = element_rect(fill = color_background)) +
      theme(legend.text = element_text(family = base_family,
                                       size = base_size * 0.7,
                                       color = color_axis_title)) +
      theme(plot.title = element_text(family = base_family,
                                      color = color_title,
                                      size = base_size * 1.2,
                                      # hjust = 4,
                                      vjust = 1.25)) +
      theme(plot.subtitle = element_text(family = subtitle_family,
                                         color = color_subtitle,
                                         size = base_size * 0.8,
                                         # hjust = -0.95,
                                         vjust = 1.25)) +
      theme(axis.text.x = element_text(family = axis_family,
                                       size = base_size * 0.7,
                                       color = color_axis_text)) +
      theme(axis.text.y = element_text(family = axis_family,
                                       size = base_size * 0.7,
                                       color = color_axis_text)) +
      theme(axis.title.x = element_text(family = axis_family,
                                        size = base_size * 0.9,
                                        color = color_axis_title,
                                        vjust = 0)) +
      theme(axis.title.y = element_text(family = axis_family,
                                        size = base_size * 0.9, color = color_axis_title, vjust = 1.25)) +
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) +
      theme(complete = TRUE) +
      # theme(legend.key = element_blank()) +
      theme(legend.title  = element_text(family = axis_family,
                                      size = base_size ,
                                      color = color_axis_text)) +
      theme(legend.position = 'bottom') +
      theme(strip.background = element_rect(color = NA, fill = NA),
            strip.text = element_text(size = base_size * 0.9,
                                      family = base_family),
            panel.spacing = unit(0, "lines"),
            panel.border = element_rect(colour = NA, fill=NA, size=0),
            plot.caption = element_text(size = base_size * 0.7,
                                        family = base_family,
                                        color = color_axis_text))

    # Commas on continous y axes
    if(y_comma){
      out <-
        list(out,
             scale_y_continuous(label = scales::comma))
    } else {
      out <- list(out)
    }
    return(out)
  }
