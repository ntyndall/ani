#' @title Plot Theme
#'
#' @description Holds the theme for googlemaps plots
#'
#' @param fontFamily A character string that defines the font
#'  family to be used for the axis / title / legend etc.
#'
#' @export


plot_theme <- function(fontFamily = 'Purisa') { # nocov start
  return(
    ggplot2::theme(
      plot.title =  ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = '#353535',
        size = 30,
        hjust = 0.5
      ),
      legend.title = ggplot2::element_text(
        family = fontFamily,
        face =  'plain',
        colour = 'black',
        size = 20
      ),
      legend.text = ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = "black",
        size = 14
      ),
      legend.key = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        family = fontFamily,
        face =  'plain',
        colour = 'black',
        size = 12,
        angle = 20,
        hjust = 1
      ),
      axis.text.y = ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = 'black',
        size = 12
      ),
      panel.border = ggplot2::element_rect(
        linetype = "dashed",
        colour = 'black',
        fill = NA,
        size = 0.8
      ),
      text = ggplot2::element_text(
        family = fontFamily,
        face = 'plain',
        colour = 'black',
        size = 20
      ),
      panel.background = ggplot2::element_rect(
        fill = 'white'
      ),
      plot.background = ggplot2::element_rect(
        fill = "white"
      ),
      legend.background = ggplot2::element_rect(
        fill = "white",
        size = 0.8,
        linetype = "dashed",
        colour = "black"
      )
    )
  )
} # nocov end

