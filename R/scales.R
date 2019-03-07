make_shade <- function(colour){
  shade <- grDevices::col2rgb(colour) * 0.6
  grDevices::rgb(shade[1,],shade[2,],shade[3,],maxColorValue = 255)
}

make_tint <- function(colour){
  tint <- grDevices::col2rgb(colour) + ((255 - grDevices::col2rgb(colour)) * 0.5)
  grDevices::rgb(tint[1,],tint[2,],tint[3,],maxColorValue = 255)
}

mpinl_base_colors <-
  c(`cyan` = "#00786A",
    `orange` = "#ED6B06",
    `purple` = "#7F206E",
    `red` = "#AA1926",
    `blue` = "#214D9D",
    `green` = "#5FA131",
    `lightblue` = "#49A7DE",
    `yellow` = "#EEA300"
  )

mpinl_light_colors <- c(
  `light_cyan` = make_tint(mpinl_base_colors["cyan"]),
  `light_orange` = make_tint(mpinl_base_colors["orange"]),
  `light_purple` = make_tint(mpinl_base_colors["purple"]),
  `light_red` = make_tint(mpinl_base_colors["red"]),
  `light_darkblue` = make_tint(mpinl_base_colors["darkblue"]),
  `light_lightgreen` = make_tint(mpinl_base_colors["green"]),
  `light_lightblue` = make_tint(mpinl_base_colors["lightblue"]),
  `light_yellow` = make_tint(mpinl_base_colors["yellow"])
)

mpinl_dark_colors <- c(
  `dark_cyan` = make_shade(mpinl_base_colors["cyan"]),
  `dark_orange` = make_shade(mpinl_base_colors["orange"]),
  `dark_purple` = make_shade(mpinl_base_colors["purple"]),
  `dark_red` = make_shade(mpinl_base_colors["red"]),
  `dark_darkblue` = make_shade(mpinl_base_colors["darkblue"]),
  `dark_green` = make_shade(mpinl_base_colors["green"]),
  `dark_lightblue` = make_shade(mpinl_base_colors["lightblue"]),
  `dark_yellow` = make_shade(mpinl_base_colors["yellow"])
)

#' @title Colours for the MPI for Psycholinguistics ggplot2 theme
#' This object contains a named character vector of
#' colours consistent with the the MPI for Psycholinguistics
#' corporate identity.
#'
#' @export
mpinl_colours <- c(mpinl_base_colors,mpinl_light_colors,mpinl_dark_colors)

#' @rdname mpinl_colours
#' @export
mpinl_colors <- c(mpinl_base_colors,mpinl_light_colors,mpinl_dark_colors)

mpinl_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- mpinl_colors[c("cyan","orange","purple","green","red","blue","lightblue","yellow")]
  if (reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal, bias=1, ...)
  ramp <- grDevices::colorRamp(pal, ...)
  return(
    function (n)
    {
      if(n <= 8){
        as.character(mpinl_colors[1:n])
      } else {
        x <- ramp(seq.int(0, 1, length.out = n))
        if (ncol(x) == 4L)
          grDevices::rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
        else grDevices::rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
      }
    }
  )
}

#' @title Colour and fill scales for the MPI for Psycholinguistics ggplot2 theme
#' These scales contain colours that play nicely with the MPI for Psycholinguistics corporate identity.
#' Otherwise, they are interchangable with the default ggplot2 discrete colour scales.
#'
#' @usage
#'
#' scale_colour_discrete(..., h = c(0, 360) + 15, c = 100, l = 65,
#' h.start = 0, direction = 1, na.value = "grey50",
#' aesthetics = "colour")
#'
#' scale_color_discrete(..., h = c(0, 360) + 15, c = 100, l = 65,
#' h.start = 0, direction = 1, na.value = "grey50",
#' aesthetics = "colour")
#'
#' scale_fill_discrete(..., h = c(0, 360) + 15, c = 100, l = 65,
#' h.start = 0, direction = 1, na.value = "grey50",
#' aesthetics = "colour")
#'
#' @inheritParams ggplot2::scale_colour_discrete
#'
#' @export
scale_colour_discrete_mpinl <- function(palette = "main", reverse = FALSE, ...) {
  pal <- mpinl_pal(palette = palette, reverse = reverse)
  ggplot2::discrete_scale("colour", paste0("mpinl_", palette), palette = pal, ...)
}

#' @rdname scale_colour_discrete_mpinl
#' @export
scale_color_discrete_mpinl <- function(palette = "main", reverse = FALSE, ...) {
  pal <- mpinl_pal(palette = palette, reverse = reverse)
  ggplot2::discrete_scale("color", paste0("mpinl_", palette), palette = pal, ...)
}

#' @rdname scale_colour_discrete_mpinl
#' @export
scale_fill_discrete_mpinl <- function(palette = "main", reverse = FALSE, ...) {
  pal <- mpinl_pal(palette = palette, reverse = reverse)
  ggplot2::discrete_scale("fill", paste0("mpinl_", palette), palette = pal, ...)
}
