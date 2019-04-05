#' mpinltheme: A package containing themes, scales and helper functions to produce
#' ggplot2 plots consistent with the MPI for Psycholinguistics corporate identity.
#'
#' @section Colour scales:
#'
#' The mpinltheme provides firstly colour scales:
#'
#' \code{\link{scale_colour_discrete_mpinl}}
#'
#' \code{\link{scale_color_discrete_mpinl}}
#'
#' \code{\link{scale_fill_discrete_mpinl}}
#'
#' And also a list of the theme colours, that you may wish to use if you need to encode colours in a specific way:
#'
#' \code{\link{mpinl_colours}}
#'
#' \code{\link{mpinl_colors}}
#'
#' @section Themes:
#'
#' The package also provides ggplot2 themes.
#' There is a general purpose variant \code{\link{theme_mpinl_general}}, and variants specifically intended for poster
#' design (\code{\link{theme_mpinl_poster}}) and slide design (\code{\link{theme_mpinl_slide}}). The difference between these
#' variants is the font size.
#'
#' @section Helper functions:
#'
#' The package also provides helper functions equivalent to ggplot2's \code{\link[ggplot2]{ggsave}}. These functions by default
#' save with the correct dimensions for the poster and slide templates:
#'
#' \code{\link{ggsave_mpinl_poster_portrait}}
#'
#' \code{\link{ggsave_mpinl_poster_landscape}}
#'
#' For more details see the poster making vignette:
#' \code{vignette("make-a-poster", package = "mpinltheme
#' ")}
#'
#' @docType package
#' @name mpinltheme
NULL


