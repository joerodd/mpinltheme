#' @title
#' MPI for Psycholinguistics ggplot2 theme, version for posters
#'
#' @description
#' Theme to produce plots that play nicely with the poster templates for the MPI for Psycholinguistics corporate identity.
#'
#' There is another version available, \code{\link{theme_mpinl_general}}, with text sizes for other purposes.
#'
#' This theme can be appended to a ggplot2 plot object.
#' The theme is intended to be used in combination with \code{\link{scale_colour_discrete_mpi_nl}} and \code{\link{scale_fill_discrete_mpi_nl}}
#' to use the recommended colour scheme. For continuous colour scales, we recommend \code{\link[viridis]{scale_color_viridis}}.
#'
#' It shouldn't be necessary to tweak the further. You might wish to use additional + theme() calls to
#' further customise the appearance of your plots, for instance to control the placement of the legend.
#'
#' @inheritParams ggplot2::theme_bw
#'
#' @usage
#' plot + scale_colour_discrete_mpinl() + theme_mpinl_poster()
#'
#' @examples
#'mtcars2 <- within(mtcars, {
#' vs <- factor(vs, labels = c("V-shaped", "Straight"))
#' am <- factor(am, labels = c("Automatic", "Manual"))
#' cyl  <- factor(cyl)
#' gear <- factor(gear)
#' })
#'
#' library(ggplot2)
#' library(mpinltheme)
#'
#' p1 <- ggplot(mtcars2) +
#'   geom_point(aes(x = wt, y = mpg, colour = gear)) +
#'     labs(title = "Fuel economy declines as weight increases",
#'          subtitle = "(1973-74)",
#'          x = "Weight (1000 lbs)",
#'          y = "Fuel economy (mpg)",
#'          colour = "Gears")
#'
#'p1 + scale_colour_discrete_mpinl() + theme_mpinl_poster()
#'ggsave_mpinl_poster_portrait("outputfilename.pdf")
#'
#' @export
theme_mpinl_poster <- function(base_size = 20, base_family = "sans", box = FALSE) {
  if(base_size != 20){
    warning("We set the default base_size to 20 to ensure consistency, hope you know what you're doing!")
  }

  if(base_family != "sans"){
    warning("We set the default base_family to sans (Usually arial) to ensure consistency, hope you know what you're doing!")
  }

  adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, ggplot2::rel(14), 0))

      # , axis.title = ggplot2::element_text(size = ggplot2::rel(1.1))
      , axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.4), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(ggplot2::rel(12), 0, 0, 0))
      , axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.4), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, ggplot2::rel(12), 0, 0))
      , axis.title.y.right = ggplot2::element_text(size = ggplot2::rel(1.4), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(12)))
      , axis.ticks.length = ggplot2::unit(ggplot2::rel(6), "points")
      , axis.text = ggplot2::element_text(size = ggplot2::rel(1))
      , axis.text.x = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(ggplot2::rel(6), 0, 0, 0))
      , axis.text.y = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, ggplot2::rel(6), 0, 0))
      , axis.text.y.right = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(6)))
      # , axis.line.x = ggplot2::element_line()
      # , axis.line.y = ggplot2::element_line()
      , legend.position = "bottom"
      , legend.title = ggplot2::element_text(size=ggplot2::rel(1.2))
      , legend.key = ggplot2::element_rect(fill = NA, color = NA)
      # , legend.key.width = ggplot2::unit(ggplot2::rel(20), "points")
      # , legend.key.height = ggplot2::unit(ggplot2::rel(20), "points")
      , legend.direction = "horizontal"
      , legend.margin = ggplot2::margin(
        t = ggplot2::rel(16)
        , r = ggplot2::rel(16)
        , b = ggplot2::rel(16)
        , l = ggplot2::rel(16)
        , unit = "points"
      )

      , panel.spacing = ggplot2::unit(ggplot2::rel(14), "points")
      , panel.grid.major.x = ggplot2::element_line(size = NA)
      , panel.grid.minor.x = ggplot2::element_line(size = NA)
      , panel.grid.major.y = ggplot2::element_line(size = NA)
      , panel.grid.minor.y = ggplot2::element_line(size = NA)

      , strip.background = ggplot2::element_rect(fill = NA, color = NA)
      , strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), margin = ggplot2::margin(0, 0, ggplot2::rel(16), 0))
      , strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(16)))
    )

  if(box) {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))
  } else {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())
  }

  adapted_theme
}

#' @title
#' MPI for Psycholinguistics ggplot2 theme, version for slides
#'
#' @description
#' Theme to produce plots that play nicely with the slide templates for the MPI for Psycholinguistics corporate identity.
#'
#' There is another version available, \code{\link{theme_mpinl_general}}, with text sizes for other purposes.
#'
#' This theme can be appended to a ggplot2 plot object.
#' The theme is intended to be used in combination with \code{\link{scale_colour_discrete_mpi_nl}} and \code{\link{scale_fill_discrete_mpi_nl}}
#' to use the recommended colour scheme. For continuous colour scales, we recommend \code{\link[viridis]{scale_color_viridis}}.
#'
#' It shouldn't be necessary to tweak the further. You might wish to use additional + theme() calls to
#' further customise the appearance of your plots, for instance to control the placement of the legend.
#'
#' @inheritParams ggplot2::theme_bw
#'
#' @usage
#' plot + scale_colour_discrete_mpinl() + theme_mpinl_slide()
#'
#' @examples
#'mtcars2 <- within(mtcars, {
#' vs <- factor(vs, labels = c("V-shaped", "Straight"))
#' am <- factor(am, labels = c("Automatic", "Manual"))
#' cyl  <- factor(cyl)
#' gear <- factor(gear)
#' })
#'
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars2) +
#'   geom_point(aes(x = wt, y = mpg, colour = gear)) +
#'     labs(title = "Fuel economy declines as weight increases",
#'          subtitle = "(1973-74)",
#'          x = "Weight (1000 lbs)",
#'          y = "Fuel economy (mpg)",
#'          colour = "Gears")
#'
#'p1 + scale_colour_discrete_mpinl() + theme_mpinl_slide()
#'
#' @export
theme_mpinl_slide <- function(base_size = 18, base_family = "ArialMT", box = FALSE) {
  if(base_size != 18){
    warning("We set the default base_size to 18 to ensure consistency, hope you know what you're doing!")
  }

  if(base_family != "ArialMT"){
    warning("We set the default base_family to ArialMT to ensure consistency, hope you know what you're doing!")
  }

  adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, ggplot2::rel(14), 0))

      # , axis.title = ggplot2::element_text(size = ggplot2::rel(1.1))
      , axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.4), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(ggplot2::rel(12), 0, 0, 0))
      , axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.4), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, ggplot2::rel(12), 0, 0))
      , axis.title.y.right = ggplot2::element_text(size = ggplot2::rel(1.4), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(12)))
      , axis.ticks.length = ggplot2::unit(ggplot2::rel(6), "points")
      , axis.text = ggplot2::element_text(size = ggplot2::rel(1))
      , axis.text.x = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(ggplot2::rel(6), 0, 0, 0))
      , axis.text.y = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, ggplot2::rel(6), 0, 0))
      , axis.text.y.right = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(6)))
      # , axis.line.x = ggplot2::element_line()
      # , axis.line.y = ggplot2::element_line()
      , legend.position = "bottom"
      , legend.title = ggplot2::element_text(size=ggplot2::rel(1.2))
      , legend.key = ggplot2::element_rect(fill = NA, color = NA)
      # , legend.key.width = ggplot2::unit(ggplot2::rel(20), "points")
      # , legend.key.height = ggplot2::unit(ggplot2::rel(20), "points")
      , legend.direction = "horizontal"
      , legend.margin = ggplot2::margin(
        t = ggplot2::rel(16)
        , r = ggplot2::rel(16)
        , b = ggplot2::rel(16)
        , l = ggplot2::rel(16)
        , unit = "points"
      )

      , panel.spacing = ggplot2::unit(ggplot2::rel(14), "points")
      , panel.grid.major.x = ggplot2::element_line(size = NA)
      , panel.grid.minor.x = ggplot2::element_line(size = NA)
      , panel.grid.major.y = ggplot2::element_line(size = NA)
      , panel.grid.minor.y = ggplot2::element_line(size = NA)

      , strip.background = ggplot2::element_rect(fill = NA, color = NA)
      , strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), margin = ggplot2::margin(0, 0, ggplot2::rel(16), 0))
      , strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(16)))
    )

  if(box) {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))
  } else {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())
  }

  adapted_theme
}


#' @title
#' MPI for Psycholinguistics ggplot2 theme, version for general use
#'
#' @description
#' Theme to produce plots that look good with the MPI for Psycholinguistics corporate identity,
#' for general use (reports etc).
#'
#' There is another version available, \code{\link{theme_mpinl_posters_slides}}, plots that play nicely with
#' the poster and slide templates for the MPI for Psycholinguistics corporate identity.
#'
#' This theme can be appended to a ggplot2 plot object.
#' The theme is intended to be used in combination with \code{\link{scale_colour_discrete_mpi_nl}} and \code{\link{scale_fill_discrete_mpi_nl}}
#' to use the recommended colour scheme. For continuous colour scales, we recommend \code{\link[viridis]{scale_color_viridis}}.
#'
#' It shouldn't be necessary to tweak the further. You might wish to use additional + theme() calls to
#' further customise the appearance of your plots, for instance to control the placement of the legend.
#'
#'
#' @usage
#'
#' plot + scale_colour_discrete_mpinl() + theme_mpinl_general()
#'
#' @inheritParams ggplot2::theme_bw
#'
#' @examples
#'mtcars2 <- within(mtcars, {
#' vs <- factor(vs, labels = c("V-shaped", "Straight"))
#' am <- factor(am, labels = c("Automatic", "Manual"))
#' cyl  <- factor(cyl)
#' gear <- factor(gear)
#' })
#'
#' library(ggplot2)
#'
#' p1 <- ggplot(mtcars2) +
#'   geom_point(aes(x = wt, y = mpg, colour = gear)) +
#'     labs(title = "Fuel economy declines as weight increases",
#'          subtitle = "(1973-74)",
#'          x = "Weight (1000 lbs)",
#'          y = "Fuel economy (mpg)",
#'          colour = "Gears")
#'
#'p1 + scale_colour_discrete_mpinl() + theme_mpinl_general()
#'
#' @export
theme_mpinl_general <- function(base_size = 10, base_family = "ArialMT", box = FALSE) {
  if(base_size != 10){
    warning("We set the default base_size to 10 to ensure consistency, hope you know what you're doing!")
  }

  if(base_family != "ArialMT"){
    warning("We set the default base_family to ArialMT to ensure consistency, hope you know what you're doing!")
  }

  adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, ggplot2::rel(14), 0))

      # , axis.title = ggplot2::element_text(size = ggplot2::rel(1.1))
      , axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.4), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(ggplot2::rel(12), 0, 0, 0))
      , axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.4), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, ggplot2::rel(12), 0, 0))
      , axis.title.y.right = ggplot2::element_text(size = ggplot2::rel(1.4), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(12)))
      , axis.ticks.length = ggplot2::unit(ggplot2::rel(6), "points")
      , axis.text = ggplot2::element_text(size = ggplot2::rel(1))
      , axis.text.x = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(ggplot2::rel(6), 0, 0, 0))
      , axis.text.y = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, ggplot2::rel(6), 0, 0))
      , axis.text.y.right = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(6)))
      # , axis.line.x = ggplot2::element_line()
      # , axis.line.y = ggplot2::element_line()
      , legend.position = "bottom"
      , legend.title = ggplot2::element_text(size=ggplot2::rel(1.2))
      , legend.key = ggplot2::element_rect(fill = NA, color = NA)
      # , legend.key.width = ggplot2::unit(ggplot2::rel(20), "points")
      # , legend.key.height = ggplot2::unit(ggplot2::rel(20), "points")
      , legend.direction = "horizontal"
      , legend.margin = ggplot2::margin(
        t = ggplot2::rel(16)
        , r = ggplot2::rel(16)
        , b = ggplot2::rel(16)
        , l = ggplot2::rel(16)
        , unit = "points"
      )

      , panel.spacing = ggplot2::unit(ggplot2::rel(14), "points")
      , panel.grid.major.x = ggplot2::element_line(size = NA)
      , panel.grid.minor.x = ggplot2::element_line(size = NA)
      , panel.grid.major.y = ggplot2::element_line(size = NA)
      , panel.grid.minor.y = ggplot2::element_line(size = NA)

      , strip.background = ggplot2::element_rect(fill = NA, color = NA)
      , strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), margin = ggplot2::margin(0, 0, ggplot2::rel(16), 0))
      , strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), margin = ggplot2::margin(0, 0, 0, ggplot2::rel(16)))
    )

  if(box) {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))
  } else {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())
  }

  adapted_theme
}
