#' @title Saving helper functions
#'
#' @description
#' These functions are wrappers around the ggplot2 convenience function \code{\link[ggplot2]{ggsave}}, for saving a plot.
#' They default to saving the last plot that you displayed.
#' The width is in each case predefined to have the correct width for
#' the MPI poster templates. Do adjust the height though!
#' There are versions for portrait and landscape poster templates, since these have different column widths.
#'
#' @inheritParams ggplot2::ggsave
#'
#' @usage
#' plot + theme_mpinl_poster()
#' ggsave_mpinl_poster_portrait("posterfigure_1.pdf") # The default height is
#' ggsave_mpinl_poster_portrait("posterfigure_2.pdf",height = 200, units = "mm")
#' ggsave_mpinl_poster_landscape("outputfilename_landscape.pdf")
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
#'
#' @export
ggsave_mpinl_poster_portrait <- function(filename, plot = ggplot2::last_plot(), device = NULL, path=NULL, scale = 1, width = NA, height = NA, units = c("in","cm","mm"),dpi=500,limitsize=TRUE,...){
  if(is.na(width)){
    if(units[1] == "in") {width = 14.72}
    else if(units[1] == "cm") {width = 37.3875}
    else {width = 373.875}
  } else {
    warning("This function has a default width that generates figures of the correct size for the columns of the portrait version of the MPI-NL poster template. Hope you know what you're doing!")
  }

  if(is.na(height)){
    if(units[1] == "in") {height = 6.89}
    else if(units[1] == "cm") {height = 17.5}
    else {height = 175}
  }
  ggplot2::ggsave(filename = filename,plot=plot,device = device,path = path,scale = scale,width = width,height=height,units=units,dpi=dpi,limitsize = limitsize,...)
}


#' @rdname ggsave_mpinl_poster_portrait
#' @export
ggsave_mpinl_poster_landscape <- function(filename, plot = ggplot2::last_plot(), device = NULL, path=NULL, scale = 1, width = NA, height = NA, units = c("in","cm","mm"),dpi=500,limitsize=TRUE,...){
  if(is.na(width)){
    if(units[1] == "in") {width = 14.12}
    else if(units[1] == "cm") {width = 35.8736}
    else {width = 358.736}
  } else {
    warning("This function has a default width that generates figures of the correct size for the columns of the landscape version of the MPI-NL poster template. Hope you know what you're doing!")
  }

  if(is.na(height)){
    if(units[1] == "in") {height = 6.89}
    else if(units[1] == "cm") {height = 17.5}
    else {height = 175}
  }
  ggplot2::ggsave(filename = filename,plot=plot,device = device,path = path,scale = scale,width = width,height=height,units=units,dpi=dpi,limitsize = limitsize,...)
}
