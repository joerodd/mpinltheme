# mpinltheme
A package containing themes, scales and helper functions to produce
ggplot2 plots consistent with the MPI for Psycholinguistics corporate identity.

## Colour scales

The mpinltheme provides firstly colour scales:

`scale_colour_discrete_mpinl`

`scale_color_discrete_mpinl`

`scale_fill_discrete_mpinl`

And also a list of the theme colours, that you may wish to use if you need to encode colours in a specific way:

`mpinl_colours`

`mpinl_colors`

## Themes

The package also provides ggplot2 themes. There is a general purpose variant `theme_mpinl_general`, and variants specifically intended for poster design (`theme_mpinl_poster`) and slide design (`theme_mpinl_slide`). The difference between these variants is the font size.

## Helper functions

The package also provides helper functions equivalent to ggplot2's ggsave. These functions by default save with the correct dimensions for the poster and slide templates:

`ggsave_mpinl_poster_portrait`

`ggsave_mpinl_poster_landscape`
