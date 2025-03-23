library(ggplot2)
library(marquee)
library(showtext)
sysfonts::font_add_google("Ubuntu", "Ubuntu")
sysfonts::font_add_google("Ubuntu Condensed", "Ubuntu Condensed")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 500)

blended_white <- colorspace::lighten("#282c35", 0.5)

theme_vas3k <- function() {
  ggplot2::theme(
    ## Background
    panel.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "#282c35"),
    legend.background = ggplot2::element_rect(fill = "#282c35"),
    ## Text
    axis.text = ggplot2::element_text(
      color = "white",
      family = "Ubuntu"
    ),
    axis.title = ggplot2::element_text(
      color = "white",
      family = "Ubuntu",
      face = "bold"
    ),
    plot.subtitle = ggplot2::element_text(
      color = "white",
      family = "Ubuntu",
    ),
    plot.caption = marquee::element_marquee(
      family = "Ubuntu",
      color = "white"
    ),
    legend.text = ggplot2::element_text(
      color = "white",
      family = "Ubuntu"
    ),
    legend.title = ggplot2::element_text(
      color = "white",
      family = "Ubuntu"
    ),
    ## Lines
    ggh4x.axis.nestline = ggplot2::element_line(
      color = blended_white,
      linewidth = 0.2
    ),
    panel.grid = ggplot2::element_line(
      color = blended_white,
      linewidth = 0.3
    ),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.line.x.bottom = ggplot2::element_line(
      color = blended_white,
      linewidth = 0.4
    )
  )
}
