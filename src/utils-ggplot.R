library(ggplot2)
library(ggtext)
library(showtext)

# Fonts
wbw_font <- "Inter"
sysfonts::font_add_google("Inter", wbw_font)
showtext::showtext_auto()

# Colors
# https://personal.sron.nl/~pault/data/colourschemes.pdf
wbw_black <- "#231f20"
wbw_blue <- "#4477AA"
wbw_cyan <- "#66CCEE"
wbw_green <- "#228833"
wbw_yellow <- "#CCBB44"
wbw_red <- "#EE6677"
wbw_purple <- "#AA3377"

# ggplot2 theme ----------------------------------------------------------
theme_wbw <- function(
  base_size = 9,
  base_family = wbw_font
) {
  ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = ggplot2::rel(1.2),
        lineheight = -1,
        face = "bold",
        color = wbw_black,
        family = base_family
      ),
      panel.border = ggplot2::element_rect(
        color = wbw_black,
        fill = NA,
        linewidth = ggplot2::unit(0.5, "lines")
      ),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(
        color = "grey92", #"#d8d9da",
        linewidth = ggplot2::unit(0.25, "lines")
      ),
      panel.grid.minor = ggplot2::element_line(
        color = "grey92",
        linewidth = ggplot2::unit(0.25, "lines")
      ),
      panel.grid.major = ggplot2::element_line(
        color = "grey92",
        linewidth = ggplot2::unit(0.25, "lines")
      ),
      axis.ticks = ggplot2::element_line(
        color = wbw_black,
        linewidth = ggplot2::unit(0.25, "lines")
      ),
      axis.ticks.length = ggplot2::unit(-0.35, "lines"),
      axis.text = ggplot2::element_text(
        family = base_family,
        color = wbw_black,
        size = ggplot2::unit(8, "lines")
      ),
      axis.text.x.bottom = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 0.5,
          r = 0,
          b = 0.5,
          l = 0,
          unit = "lines"
        )
      ),
      axis.text.x.top = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 0.5,
          r = 0,
          b = 0.5,
          l = 0,
          unit = "lines"
        )
      ),
      axis.text.y.left = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 0,
          r = 0.5,
          b = 0,
          l = 0.3,
          unit = "lines"
        )
      ),
      axis.text.y.right = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 0,
          r = 0.5,
          b = 0,
          l = 0.5,
          unit = "lines"
        )
      ),
      axis.title = ggtext::element_markdown(
        family = base_family,
        color = wbw_black,
        size = 9,
        # size = ggplot2::unit(9, "lines")
      ),
      axis.title.x.bottom = ggplot2::element_text(
        margin = ggplot2::margin(
          t = -0.5,
          r = 0,
          b = 0.25,
          l = 0,
          unit = "lines"
        )
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 0,
          r = -0.5,
          b = 0,
          l = 0.5,
          unit = "lines"
        )
      ),
      strip.text = ggplot2::element_text(
        family = base_family,
        color = wbw_black,
        size = ggplot2::unit(9, "lines"),
        hjust = 0,
        vjust = 0.3,
        face = "bold.italic"
      ),
      strip.text.x = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 0.15,
          l = 0.15,
          b = 0.15,
          unit = "lines"
        )
      ),
      strip.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.box.margin = ggplot2::margin(
        t = -0.5,
        unit = "lines"
      ),
      legend.margin = ggplot2::margin(t = 0),
      legend.position = "bottom",
      legend.justification = "left",
      legend.key.height = ggplot2::unit(0.7, "lines"),
      legend.key.width = ggplot2::unit(1, "lines"),
      legend.spacing = ggplot2::unit(0.3, "lines"),
      plot.margin = ggplot2::margin(
        t = 0.5,
        r = 0.5,
        l = -0.25,
        b = 0.25,
        "lines"
      )
    )
}


# Save function ----------------------------------------------------------
wbw_save <-
  function(filename, plot, dpi = 1000, w = 16, h = 12, units = "cm") {
    showtext::showtext_opts(dpi = dpi)

    ggplot2::ggsave(
      filename = filename,
      plot = plot,
      device = ragg::agg_png,
      dpi = dpi,
      width = w,
      height = h,
      units = units
    )

    showtext::showtext_opts(dpi = 96)
  }

# log-transformed x and y scales -----------------------------------------
scale_y_wbw <- function(
  breaks = scales::log_breaks(),
  labels = scales::label_number(big.mark = ","),
  guide = ggplot2::guide_axis_logticks(
    long = 1.5,
    mid = 1,
    short = 0.5
  ),
  minor_breaks = scales::minor_breaks_log(detail = 5),
  ...
) {
  list(
    ggplot2::scale_y_log10(
      breaks = breaks,
      labels = labels,
      guide = guide,
      minor_breaks = minor_breaks,
      ...
    )
  )
}

scale_x_wbw <- function(
  breaks = scales::log_breaks(),
  labels = scales::label_number(big.mark = ","),
  guide = ggplot2::guide_axis_logticks(
    long = 1.5,
    mid = 1,
    short = 0.5
  ),
  minor_breaks = scales::minor_breaks_log(detail = 5),
  ...
) {
  list(
    ggplot2::scale_x_log10(
      breaks = breaks,
      labels = labels,
      guide = guide,
      minor_breaks = minor_breaks,
      ...
    )
  )
}


# Point ------------------------------------------------------------------
geom_point_wbw <-
  function(
    shape = 21,
    stroke = 0.5,
    size = 1.5,
    alpha = 0.7,
    color = wbw_black,
    ...
  ) {
    list(
      ggplot2::geom_point(
        shape = shape,
        stroke = stroke,
        size = size,
        alpha = alpha,
        color = color,
        ...
      )
    )
  }
