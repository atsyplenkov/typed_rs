library(dplyr)
library(lubridate)
library(geomtextpath)
library(ggh4x)

source("src/ggplot_theme.R")

# Load data --------------------------------------------------------------
frank_energy <-
  utils::read.csv("data/frank_power.csv") |>
  dplyr::mutate(
    month = as.Date(month, format = "%d-%m-%y"),
    date = lubridate::month(month),
    date = ifelse(date == 1, 12, date - 1),
    date = factor(
      date,
      levels = seq_len(12),
      #fmt: skip
      labels = c(
        "Янв", "Фев", "Мар", "Апр", "Май", "Июн",
        "Июл", "Авг", "Сен", "Окт", "Ноя", "Дек"
      )
    ),
    date = as.character(date),
    year = dplyr::lag(lubridate::year(month), default = 2023),
  )

# Среднемесячные траты на электричество
frank_mean <- round(mean(frank_energy$usage))

# Plot -------------------------------------------------------------------
frank_plot <-
  frank_energy |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = ggh4x::weave_factors(date, year),
      y = usage
    )
  ) +
  ggplot2::geom_col(
    fill = colorspace::lighten("#1cb600", 0.3),
    alpha = 0.8,
    lwd = 0,
    width = 0.8
  ) +
  geomtextpath::geom_texthline(
    label = glue::glue("В среднем ${frank_mean} NZD"),
    yintercept = frank_mean,
    lwd = ggplot2::rel(1.5),
    lty = "dashed",
    color = colorspace::lighten("firebrick3", 0.3),
    vjust = -0.1,
    hjust = 0.85,
    family = "Ubuntu",
    fontface = "bold"
  ) +
  ggplot2::annotate(
    geom = "marquee",
    label = glue::glue(" ![](assets/frank_logo.png)"),
    x = -Inf,
    y = Inf,
    hjust = -6.5,
    vjust = -0.4
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    expand = ggplot2::expansion(mult = c(0, 0.05))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::guides(x = "axis_nested") +
  ggplot2::labs(
    x = "",
    y = "Стоимость электричества, $NZD/мес",
    title = glue::glue(
      "**Траты на электричество в Новой Зеландии в 2023–2024 гг.**"
    ),
    caption = "![](https://www.svgrepo.com/show/364000/telegram-logo-duotone.svg) @typed_rs"
  ) +
  theme_vas3k() +
  ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
  ggplot2::theme(
    plot.title = marquee::element_marquee(
      family = "Ubuntu",
      color = "white"
    ),
    plot.caption = marquee::element_marquee(
      margin = ggplot2::margin(t = -10),
      family = "Ubuntu",
      color = blended_white
    )
  )

# Save -------------------------------------------------------------------
ggplot2::ggsave(
  filename = "figures/frank_plot.png",
  plot = frank_plot,
  device = ragg::agg_png,
  dpi = 500,
  width = 3500,
  height = 2500,
  units = "px"
)
