library(httr2)
library(dplyr)
library(lubridate)
library(marquee)

library(ggplot2)
library(ggtext)
library(showtext)

# Fonts
rs_font <- "Inter"
sysfonts::font_add_google("Inter", rs_font)
showtext::showtext_auto()

# Colors
# https://personal.sron.nl/~pault/data/colourschemes.pdf
rs_black <- "#231f20"
rs_blue <- "#4477AA"
rs_cyan <- "#66CCEE"
rs_green <- "#228833"
rs_yellow <- "#CCBB44"
rs_red <- "#EE6677"
rs_purple <- "#AA3377"

# ggplot2 theme ----------------------------------------------------------
theme_rs <- function(
  base_size = 9,
  base_family = rs_font
) {
  ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = ggplot2::rel(1.2),
        lineheight = -1,
        face = "bold",
        color = rs_black,
        family = base_family
      ),
      panel.border = ggplot2::element_rect(
        color = rs_black,
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
        color = rs_black,
        linewidth = ggplot2::unit(0.25, "lines")
      ),
      axis.ticks.length = ggplot2::unit(-0.35, "lines"),
      axis.text = ggplot2::element_text(
        family = base_family,
        color = rs_black,
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
        color = rs_black,
        size = 9,
        # size = ggplot2::unit(9, "lines")
      ),
      axis.title.x.bottom = ggplot2::element_text(
        margin = ggplot2::margin(
          t = -0.5,
          r = 0,
          b = 0,
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
        color = rs_black,
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

ggplot2::theme_set(theme_rs())

# Save function ----------------------------------------------------------
rs_save <-
  function(
    filename,
    plot,
    dpi = 1000,
    w = 16,
    h = 12,
    units = "cm"
  ) {
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

# Parsed tags ------------------------------------------------------------
tags <- c(
  "python;shapely",
  "pyproj",
  "python;gdal",
  "geopandas",
  "rioxarray",
  "xarray",
  "r;sf",
  "r;sp",
  "r;terra",
  "r;stars",
  "r;raster",
  "r;gdal"
)

# Parsing settings -------------------------------------------------------
base_url <- "https://api.stackexchange.com/2.3/questions"
end_date <- Sys.Date()
start_date <- Sys.Date() - lubridate::years(10)
fromdate <- as.numeric(as.POSIXct(start_date, tz = "UTC"))
todate <- as.numeric(as.POSIXct(end_date, tz = "UTC"))

# Get API responses ------------------------------------------------------
results <- list()

for (tag in tags) {
  all_questions <- list()
  page <- 1
  has_more <- TRUE
  while (has_more) {
    req <- httr2::request(base_url) |>
      httr2::req_url_query(
        order = "asc",
        sort = "creation",
        tagged = tag,
        site = "gis",
        fromdate = fromdate,
        todate = todate,
        page = page,
        pagesize = 100
      )
    resp <- httr2::req_perform(req)
    data <- httr2::resp_body_json(resp)
    all_questions <- c(all_questions, data$items)
    has_more <- data$has_more
    page <- page + 1
    Sys.sleep(0.5)
  }
  # Keep only creation date and question ID
  df <- data.frame(
    creation_date = as.POSIXct(
      sapply(all_questions, `[[`, "creation_date"),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    question_id = sapply(all_questions, `[[`, "question_id")
  )
  # Add the tag
  df$tag <- tag
  results[[tag]] <- df
}


# Responses cleaning ------------------------------------------------------
# Bind together individual tag's responses
final_df <-
  results |>
  dplyr::bind_rows() |>
  dplyr::as_tibble()

# qs::qsave(final_df, file = "data/gis_se_responses_22Mar2025.qs")
# final_df <- qs::qread("data/gis_se_responses_22Mar2025.qs")

# Count unique questions per day
tag_df <-
  final_df |>
  dplyr::distinct(question_id, .keep_all = TRUE) |>
  dplyr::mutate(date = lubridate::as_date(creation_date)) |>
  dplyr::group_by(date, tag) |>
  dplyr::reframe(count = dplyr::n())

# EDA plot
tag_df |>
  dplyr::filter(!grepl("gdal", tag)) |>
  dplyr::group_by(tag) |>
  dplyr::arrange(date, .by_group = TRUE) |>
  dplyr::mutate(count = cumsum(count)) |>
  dplyr::ungroup() |>
  ggplot2::ggplot(ggplot2::aes(
    x = date,
    y = count,
    color = tag
  )) +
  ggplot2::geom_line()


# Cumulative sums --------------------------------------------------------
cumsum_df <-
  tag_df |>
  dplyr::filter(!grepl("gdal", tag)) |>
  dplyr::mutate(
    lang = dplyr::case_when(
      grepl("python;", tag) ~ "python",
      grepl("geopandas", tag) ~ "python",
      grepl("xarray", tag) ~ "python",
      grepl("pyproj", tag) ~ "python",
      grepl("sf$", tag) ~ "r",
      grepl("sp$", tag) ~ "r",
      grepl("r;", tag) ~ "r"
    )
  ) |>
  dplyr::group_by(lang) |>
  dplyr::arrange(date, .by_group = TRUE) |>
  dplyr::mutate(count = cumsum(count)) |>
  dplyr::ungroup()

# Final plot -------------------------------------------------------------
# Annotation text
md_text <-
  "# ![](https://cdn.sstatic.net/Sites/gis/Img/logo.svg)<br>
Cumulative sum of unique questions at _https://gis.stackexchange.com/_,
tagged either with {#EE6677 **Python**} ![](figures/Python-logo-notext.svg) or 
![](https://cran.r-project.org/Rlogo.svg) languages since 2015 to present

{#4477AA **R**} libraries include `sf`, `sp`, `raster`, 
`terra`, `stars` tags

{#EE6677 **Python**} libraries include `geopandas`, `xarray`, `rioxarray`, 
`PyProj`, `shapely` tags
"

md_text_ru <-
  "# ![](https://cdn.sstatic.net/Sites/gis/Img/logo.svg)<br>
Кумулятивная сумма уникальных вопросов на _https://gis.stackexchange.com/_,
с ключевыми словами {#EE6677 **Python**} ![](figures/Python-logo-notext.svg) или 
![](https://cran.r-project.org/Rlogo.svg) с 2015 по сегодня

{#4477AA **R**} тэги `sf`, `sp`, `raster`, 
`terra`, `stars`

{#EE6677 **Python**} тэги `geopandas`, `xarray`, `rioxarray`, 
`PyProj`, `shapely`
"

# Annotation style
text_box_style <-
  marquee::modify_style(
    marquee::classic_style(
      base_size = 4,
      body_font = "Inter",
      header_font = "Inter",
    ),
    "body",
    padding = marquee::skip_inherit(
      marquee::trbl(-10, 2, 2, 2)
    ),
    border_radius = 3
  )

# Highlight most significant points
max_points <-
  cumsum_df |>
  dplyr::group_by(lang) |>
  dplyr::filter(count == max(count)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    label = ifelse(
      lang == "python",
      paste0(
        "{#EE6677 **",
        count,
        "**} ![](figures/Python-logo-notext.svg)"
      ),
      paste0(
        "{#4477AA **",
        count,
        "**} ![](https://cran.r-project.org/Rlogo.svg)"
      )
    )
  )

# Plot
plt_gis <-
  cumsum_df |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = date,
      y = count,
      color = lang
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_curve(
    xend = lubridate::as_date("2024-09-01"),
    yend = 2570,
    x = max_points$date[1],
    y = max_points$count[1],
    color = "grey50",
    curvature = -0.25,
    lwd = ggplot2::rel(0.2),
    inherit.aes = FALSE
  ) +
  ggplot2::geom_curve(
    xend = lubridate::as_date("2024-12-01"),
    yend = 2250,
    x = max_points$date[2],
    y = max_points$count[2],
    color = "grey50",
    curvature = -0.25,
    lwd = ggplot2::rel(0.2),
    inherit.aes = FALSE
  ) +
  ggplot2::geom_point(
    data = max_points,
    show.legend = FALSE
  ) +
  ggplot2::annotate(
    GeomMarquee,
    label = max_points$label,
    x = lubridate::as_date(c("2025-01-10", "2025-04-01")),
    y = c(2660, 2250),
    family = "Inter",
    size = 2.6,
    hjust = "right",
    vjust = "top"
  ) +
  ggplot2::annotate(
    GeomMarquee,
    label = md_text_ru,
    x = lubridate::as_date("2014-11-01"),
    y = 2660,
    family = "Inter",
    style = text_box_style,
    size = 2.4,
    fill = colorspace::lighten("#efebe1", 0.3),
    width = 0.5,
    hjust = "left",
    vjust = "top"
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(c(0, 0.01)),
    breaks = scales::pretty_breaks(),
    labels = scales::number_format(big.mark = ","),
    limits = c(0, 2700)
  ) +
  ggplot2::scale_x_date(
    labels = scales::date_format("%Y"),
    breaks = scales::date_breaks(width = "2 years"),
    minor_breaks = scales::date_breaks("1 year")
  ) +
  ggplot2::scale_color_manual(
    values = c(rs_red, rs_blue)
  ) +
  ggplot2::labs(
    y = "**Количество уникальных вопросов**",
    x = "",
    color = "",
    caption = "![](https://www.svgrepo.com/show/364000/telegram-logo-duotone.svg) @typed_rs"
  ) +
  ggplot2::theme(
    legend.position = "none",
    plot.caption = marquee::element_marquee(
      margin = ggplot2::margin(t = -10),
      family = "Inter",
      color = "grey50"
    )
  )

# Save
rs_save(
  "figures/gis_se_trends_ru.png",
  plot = plt_gis,
  dpi = 500,
  w = 14,
  h = 11
)
