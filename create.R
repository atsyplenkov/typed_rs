create_new_post <-
  function(name) {
    # Create folder
    date <- Sys.Date()
    year <- format(date, "%Y")
    month <- tolower(format(date, "%B"))

    post_dir <- fs::path_join(c(year, month, name))

    # Prevent from overwriting
    stopifnot(
      "Directory exists" = !fs::dir_exists(post_dir)
    )
    fs::dir_create(post_dir)

    # Create folder struct
    fs::dir_create(fs::path_join(c(post_dir, "figures")))
    fs::dir_create(fs::path_join(c(post_dir, "data")))

    # Copy dotfiles
    # Air formatter
    fs::file_copy(
      "2025/march/gis_stack_exchange/air.toml",
      fs::path_join(c(post_dir, "air.toml"))
    )
    # renv settings
    fs::file_copy(
      "2025/march/gis_stack_exchange/.Renviron",
      fs::path_join(c(post_dir, ".Renviron"))
    )
  }

create_new_post("frank_energy")
