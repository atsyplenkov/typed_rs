library(httr2)
library(tibble)
library(dplyr)
library(rvest)

get_genius_artist_songs <- function(
  artist_id = 1825204,
  token,
  per_page = 50
) {
  songs_list <- list()
  page <- 1

  # Loop through pages until no more songs are returned
  repeat {
    # Build the API request URL with query parameters
    req <- request(paste0(
      "https://api.genius.com/artists/",
      artist_id,
      "/songs"
    )) %>%
      req_headers(Authorization = paste("Bearer", token)) %>%
      req_url_query(per_page = per_page, page = page)

    # Perform the request using httr2
    resp <- req %>% req_perform()
    data <- resp %>% resp_body_json()

    # Break the loop if no songs are returned
    if (length(data$response$songs) == 0) break

    songs_list <- c(songs_list, data$response$songs)
    page <- page + 1
  }

  # Extract details into a tibble.
  # Note: the album info might be missing so we use conditional extraction.
  songs_df <- tibble(
    album_year = sapply(songs_list, function(x) {
      if (!is.null(x$release_date) && nchar(x$release_date) >= 4) {
        substr(x$release_date, 1, 4)
      } else {
        NA
      }
    }),
    album_name = sapply(songs_list, function(x) {
      if (!is.null(x$album) && !is.null(x$album$name)) {
        x$album$name
      } else {
        NA
      }
    }),
    song_name = sapply(songs_list, function(x) x$title),
    song_url = sapply(songs_list, function(x) x$url)
  )

  # Define a helper function to scrape lyrics from a song page
  get_lyrics <- function(url) {
    tryCatch(
      {
        # Read the HTML page of the song
        page <- read_html(url)
        # First attempt: older page structure using a .lyrics container
        lyrics <- page %>%
          html_node(".lyrics") %>%
          html_text(trim = TRUE)
        # If no lyrics found, try the newer structure with divs that have a class starting with 'Lyrics__Container'
        if (is.na(lyrics) || length(lyrics) == 0) {
          lyrics <- page %>%
            html_nodes("div[class^='Lyrics__Container']") %>%
            html_text(trim = TRUE) %>%
            paste(collapse = "\n")
        }
        lyrics
      },
      error = function(e) NA
    )
  }

  # Scrape lyrics for each song (this may take some time)
  songs_df <- songs_df %>%
    rowwise() %>%
    mutate(song_lyrics = get_lyrics(song_url)) %>%
    ungroup() %>%
    select(album_year, album_name, song_name, song_lyrics)

  return(songs_df)
}


df <- get_genius_artist_songs(token = Sys.getenv("GENIUS_API_TOKEN"))

df$song_name


req <- request(paste0(
  "https://api.genius.com/artists/",
  1825204,
  "/songs"
)) |>
  req_headers(Authorization = paste("Bearer", token)) |>
  req_url_query(per_page = 100, page = page)

# Perform the request using httr2
resp <- req %>% req_perform()
data <- resp %>% resp_body_json()


renv::install("reticulate")

library(reticulate)

py_require("lyricsgenius")

token <- Sys.getenv("GENIUS_API_TOKEN")
lyricsgenius <- import("lyricsgenius")
genius <- lyricsgenius$Genius(token)

artist <- genius$search_artist(
  "Кобыла и Трупоглазые Жабы Искали Цезию, Нашли Поздно Утром Свистящего Хна (Kobyla)"
)
artist$save_lyrics(filename = "lyrics.json", ensure_ascii = FALSE)
