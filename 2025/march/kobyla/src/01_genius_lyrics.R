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


# Reticulate approach ----------------------------------------------------
library(reticulate)

py_require("lyricsgenius")
py_require("nltk")
py_require("simplemma")

nltk <- import("nltk")
lemma <- import("nltk.stem")
simplemma <- import("simplemma")
lyricsgenius <- import("lyricsgenius")

nltk$download('averaged_perceptron_tagger_rus')
nltk$download('punkt_tab')
nltk$download('wordnet')

token <- Sys.getenv("GENIUS_API_TOKEN")
genius <- lyricsgenius$Genius(token)

artist <-
  genius$search_artist(
    "Кобыла и Трупоглазые Жабы Искали Цезию, Нашли Поздно Утром Свистящего Хна (Kobyla)"
  )

songs_to_df <- function(x) {
  songs <- x$songs
  l <- length(songs)

  df <-
    tibble::tibble(
      id = integer(length = l),
      album = character(length = l),
      song = character(length = l),
      lyrics = character(length = l),
      clean_lyrics = character(length = l),
      .rows = l
    )

  album_name <- character(length = 0L)

  for (i in seq_len(l)) {
    album_name <- songs[[i]]$album$name

    df$id[i] <- i
    df$album[i] <- ifelse(
      is.null(album_name),
      NA_character_,
      album_name
    )
    df$song[i] <- songs[[i]]$title
    df$lyrics[i] <- songs[[i]]$lyrics
    df$clean_lyrics[i] <- gsub("\\[([^]]+)\\]", "", songs[[i]]$lyrics)
  }
  df
}

lyrics_lemmatise <- function(lyrics) {
  words <- nltk$word_tokenize(lyrics, language = "russian")
  words_pos <- nltk$pos_tag(words, lang = "rus")
  words_clean <-
    vapply(
      words_pos,
      FUN = \(x)
        ifelse(
          x[[2]] %in%
            c(
              "NONLEX",
              "PR",
              "CONJ",
              "S-PRO",
              "A-PRO",
              "INIT=abbr",
              "ADV-PRO",
              "PART"
            ),
          NA_character_,
          x[[1]]
        ),
      FUN.VALUE = character(1)
    )
  words_clean <- words_clean[!is.na(words_clean)]
  words_lemma <- vapply(
    words_clean,
    \(x) simplemma$lemmatize(x, lang = 'ru'),
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )

  words_lemma <- words_lemma[words_lemma != "–"]
  words_lemma
}

library(dplyr)
library(tidyr)
kobyla_df <-
  songs_to_df(artist) |>
  dplyr::group_by(id) |>
  tidyr::nest() |>
  dplyr::mutate(
    lemma_lyrics = lapply(data, \(x) lyrics_lemmatise(x$clean_lyrics))
  ) |>
  tidyr::unnest(data) |>
  dplyr::ungroup()

qs::qsave(kobyla_df, "data/kobyla_lyrics.qs")

