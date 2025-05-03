library(dplyr)
library(tidyr)
library(taylor)
library(tidytext)

# Load lyrics dataset ----------------------------------------------------
df <- qs::qread("data/kobyla_lyrics.qs")

# Album dates from
# https://ru.wikipedia.org/wiki/%D0%9A%D0%BE%D0%B1%D1%8B%D0%BB%D0%B0_%D0%B8_%D1%82%D1%80%D1%83%D0%BF%D0%BE%D0%B3%D0%BB%D0%B0%D0%B7%D1%8B%D0%B5_%D0%B6%D0%B0%D0%B1%D1%8B_%D0%B8%D1%81%D0%BA%D0%B0%D0%BB%D0%B8_%D1%86%D0%B5%D0%B7%D0%B8%D1%8E,_%D0%BD%D0%B0%D1%88%D0%BB%D0%B8_%D0%BF%D0%BE%D0%B7%D0%B4%D0%BD%D0%BE_%D1%83%D1%82%D1%80%D0%BE%D0%BC_%D1%81%D0%B2%D0%B8%D1%81%D1%82%D1%8F%D1%89%D0%B5%D0%B3%D0%BE_%D0%A5%D0%BD%D0%B0#%D0%94%D0%B8%D1%81%D0%BA%D0%BE%D0%B3%D1%80%D0%B0%D1%84%D0%B8%D1%8F
# fmt: skip
album_dates <- tibble::tribble(
  ~year, ~album,
  2008, "Какаха",
  2008, "750 жа, рК.о cV",
  2009, "Калахари",
  2010, "Блевал Карлик",
  2010, "Гунявый",
  2010, "Малеев",
  2010, "Султан-Гирей Клыч",
  2010, "1988",
  2011, "Удоды",
  2011, "№10",
  2011, "Серенгети",
  2012, "Рептильки",
  2012, "Киржач",
  2013, "Тахрир",
  2013, "Саяны",
  2015, "Кобыла и Трупоглазые Жабы Искали Цезию, Нашли Поздно Утром Свистящего Хна",
  2015, "Хасавюрт",
  2015, "Великое князьство Литовское, Руское, Жомойтское",
  2018, "Единственный нормальный русский рэп",
  2018, "1917",
  2022, "Бойня",
  2022, "Поруха",
  2024, "Эллада-2510",
  2009, "Редигер",
  2011, "Камог Камог",
  2011, "Палеоген",
  2012, "Лазарь",
  2012, "Простые Люди",
  2014, "Сокольники",
  2015, "Библио Глобус",
  2017, "День Гадов",
  2017, "Сербский Деспотат",
  2013, "Рок в Махачкале",
  2015, "Меловая Резня",
  2016, "Grażyna"
)

# Clean it ---------------------------------------------------------------
df_clean <-
  df |>
  dplyr::filter(lyrics != "") |>
  # Keep only Kobyla's songs
  dplyr::filter(!grepl("Летов", album)) |>
  # tidyr::unnest(c(lemma_lyrics)) |>
  dplyr::mutate(
    id = as.integer(id),
    album = gsub("\\s*\\(.*\\)", "", album),
    track_name = gsub("\\s*\\(.*\\)", "", song),
    word = lemma_lyrics,
    .keep = "none"
  ) |>
  # Add album dates
  dplyr::left_join(album_dates, by = dplyr::join_by(album)) |>
  dplyr::mutate(
    year = dplyr::case_when(
      track_name == "День гадов" ~ 2017,
      track_name == "Меньшевистская Грузия" ~ 2012,
      track_name == "Дисс на Александра III" ~ 2017,
      .default = year
    ),
    album = ifelse(is.na(album), track_name, album)
  ) |>
  # Order Albums and songs
  mutate(
    album = fct_reorder(album, year)
  )

df_unnest <-
  df_clean |>
  tidyr::unnest(c(word)) |>
  filter(!grepl("о-о-о", word)) |>
  filter(!grepl("а-а-а", word)) |>
  # filter(!grepl("боро-", word)) |>
  filter(!word %in% c("а-а", "О", "эй", "наш")) |>
  anti_join(get_stopwords(language = "ru"))

# Train a topic model ----------------------------------------------------
lyrics_sparse <-
  df_unnest |>
  filter(track_name != "Мой любимый трек") |>
  # filter(track_name != "Бородинская панорама") |>
  count(track_name, word) |>
  # filter(n > 1) |>
  cast_sparse(track_name, word, n)

library(stm)
set.seed(123)
topic_model <-
  stm(lyrics_sparse, K = 3, verbose = FALSE)

word_topics <- tidy(topic_model, matrix = "beta")

word_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(
    aes(
      beta,
      reorder_within(term, beta, topic),
      fill = topic
    )
  ) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y", ncol = 2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)

song_topics <- tidy(
  topic_model,
  matrix = "gamma",
  document_names = rownames(lyrics_sparse)
)
song_topics


song_topics |>
  left_join(
    select(df_clean, album, track_name, year),
    by = join_by(document == track_name)
  ) |>
  mutate(
    track_name = fct_reorder(document, year),
    topic = factor(topic)
  ) |>
  ggplot(aes(gamma, topic, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(track_name), nrow = 6) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = expression(gamma), y = "Topic")


lyrics_gamma <- tidy(
  topic_model,
  matrix = "gamma",
  document_names = rownames(lyrics_sparse)
)

library(ggplot2)
library(forcats)
lyrics_gamma |>
  left_join(
    df_clean |>
      select(album, document = track_name)
  ) |>
  mutate(topic = factor(topic)) |>
  group_by(album, topic) |>
  count()
ggplot2::ggplot(
  aes(x = topic, y = album, fill = topic)
) +
  geom_col(position = position_stack())


effects <-
  estimateEffect(
    1:4 ~ album,
    topic_model,
    df_clean %>%
      distinct(track_name, album) %>%
      arrange(track_name)
  )


# Find best K ------------------------------------------------------------
# https://juliasilge.com/blog/evaluating-stm/
library(purrr)

many_models <-
  data_frame(K = c(4, 6, 8, 10, 12, 14)) %>%
  mutate(
    topic_model = map(
      K,
      ~ stm(lyrics_sparse, K = ., verbose = FALSE)
    )
  )


heldout <- make.heldout(lyrics_sparse)

k_result <- many_models %>%
  mutate(
    exclusivity = map(topic_model, exclusivity),
    semantic_coherence = map(
      topic_model,
      semanticCoherence,
      lyrics_sparse
    ),
    eval_heldout = map(topic_model, eval.heldout, heldout$missing),
    residual = map(topic_model, checkResiduals, lyrics_sparse),
    bound = map_dbl(
      topic_model,
      function(x) max(x$convergence$bound)
    ),
    lfact = map_dbl(
      topic_model,
      function(x) lfactorial(x$settings$dim$K)
    ),
    lbound = bound + lfact,
    iterations = map_dbl(
      topic_model,
      function(x) length(x$convergence$bound)
    )
  )

k_result


k_result %>%
  transmute(
    K,
    `Lower bound` = lbound,
    Residuals = map_dbl(residual, "dispersion"),
    `Semantic coherence` = map_dbl(semantic_coherence, mean),
    `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")
  ) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(
    x = "K (number of topics)",
    y = NULL,
    title = "Model diagnostics by number of topics",
    subtitle = "These diagnostics indicate that a good number of topics would be around 60"
  )


k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(12, 8)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    x = "Semantic coherence",
    y = "Exclusivity",
    title = "Comparing exclusivity and semantic coherence",
    subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity"
  )

topic_model <- k_result %>%
  filter(K == 12) %>%
  pull(topic_model) %>%
  .[[1]]


# Best K seems to be 8 ---------------------------------------------------
set.seed(123)
topic_model <-
  stm(lyrics_sparse, K = 8, verbose = FALSE)

td_beta <- tidy(topic_model)
td_gamma <- tidy(
  topic_model,
  matrix = "gamma",
  document_names = rownames(lyrics_sparse)
)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>%
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(
    topic = paste0("Topic ", topic),
    topic = reorder(topic, gamma)
  )

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(
    aes(topic, gamma, label = terms, fill = topic)
  ) +
  geom_col(show.legend = FALSE) +
  geom_text(
    hjust = 0,
    nudge_y = 0.0005,
    size = 3,
    family = "IBMPlexSans"
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 0.50),
    labels = scales::percent_format()
  ) +
  theme(
    plot.title = element_text(size = 16, family = "IBMPlexSans-Bold"),
    plot.subtitle = element_text(size = 13)
  ) +
  labs(
    x = NULL,
    y = expression(gamma),
    title = "Top 20 topics by prevalence in the Hacker News corpus",
    subtitle = "With the top words that contribute to each topic"
  )

song_topics <- tidy(
  topic_model,
  matrix = "gamma",
  document_names = rownames(lyrics_sparse)
)

song_topics |>
  left_join(
    select(df_clean, album, track_name, year),
    by = join_by(document == track_name)
  ) |>
  mutate(
    track_name = fct_reorder(document, year),
    topic = factor(topic)
  ) |>
  ggplot(aes(gamma, topic, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(track_name), nrow = 6) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = expression(gamma), y = "Topic")


lyrics_gamma <- tidy(
  topic_model,
  matrix = "gamma",
  document_names = rownames(lyrics_sparse)
)

lyrics_gamma |>
  left_join(
    df_clean |>
      select(album, document = track_name)
  ) |>
  mutate(topic = factor(topic)) |>
  ggplot(aes(gamma, topic, fill = topic)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  facet_wrap(vars(album)) +
  labs(x = expression(gamma))


lyrics_gamma |>
  rename(track_name = document) |>
  left_join(
    df_clean |>
      select(album, track_name, year)
  ) |>
  group_by(album, track_name) |>
  filter(gamma == max(gamma)) |>
  group_by(album, topic) |>
  count() |>
  group_by(album) |>
  mutate(pct = n / sum(n)) |>
  ggplot() +
  geom_col(
    aes(x = pct, y = album, fill = as_factor(topic)),
    position = position_stack(reverse = F),
    width = .3
  ) +
  scale_fill_brewer(name = "", palette = "Set1") +
  theme(legend.position = "bottom")


lyrics_gamma |>
  rename(track_name = document) |>
  left_join(
    df_clean |>
      select(album, track_name, year)
  ) |>
  group_by(album, track_name) |>
  filter(gamma == max(gamma)) |>
  mutate(topic = as_factor(topic)) |>
  ggplot(aes(y = album, fill = topic, group = topic)) +
  geom_bar(position = position_dodge()) +
  scale_fill_brewer(name = "", palette = "Set1") +
  theme(legend.position = "bottom")
