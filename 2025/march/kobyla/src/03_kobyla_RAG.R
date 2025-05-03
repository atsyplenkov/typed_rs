library(dplyr)
library(qs)
library(ragnar)

# Load lyrics dataset ----------------------------------------------------
df <-
  qs::qread("data/kobyla_lyrics.qs") |>
  filter(clean_lyrics != "")

# Download model ---------------------------------------------------------
embedding_model <- "evilfreelancer/enbeddrus:latest"
# ollamar::pull(embedding_model)
# ollamar::list_models()

# RAG lyrics -------------------------------------------------------------
# See https://ragnar.tidyverse.org/

# Create store
store_location <- "data/kobyla.ragnar.duckdb"

store <- ragnar_store_create(
  store_location,
  embed = \(x)
    ragnar::embed_ollama(x, model = "evilfreelancer/enbeddrus:latest")
)

# Ingest lyrics
# TODO:
# Try first creating a one md document of all songs and
# them chunking it by Ragnar.
for (i in seq_along(df$id)) {
  chunks <-
    df |>
    slice(i) |>
    dplyr::mutate(
      text = glue::glue(
        r"---(
        # Песни группы "Кобыла и трупоглазые жабы искали цезию, нашли поздно утром свистящего Хна"
        Название: {song}
        Альбом: {album}
        Текст: {clean_lyrics}
    
        )---"
      ),
      .keep = "none"
    )

  ragnar_store_insert(store, chunks)
}

ragnar_store_build_index(store)

# Connect -----------------------------------------------------------------
index <- ragnar_store_connect(store_location, read_only = TRUE)

text <- "посевной"

embedding_near_chunks <-
  ragnar_retrieve_bm25(index, text, top_k = 3)
embedding_near_chunks

embedding_near_chunks$text[1] |> cat(sep = "\n~~~~~~~~\n")

# Ask --------------------------------------------------------------------
system_prompt <-
  stringr::str_squish(
    r"--(
      Role Description
      You are a high-level, experienced analyst with deep expertise in 
      technology, AI, and innovation. Your job is not to summarize earnings 
      mechanically, but to extract meaningful insight. Think like a 
      strategist and storyteller.
      
      Tone & Format:
      • Clear, analytical, and engaging — written for intelligent readers, not just finance professionals
      • No hard word limit, but aim for brevity and clarity
      • Format for ease of reading (e.g., use headers, spacing, and short paragraphs)
      
      Approach:
      Do not rush. Prioritize clarity of thinking and depth of 
      analysis over speed. Take time to connect dots, infer 
      strategy, and deliver reports that are both useful and 
      forward-looking.)--"
  )

# ollamar::list_models()
chat <- ellmer::chat_ollama(model = "qwen3")

ragnar_register_tool_retrieve(
  chat,
  index,
  store_description = "Тексты песен группы 'Кобыла и трупоглазые жабы искали цезию, нашли поздно утром свистящего Хна'"
)

chat$chat(
  "Проанализируй тексты песен группы 'Кобыла и трупоглазые жабы искали цезию, нашли поздно утром свистящего Хна' и выдели четыре основные темы их творчества. Скажи сколько песен ты проанализировал. Ответь на русском."
)
