library(ollamar)

test_connection()

list_models()  


library(mall)
data("reviews")


reviews |>
  llm_sentiment(review)

# Download https://ollama.com/library/gemma3:12b
# pull("gemma3:12b")


my_prompt <- paste(
  "Answer a question.",
  "Return only the answer, no explanation",
  "Acceptable answers are 'TRUE', 'FALSE'",
  "Answer this about the following text, is this a happy customer?:"
)

reviews |>
  llm_custom(review, my_prompt)
