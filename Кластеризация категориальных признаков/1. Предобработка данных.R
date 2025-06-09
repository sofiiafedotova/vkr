library(officer)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(purrr)
library(tibble)
library(readxl)
library(writexl)

# прочитываю док Основного корпуса
files <- list.files(path = ".", pattern = "docx") 

doc <- read_docx(files[2])
content <- docx_summary(doc)

# убираю лишние колонки, пустые строки и строки с текстом внутри [] (высказывания интервьюера)
content_clean <- content %>% 
  select(text) %>% 
  filter(!text == "") %>% 
  filter(!text == "<...>") %>% 
  filter(!str_detect(text, "^\\[.*?\\]"))

# присваиваю id на основе строки "Место событий" (лаг - новый текст начинается после этой строки)
# переставляю колонку id на первое место
content_grouped <- content_clean %>%
  mutate(new_text = lag(str_detect(text, "^Место событий:"), default = TRUE)) %>%
  mutate(id = cumsum(new_text), .keep = "unused") %>%
  select(id, everything())


# разношу по разным колонкам метаданные
texts_df <- content_grouped %>%
  mutate(
    respondent = ifelse(str_detect(text, "^Респондент:"), str_remove(text, "Респондент:"), NA),
    year = ifelse(str_detect(text, "\\d{4}.*"), str_extract(text, "(\\d{4}.*)"), NA),
    recording = ifelse(str_detect(text, "^Запись:"), str_remove(text, "Запись: "), NA),
    recording_place = ifelse(str_detect(text, "^Место записи:"), str_remove(text, "Место записи: "), NA),
    event_place = ifelse(str_detect(text, "^Место событий:"), str_remove(text, "Место событий: "), NA),
    text_clean = str_remove(text, "\\d{4}.*")
  ) %>%
  group_by(id) %>%
  fill(respondent, year, recording, recording_place, event_place, .direction = "downup") %>% 
  summarise(
    full_text = paste(text_clean[!str_detect(text, "^(Респондент:|Запись:|Место записи:|Место событий:)")], collapse = "\n"),
    respondent = first(respondent),
    year = first(year),
    recording = first(recording),
    recording_place = first(recording_place),
    event_place = first(event_place),
    .groups = "drop"
  )


# убираю лишее 
texts_df <- texts_df %>%
  mutate(
    full_text = str_remove_all(full_text, "\\[нрзб\\]"),
    full_text = str_remove_all(full_text, "<...>"),
    full_text = str_remove_all(full_text, "\\(\\?\\)")
  ) %>% 
  mutate(event_place = str_trim(event_place))

  
#write.csv(texts_df, "texts_df.csv", row.names = FALSE)
#texts_df <- read_excel("texts_df.xlsx")
