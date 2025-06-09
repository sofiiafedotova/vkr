library(officer)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(purrr)
library(stopwords)
library(tibble)
library(progress)
library(readxl)
library(writexl)

# прочитываю док
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
texts_df <- read_excel("texts_df.xlsx")

# токенизирую
tokens <- texts_df %>% 
  unnest_tokens(tokens, full_text)

# путь к mystem
path <- "C:/Users/Sofiia/Documents/Диплом/mystem.exe -cd"

# функция по применению mystem 
mystem = function(doc) {
  sdoc = system(path, intern=T, input=doc)
 #attributes(sdoc) <- attributes(doc)
 return(sdoc)
}

# итерация функцией mystem по всем строкам датафрейма с прогресс-баром
process_with_progress <- function(data) {
  #  прогресс-бар
  pb <- progress_bar$new(total = nrow(data), clear = FALSE, width = 60, format = "[:bar] :current из :total")

  data$lemma <- map_chr(data$tokens, function(tokens) {
    result <- paste(mystem(tokens), collapse = " ")
    pb$tick() 
    return(result)
  })
  
  return(data)
}

# применяю
stem <- process_with_progress(tokens)

#save(stem, file = "stem.rdata")
#load("~/Диплом/stem.rdata")

# очищаю от ненужных символов, слов короче 3 букв, заменяю ошибки лемматизации
stem_clean <- stem %>% 
  mutate(
    lemma = str_extract(lemma, "\\{.([^|]+)"),
    lemma = str_remove_all(lemma, "\\{"),
    lemma = str_remove_all(lemma, "\\}"),
    lemma = str_remove_all(lemma, "\\?")) %>% 
  filter(
    lemma != "нрзб",
    nchar(lemma) > 2  
    
  ) %>%

  mutate(lemma = case_when(
    lemma == "грита" ~ "грить",
    lemma == "тепеь" ~ "теперь",
    lemma == "выидешь" ~ "выйти", 
    lemma == "ночю" ~ "ночь",
    lemma == "вося" ~ "восемь",
    lemma == "гдет" ~ "где",
    lemma == "дет" ~ "год", #опечатка с лет
    lemma == "женски" ~ "женский",
    lemma == "деевушка" ~ "девушка",
    lemma == "фанзушко" ~ "фанзушка",
    lemma == "уот" ~ "вот",
    lemma == "эта" ~ "это",
    lemma == "бабки" ~ "бабка",
    lemma == "собираиваться" ~ "собираться",
    lemma == "бегоют" ~ "беготь",
    lemma == "вроди" ~ "вроде",
    lemma == "дашла" ~ "дайти",
    lemma == "кашлить" ~ "кашлять",
    lemma == "патимнеть" ~ "потемнеть",
    lemma == "пашла" ~ "пойти",
    lemma == "маненок" ~ "манинка",
    lemma == "биз" ~ "без",
    lemma == "людна" ~ "людный",
    lemma == "бываит" ~ "бывать",
    lemma == "вадичкать" ~ "вадичка",
    lemma == "приближаец" ~ "приближаться",
    lemma == "прыгат" ~ "прыгать", 
    lemma == "прыгаит" ~ "прыгать",
    lemma == "минь" ~ "я",
    lemma == "потихо" ~ "потихонечку",
    lemma == "нич" ~ "ничо",
    lemma == "дома" ~ "дом",
    lemma == "че" ~ "чо",
    lemma == "каляска" ~ "коляска",
    lemma == "съижжяим" ~ "съежжять",
    lemma == "съежьжяйте" ~ "съежжять",
    lemma == "правило" ~ "править",
    lemma == "подпа" ~ "",
    lemma == "улам" ~ ""
      TRUE ~ lemma
    )
  )# %>% 
  #filter(event_place != "Улан-Батор (?)")



# списки стоп-слов
stop_snowball <- stopwords::stopwords("ru", source = "snowball")
stop_marimo   <- stopwords::stopwords("ru", source = "marimo")
stop_nltk     <- stopwords::stopwords("ru", source = "nltk")

# Объединяем и убираем дубликаты
russian_stopwords <- unique(c(stop_snowball, stop_marimo, stop_nltk))

# доп. список стоп-слов
more_stopwords <- data.frame(lemma = c("это", "еле", "щас", "нету", 
                                       "никто", "туда", "значит", "неет", 
                                       "нибть", "така", "дыж", "така", "можить",
                                       "гас", "ище", "ваш", "почему", 
                                       "блять",  "грить", "ка", "типа", 
                                       "жи", "онечку", "нить",
                                       "дак", "грт", "некоторые", "пятьдесят", 
                                       "семидесятый","таки", "всяко", "ака", "ниво",
                                      ))

# загрузка лемматизированного списка именованных сущностей
load("~/Диплом/NER_stem.rdata")

# очищаю от стоп-слов и именованных сущностей + доочищаю колонку event_place
stem_clean <- stem_clean %>% 
  mutate(event_place = str_replace_all(event_place, "\\(\\?\\)", "")) %>% 
  mutate(event_place = str_trim(event_place)) %>% 
  anti_join(NER_stem, by = "lemma") %>% 
  anti_join(more_stopwords, by = "lemma") %>% 
  filter(!lemma %in% russian_stopwords) 

# подсчитываю кол-во вхождений леммы, фильтрую по id тексты с сомнительной атрибуцией к Улан-Батору
counted <- stem_clean %>% 
  group_by(id) %>% 
  count(lemma) %>%
  filter(!id %in% c(5, 6, 8, 15, 35, 36, 42, 46, 49, 77, 80, 94, 95, 97, 99, 100))

# фильтрую редкие и очень частотные слова
doc_freqs <- counted %>%
  group_by(lemma) %>%
  summarize(doc_freq = n_distinct(id), .groups = "drop")

n_docs <- n_distinct(counted$id)

counted <- counted %>%
  inner_join(doc_freqs, by = "lemma") %>%
  filter(doc_freq >= 2, doc_freq <= n_docs * 0.5)

# tf-idf

tfidf <- counted %>% 
  bind_tf_idf(id, lemma, n) %>% 
  dplyr::select(id, lemma, tf_idf)

wide_tfidf <- tfidf %>%  
  pivot_wider(names_from = lemma, values_from = tf_idf, values_fill = 0) %>% 
  column_to_rownames('id') %>% 
  as.matrix()

