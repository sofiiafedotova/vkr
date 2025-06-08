library(officer)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(purrr)
library(stopwords)
library(tibble)
library(progress)


# прочитываю док
files <- list.files(path = ".", pattern = "docx") 

doc <- read_docx(files[7])
content2 <- docx_summary(doc)

# собираю датафрейм
content_clean2 <- content2 %>%
  mutate(
    group = cumsum(str_detect(text, "^\\d+\\."))  
  ) %>%
  group_by(group) %>%
  summarise(
    id = text[str_detect(text, "^\\d+\\.")][1],
    place = str_remove(text[str_detect(text, "^Место:")][1], "^Место:\\s*"),
    content = str_c(text[!str_detect(text, "^\\d+\\.|^Место:")], collapse = " ")
  ) %>%
  ungroup() %>% 
  mutate(id = str_replace(id, "\\.", "")) %>% 
  mutate(id = trimws(id)) %>% 
  mutate(place = trimws(place)) %>% 
  mutate(
    place = gsub("p-на", "р-на", place)
  )

  
#write.csv(content_clean3, "texts_df3.csv", row.names = FALSE)

# токенизирую
tokens2 <- content_clean2 %>% 
  unnest_tokens(tokens, content)

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
stem2 <- process_with_progress(tokens3)
#save(stem2, file = "zin.rdata")
#load("~/Диплом/zin.rdata")

# очищаю от ненужных символов, слов короче 3 букв
stem_clean2 <- stem2 %>% 
  mutate(
    lemma = str_extract(lemma, "\\{.([^|]+)"),
    lemma = str_remove_all(lemma, "\\{"),
    lemma = str_remove_all(lemma, "\\}"),
    lemma = str_remove_all(lemma, "\\?")) %>% 
  filter(
    lemma != "нрзб",
    nchar(lemma) > 2  
    
  ) 

#списки стоп-слов
stop_snowball <- stopwords::stopwords("ru", source = "snowball")
stop_marimo   <- stopwords::stopwords("ru", source = "marimo")
stop_nltk     <- stopwords::stopwords("ru", source = "nltk")
russian_stopwords <- unique(c(stop_snowball, stop_marimo, stop_nltk))

# доп. список стоп-слов
more_stopwords2 <- data.frame(lemma = c("это", "еле", "щас", "нету", 
                                       "никто", "туда", "значит", "неет", 
                                       "нибть", "така", "дыж", "така", "можить",
                                       "подпа", "гас", "ище", "ваш", "грить"))

# загрузка лемматизированного списка именованных сущностей
load("~/Диплом/NER_stem3.rdata")

# очищаю от стоп-слов и именованных сущностей, добавляю приставку zin_ к id, уравниваю кол-во текстов в корпусе 
stem_clean2 <- stem_clean2 %>% 
  anti_join(NER_stem2, by = "lemma") %>% 
  anti_join(more_stopwords2, by = "lemma") %>% 
  filter(!lemma %in% russian_stopwords) %>% 
  left_join(content_clean2 %>% select(id, place), by = "id") %>% 
  mutate(id = paste0("zin_", id)) %>% 
  filter(!id %in% c("zin_3", "zin_304", "zin_306", "zin_383", "zin_384", 
  "zin_385", "zin_386", "zin_389", "zin_393", "zin_4", "zin_5", "zin_63", "zin_66", 
"zin_72", "zin_74", "zin_75", "zin_78", "zin_79", "zin_82", "zin_9")) %>% 
  group_by(id, place) %>%
  summarise(text = paste(lemma, collapse = " ")) %>%
  ungroup()
  

#write_xlsx(stem_clean2, path = "stem_clean2.xlsx")

# подсчитываю кол-во вхождений леммы
counted2 <- stem_clean2 %>% 
  group_by(id) %>% 
  count(lemma) 

# фильтрую редкие и очень частотные слова
doc_freqs2 <- counted2 %>%
  group_by(lemma) %>%
  summarize(doc_freq = n_distinct(id), .groups = "drop")

n_docs2 <- n_distinct(counted3$id)

counted2 <- counted2 %>%
  inner_join(doc_freqs2, by = "lemma") %>%
  filter(doc_freq >= 2, doc_freq <= n_docs3 * 0.5)

# tf-idf
tfidf2 <- counted2 %>% 
  bind_tf_idf(id, lemma, n) %>% 
  select(id, lemma, tf_idf)

wide_tfidf3 <- tfidf3 %>%  
  pivot_wider(names_from = lemma, values_from = tf_idf, values_fill = 0) %>% 
  column_to_rownames('id') %>% 
  as.matrix()

# кластеризация (9 центроидов - 9 районов)
km.out2 <- kmeans(wide_tfidf2, centers = 9, nstart = 20)
table(km.out2[["cluster"]])
