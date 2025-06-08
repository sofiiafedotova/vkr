library(dbscan)


# снижаем размерность (2 компоненты)
set.seed(0105)
pca_out <- prcomp(wide_tfidf)
pca_data <- pca_out$x[, 1:2]  

# кластеризация dbscan
set.seed(0105)
db <- dbscan(pca_data, eps = 0.7, minPts = 2)
table(db[["cluster"]])

#--------------------------------------------------------------


# рассчет метрик соответствия ожиданиям

# отдельный датафрейм для сравнения

texts_dfn <- texts_df %>% 
  filter(event_place != "Улан-Батор (?)")

# присвоение пространственных меток 
texts_dfn <- texts_dfn %>%
  mutate(event_place = str_replace_all(event_place, "\\(.*?\\)", "")) %>%      # убрать скобки с вопросами 
  mutate(event_place = str_squish(tolower(event_place))) %>% 
  mutate(recording_place = str_squish(tolower(recording_place))) %>% 
  mutate(place_labels = case_when(
    event_place %in% c("китай") ~ "Китай",
    event_place %in% c("ховд", "монгольский алтай") ~ "Ховд",
    event_place %in% c("чойболсан", "керулен") ~ "Керулен",
    event_place %in% c("улан-батор", "амгалан") ~ "Улан-Батор",
    event_place %in% c("булган") ~ "Булган",
    event_place %in% c("дзунхара", "дархан", "сухэ-батор", "карнаковка", 
                       "алтан-булак", "шамара", "оджук") ~ "Дзунхара",
    event_place %in% c("улан-удэ", "братск") ~ "РФ",
    event_place %in% c("неизвестно-далеко", "неизвестно-близко", "русская деревня") ~ "N/A"
  )) 


# создание нового столбца в датафрейме с результатом кластеризации
texts_dfn$cluster <- db$cluster

# предсказанные значения - результат кластеризации, ожидаемые значения - пространственные метки
pred_labels <- setNames(texts_dfn$cluster, texts_dfn$id)
true_labels <- setNames(texts_dfn$place_labels, texts_dfn$id) 

# метрики: гомогенность, полнота, v-мера, индекс adjusted rand (пакет clevr)
homogeneity(true_labels, pred_labels)
completeness(true_labels, pred_labels)
v_measure(true_labels, pred_labels)
adj_rand_index(true_labels, pred_labels)
