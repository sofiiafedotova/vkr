library(clevr)

# кластеризация с 2-мя центроидами
set.seed(0105)
km.out <- kmeans(scale(wide_tfidf), centers = 2, nstart = 20)
table(km.out[["cluster"]]) 

# кластеризация с 7-ю центроидами
set.seed(0105)
km.out <- kmeans(scale(wide_tfidf), centers = 7, nstart = 20)
table(km.out[["cluster"]])

# топ-20 ближайших к центру слов для каждого кластера 
k <- length(unique(km.out$cluster))
for(i in 1:k){
   cat(paste("cluster", i, ": ", sep=""))
   s <- sort(km.out$centers[i,], decreasing=T)
   cat(names(s)[1:20],"\n")}

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
texts_dfn$cluster <- km.out$cluster

# предсказанные значения - результат кластеризации, ожидаемые значения - пространственные метки
pred_labels <- setNames(texts_dfn$cluster, texts_dfn$id)
true_labels <- setNames(texts_dfn$place_labels, texts_dfn$id) 

# метрики: гомогенность, полнота, v-мера, индекс adjusted rand (пакет clevr)
homogeneity(true_labels, pred_labels)
completeness(true_labels, pred_labels)
v_measure(true_labels, pred_labels)
adj_rand_index(true_labels, pred_labels)
