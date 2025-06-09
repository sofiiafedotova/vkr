library(readxl)
library(klaR)
library(clevr)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(stringr)

# чтение датафрейма
code_df <- read_excel("Разметка.xlsx")

# преобразование датафрейма
code_df <- code_df %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_colnames() %>% 
  mutate(across(everything(), ~ as.character(.))) %>% 
  mutate(across(everything(), ~ as.integer(.))) 
  
code_df[] <- lapply(code_df, as.factor)


# кластеризация (2 мода)

set.seed(0105)
kmodes_res <- kmodes(code_df, modes = 2)

# кластеризация (7 модов)

set.seed(0105)
kmodes_res <- kmodes(code_df, modes = 7)


# отдельный датафрейм для сравнения
texts_dfn <- texts_df

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

# присвоение меток дистанции
texts_dfn <- texts_dfn %>%
  mutate(distance = if_else(recording_place == event_place, "близко", "далеко")) %>% 
  mutate(distance = case_when(
    event_place == "неизвестно-близко" ~ "близко",
    TRUE ~ distance))  
  
  
# создание нового столбца в датафрейме с результатом кластеризации
texts_dfn$cluster <- kmodes_res$cluster

# предсказанные значения - результат кластеризации
cluster_membership <- setNames(texts_dfn$cluster, texts_dfn$id)
# ожидаемые значения - пространственные метки (7 модов)
true_labels <- setNames(texts_dfn$place_labels, texts_dfn$id) 
# ИЛИ 
# ожидаемые значения - дистанция (2 мода)
#true_labels <- setNames(texts_dfn$distance, texts_dfn$id)  

# метрики: гомогенность, полнота, v-мера, индекс adjusted rand (пакет clevr)
homogeneity(true_labels, cluster_membership)
completeness(true_labels, cluster_membership)
v_measure(true_labels, cluster_membership)
adj_rand_index(true_labels, cluster_membership)


#-------------------------------------------------------------------------------

#Тест Фишера

# таблица сопряженности: во второй аргумент передать интересующую колонку
table_distance_group <- as.matrix(table(texts_dfn$distance, code_df$mc_gender))
table_distance_group
fisher.test(table_distance_group)

#-------------------------------------------------------------------------------
  
# MCA
  
set.seed(04052025)  
mca_res <- MCA(code_df, graph = FALSE)


mca_coords <- as.data.frame(mca_res$ind$coord)
mca_coords$cluster <- as.factor(kmodes_res$cluster)

# визуализация

fviz_mca_ind(mca_res,
             habillage = mca_coords$cluster,
             repel = TRUE,
             title = "MCA")