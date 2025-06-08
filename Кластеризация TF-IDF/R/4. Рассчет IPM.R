library(dplyr)

# подсчёт общего числа слов
total_tokens <- nrow(stem_cleans)

# подсчёт частоты каждого слова и IPM
ipm_table <- stem_clean %>%
  count(lemma) %>%  
  mutate(ipm = (n / total_tokens) * 1e6) %>%
  arrange(desc(ipm))


# тиббл с центрами кластеров
centers <- km.out$centers %>%
  as.data.frame() %>%
  rownames_to_column("cluster") %>%
  pivot_longer(-cluster, names_to = "lemma", values_to = "center_weight") %>%
  mutate(cluster = as.factor(cluster))

# присоединяем частотность (IPM)
centers_ipm <- centers %>%
  left_join(ipm_table, by = "lemma")


# Оставим только топ-20 слов на кластер 
top_centers_ipm <- centers_ipm %>%
  group_by(cluster) %>%
  slice_max(order_by = center_weight, n = 20, with_ties = FALSE) %>%
  arrange(desc(ipm)) %>%
  ungroup()