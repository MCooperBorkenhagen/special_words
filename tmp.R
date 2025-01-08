
all_lists %>% 
  group_by(source,  word) %>% 
  summarise(word = first(word)) %>% 
  ungroup() %>% 
  group_by(source) %>% 
  summarise(n())



length(unique(all_lists$word))
names(tasa)

cor(tasa$aoa, tasa$tasa_freq, use = "pairwise.complete.obs", method = "spearman")
