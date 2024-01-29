coca = read_csv('data/fiction_counts.csv') %>% 
  mutate(word = tolower(word)) %>% 
  group_by(word) %>% 
  summarise(freq = sum(freq)) %>%
  ungroup() %>% 
  filter(!str_detect(word, '[[:punct:]]')) %>% 
  filter(!str_detect(word, '\\d')) %>% 
  arrange(desc(freq)) %>% 
  mutate(rank = seq_len(n())) %>% 
  rename(freq = freq) %>% 
  filter(word %in% unique(all_lists$word))
