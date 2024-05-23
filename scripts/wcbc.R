

# WCBC
wcbc = read_csv('data/wcbc_freq.csv') %>% 
  rename(word = orth, wcbc_rank = rank) %>% 
  mutate(source = "wcbc")


msd = wcbc %>% 
  summarise(wcbc_m = mean(wcbc_freq, na.rm = T),
            wcbc_sd = sd(wcbc_freq, na.rm = T),
            wcbc_rank_m = mean(wcbc_rank, na.rm = T),
            wcbc_rank_sd = sd(wcbc_rank, na.rm = T)) %>% 
  mutate(source = "wcbc")


wcbc = wcbc %>% 
  left_join(msd) %>% 
  mutate(wcbc_freq_z = (wcbc_freq - wcbc_m)/wcbc_sd,
         wcbc_rank_z = (wcbc_rank - wcbc_rank_m)/ wcbc_rank_sd) %>% 
  select(word, wcbc_freq, wcbc_rank, wcbc_freq_z, wcbc_rank_z) %>% 
  mutate(source = 'wcbc')

rm(msd)