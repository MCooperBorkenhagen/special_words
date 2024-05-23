

tasa = read_csv('data/tasa.csv') %>% 
  arrange(desc(tasa_freq)) %>% 
  mutate(tasa_rank = seq_len(n()),
         source = 'tasa')

tasa_msd = tasa %>% 
  summarise(tasa_m = mean(tasa_freq, na.rm = T),
            tasa_sd = sd(tasa_freq, na.rm = T),
            tasa_rank_m = mean(tasa_rank, na.rm = T),
            tasa_rank_sd = sd(tasa_rank, na.rm = T)) %>% 
  mutate(source = 'tasa')


tasa = tasa %>% 
  left_join(tasa_msd) %>% 
  mutate(tasa_freq_z = (tasa_freq - tasa_m)/ tasa_sd,
         tasa_rank_z = (tasa_rank - tasa_rank_m)/ tasa_rank_sd) %>% 
  select(-source)


# tasa is the reference corpus  
tasa = tasa %>% 
  mutate(letters = str_length(word)) %>% 
  left_join(read_csv('data/aoa.csv')) %>% 
  left_join(read_csv('data/imageability.csv')) %>% 
  left_join(read_csv('data/tasa.csv')) %>% 
  left_join(read_csv('data/chee_consistency.csv') %>% 
              mutate(consistency = case_when(n_syll == 1 ~ ff_1_r,
                                             n_syll > 1 ~ ff_all_r)) %>% 
              select(word, consistency)) %>% 
  left_join(childes) %>% 
  left_join(coca) %>% 
  left_join(wcbc) %>% 
  left_join(readxl::read_xlsx('../words/unilex/unilex.xlsx') %>% 
              mutate(word = tolower(Word)) %>% 
              select(word, morphemes = `Number of morphemes (Unisyn)`,
                     syllables = `Number of syllables (Unisyn)`)) %>% 
  mutate(source = 'tasa') # add this for the join with msd below
