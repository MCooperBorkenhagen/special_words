## This script aggregates data for a range of variables for words in the WCBC. This data is written to file so that standard scores can be constructred for words in/across sources for the special words project.


load(file = '../words/childes/data/clean/childes-types.rda')

unl = readxl::read_xlsx('../words/unilex/unilex.xlsx') %>% 
  mutate(word = tolower(Word)) %>% 
  select(word, morphemes = `Number of morphemes (Unisyn)`,
         syllables = `Number of syllables (Unisyn)`)

childes = childes_eng_na_types_0_71_months %>% 
  mutate(word = tolower(gloss)) %>% 
  group_by(word) %>% 
  summarise(childes = sum(count, na.rm = T)) %>% 
  arrange(desc(childes)) %>% 
  mutate(childes_rank = seq_len(n()),
         source = 'childes') %>% 
  filter(word %nin% c('xxx'))

childes_msd = childes %>% 
  summarise(childes_m = mean(childes, na.rm = T),
            childes_sd = sd(childes, na.rm = T),
            childes_rank_m = mean(childes_rank, na.rm = T),
            childes_rank_sd = sd(childes_rank, na.rm = T)) %>% 
  mutate(source = 'childes')

childes = childes %>% 
  left_join(childes_msd) %>% 
  mutate(childes_freq_z = (childes - childes_m)/ childes_sd,
         childes_rank_z = (childes_rank - childes_rank_m)/ childes_rank_sd) %>% 
  select(-source)

consistency = read_csv('data/chee_consistency.csv') %>% 
  mutate(consistency = case_when(n_syll == 1 ~ ff_1_r,
                                 n_syll > 1 ~ ff_all_r)) %>% 
  select(word, consistency)

coca = read_csv('data/fiction_counts.csv') %>% 
  mutate(word = tolower(word)) %>% 
  group_by(word) %>% 
  summarise(freq = sum(freq)) %>%
  ungroup() %>% 
  filter(!str_detect(word, '[[:punct:]]')) %>% 
  filter(!str_detect(word, '\\d')) %>% 
  arrange(desc(freq)) %>% 
  mutate(coca_rank = seq_len(n())) %>% 
  rename(coca_freq = freq) %>% 
  mutate(source = 'coca')

coca_msd = coca %>% 
  summarise(coca_m = mean(coca_freq, na.rm = T),
            coca_sd = sd(coca_freq, na.rm = T),
            coca_rank_m = mean(coca_rank, na.rm = T),
            coca_rank_sd = sd(coca_rank, na.rm = T)) %>% 
  mutate(source = 'coca')

coca = coca %>% 
  left_join(coca_msd) %>% 
  mutate(coca_freq_z = (coca_freq - coca_m)/ coca_sd,
         coca_rank_z = (coca_rank - coca_rank_m)/ coca_rank_sd) %>% 
  select(-source)

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


# join with WCBC
wcbc = read_csv('data/wcbc_freq.csv') %>% 
  rename(word = orth, wcbc_rank = rank) %>% 
  mutate(source = "wcbc")

# tasa is the reference corpus  
tasa = tasa %>% 
  mutate(letters = str_length(word)) %>% 
  left_join(read_csv('data/aoa.csv')) %>% 
  left_join(read_csv('data/imageability.csv')) %>% 
  left_join(read_csv('data/tasa.csv')) %>% 
  left_join(consistency) %>% 
  left_join(childes) %>% 
  left_join(coca) %>% 
  left_join(wcbc) %>% 
  left_join(unl) %>% 
  mutate(source = 'tasa') # add this for the join with msd below


# clean up
rm(childes_eng_na_types_0_71_months, unl)

# First calculate the means and SDs to standardize for all words in the WCBC set.
msd = wcbc %>% 
  summarise(wcbc_m = mean(wcbc_freq, na.rm = T),
            wcbc_sd = sd(wcbc_freq, na.rm = T),
            wcbc_rank_m = mean(wcbc_rank, na.rm = T),
            wcbc_rank_sd = sd(wcbc_rank, na.rm = T)) 

wcbc = wcbc %>% 
  left_join(msd) %>% 
  mutate(wcbc_freq_z = (wcbc_freq - wcbc_m)/wcbc_sd,
         wcbc_rank_z = (wcbc_rank - wcbc_rank_m)/ wcbc_rank_sd) %>% 
  select(word, wcbc_freq, wcbc_rank, wcbc_freq_z, wcbc_rank_z) %>% 
  mutate(source = 'wcbc')

rm(msd)

# First calculate the means and SDs to standardize for all words in the TASA set.

msd = tasa %>% 
  summarise(letters_m = mean(letters, na.rm = T),
            letters_sd = sd(letters, na.rm = T),
            aoa_m = mean(aoa, na.rm = T),
            aoa_sd = sd(aoa, na.rm = T),
            imageability_m = mean(imageability, na.rm = T),
            imageability_sd = sd(imageability, na.rm = T),
            morphemes_m = mean(morphemes, na.rm = T),
            morphemes_sd = sd(morphemes, na.rm = T),
            syllables_m = mean(syllables, na.rm = T),
            syllables_sd = sd(syllables, na.rm = T), 
            consistency_m = mean(consistency, na.rm = T),
            consistency_sd = sd(consistency, na.rm = T)) %>% 
  mutate(source = 'tasa')

tasa %>% 
  left_join(msd) %>% 
  mutate(letters_z = (letters - letters_m)/ letters_sd,
         morphemes_z = (morphemes - morphemes_m)/ morphemes_sd,
         syllables_z = (syllables - syllables_m)/ syllables_sd,
         aoa_z = (aoa - aoa_m)/ aoa_sd,
         imageability_z = (imageability - imageability_m)/ imageability_sd,
         consistency_z = (consistency - consistency_m)/ consistency_sd) %>% 
  left_join(wcbc, by = "word") %>% 
  select(word, tasa_freq_z, tasa_rank_z, wcbc_freq_z, wcbc_rank_z, coca_freq_z, coca_rank_z,
         childes_freq_z, childes_rank_z, morphemes_z, syllables_z, aoa_z, letters_z, imageability_z, consistency_z) %>%
  write_csv('data/tasa_with_z.csv')

rm(msd)

# Generate the Z scores

d = read_csv('data/all_lists_v2.csv') %>% # old version calls ...read_csv('data/all_lists.csv')
  left_join(read_csv('data/tasa_with_z.csv'))

factor_vars = function(df, src){
  
  wcbc = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = wcbc_freq_z) %>% 
    mutate(var = 'wcbc')
  
  wcbc_rank = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = wcbc_rank_z) %>% 
    mutate(var = 'wcbc_rank')
  
  tasa = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = tasa_freq_z) %>% 
    mutate(var = 'tasa')
  
  tasa_rank = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = tasa_rank_z) %>% 
    mutate(var = 'tasa_rank')
  
  coca = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = coca_freq_z) %>% 
    mutate(var = 'coca')
  
  coca_rank = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = coca_rank_z) %>% 
    mutate(var = 'coca_rank')
  
  childes = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = childes_freq_z) %>% 
    mutate(var = 'childes')
  
  childes_rank = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = childes_rank_z) %>% 
    mutate(var = 'childes_rank')
  
  morphemes = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = morphemes_z) %>% 
    mutate(var = 'morphemes')
  
  syllables = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = syllables_z) %>% 
    mutate(var = 'syllables')
  
  letters = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = letters_z) %>% 
    mutate(var = 'letters')
  
  aoa = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = aoa_z) %>% 
    mutate(var = 'aoa')
  
  imageability = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = imageability_z) %>% 
    mutate(var = 'imageability')
  
  consistency = df %>% 
    filter(source == src) %>% 
    select(source, word, Z = consistency_z) %>% 
    mutate(var = 'consistency')
  
  return(rbind(wcbc, wcbc_rank, tasa, tasa_rank, coca, coca_rank, childes, childes_rank, 
               morphemes, syllables, letters, aoa, imageability, consistency))
  
}




dolch = factor_vars(d, 'dolch')
fry = factor_vars(d, 'fry')
fundations = factor_vars(d, 'fundations')
kilpatrick = factor_vars(d, 'kilpatrick')
wonders = factor_vars(d, 'wonders')
fountas_pinnell = factor_vars(d, 'fountas_pinnell')

Zs = rbind(dolch, fry, fundations, kilpatrick, wonders, fountas_pinnell) %>% 
  mutate(source_order = case_when(source == 'dolch' ~ 1,
                                  source == 'fry' ~ 2,
                                  source == 'fountas_pinnell' ~ 3,
                                  source == 'fundations' ~ 4,
                                  source == 'wonders' ~ 5,
                                  source == 'kilpatrick' ~ 6),
         source = case_when(source == 'dolch' ~ 'Dolch',
                            source == 'fry' ~ 'Fry',
                            source == 'fundations' ~ 'Fundations',
                            source == 'kilpatrick' ~ 'Kilpatrick',
                            source == 'wonders' ~ 'Wonders',
                            source == 'fountas_pinnell' ~ 'Fountas & Pinnell'))


Zs %>% 
  write_csv('data/Zs.csv')


rm(dolch, fry, fundations, kilpatrick, wonders, d, factor_vars, childes_msd, coca_msd, consistency, tasa_msd)




