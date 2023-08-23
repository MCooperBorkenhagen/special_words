
VARS = c("aoa",  "consistency", "letters", "syllables", "imageability")

affils = Zs %>% 
  mutate(affil = case_when(word %in% intersect_words ~ 'All',
                           word %in% symmetrical_difference_words ~ 'One',
                           TRUE ~ 'Remaining')) %>% 
  select(word, affil)

# aoa
tmp = Zs %>% 
  filter(var == 'aoa') %>% 
  left_join(affils)

z_tests = split(tmp$Z, tmp$affil) %>% 
  map(t.test)

z_table_aoa_affil = z_tests_to_table(z_tests) %>% 
  mutate(var = 'aoa')


# consistency
tmp = Zs %>% 
  filter(var == 'consistency') %>% 
  left_join(affils)

z_tests = split(tmp$Z, tmp$affil) %>% 
  map(t.test)

z_table_consistency_affil = z_tests_to_table(z_tests) %>% 
  mutate(var = 'consistency')


# letters
tmp = Zs %>% 
  filter(var == 'letters') %>% 
  left_join(affils)

z_tests = split(tmp$Z, tmp$affil) %>% 
  map(t.test)

z_table_letters_affil = z_tests_to_table(z_tests) %>% 
  mutate(var = 'letters')



# syllables
tmp = Zs %>% 
  filter(var == 'syllables') %>% 
  left_join(affils)

z_tests = split(tmp$Z, tmp$affil) %>% 
  map(t.test)

z_table_syllables_affil = z_tests_to_table(z_tests) %>% 
  mutate(var = 'syllables')




# imageability
tmp = Zs %>% 
  filter(var == 'imageability') %>% 
  left_join(affils)

z_tests = split(tmp$Z, tmp$affil) %>% 
  map(t.test)

z_table_imageability_affil = z_tests_to_table(z_tests) %>% 
  mutate(var = 'imageability')


# combine

z_tables_lexical_affil = rbind(z_table_aoa_affil, z_table_consistency_affil, z_table_letters_affil, z_table_syllables_affil, z_table_imageability_affil) %>% 
  mutate(group = toTitleCase(Source),
         group_order = case_when(Source == 'Dolch' ~ 1,
                                 Source == 'Fry' ~ 2,
                                 Source == 'Fountas & Pinnell' ~ 3,
                                 Source == 'Fundations' ~ 4,
                                 Source == 'Wonders' ~ 5,
                                 Source == 'Kilpatrick' ~ 6))



rm(z_tests)
