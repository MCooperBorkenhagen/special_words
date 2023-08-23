
# aoa
tmp = Zs %>% 
  filter(var == 'aoa')

z_tests = split(tmp$Z, tmp$source) %>% 
  map(t.test)

z_table_aoa = z_tests_to_table(z_tests) %>% 
  mutate(var = 'aoa')


# consistency
tmp = Zs %>% 
  filter(var == 'consistency')

z_tests = split(tmp$Z, tmp$source) %>% 
  map(t.test)

z_table_consistency = z_tests_to_table(z_tests) %>% 
  mutate(var = 'consistency')


# letters
tmp = Zs %>% 
  filter(var == 'letters')

z_tests = split(tmp$Z, tmp$source) %>% 
  map(t.test)

z_table_letters = z_tests_to_table(z_tests) %>% 
  mutate(var = 'letters')


# syllables
tmp = Zs %>% 
  filter(var == 'syllables')

z_tests = split(tmp$Z, tmp$source) %>% 
  map(t.test)

z_table_syllables = z_tests_to_table(z_tests) %>% 
  mutate(var = 'syllables')



# imageability
tmp = Zs %>% 
  filter(var == 'imageability')

z_tests = split(tmp$Z, tmp$source) %>% 
  map(t.test)

z_table_imageability = z_tests_to_table(z_tests) %>% 
  mutate(var = 'imageability')


# combine
z_tables_lexical = rbind(z_table_aoa, z_table_consistency, z_table_letters, z_table_syllables, z_table_imageability) %>% 
  mutate(Source = toTitleCase(Source),
         source_order = case_when(Source == 'Dolch' ~ 1,
                                  Source == 'Fry' ~ 2,
                                  Source == 'Fountas & Pinnell' ~ 3,
                                  Source == 'Fundations' ~ 4,
                                  Source == 'Wonders' ~ 5,
                                  Source == 'Kilpatrick' ~ 6)) %>% 
  select(-CI_low, -CI_hi, -p_derived)

rm(z_table_aoa, z_table_consistency, z_table_imageability, z_table_letters, z_table_syllables, z_tests)
