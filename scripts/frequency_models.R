
# WCBC
tmp = Zs %>% 
  filter(var == 'wcbc_rank')

z_tests = split(tmp$Z, tmp$source) %>% 
  map(t.test)

z_table_wcbc = z_tests_to_table(z_tests) %>% 
  mutate(var = 'WCBC')

# TASA
tmp = Zs %>% 
  filter(var == 'tasa_rank')

z_tests = split(tmp$Z, tmp$source) %>% 
  map(t.test)

z_table_tasa = z_tests_to_table(z_tests, latex = F) %>% # you will want to change latex = T if rendering into PDF
  mutate(var = 'TASA')


# COCA
tmp = Zs %>% 
  filter(var == 'coca_rank')

z_tests = split(tmp$Z, tmp$source) %>% 
  map(t.test)

z_table_coca = z_tests_to_table(z_tests) %>% 
  mutate(var = 'COCA')

# CHILDES
tmp = Zs %>% 
  filter(var == 'childes_rank')

z_tests = split(tmp$Z, tmp$source) %>% 
  map(t.test)

z_table_childes = z_tests_to_table(z_tests) %>% 
  mutate(var = 'CHILDES')

#z_tables_freq <- #rbind(z_table_wcbc, z_table_tasa, z_table_coca, z_table_childes) %>% 
#z_table_wcbc %>% 
#  mutate(a = case_when(a == 'Fountas & Pinnell' ~ 'fp',
#                       TRUE ~ a)) %>% 
#  mutate(b = str_replace(b, '\\\\', ''),
#         b = str_replace(b, '\\{', ''),
#         b = str_replace(b, '\\}', ''),
#         b = str_replace(b, 'textbf', '')) %>% 
#  mutate(source_order = case_when(Source == 'Dolch' ~ 1,
#                                  Source == 'Fry' ~ 2,
#                                  Source == 'Fountas & Pinnell' ~ 3,
#                                  Source == 'Fundations' ~ 4,
#                                  Source == 'Wonders' ~ 5,
#                                  Source == 'Kilpatrick' ~ 6))

#rm(z_table_childes, z_table_coca, z_table_tasa, z_table_wcbc, z_tests)
