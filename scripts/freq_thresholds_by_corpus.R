

get_top_n = function(){
  
  
  
  
  
  
}




# TASA
SUMF = sum(tasa$tasa_freq, na.rm = T)

tasa = tasa %>% arrange(desc(tasa_freq)) %>%
  mutate(qtile = cumsum(tasa_freq / SUMF)) %>% 
  mutate(top_10 = case_when(qtile <= .10 ~ TRUE,
                           TRUE ~ FALSE),
         top_25 = case_when(qtile <= .25 ~ TRUE,
                            TRUE ~ FALSE),
         top_50 = case_when(qtile <= .5 ~ TRUE,
                            TRUE ~ FALSE))



# CHILDES
SUMF = sum(childes$childes)

childes = childes %>% arrange(desc(childes)) %>%
  mutate(qtile = cumsum(childes / SUMF)) %>% 
  mutate(top_10 = case_when(qtile <= .10 ~ TRUE,
                            TRUE ~ FALSE),
         top_25 = case_when(qtile <= .25 ~ TRUE,
                            TRUE ~ FALSE),
         top_50 = case_when(qtile <= .5 ~ TRUE,
                            TRUE ~ FALSE))


SUMF = sum(wcbc$wcbc_freq)
wcbc = wcbc %>% arrange(desc(wcbc_freq)) %>%
  mutate(qtile = cumsum(wcbc_freq / SUMF)) %>% 
  mutate(top_10 = case_when(qtile <= .10 ~ TRUE,
                            TRUE ~ FALSE),
         top_25 = case_when(qtile <= .25 ~ TRUE,
                            TRUE ~ FALSE),
         top_50 = case_when(qtile <= .5 ~ TRUE,
                            TRUE ~ FALSE))


