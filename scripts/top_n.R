

top_50_wcbc = wcbc %>% 
  filter(wcbc_rank <= 50) %>% 
  select(word) %>% 
  mutate(corpus = "wcbc", top_n = "50")

top_100_wcbc = wcbc %>% 
  filter(wcbc_rank <= 100) %>% 
  select(word) %>% 
  mutate(corpus = "wcbc", top_n = "100")
  
top_500_wcbc = wcbc %>% 
  filter(wcbc_rank <= 500) %>% 
  select(word) %>% 
  mutate(corpus = "wcbc", top_n = "500")

top_1000_wcbc = wcbc %>% 
  filter(wcbc_rank <= 1000) %>% 
  select(word) %>% 
  mutate(corpus = "wcbc", top_n = "1000")

top_50_tasa = tasa %>% 
  filter(tasa_rank <= 50) %>% 
  select(word) %>% 
  mutate(corpus = "tasa", top_n = "50")

top_100_tasa = tasa %>% 
  filter(tasa_rank <= 100) %>% 
  select(word) %>% 
  mutate(corpus = "tasa", top_n = "100")

top_500_tasa = tasa %>% 
  filter(tasa_rank <= 500) %>% 
  select(word) %>% 
  mutate(corpus = "tasa", top_n = "500")

top_1000_tasa = tasa %>% 
  filter(tasa_rank <= 1000) %>% 
  select(word) %>% 
  mutate(corpus = "tasa", top_n = "1000")


top_50_childes = childes %>% 
  filter(childes_rank <= 50) %>% 
  select(word) %>% 
  mutate(corpus = "childes", top_n = "50")

top_100_childes = childes %>% 
  filter(childes_rank <= 100) %>% 
  select(word) %>% 
  mutate(corpus = "childes", top_n = "100")
  
  
top_500_childes = childes %>% 
  filter(childes_rank <= 500) %>% 
  select(word) %>% 
  mutate(corpus = "childes", top_n = "500")

top_1000_childes = childes %>% 
  filter(childes_rank <= 1000) %>% 
  select(word) %>% 
  mutate(corpus = "childes", top_n = "1000")


top_N = rbind(top_50_wcbc, top_100_wcbc, top_500_wcbc, top_1000_wcbc, top_50_tasa, top_100_tasa, top_500_tasa, top_1000_tasa, top_50_childes, top_100_childes, top_500_childes, top_1000_childes)

rm(top_100_wcbc, top_500_wcbc, top_1000_wcbc, top_100_tasa, top_500_tasa, top_1000_tasa, top_100_childes, top_500_childes, top_1000_childes)
