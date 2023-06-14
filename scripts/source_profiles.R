
require(tidyverse)

d = read_csv('all_lists.csv')

# aoa, freq, consistency

# mean, SDs
aoa_mean_sd = d %>% 
  group_by(source) %>% 
  summarise(M_aoa = mean(aoa, na.rm = T),
            SD_aoa = sd(aoa, na.rm = T))


freq_mean_sd = d %>% 
  group_by(source) %>% 
  summarise(M_freq = mean(wcbc_freq, na.rm = T),
            SD_freq = sd(wcbc_freq, na.rm = T))


consistency_mean_sd = d %>% 
  group_by(source) %>% 
  summarise(M_consistency = mean(consistency, na.rm = T),
            SD_consistency = sd(consistency, na.rm = T))




# dolch
aoa_dolch = d %>% 
  filter(source == 'dolch') %>% 
  select(source, word, aoa) %>% 
  left_join(aoa_mean_sd) %>% 
  mutate(aoa_Z = (aoa - M_aoa)/SD_aoa) %>% 
  select(source, word, Z = aoa_Z) %>% 
  mutate(variable = 'aoa')

freq_dolch = d %>% 
  filter(source == 'dolch') %>% 
  select(source, word, wcbc_freq) %>% 
  left_join(freq_mean_sd) %>% 
  mutate(freq_Z = (wcbc_freq - M_freq)/SD_freq) %>% 
  select(source, word, Z = freq_Z) %>% 
  mutate(variable = 'frequency')


consistency_dolch = d %>% 
  filter(source == 'dolch') %>% 
  select(source, word, consistency) %>% 
  left_join(consistency_mean_sd) %>% 
  mutate(consistency_Z = (consistency - M_consistency)/SD_consistency) %>% 
  select(source, word, Z = consistency_Z) %>% 
  mutate(variable = 'consistency')



aoa_dolch %>% 
  rbind(freq_dolch) %>% 
  rbind(consistency_dolch) %>% 
  group_by(variable) %>% 
  ggplot(aes(variable, Z, color = variable)) +
  geom_jitter(width = .05, size = .05) +
  geom_violin() +
  ylim(c(-3, 4))

