nsd = Zs %>% 
  group_by(source, var) %>% 
  summarise(SD = sd(Z, na.rm = T),
            n = n())

plot_data = Zs %>% 
  filter(var == 'wcbc_rank') %>% 
  select(var, source, M = Z) %>% 
  mutate(var = case_when(var == 'wcbc_rank' ~ 'WCBC (Rank)'),
         source = case_when(source == 'dolch' ~ 'Dolch',
                            source == 'fry' ~ 'Fry',
                            source == 'fundations' ~ 'Fundations',
                            source == 'kilpatrick' ~ 'Kilpatrick',
                            source == 'wonders' ~ 'Wonders'))

Zs %>% 
  group_by(source, var) %>% 
  summarise(M = mean(Z, na.rm = T)) %>% 
  left_join(nsd) %>% 
  mutate(SEM = SD/(sqrt(n))) %>%
  filter(var %in% c('wcbc_rank')) %>% 
  mutate(var = case_when(var == 'wcbc_rank' ~ 'WCBC (Rank)'),
         source = case_when(source == 'dolch' ~ 'Dolch',
                            source == 'fry' ~ 'Fry',
                            source == 'fundations' ~ 'Fundations',
                            source == 'kilpatrick' ~ 'Kilpatrick',
                            source == 'wonders' ~ 'Wonders')) %>% 
  ggplot(aes(source, M, color = source, fill = source, group = source)) +
  geom_point(data = plot_data, position = position_jitter(width = .04, height = .001), size = .01, color = 'grey') +
  geom_point() +
  geom_errorbar(aes(ymin = M-SD, ymax = M + SD, color = source), width = .1) +
  ylim(c(-2.5, 2.5)) +
  labs(color = 'Variable', x = 'Source', y = 'Rank frequency (standardized)') +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, linetype = 'dotted', color = 'red')


# boxplot version

Zs %>% 
  #group_by(source, var) %>% 
  #summarise(M = mean(Z, na.rm = T)) %>% 
  #left_join(nsd) %>% 
  #mutate(SEM = SD/(sqrt(n))) %>%
  filter(var %in% c('aoa', 'consistency', 'letters', 'syllables', 'imageability')) %>% 
  mutate(var = case_when(var == 'consistency' ~ 'Consistency',
                         var == 'syllables' ~ 'Syllables',
                         var == 'aoa' ~ 'AoA',
                         var == 'imageability' ~ 'Imageability',
                         var == 'letters' ~ 'Letters'),
         source = case_when(source == 'dolch' ~ 'Dolch',
                            source == 'fry' ~ 'Fry',
                            source == 'fundations' ~ 'Fundations',
                            source == 'kilpatrick' ~ 'Kilpatrick',
                            source == 'wonders' ~ 'Wonders')) %>% 
  ggplot(aes(var, Z, color = var)) +
  scale_color_manual(values = COLORS) +
  #geom_point(position = position_jitter(width = .1), alpha = .1, size = .02) +
  geom_boxplot(width = .3, outlier.shape = NA) +
  ylim(c(-2, 2)) +
  labs(color = 'Variable', x = 'Source', y = 'Z') +
  facet_grid(~source)  +
  geom_hline(yintercept = 0, linetype = 'dotted', color = 'grey45') +
  theme_bw() +
  theme(legend.position = 'top',
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank())


# points with SEM
plot_data = Zs %>% 
  select(var, source, M = Z) %>% 
  filter(var %in% c('aoa', 'consistency', 'letters', 'syllables', 'imageability')) %>%
  mutate(var = case_when(var == 'consistency' ~ 'Consistency',
                         var == 'syllables' ~ 'Syllables',
                         var == 'aoa' ~ 'AoA',
                         var == 'imageability' ~ 'Imageability',
                         var == 'letters' ~ 'Letters'),
         source = case_when(source == 'dolch' ~ 'Dolch',
                            source == 'fry' ~ 'Fry',
                            source == 'fundations' ~ 'Fundations',
                            source == 'kilpatrick' ~ 'Kilpatrick',
                            source == 'wonders' ~ 'Wonders'))


Zs %>% 
  group_by(source, var) %>% 
  summarise(M = mean(Z, na.rm = T)) %>% 
  left_join(nsd) %>% 
  mutate(SEM = SD/(sqrt(n))) %>%
  filter(var %in% c('aoa', 'consistency', 'letters', 'syllables', 'imageability')) %>% 
  mutate(var = case_when(var == 'consistency' ~ 'Consistency',
                         var == 'syllables' ~ 'Syllables',
                         var == 'aoa' ~ 'AoA',
                         var == 'imageability' ~ 'Imageability',
                         var == 'letters' ~ 'Letters'),
         source = case_when(source == 'dolch' ~ 'Dolch',
                            source == 'fry' ~ 'Fry',
                            source == 'fundations' ~ 'Fundations',
                            source == 'kilpatrick' ~ 'Kilpatrick',
                            source == 'wonders' ~ 'Wonders')) %>% 
  ggplot(aes(var, M, color = var)) +
  scale_color_manual(values = COLORS) +
  #geom_point(position = position_jitter(width = .1), alpha = .1, size = .02) +
  geom_point(data = plot_data, color = 'grey', size = .1, alpha = .2, position = position_jitter(width = .1, height = .02)) +
  #geom_point(size = .5) +
  geom_linerange(aes(ymin = M-SEM, ymax = M+SEM), width = .2, size = 1) +
  #geom_boxplot(width = .3, outlier.shape = NA) +
  ylim(c(-1.2, .5)) +
  labs(color = 'Variable', x = 'Source', y = 'Z') +
  facet_grid(~source)  +
  geom_hline(yintercept = 0, linetype = 'dotted', color = 'grey45') +
  theme_bw() +
  theme(legend.position = 'top',
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank())
