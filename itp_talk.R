





# affil figures
bardata %>% 
  mutate(var = case_when(var == 'consistency' ~ 'Consistency',
                         var == 'syllables' ~ 'Syllables',
                         var == 'aoa' ~ 'AoA',
                         var == 'imageability' ~ 'Imageability',
                         var == 'letters' ~ 'Letters')) %>% 
  filter(var == 'Consistency') %>% 
  ggplot(aes(group, M, color = group, group = var)) +
  #geom_hline(data = hlines, aes(yintercept = sourcemean), color = 'black', linetype = 'dotted') +
  scale_color_manual(values = COLORS_AFFIL) +
  geom_point(size = 2) +
  geom_line(color = 'grey') +
  geom_errorbar(aes(ymin = M-se, ymax = M+se), size = 1, width = .25) +
  labs(x = 'Group', y = 'Standardized value (within WCBC)') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  #ylim(c(-1.3, 0)) +
  theme_bw() +
  theme(legend.position = 'none', strip.background=element_rect(fill = 'grey'), 
        axis.title = element_text(size = 28),
              plot.title = element_text(size = 42, hjust = .5),
        axis.text = element_text(size = 22))



# zipfian distribution #2
min = wcbc %>% 
  filter(word %in% symmetrical_difference_words) %>% 
  summarise(min_ = min(wcbc_rank)) %>% 
  pull(min_)


max = wcbc %>% 
  filter(word %in% symmetrical_difference_words) %>%
  summarise(max_ = max(wcbc_rank)) %>% 
  pull(max_)

PEAK = max(wcbc$wcbc_freq)

wcbc %>% 
  ggplot(aes(wcbc_rank, wcbc_freq)) +
  geom_rect(aes(xmin = min, xmax = max, ymin = 0, ymax = PEAK), fill = 'grey68') +
  geom_line(color = 'pink') +
  geom_point(size = .2) +
  labs(x = 'Rank frequency', y = 'Raw frequency') +
  theme_minimal() +
  theme(axis.title = element_text(size = 14))


# consistency alone

bardata = z_tables_lexical %>% 
  select(source = Source, se = `*SE*`, var, M = `*b*`) %>% 
  mutate(source = tolower(source), 
         se = as.numeric(se),
         M = as.numeric(str_remove_all(M, '\\*')))

hlines = nsd %>% 
  filter(var %in% VARS) %>% 
  group_by(var) %>% 
  summarise(sourcemean = mean(M_, na.rm = T)) %>% 
  mutate(var = case_when(var == 'consistency' ~ 'Consistency',
                         var == 'syllables' ~ 'Syllables',
                         var == 'aoa' ~ 'AoA',
                         var == 'imageability' ~ 'Imageability',
                         var == 'letters' ~ 'Letters')) %>% 
  filter(var == 'Consistency')



Zs %>% 
  group_by(source, var) %>% 
  summarise(source_order = first(source_order)) %>% 
  filter(var %in% VARS) %>% 
  left_join(bardata) %>% 
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
  filter(var == 'Consistency') %>% 
  ggplot(aes(reorder(source, source_order), M, color = reorder(source, source_order), group = var)) +
  geom_hline(data = hlines, aes(yintercept = sourcemean), color = 'black', linetype = 'dotted') +
  scale_color_manual(values = COLORS_SOURCE) +
  geom_point(size = 2) +
  geom_line(color = 'grey') +
  geom_errorbar(aes(ymin = M-se, ymax = M+se), size = 1, width = .4) +
  labs(color = '', title = 'Consistency', x = 'Resource', y = 'Standardized value (within WCBC)') +
  #facet_grid(~var)  +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  ylim(c(-1.3, 0)) +
  theme_bw() +
  theme(legend.position = 'top',
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        strip.background=element_rect(fill = 'grey'),
        legend.text = element_text(size = 26),
        axis.title = element_text(size = 28),
        plot.title = element_text(size = 42, hjust = .5))




# words in sources by level
summary <- read_csv('data/summary.csv')
# word count
count_sum <- summary %>%
  filter(level != 'all')

COLORS_GRADES = c('Kindergarten' = 'blue', 'Grade 1' = 'goldenrod1', 'Grade 2' = 'red4', 'Grade 3' = 'darkorange', 'Not specified' = 'purple', 'Pre K' = 'green')

all_lists %>% 
  filter(level != 'noun') %>% 
  mutate(source = case_when(source == 'dolch' ~ 'Dolch',
                            source == 'fry' ~ 'Fry',
                            source == 'fundations' ~ 'Fundations',
                            source == 'kilpatrick' ~ 'Kilpatrick',
                            source == 'wonders' ~ 'Wonders'),
         source_order = case_when(source == 'dolch' ~ 1,
                                  source == 'fry' ~ 2,
                                  source == 'fundations' ~ 3,
                                  source == 'wonders' ~ 4,
                                  source == 'kilpatrick' ~ 5),
         level = as.factor(case_when(level == 'k' ~ 'Kindergarten',
                              level == 'g1' ~ 'Grade 1',
                              level == 'g2' ~ 'Grade 2',
                              level == 'g3' ~ 'Grade 3',
                              level == 'pre_k' ~ 'Pre K',
                              level == 'none' ~ 'Not specified')),
         level = fct_relevel(level, 'Pre K', 'Kindergarten', 'Grade 1', 'Grade 2', 'Grade 3', 'Not specified')) %>% 
  group_by(source, level) %>%
  summarise(n = n()) %>% 
  mutate(source_order = case_when(source == 'Dolch' ~ 1,
                                  source == 'Fry' ~ 2,
                                  source == 'Fundations' ~ 3,
                                  source == 'Wonders' ~ 4,
                                  source == 'Kilpatrick' ~ 5)) %>% 
  ggplot(aes(reorder(source, source_order), n, fill = level)) +
  geom_bar(position = "stack", stat = "identity", color = 'black') +
  theme_classic() +
  scale_fill_manual(values = COLORS_GRADES) +
  labs(x = 'Resource', y = '# of words', fill = 'Grade')

