# perform set operations to determine intersect, symmetrical difference, etc

all_lists_ = list(Kilpatrick = all_lists %>% filter(source == 'kilpatrick') %>%  pull(word) %>% tolower() %>% unique(),
                  Dolch = all_lists %>% filter(source == 'dolch') %>%  pull(word) %>% tolower() %>% unique(),
                  Fry = all_lists %>% filter(source == 'fry') %>%  pull(word) %>% tolower() %>% unique(),
                  Fundations = all_lists %>% filter(source == 'fundations') %>%  pull(word) %>% tolower() %>% unique(),
                  Wonders = all_lists %>% filter(source == 'wonders') %>%  pull(word) %>% tolower() %>% unique(),
                  Fountas_Pinnell = all_lists %>% filter(source == 'fountas_pinnell') %>%  pull(word) %>% tolower() %>% unique())




intersect_words = wcbc %>% 
  filter(word %in% Reduce(intersect, all_lists_)) %>% 
  pull(word)



# This script determines the symmetrical difference of all words across all sources.

symmetrical_difference = list()

for (s in unique(all_lists$source)){
  
  o = all_lists %>% 
    mutate(word = tolower(word)) %>% 
    filter(source != s) %>% 
    distinct(word) %>% 
    pull(word)
  u = all_lists %>% 
    mutate(word = tolower(word)) %>% 
    filter(source == s) %>% 
    filter(word %nin% o) %>% 
    pull(word)
  
  symmetrical_difference[[s]] = u
}


symmetrical_difference_n = Reduce(sum, lapply(symmetrical_difference, length))
symmetrical_difference_words = unique(unlist(symmetrical_difference), use.names = FALSE)
