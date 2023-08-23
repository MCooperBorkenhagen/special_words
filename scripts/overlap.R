
nsd = Zs %>% 
  group_by(source, var) %>% 
  summarise(SD = sd(Z, na.rm = T),
            M_ = mean(Z, na.rm = T),
            n = n(),
            source_order = first(source_order))


