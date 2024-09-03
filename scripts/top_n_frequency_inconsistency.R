


programs_by_rank_inconsistency_rank_frequency = expand.grid(source = names(all_lists_),
                                                            n = c(20, 50, 100, 250),
                                                            Proportion = NA,
                                                            Coverage = NA)

for (n_ in c(20, 50, 100, 250)){
  
  for (program in names(all_lists_)) {
    
    coverage = length(intersect(all_lists_[[program]], words_by_rank_inconsistency_rank_frequency[[n_]]))
    proportion = coverage/n_
    
    programs_by_rank_inconsistency_rank_frequency = programs_by_rank_inconsistency_rank_frequency %>% 
      mutate(Proportion = case_when(source == program & n == n_ ~ proportion,
                                    TRUE ~ Proportion),
             Coverage = case_when(source == program & n == n_ ~ coverage,
                                  TRUE ~ Coverage))
    
    
  } 
  
  
}


write_csv(programs_by_rank_inconsistency_rank_frequency, file = "data/programs_by_rank_inconsistency_rank_frequency.csv")
