
# Kilatrick
Kilpatrick_coverage = c('Kilpatrick', 'CHILDES')
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, filter(childes, top_10 == T)$word))/length(filter(childes, top_10 == T)$word))
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, filter(childes, top_25 == T)$word))/length(filter(childes, top_25 == T)$word))
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, filter(childes, top_50 == T)$word))/length(filter(childes, top_50 == T)$word))
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, unique(childes$word)))/length(unique(childes$word)))

# Dolch
Dolch_coverage = c('Dolch', 'CHILDES')
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, filter(childes, top_10 == T)$word))/length(filter(childes, top_10 == T)$word))
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, filter(childes, top_25 == T)$word))/length(filter(childes, top_25 == T)$word))
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, filter(childes, top_50 == T)$word))/length(filter(childes, top_50 == T)$word))
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, unique(childes$word)))/length(unique(childes$word)))

# Fry

Fry_coverage = c('Fry', 'CHILDES')
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, filter(childes, top_10 == T)$word))/length(filter(childes, top_10 == T)$word))
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, filter(childes, top_25 == T)$word))/length(filter(childes, top_25 == T)$word))
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, filter(childes, top_50 == T)$word))/length(filter(childes, top_50 == T)$word))
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, unique(childes$word)))/length(unique(childes$word)))


Fundations_coverage = c('Fundations', 'CHILDES')
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, filter(childes, top_10 == T)$word))/length(filter(childes, top_10 == T)$word))
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, filter(childes, top_25 == T)$word))/length(filter(childes, top_25 == T)$word))
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, filter(childes, top_50 == T)$word))/length(filter(childes, top_50 == T)$word))
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, unique(childes$word)))/length(unique(childes$word)))

Wonders_coverage = c('Wonders', 'CHILDES')
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, filter(childes, top_10 == T)$word))/length(filter(childes, top_10 == T)$word))
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, filter(childes, top_25 == T)$word))/length(filter(childes, top_25 == T)$word))
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, filter(childes, top_50 == T)$word))/length(filter(childes, top_50 == T)$word))
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, unique(childes$word)))/length(unique(childes$word)))

df1 = data.frame(rbind(Kilpatrick_coverage, Dolch_coverage, Fry_coverage, Wonders_coverage, Fundations_coverage))

# TASA
# Kilpatrick
Kilpatrick_coverage = c('Kilpatrick', 'TASA')
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, filter(tasa, top_10 == T)$word))/length(filter(tasa, top_10 == T)$word))
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, filter(tasa, top_25 == T)$word))/length(filter(tasa, top_25 == T)$word))
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, filter(tasa, top_50 == T)$word))/length(filter(tasa, top_50 == T)$word))
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, unique(tasa$word)))/length(unique(tasa$word)))

# Dolch
Dolch_coverage = c('Dolch', 'TASA')
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, filter(tasa, top_10 == T)$word))/length(filter(tasa, top_10 == T)$word))
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, filter(tasa, top_25 == T)$word))/length(filter(tasa, top_25 == T)$word))
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, filter(tasa, top_50 == T)$word))/length(filter(tasa, top_50 == T)$word))
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, unique(tasa$word)))/length(unique(tasa$word)))

# Fry

Fry_coverage = c('Fry', 'TASA')
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, filter(tasa, top_10 == T)$word))/length(filter(tasa, top_10 == T)$word))
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, filter(tasa, top_25 == T)$word))/length(filter(tasa, top_25 == T)$word))
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, filter(tasa, top_50 == T)$word))/length(filter(tasa, top_50 == T)$word))
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, unique(tasa$word)))/length(unique(tasa$word)))


Fundations_coverage = c('Fundations', 'TASA')
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, filter(tasa, top_10 == T)$word))/length(filter(tasa, top_10 == T)$word))
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, filter(tasa, top_25 == T)$word))/length(filter(tasa, top_25 == T)$word))
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, filter(tasa, top_50 == T)$word))/length(filter(tasa, top_50 == T)$word))
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, unique(tasa$word)))/length(unique(tasa$word)))

Wonders_coverage = c('Wonders', 'TASA')
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, filter(tasa, top_10 == T)$word))/length(filter(tasa, top_10 == T)$word))
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, filter(tasa, top_25 == T)$word))/length(filter(tasa, top_25 == T)$word))
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, filter(tasa, top_50 == T)$word))/length(filter(tasa, top_50 == T)$word))
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, unique(tasa$word)))/length(unique(tasa$word)))

df2 = data.frame(rbind(Kilpatrick_coverage, Dolch_coverage, Fry_coverage, Wonders_coverage, Fundations_coverage))



# WCBC
# Kilpatrick
Kilpatrick_coverage = c('Kilpatrick', 'WCBC')
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, filter(wcbc, top_10 == T)$word))/length(filter(wcbc, top_10 == T)$word))
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, filter(wcbc, top_25 == T)$word))/length(filter(wcbc, top_25 == T)$word))
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, filter(wcbc, top_50 == T)$word))/length(filter(wcbc, top_50 == T)$word))
Kilpatrick_coverage = c(Kilpatrick_coverage, length(intersect(all_lists_$Kilpatrick, unique(wcbc$word)))/length(unique(wcbc$word)))

# Dolch
Dolch_coverage = c('Dolch', 'WCBC')
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, filter(wcbc, top_10 == T)$word))/length(filter(wcbc, top_10 == T)$word))
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, filter(wcbc, top_25 == T)$word))/length(filter(wcbc, top_25 == T)$word))
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, filter(wcbc, top_50 == T)$word))/length(filter(wcbc, top_50 == T)$word))
Dolch_coverage = c(Dolch_coverage, length(intersect(all_lists_$Dolch, unique(wcbc$word)))/length(unique(wcbc$word)))

# Fry

Fry_coverage = c('Fry', 'WCBC')
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, filter(wcbc, top_10 == T)$word))/length(filter(wcbc, top_10 == T)$word))
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, filter(wcbc, top_25 == T)$word))/length(filter(wcbc, top_25 == T)$word))
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, filter(wcbc, top_50 == T)$word))/length(filter(wcbc, top_50 == T)$word))
Fry_coverage = c(Fry_coverage, length(intersect(all_lists_$Fry, unique(wcbc$word)))/length(unique(wcbc$word)))


Fundations_coverage = c('Fundations', 'WCBC')
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, filter(wcbc, top_10 == T)$word))/length(filter(wcbc, top_10 == T)$word))
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, filter(wcbc, top_25 == T)$word))/length(filter(wcbc, top_25 == T)$word))
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, filter(wcbc, top_50 == T)$word))/length(filter(wcbc, top_50 == T)$word))
Fundations_coverage = c(Fundations_coverage, length(intersect(all_lists_$Fundations, unique(wcbc$word)))/length(unique(wcbc$word)))

Wonders_coverage = c('Wonders', 'WCBC')
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, filter(wcbc, top_10 == T)$word))/length(filter(wcbc, top_10 == T)$word))
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, filter(wcbc, top_25 == T)$word))/length(filter(wcbc, top_25 == T)$word))
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, filter(wcbc, top_50 == T)$word))/length(filter(wcbc, top_50 == T)$word))
Wonders_coverage = c(Wonders_coverage, length(intersect(all_lists_$Wonders, unique(wcbc$word)))/length(unique(wcbc$word)))

df3 = data.frame(rbind(Kilpatrick_coverage, Dolch_coverage, Fry_coverage, Wonders_coverage, Fundations_coverage))

coverage = rbind(df1, df2, df3)

rm(df1, df2, df3, Kilpatrick_coverage, Fry_coverage, Dolch_coverage, Fundations_coverage, Wonders_coverage)
names(coverage) = c('Resource', 'Corpus', 'Top 10%', 'Top 25%', 'Top 50%', 'Entire corpus')

coverage = coverage %>% 
  pivot_longer(!c(Resource, Corpus), names_to = 'Top', values_to = 'Proportion') %>% 
  mutate(Proportion = as.numeric(Proportion))

