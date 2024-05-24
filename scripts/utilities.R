

rnd = function(x, dgt = 2, nsml = 2){
  return(format(round(x, digits = dgt), nsmall = nsml))
}


desc_by_var = function(x, var_, combine_ = T){
  
  df = x %>% 
    filter(var == var_) %>%
    group_by(source) %>% 
    summarise(M = mean(Z, na.rm = T),
              SD = sd(Z, na.rm = T))
  if (combine_){
    
    df = df %>% 
      mutate(dsc = paste(rnd(M), ' (', rnd(SD), ')', sep = '')) %>% 
      select(source, var = dsc)
    
    return(df)
  }
  else{return(df)}
}



check_p = function(b, p, alpha = .05){
  
  bp = rnd(b)
  if (p >= alpha){
    return(c(bp, rnd(p)))
  }
  if (p < alpha & p > .01){return(c(paste('**', bp, '**', sep = ''), '< .05'))}
  if (p < alpha & p > .001){return(c(paste('**', bp, '**', sep = ''), '< .01'))}
  if (p < alpha & p < .001){return(c(paste('**', bp, '**', sep = ''), '< .001'))}
}





bold_cell = function(x, nsmall = 2){
  paste0("\\textbf{", format(x, nsmall = nsmall), "}")}

z_tests_to_table = function(l, nsmall = 2, latex = TRUE){
  
  #' Generates a dataframe of numeric data from a list of t.tests for graphing not tables
  #' @param l A list of t.tests
  #' @return A dataframe with one row per test in l
  
  df = tibble(Source = names(l),
              b = NA, t = NA,
              df = NA, SE = NA,
              `CI95` = NA, CI_low = NA,
              CI_hi = NA, p_derived = NA)
  
  for (i in seq(length(names(l)))){
    
    b = l[[i]]$estimate
    p = l[[i]]$p.value
    
    df$Source[i] = names(l)[i]
    df$b[i] = round(b, digits = 2)
    df$t[i] = round(l[[i]]$statistic, digits = 2)
    df$df[i] = format(l[[i]]$parameter, nsmall = 0)
    df$SE[i] = round(l[[i]]$stderr, digits = 2)
    df$CI95[i] = paste('[', round(l[[i]]$conf.int[1], digits = 2), ', ', round(l[[i]]$conf.int[2], digits = 2), ']', sep = '')
    df$CI_low[i] = l[[i]]$conf.int[1]
    df$CI_hi[i] = l[[i]]$conf.int[2]
    df$p_derived[i] = l[[i]]$p.value
    
  }
  
  df = df %>% 
    mutate(p = case_when(p_derived == 0 ~ '<.001', # an idiosyncracy of t.test() is that is sometimes give
                         p_derived < .001 ~ '<.001',
                         p_derived < .01 & p_derived >= .001 ~ '<.01',
                         p_derived < .05 & p_derived >= .01 ~ '<.01',
                         p_derived >= .05 ~ as.character(round(p_derived, digits = 2))))
  
  if (latex){

    df = df %>% 
      mutate(b = case_when(p_derived < .001 ~ bold_cell(b, nsmall = nsmall),
                           TRUE ~ format(b, nsmall = nsmall)))
    
    colnames(df) = c('Source', '\\textit{b}', '\\textit{t}', '\\textit{df}', '\\textit{SE}', '95\\% CI', 'CI_low', 'CI_hi', 'p_derived', '\\textit{p}')

    
    }
  
  return(df)}


latex_to_num = function(x, latex = 'bold', as_numeric = TRUE){
  
  if (latex == 'bold'){
    target = 'textbf'
  }
  if (latex == 'italic'){
    target = 'textit'
  }
  
  y = str_replace(x, '\\\\', '')
  y = str_replace(y, paste(target, '\\{', sep = ''), '')
  y = str_replace(y, '\\}', '')
  if (as_numeric){
    return(as.numeric(y))}
  if (!as_numeric){
    return(y)
  }
}



symdiff <- function(x, y) {setdiff( union(x, y), intersect(x, y))}

frequency_by_corpus = function(data){
  
  tmp1 = data %>% 
    group_by(word) %>% 
    summarise(wcbc_freq = first(wcbc_freq)) %>% 
    ungroup() %>% 
    summarise(Corpus = 'WCBC',
              M = mean(wcbc_freq, na.rm = T),
              SD = sd(wcbc_freq, na.rm = T),
              max = max(wcbc_freq, na.rm = T))
  
  tmp2 = data %>% 
    group_by(word) %>% 
    summarise(tasa_freq = first(tasa_freq)) %>% 
    ungroup() %>% 
    summarise(Corpus = 'TASA',
              M = mean(tasa_freq, na.rm = T),
              SD = sd(tasa_freq, na.rm = T),
              max = max(tasa_freq, na.rm = T))
  
  tmp3 = data %>% 
    group_by(word) %>% 
    summarise(childes_freq = first(childes_freq)) %>% 
    ungroup() %>% 
    summarise(Corpus = 'CHILDES',
              M = mean(childes_freq, na.rm = T),
              SD = sd(childes_freq, na.rm = T),
              max = max(childes_freq, na.rm = T))
  
  
  return(rbind(tmp1, tmp2, tmp3))
  
}


l2 <- function(coordinate_a, coordinate_b) {
  if (length(coordinate_a) != length(coordinate_b)) {
    stop("Coordinates must have the same number of dimensions.")
  }

  return(sqrt(sum((coordinate_b - coordinate_a)^2)))
}

round_axis_text = function(x) sprintf("%.2f", x)

whole_axis_text <- function(x) {
  format(round(x, digits = 0), big.mark = "", scientific = FALSE)
}
