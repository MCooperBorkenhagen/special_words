

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



z_tests_to_table = function(l, apa = T, ps_and_bs = T){
  
  #' Generates a dataframe from a list of t.tests
  #' @param l A list of t.tests
  #' @return A dataframe with one row per test in l
  
  cls = c('Source', 'b', 't', 'df', 'SE', '95% CI', 'CI_low', 'CI_hi', 'p')
  df = data.frame(matrix(nrow = length(names(l)), ncol = length(cls)))
  colnames(df) = cls
  
  for (i in seq(length(names(l)))){
    
    b = l[[i]]$estimate
    p = l[[i]]$p.value
    
    if (ps_and_bs){
      b_ = check_p(b, p)[1]
      p_ = check_p(b, p)[2]
    }
    else{
      b_ = rnd(b)
      p_ = rnd(p)
    }
    
    df$Source[i] = names(l)[i]
    df$b[i] = b_
    df$t[i] = rnd(l[[i]]$statistic)
    df$df[i] = rnd(l[[i]]$parameter, dgt = 0, nsml = 0)
    df$SE[i] = rnd(l[[i]]$stderr)
    df$`95% CI`[i] = paste('[', rnd(l[[i]]$conf.int[1]), ', ', rnd(l[[i]]$conf.int[2]), ']', sep = '')
    df$CI_low[i] = l[[i]]$conf.int[1]
    df$CI_hi[i] = l[[i]]$conf.int[2]
    df$p[i] = p_
  }
  
  if (apa){
    names(df) = c('Source', '*b*', '*t*', '*df*', '*SE*', '95% CI', 'CI_low', 'CI_hi', '*p*')
    return(df)
  }
  else(return(df))}



z_tests_to_table_num = function(l){
  
  #' Generates a dataframe of numeric data from a list of t.tests for graphing not tables
  #' @param l A list of t.tests
  #' @return A dataframe with one row per test in l
  
  cls = c('Source', 'b', 't', 'df', 'SE', '95% CI', 'CI_low', 'CI_hi', 'p')
  df = data.frame(matrix(nrow = length(names(l)), ncol = length(cls)))
  colnames(df) = cls
  
  for (i in seq(length(names(l)))){
    
    b = l[[i]]$estimate
    p = l[[i]]$p.value
    
    df$Source[i] = names(l)[i]
    df$b[i] = b
    df$t[i] = l[[i]]$statistic
    df$df[i] = l[[i]]$parameter
    df$SE[i] = l[[i]]$stderr
    df$`95% CI`[i] = paste('[', rnd(l[[i]]$conf.int[1]), ', ', rnd(l[[i]]$conf.int[2]), ']', sep = '')
    df$CI_low[i] = l[[i]]$conf.int[1]
    df$CI_hi[i] = l[[i]]$conf.int[2]
    df$p[i] = p}

    return(df)}

symdiff <- function(x, y) {setdiff( union(x, y), intersect(x, y))}
