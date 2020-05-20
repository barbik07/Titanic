library(tidyverse)
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

hiv <- read_data("thornton_hiv.dta")


# creating the permutations

tb <- NULL

permuteHIV <- function(df, random = TRUE){
  tb <- df
  first_half <- ceiling(nrow(tb)/2)
  second_half <- nrow(tb) - first_half
  
  if(random == TRUE){
    tb <- tb %>%
      sample_frac(1) %>%
      mutate(any = c(rep(1, first_half), rep(0, second_half)))
  }
  
  model <- lm(got ~any+male, data = tb)
  
  delta <-model$coefficients[[2]]
  
  ate <- delta
  
  return(ate)
}


permuteHIV(hiv, random = FALSE)



iterations <- 10000

permutation <- tibble(
  iteration = c(seq(iterations)), 
  ate = as.numeric(
    c(permuteHIV(hiv, random = FALSE), map(seq(iterations-1), ~permuteHIV(hiv, random = TRUE)))
  )
)

permutation
#calculating the p-value



  permutation <- permutation %>% 
    arrange(-ate) %>% 
    mutate(rank = seq(iterations))
  
  p_value <- permutation %>% 
    filter(iteration == 1) %>% 
    pull(rank)/iterations
  

  # 4
  
hist(permutation$ate, breaks = 150)


  #5















































