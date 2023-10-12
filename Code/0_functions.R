#### FUNCTIONS FOR POLARIZATION DATA ANALYSIS ####

# Binary recode function
binaryrecode <-  function(x){
  if_else(x==1, 1, 0)
}

# Standard Error

se <- function(x, ...){
  sd <- sd(x,...)
  n <- n()
  se <- sd/sqrt(n)
}

# Number of Standard Deviations from the mean 
# Adapted from Polacko, M. (2022). Inequality, policy polarization and the income gap in turnout. Party Politics, 28(4), 739-754. for individuals. 

sd_mean <- function(x, ...){
  sqrt(((x - mean(x, ...))/ sd(x, ...))^2)
}


#### EXTRACT INTERACTION MODELS ####

extract_model <- function(x){
mods_interact[[2]][[x]] %>% 
  ggpredict(c("Interest", "Primary_Media")) 
}


#### GRAPH INTERACTION MODELS ####

graph_interaction <- function(df){
  df %>% 
    ggplot( aes(x = x, y = predicted)) + 
    geom_line(colour = "forestgreen") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
    facet_wrap(~group) + theme_bw() 
}


