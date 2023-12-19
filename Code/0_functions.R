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

#### GRAPH REGRESSION MODEL ####

graph_regression <- function(model, graphname){
  require(tidyverse)
 r <- tidy(model)
ci <- confint(model, level = 0.95) %>% 
  data.frame() %>% 
  rename("conf.low_95" = "X2.5..", "conf.high_95" = "X97.5..")
r <- bind_cols(r, ci) %>% 
  rename(Variable = term, 
         Coefficient = estimate,
         SE = std.error) %>%
  filter(!Variable %in% c("(Intercept)"))
r <- r %>% select(-SE, -statistic, - p.value)

ggplot(r, aes(x = Variable, y = Coefficient)) +  geom_hline(yintercept = 0, color = gray(1/2), lty =2) + geom_point()+
  geom_linerange(aes(x = Variable, ymin = conf.low_95, ymax = conf.high_95)) + theme_bw() + coord_flip()
}

# library(calculus)
# 
# f <- function(x, y){ abs(rnorm(x, 0, 1) - rnorm(y, 2.3, 5.4))}
# int2(f, c(1,2), c(2,4))
# integral(f(100,100), bounds = list(x = c(-Inf, Inf)))
