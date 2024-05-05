#### FUNCTIONS FOR POLARIZATION DATA ANALYSIS ####
pacman::p_load("tidyverse",
               "bayestestR",
               "mousetrap",
               "lavaan",
               "psych",
               "overlapping")
# Binary recode function for political knowledge
binaryrecode <-  function(x){
  if_else(x==1, 1, 0)
}

#rescale from 0 to 1

range01 <- function(x, ...){(x-min(x, ...))/(max(x, ...)-min(x, ...))} 

# Standard Error

se <- function(x, ...){
  sd <- sd(x,...)
  n <- n()
  se <- sd/sqrt(n)
}

#### GRAPH REGRESSION MODEL ####

graph_regression <- function(model, graphname){
  require(tidyverse)
 r <- tidy(model)
ci <- confint(model, level = 0.95, HC_type = "HC0") %>% 
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


#Calculate Affective Polarization Scores

unweighted_like_scores <- function(parties, df){
  
  data <- df[, parties]
  
  data$mean_like <- ifelse(
    rowSums(!is.na(data)) >= 2, 
    rowMeans(data, na.rm = T)
    , NA
  )
  data$distance <- rowSums((data[, parties] - data$mean_like)^2, na.rm = T)/(rowSums(!is.na(data[, parties])))
  data$spread <- modify(data$distance, sqrt)
  data$spread
  
}



