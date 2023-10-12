#Get qualitative answers
source('Code/1_load_on18.R')
nrow(on18)
look('feel', on18.labs)
names(on18)
on18$id
# on18 %>% x
#   select(indivfinfeel, immfeel) %>% 
#   write_csv(., path='feelings_texts.csv'  )
library(tidytext)
library(tidyverse)
library(wordcloud)
nrc<-get_sentiments('nrc')


#Read in spell-checked file
spell_checked_feelings<-import('feelings_workfile_spellcheck_Aug27.xlsx')
spell_checked_feelings %>% 
  select(-id) %>% 
  bind_cols(on18, .) -> on18

on18 %>% 
  select(Vote, indivfinfeel, id) %>% 
  unnest_tokens(word,indivfinfeel) -> financial

on18 %>% 
  select(Vote, immfeel, id) %>% 
  unnest_tokens(word, immfeel)-> immigration
library(forcats)
png(filename='./plots/financial.png', width=480, height=480, res=150, type='quartz')

#financial %>% 
  count(word, sort=T) %>% 
  anti_join(stop_words) %>% 
  with(wordcloud(word, n, scale=c(2.5, 0.2), min.freq=20, random.order=F))-> top_financial_plot
#dev.off()


#png(filename='immigration.png', width=480, height=480, res=150, type='quartz')
immigration %>% 
  count(word, sort=T) %>% 
  anti_join(stop_words) %>% 
  with(wordcloud(word, n, scale=c(3, 0.2), min.freq=30, random.order=F))
#dev.off()

#By vote
financial %>% 
  count(Vote, word, sort=T) %>% 
  anti_join(stop_words) %>% 
  filter(is.na(Vote)==F) %>% 
  group_by(Vote) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  mutate(word=fct_reorder(as.factor(word),n,.fun=sum, desc=F)) %>% 
  mutate(question=rep('Financial', nrow(.)))->financial_by_vote_plot
 

nrow(on18)
names(on18)
