#Problem of news bubbles
source("Code/1_load_on18.R")
library(labelled)
lookfor(on18, "news")
on18%>% 
  lookfor(., "news")
on18 %>% 
  select(num_range("primarynews_", range=1:7)) %>% 
  pivot_longer(., cols=everything())
names(on18)

on18 %>% 
 # select(id, CBCTV:HuffingtonPostonline) %>% 
  rowwise() %>% 
  mutate(n_news=rowSums(across(CBCTV:HuffingtonPostonline), na.rm=T)) %>% 
  ungroup()->on18
names(on18)
on18 %>% 
  select(CBCTV:HuffingtonPostonline) %>% 
  names()
#What proportion of respondents live in news bubbles
table(on18$n_news)
#Differentiate high interest from low-interest news bubblers
#Correlate # of news with interest
look_for(on18, "interest")
cor(on18$n_news, on18$pointerst_ONint, use="complete.obs")
on18 %>% 
  ggplot(., aes(x=n_news, y=pointerst_ONint))+geom_point()+
  geom_smooth(method="loess")+geom_jitter()

#Crosstab high interest news bubblers with low-interest news-bubblers

qplot(on18$pointerst_ONint, geom="histogram", main="Distribution of interest in Ontario 2018 election")

on18$Interest<-cut(on18$pointerst_ONint, breaks=3, labels=c("Low", "Medium", "High"))

#News Bubblers 
on18$News<-Recode(on18$n_news,as.factor=T, "0='None'; 1='One' ; 2='Two' ; 3:99='More than two'", 
       levels=c("None", "One", "Two", "More than two"))
table(on18$Interest, on18$n_news)
on18$Vote
on18 %>% 
  select(Vote, News, Interest) %>% 
  group_by(Interest, News, Vote) %>% 
  summarize(n=n()) %>% 
  filter(!is.na(Interest)) %>% 
  filter(!is.na(Vote)) %>% 
  mutate(Percent=n/sum(n)) %>% 
ggplot(., aes(x=News, y=n, fill=Vote))+
  geom_col(position="dodge")+
  facet_grid(~Interest)+scale_fill_manual(values=c("darkred", "darkblue", "orange", "darkgreen"))
