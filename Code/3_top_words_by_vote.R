source('2_separate_text_answers_wordclouds.R')
immigration %>% 
  count(Vote, word, sort=T) %>% 
  anti_join(stop_words) %>% 
  filter(is.na(Vote)==F) %>% 
  group_by(Vote) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  mutate(word=fct_reorder(as.factor(word),n,.fun=sum, desc=F)) %>% 
  mutate(question=rep('Immigration', nrow(.)))-> top_immigration


top_financial_plot<-top_financial %>% 
  ggplot(., aes(x=word, y=n, fill=Vote))+geom_col(position='dodge')+coord_flip()+scale_fill_manual(values=c('red', 'blue', 'orange','darkgreen'))
top_financial %>% filter(Vote=='PC')
top_financial_plot
top_immigration_plot<-top_immigration %>% 
  ggplot(., aes(x=word, y=n, fill=Vote))+geom_col(position='dodge')+coord_flip()+scale_fill_manual(values=c('red', 'blue', 'orange','darkgreen'))


#Correlate negative -positive ratio
financial

financial %>% 
select(word, id)%>% 
  inner_join(., nrc) %>% 
  filter(sentiment=='positive'|sentiment=='negative') %>% 
  count(id, sentiment, word) %>% 
  spread(., sentiment, n, fill=0) %>%
  group_by(id) %>% 
  summarize_at(vars(negative, positive), funs(sum)) %>% 
  mutate(econ_sentiment=positive-negative)-> financial_net_sentiment

immigration %>% 
  select(word, id)%>% 
  inner_join(., nrc) %>% 
  filter(sentiment=='positive'|sentiment=='negative') %>% 
  count(id, sentiment, word) %>% 
  spread(., sentiment, n, fill=0) %>%
  group_by(id) %>% 
  summarize_at(vars(negative, positive), funs(sum)) %>% 
  mutate(immigration_sentiment=positive-negative)-> immigration_net_sentiment
immigration_net_sentiment
dev.off()
immigration_net_sentiment
immigration_net_sentiment %>% 
  qplot(immigration_sentiment, geom='histogram', data=.)
summary(immigration_net_sentiment)


financial_net_sentiment %>% 
  select(id, econ_sentiment) %>% 
  inner_join(., on18, by='id')-> on18
immigration_net_sentiment %>% 
  select(id, immigration_sentiment) %>% 
  inner_join(., on18, by='id')-> on18
library(GGally)
on18 %>% 
  select(econ_stress, econ_comfort, econ_better, econ_policies, econ_macro, econ_sentiment) %>% 
  rename(., `Stressed`=econ_stress, `Living Comfortably`=`econ_comfort`, `Situation gotten better`=econ_better, `ON policies made you better off`=econ_policies, `ON economy better`=econ_macro, `Sentiment`=econ_sentiment) %>% 

ggpairs(., lower=list(continuous='smooth'),labeller = label_wrap_gen(10))
ggsave('economic_sentiment_correlation.png')

on18 %>% 
  select(starts_with('imm_'), immigration_sentiment) %>% 
rename(., `Fake Refugees`=imm_refugees, `Criminals`=imm_criminals, `Take jobs`=imm_jobs, `Net beneft`=`imm_netbenefit`, `Mean Immigration`=imm_mean, `Sentiment`=`immigration_sentiment`) %>% 
ggpairs(., lower=list(continuous='smooth'),labeller = label_wrap_gen(10))
ggsave('immigration_sentiment_correlation.png')
#Econ correlation
cor(on18$econ_sentiment, on18$immigration_sentiment, use='complete.obs')
on18 %>% 
  filter(econ_sentiment>-4&econ_sentiment<4) %>% 
  ggplot(., aes(x=econ_sentiment, y=immigration_sentiment))+geom_point()+geom_smooth(method='loess')
qplot(econ_sentiment, immigration_sentiment, data=on18, geom=c('point' ,'smooth'))

library(tidytext)
nrc<-get_sentiments('nrc')

nrc_emotions %>% 
  filter(term=='income')
