library(haven)
library(labelled)
str(ipsos)
class(ipsos$resp_gender)

ipsos<-read_sav('Data/ipsos_on18.sav')

ipsos.vars<-var_label(ipsos)
ipsos.vals<-val_labels(ipsos)

#leader approval
ipsos %>% 
  select(starts_with('Q16')) %>% 
  mutate_all(funs(as_factor(.))) -> leader_approval


data.frame(text=unlist(ipsos.vars)) %>% 
  rownames_to_column() %>% 
  filter(., grepl('^Q16', rowname)) ->out

library(stringr)
head(out)
out$text<-str_replace_all(out$text, 'Someone who has |Someone who will |Someone who |Someone I ', '')

out$text2<-c('Protect minorities', 'Get things done', 'Manage tough times', 'Fight for the middle class', 'Deal with traffic', 'Tackle deficit', 'Improve health care', 'provide good government', 'Represent my values', 'Improve Housing affordability', 'Can trust', 'Right temperament to be Premier', 'spent taxpayers\' money wisely', 'sort out hydro', 'stand up for Ontario', 'like to have beer or coffee with', 'cant\'t trust' )

names(leader_approval)<-out$text2

#Get Most important problem variables
ipsos%>% 
  select(starts_with('Q28')) %>% 
  var_label() %>% 
  unlist() %>% 
  data.frame() %>% 
  rownames_to_column()-> most_important_problem

names(most_important_problem)<-c('var', 'text')
most_important_problem$text

most_important_problem$text2<-c('Corruption', 'Economy and Jobs', 'Debt', 'Health Care', 'Roads', 'Public Infrastructure', 'Public transportation', 'Education Funding', 'Lower Taxes', 'Aboriginal issues', 'Small business issues', 'Tolerance for minorities', 'Integrity in Government', 'Hydro', 'Minimum Wage', 'Social Assistance','Big Cities', 'Small-town, rural', 'Commuter issues', 'Student issues','Climate Change' , 'Marijuana', 'New immigrant issues', 'Other' )
ipsos %>% 
  select(starts_with('Q28')) %>% 
  setNames(most_important_problem$text2) %>% 
  bind_cols(ipsos, .) -> ipsos


library(car)
class(ipsos$Q3)
ipsos$Vote<-Recode(as.numeric(ipsos$Q3), "1='PC' ;2='Liberal'; 3='NDP' ; 4='Other' ; else=NA")
look_for(ipsos, "Small business")
ipsos$Q28_10_Q28_new_scale
ipsos$Q41

look_for(ipsos, "weight")
ipsos$weight2
ipsos$weight4
prop.table(table(ipsos$Vote))
library(srvyr)

ipsos$Gender<-to_factor(ipsos$resp_gender)

#Make survey designs
ipsos2<-as_survey_design(ipsos, weights=weight2)
#Chc
ipsos2
ipsos2 %>%
 group_by(Vote) %>% 
 filter(is.na(Vote)==F) %>% 
  summarize(Percent=survey_mean(),Weighted_Vote=survey_total())
ipsos2 %>%
 group_by(Gender) %>% 
 filter(is.na(Gender)==F) %>% 
  summarize(Percent=survey_mean(),Weighted_Vote=survey_total())


ipsos3 %>%
 group_by(Vote) %>% 
  filter(is.na(Vote)==F) %>% 
  summarize(Percent=survey_mean(),Weighted_Vote=survey_total())
ipsos3<-as_survey_design(ipsos, weights=weight4)

ipsos3 %>% 
  group_by(Gender) %>% 
  summarize(Percent=survey_mean())
library(tidyverse)
ipsos$Q41
theme_set(theme_bw())
ipsos %>% 
  group_by(employment=to_factor(Q41), Vote) %>%
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(is.na(Vote)==F) %>%
  filter(employment!="Other") %>% 
  filter(Vote!="Other") %>% 
  ggplot(., aes(x=fct_rev(fct_relevel(employment, "Self-employed","Retired", "Homemaker")), y=pct, fill=Vote))+geom_col(position="dodge")+scale_fill_manual(values=c('darkred', 'orange', "darkblue"))+labs(y="Percent", title="ON18 Vote by Position in Labour Market", x="Labour Market")+coord_flip()
ggsave(filename="labour_market_vote_ontario_2018.png")
ipsos %>% 
  group_by(Vote, Q41) %>%
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(Q41==3)

ipsos$Q42
ipsos %>% 
  group_by(job=to_factor(Q42), Vote) %>% 
  summarize(n=n()) %>% 
  mutate(pct=n/sum(n)) %>% 
  filter(str_detect(job, "Retail")) 
  
