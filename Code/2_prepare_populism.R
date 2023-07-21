source("Code/1_load_on18.R")

#Get the distribution

populism<-on18 %>% 
  select(agreepopulism_1, agreepopulism_2, agreepopulism_3, agreepopulism_4,agreepopulism_6,  agreepopulism_7)

#Reverse codes agreepopulism_1, agreepopulism_2, and agreepopulism_6
#For Levente
populism.out<-populism
#Levente's Names
names(populism.out)<-c('Ppl1', 'Ppl2', 'Ant1', 'Ant2', 'Man1', 'Man2')


populism_cor_matrix<-cor(populism, use='complete.obs')
populism_cor_matrix
library(GGally)

ggpairs(populism.out[complete.cases(populism.out),], upper=list(continuous='smooth'), lower=list(continuous='cor'))

populism_text<-on18.labs[names(on18.labs) %in% names(populism)]
populism_text
library(stringr)
unlist(populism_text)

Text<-str_extract(unlist(populism_text),"[[:punct:]].*[[:punct:]]" )
detach('package:stringr')
populism_text<-data.frame(Variable=rownames(as.matrix(populism_text)), Text=Text)
populism_text
library(forcats)
populism
names(populism)
populism_text
populism %>% 
  gather(Variable, Value) %>% 
  full_join(., populism_text) %>% 
  group_by(Variable) %>% 
  mutate(avg=mean(Value, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Variable=fct_reorder(Variable, avg),
         Text=fct_reorder(Text, avg)) %>% 
  ggplot(., aes(x=Variable,y= Value, group=Variable)) +geom_boxplot(aes(col=Variable))+theme(axis.text.x=element_text(angle=45, hjust=1))+scale_colour_manual(values=rep(c('black'), 12), labels=populism_text$Text)

#Reverse code and Rescale
names(populism)
populism %>% 
 select(agreepopulism_1, agreepopulism_3, agreepopulism_6) %>% 
  mutate_all(funs(rev=dplyr::recode(., 4,3,2,1))) %>% 
  select(ends_with('rev')) %>% 
  bind_cols(populism, .)-> populism

populism %>% 
  select(agreepopulism_2, agreepopulism_4, agreepopulism_7, ends_with('rev')) %>% 
  mutate_all(funs(rescale=dplyr::recode(., 0, 0.33, 0.66, 1))) %>% 
  select_all(., funs(gsub('agree', '', .))) %>% 
  select(ends_with('rescale'))-> populism

populism %>% 
mutate(populism_mean=rowMeans(., na.rm=T)) %>% 
  bind_cols(on18, .)-> on18

names(on18)

#Populism by party
on18$Vote
names(on18)
on18 %>% 
  select(Vote, populism_mean, imm_mean, stress, comfort) %>% 
  gather(Variable, Average, -Vote) %>% 
  ggplot(., aes(x=Vote, y=Average, group=Vote))+geom_boxplot(aes(col=Vote))+facet_wrap(~Variable)





