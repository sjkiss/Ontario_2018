#load libraries
library(tidyverse)
library(haven)
library(here)
library(labelled)
library(broom)
library(ggeffects)
library(modelsummary)
library(ggpubr)
#### Import Data#### 
on18<-read_sav(file=here("Data/Ontario ES 2018 LISPOP.sav"))

#### Run straightliner ####
source(here("Code/2_straightlining.r"))
#### Remove straighliners###
straightliners <- on18 %>% 
  filter(straightliner == 1)
on18 %>% 
  filter(!straightliner>0)->on18
names(on18)
table(on18$`filter_$`)
library(car)
#### Individual Finanical Situation Questions ####
#Recode $50 stress question, 1 disagree, 0 disagree
#Note for all economic questions, indivfin, I am recoding 1 to be the rich, comfortable or better answer. Overall, rich people will have higher values. Variables marked with_rev are coded in the other direction. 

on18$indivfin
on18$indiv_stress<-recode(as.numeric(on18$indivfin), "1=0; 2=0.33; 3=0.66; 4=1")
on18$indiv_stress_rev<-recode(as.numeric(on18$indivfin), "1=1; 2=0.66; 3=0.33; 4=0")
  
#recode ESS income question 0 = very difficult, 1 - very comfortable
on18$indivfin2
  on18$indiv_comfort<-recode(as.numeric(on18$indivfin2), "1=1; 4=0; 2=0.66; 3=0.33")

# 
# 
# #recode ESS income question0 = very difficult, 1 - very comfortable
# on18$indivfin2_rev<-recode(as.numeric(on18$indivfin2), "1=0; 4=1; 2=0.33; 3=0.66")

#recode personal financial situation 0=Worse, 1 =gotten better
on18$indivfin3

#recode personal financial situation 0=Worse, 1 =gotten better

on18$indiv_better<-recode(as.numeric(on18$indivfin3), "1=1; 2=0; 3=0.5")
#recode personal financial situation 1=Worse, 0 =gotten better

on18$indiv_better_rev<-recode(as.numeric(on18$indivfin3), "1=0; 2=1; 3=0.5")
#policies of the government

on18$indiv_policies<-recode(as.numeric(on18$indivfinonpol), "1=1; 2=0; 3=0.5")
lookfor(on18, 'econ')
#Recode View of Economic performance
on18$onecon_better<-recode(as.numeric(on18$oneconyear), "1=1; 2=0; 3=0.5")
on18$onecon_better_rev<-recode(as.numeric(on18$oneconyear), "1=0; 2=1; 3=0.5")

#Recode View of government policies on econom
on18$oneconpol_better<-recode(as.numeric(on18$oneconpol), "1=1; 2=0; 3=0.5")
on18$oneconpol_better_rev<-recode(as.numeric(on18$oneconpol), "1=0; 2=1; 3=0.5")


#immigration
lookfor(on18, 'immigration')
lookfor(on18, 'refu')
names(on18)

#### Recode Immigration Questions ####
#All immigration variables are coded such that 1 is a pro-immigration attitude
#reverse code Immigration Questions 1 and 3
library(car)

names(on18)
lookfor(on18, 'immigrat')

lookfor(on18, 'agreeref')
val_labels(on18$agreerefimm_1)
var_label(on18$agreerefimm_1)
on18$imm_refugees<-Recode(as.numeric(on18$agreerefimm_1), "5=1; 1=0; 3=0.5; 2=0.25; 4=0.75", as.numeric=T)
on18$imm_criminals<-Recode(as.numeric(on18$agreerefimm_2), "5=0; 1=1; 3=0.5; 2=0.75; 4=0.25" , as.numeric=T)

on18$imm_jobs<-Recode(as.numeric(on18$agreerefimm_3), "5=1; 1=0; 3=0.5; 2=0.25; 4=0.75", as.numeric=T)
4
on18$imm_netbenefit<-Recode(as.numeric(on18$agreerefimm_4), "5=0; 1=1; 3=0.5; 2=0.75; 4=0.25", as.numeric=T)
class(on18$imm_refugees)


#Uncomment if you want to reverse code the immigration items
# I think this creates a series of variables where 1 is the anti-immigration position. 
# Important to check with crosstabs against original variables and data dictionary.

on18 %>% 
  select(starts_with('imm_')) %>% 
  mutate_all(funs(rev=Recode(., "0=1;  0.25=0.75; 0.5=0.5; 0.75=0.25; 1=0", as.numeric=T))) %>% 
  select(ends_with('rev')) %>% 
  mutate(imm_rev=rowMeans(., na.rm=T)) %>% 
bind_cols(on18, .)-> on18

library(stringr)
#### Modify some variable labels on immigration for pretty prettying####
#This code just extracts the bit between [] and sets it as the variable label
lookfor(on18, 'agreerefimm')
on18 %>% 
  select(starts_with('agreerefimm')) %>% 
  var_label() %>% 
 map(., str_extract, '(\\[.*?\\])') ->modified_immigration_labels
var_label(on18)<-modified_immigration_labels
on18 %>% 
  select(starts_with('agreerefimm')) %>% 
  var_label()
# Set variable label for individual financial stress.
var_label(on18$indivfin)<-'An extra $50 a month would really make a difference'

#### Set Vote Variable ####
on18$Vote<-Recode(as.numeric(on18$partyvote2018), "1='Liberal' ; 2='PC' ; 3='NDP' ; 4='Green'", levels=c('Liberal', 'PC', 'NDP', 'Green', 'DK'), as.factor=T)
on18$advote2018
on18 %>%
  mutate(Vote=case_when(
    partyvote2018==1|advvote2018==1~1,
    partyvote2018==2|advvote2018==2~2,
    partyvote2018==3|advvote2018==3~3,
    partyvote2018==4|advvote2018==4~4

  ))->on18

val_labels(on18$Vote)<-c(Liberal=1, Conservative=2, NDP=3, Green=4)
on18$Vote<-as_factor(on18$Vote)
on18$Vote
on18$pc<-recode(as.numeric(on18$partyvote2018), "5=NA ; 2=1; else=0")


on18$ndp<-recode(as.numeric(on18$partyvote2018), "3=1; 5=NA; else=0")
on18$Vote
#on14vote
lookfor(on18, 'vote')

on18$pc14<-Recode(as.numeric(on18$partyvote2014), " 2=1; else=0 ")

on18$ndp14<-recode(as.numeric(on18$partyvote2014), "5=NA ; 3=1; else=0")


on18$Vote2014<-recode(as.numeric(on18$partyvote2014), "1='Liberal' ; 2='Conservative' ; 3='NDP' ; 4='Green' ; 0=NA")

on18 %>% 
  unite(col='voteswing', sep='-',c(Vote2014, Vote), remove=F) ->on18

on18$Vote2014

####Climate Change####

on18$area<-as_factor(on18$urbansubrur)

on18$climatechng
on18$climate<-recode(as.numeric(on18$climatechng), "1='Human' ; 2='Natural';else=NA", as.factor=T)
on18$parisagmt
on18$paris<-Recode(as.numeric(on18$parisagmt), "1='Keep cap trade'; 2='Carbon tax' ; 3='Federal solution'", as.factor=T, levels=c('Keep cap trade', 'Carbon tax', 'Federal solution'))


on18$paris_bin<-Recode(as.numeric(on18$parisagmt), "1=0; 2=0 ; 3=1", as.factor=F)
on18$parisagmt
on18$climatechng
on18$climate_bin<-Recode(as.numeric(on18$climatechng), "1=0; 2=1")

#### Hydro bills gotten better or worse ####
# hydro 1= Gotten Better 0=Gotten Worse

lookfor(on18, 'hydro')
on18$hydro
on18$hydro2<-Recode(as.numeric(on18$hydro), "1=1; 2=0; 3=0.5")
table(on18$hydro, as.numeric(on18$hydro))
on18$hydro2_rev<-Recode(as.numeric(on18$hydro), "1=0; 2=1; 3=0.5")
on18$hydro<-Recode(as.numeric(on18$hydro), "1='Gotten better'; 2='Gotten worse' ; 3='Stayed the same'", levels=c('Gotten worse', 'Stayed the same', 'Gotten better'), as.factor=T)

lookfor(on18, 'education')
on18$education
#### Demographics ####
# Degree 
on18$degree<-Recode(as.numeric(on18$education), "1:8=0 ; 9:11=1")
val_labels(on18$degree)<-c("No degree"=0, "Degree"=1)
# Age
lookfor(on18, 'year')
on18$YOB
summary(on18$age)
on18 <- on18 %>% 
  mutate(age = replace(age, age == 117, NA))
on18$age2<-recode(on18$age, "45:100=1; 0:44=0; else=NA")
on18$age3<-scales::rescale(on18$age, to=c(0,1))

#Rescale Political Interest
on18$Interest<-scales::rescale(as.numeric(on18$pointerst_ONint), to=c(0,1))
# Income
#Income
on18$income
library(labelled)

val_labels(on18$income)<-c('less than $20,000'=1, 
                           '$20,000-$39,999'=2,
                           '$40,000 and $59,999'=3,
                           '$60,000 and $79,999'=4,
                           '$80,000 and $99,999'=5,
                           '$100,000 and $119,999'=6,
                           '$120,000 and $139,999'=7,
                           '$140,000 and $159,999'=8,
                           '$160,000 or more'=9)
on18$income
on18$income2<-as.numeric(on18$income)

on18$income3<-scales::rescale(on18$income2, to=c(0,1))

#### Party Evaluations ####
# Check party evals for missing values

on18 %>% 
  select(contains('eval_')) %>% 
  mutate_all(funs(Recode(., "98=NA"))) %>% 
  mutate_all(funs(out=scales::rescale(as.numeric(.), to=c(0,1)))) %>% 
  select(ends_with('out')) %>% 
  bind_cols(on18, .)-> on18


#### Self-Reported News Media Outlets#### 
#Add in news media outlet names
#Get names of news sources
on18 %>% 
  #Select variables that start with dailynews
  select(starts_with('dailynews_')) %>% 
  #Get their variable labels
  var_label() %>% 
#Unlist
    unlist() %>% 
#Turn to dataframe
  data.frame() %>%
  #Take rownames and turn to columns
  rownames_to_column() %>%
  #Rename the variables
  rename(., text=., variable=rowname) %>%
 # filter(., grepl('^dailynews_', variable)) %>%
  #Dump into out
  select(text) -> out
#Check
out
#Text modificaditon, eliminating strings
out$text
out$text<-str_replace_all( as.character(out$text), '[:punct:]print or online[:punct:]','')
out$text
#out$text<-str_replace_all(out$text, '[:punct:]Toronto[:punct:]|[:punct:]other local station[:punct:]', 'Local')
out$text<-  str_extract(as.character(out$text), '.+[:punct:] Which')
out$text<-str_replace_all(out$text, ' Which', '')
out$text<-str_replace_all(out$text, '[:punct:]', '')
out$text
#Take the news media consumption variables and dump them into news
on18 %>%
  select(starts_with('dailynews')) -> news
names(news)
#ASsign new names from out
names(news)<-out$text
#Get rid of out
rm(out)
#get rid of one unnecessary column
ncol(news)
news<-news[,1:23]
head(news)
#Remove whitespace
names(news)<-str_replace_all(names(news), ' ', '')
names(on18)
#bind back to on18
on18<-bind_cols(on18, news)
#Check
names(on18)
# 
# on18 %>% 
#   mutate(Newspaper=scales::rescale(as.numeric(NationalPost+TorontoStar+TorontoSun+BramptonGuardian+HamiltonSpectator+MississaugaNews+KingstonWhigStandard+LondonFreePress+OttawaCitizen+WaterlooRecord+WindsorStar+Otherlocalnewspaper))) ->on18
# 
# #TV
# names(on18)
# on18 %>% 
#   mutate(TV=scales::rescale(as.numeric(CBCTV+GlobalNews+CTVNewsToronto+CTVNewsotherlocalstation+OMNI+CityTV))) ->on18
# names(on18)  


#
#### Media Consumption Habits ####
#Define TV watchers
on18 %>%
  mutate(TV=case_when(
    CBCTV == 1 ~1,
     GlobalNews==1 ~1,
     CTVNewsToronto == 1 ~1,
    OMNI == 1 ~1,
   CityTV==1 ~1,
   TRUE~0
  )) -> on18
#Define Newspaper Watchers
on18 %>%
mutate(Newspaper=case_when(
  GlobeandMail == 1 ~ 1,
  NationalPost == 1 ~ 1,
  BramptonGuardian == 1 ~ 1,
  HamiltonSpectator == 1 ~ 1,
  MississaugaNews == 1 ~ 1,
  KingstonWhigStandard ==1 ~ 1,
  LondonFreePress == 1 ~ 1,
  OttawaCitizen == 1 ~ 1,
  WaterlooRecord == 1 ~ 1,
  WindsorStar == 1 ~ 1,
  Otherlocalnewspaper == 1 ~ 1,
  TorontoStar == 1 ~ 1,
  TRUE ~ 0
))-> on18

on18 %>%
  mutate(Online=case_when(
    HuffingtonPostonline ==1 ~ 1,
    CBConline == 1 ~ 1,
    TRUE ~ 0
  ))-> on18

# #Find exclusive media
#
# names(on18)
#
on18 %>%
  mutate(TV_exclusive=case_when(
    TV==1 & Newspaper==0 & Online==0 ~ 1,
    TRUE ~ 0
  ))->on18
on18 %>%
  mutate(Newspaper_exclusive=case_when(
    TV==0 & Newspaper==1 & Online==0 ~ 1,
    TRUE ~ 0
  ))->on18

on18 %>% 
  mutate(Media=case_when(
    TV_exclusive==1 ~ 'TV Exclusive',
    Newspaper_exclusive==1 ~ 'Newspaper Exclusive',
    TRUE ~ 'Mixed'
  ))->on18
on18$Media<-factor(on18$Media)
#Add in ethnicities
ethnicities<-rio::import(here('data/ethnicities_read_in.csv'))

on18$ethnicity1<-ifelse(on18$ethnicity %in% ethnicities$ethnicity, ethnicities$ethnicity1, NA)
on18$ethnicity2<-ifelse(on18$ethnicity %in% ethnicities$ethnicity, ethnicities$ethnicity2, NA)
on18$ethnicity1<-ifelse(on18$ethnicity=='', NA, on18$ethnicity1)
on18$ethnicity2<-ifelse(on18$ethnicity=='', NA, on18$ethnicity2)

on18$ethnicity1<-recode(on18$ethnicity1, "1='white'; 2='indigenous'; 3='african-caribbean'; 4='asian' ; 5='latin american'; 6='canadian'; 7='middle eastern' ; 8='jewish' ; 9='mixed' ; else=NA; ''=NA", as.factor=T)
on18$ethnicity2<-recode(on18$ethnicity2, "1='white'; 2='indigenous'; 3='african-caribbean'; 4='east asian' ; 5='latin american'; 6='canadian'; 7='middle eastern' ; 8='jewish' ; 9='mixed' ; 10='south asian'; else=NA", as.factor=T)

#Check
on18$ethnicity
table(on18$ethnicity1, on18$ethnicity2)
table(on18$ethnicity, on18$ethnicity1)
on18 %>% 
  filter(ethnicity=='') %>% 
  select(ethnicity1)
table(on18$ethnicity, on18$ethnicity1)
names(on18)
var_label(on18)
names(on18)
names(on18)

#Satisfaction
on18 %>% 
  select(starts_with('satisf'))
on18$gov_satisfaction<-ifelse(is.na(on18$satisfyontgvmnt) ==T, on18$satisfywynne, on18$satisfyontgvmnt)

#on18$satisfaction<-Recode(on18$satisfaction, "4=1; 3=2; 2=3; 1=4")
on18$gov_satisfaction<-Recode(on18$gov_satisfaction, "4=1; 3=0.66; 2=0.33; 1=0", as.numeric=T)
on18$wynne_satisfaction<-Recode(on18$satisfywynne, "4=1; 3=0.66; 2=0.33; 1=0", as.numeric=T)
### People of color
on18$selfborninCA
table(on18$ethnicity1)
table(on18$ethnicity1, on18$selfborninCA)
on18 %>% 
  mutate(status=case_when(
    #This line defines white, canadian born canadians as whites
    selfborninCA==1 & ethnicity1=="white" ~ "white",
    #this line defines all other Canadian born as visible minorities
    selfborninCA==1 & (ethnicity1!="canadian"| ethnicity1!="white") ~ "vismin",
    #this line defines "Canadian"canadian-born canadians as na
    selfborninCA==1 & ethnicity!="canadian"~ NA_character_,
   
    #indigeneous as missing
    ethnicity1=="indgenous" ~ NA_character_,
    #Not born in canada as immigtrant
    selfborninCA==2 ~ "immigrant",
   #All else as missing
     TRUE ~ NA_character_
  ))->on18
table(on18$status)
on18$status<-factor(on18$status, levels=c("immigrant", "vismin", "white"))

on18 %>% 
  mutate(white=case_when(
    status!="immigrant" & ethnicity1=="white" ~ 1,
    status!="immigrant" & ethnicity1=="canadian" ~ 0,
  #  status!="immigrant" ~ 0,
    TRUE ~ NA_real_
  ))->on18

table(on18$status, on18$ethnicity1)
table(on18$ethnicity1)
table(on18$white)


val_labels(on18$white)<-c("White"=1, "Canadian"=0)

on18 %>% 
  rename(voted2014=vote2014)->on18

#source("Code/2_add_emotion_variables.R")
on18$regions<-as_factor(on18$regions)
#Add in variable labels
# on18 %>% 
#   set_variable_labels(emotion_fin="Qualitative rating of r immigration response",
#                       intensity_fin="Qualitative rating of r financial emotions", 
#                       words_fin="Qualitative rating of key words in r financial response", 
#                       issues_fin="Qualitative rating of key issues in r financial response", 
#                       emotion_imm="Qualitative rating of r immigration response", 
#                       intensity_imm="Qualitative rating of r immigration emotions", 
#                       words_imm="Qualitative rating of key words in r immigration response",
#                       issues_imm="Qualitative rating of key issues in r immigration response", 
#                       fin.x="Unspellchecked text of response to question about individual financial situation", 
#                       fin.y="Spellchecked text of response to question about individual financial situation",
#                       imm.x="Unspellchecked text of response to question about immigration", 
#                       imm.y="Spellchecked text of response to question about immigration" ,
#                       FSA="Forward Sortation Area", 
#                       regions="Regions - check with Andrea Perrella", 
#                       indiv_stress="indivfin recoded 0 to 1", 
#                       indiv_stress_rev="indivfin recoded 0 to 1 and reverssed", 
#                       indiv_comfort="indivfin2 recoded 0 to 1", 
#                       #indiv_comfort_rev="indivfin2, recoded 0 to 1 and reversed", 
#                       indiv_better="indivfin3 recoded 0 to 1", 
#                       indiv_better_rev="indivfin3 recoded 0 to 1 and reversed",
#                       indiv_policies="on18$indivfinonpol, recoded 0 to 1", 
#                       onecon_better="Recoded oneconyear, 0 to 1", 
#                       onecon_better_rev="Recoeded oneconyear, 0 to 1 and reversed", 
#                       oneconpol_better="Recoded oneconpol, 0 to 1", 
#                       oneconpol_better_rev="Recoded oneconpol better 0 to 1 and reversed",
#                       TV="R reported watching any TV outlets", 
#                       Newspaper="R reported reading any newspapers", 
#                       Online="R reported reading any online news outlets",
#                       TV_exclusive="R reported watching only TV", 
#                       Newspaper_exclusive="R reported reading only newspapers", 
#                       Media="R is TV watcher, newspaper reader or mixsed", 
#                       ethnicity1="Ethnicity using 1 coding scheme", 
#                       ethnicity2="Ethnicity collapsing some categories", 
#                       white="Dichotomous variable, R is white",
#                       income2="income transformed to numeric",
#                       income3="income scaled 0 to 1",
#                       imm_refugees="agreerefimm1 0 to 1",
#                       imm_criminals="agreerefimm2 0 to 1",
#                       imm_jobs="agreerefimm3 0 to 1",
#                       imm_netbenefit="agreerefimm4 0 to1"                      , 
#                       voteswing="Combined variable of 2014 and 2018 vote choices",
#                       Vote="2018 Vote", 
#                       Vote2014="Vote choice in 2014",
#                       pc="Dichotomous variable voted PC in 2018", 
#                       ndp="Voted NDP in 2018",
#                       pc14="Voted PC in 2014", 
#                       ndp14="Voted NDP in 2014", 
#                       area="Urban Suburban or Rural", 
#                       climate="Blame for climate change", 
#                       paris="From parisagmt, factor variable", 
#                       paris_bin="Dichtomous variable, 0 = keep cap and trade or carbon tax, 1=federal solution", 
#                       hydro2="from hydro scaled 0 to 1", 
#                       hydro2_rev="from hydro scaled 0 to 1 and reversed")->on18


#### Combine Trust in media ####
on18$trustbiasnews2<-scales::rescale(as.numeric(on18$trustbiasnews))
on18$trustfactnews2<-scales::rescale(as.numeric(on18$trustfactnews))
on18$trustfactnews2
# This calculates the average of the two. 
on18 %>% rowwise() %>% mutate(trust_media= mean(c(trustbiasnews2,trustfactnews2), na.rm=T))->on18

#### Create Social Media Users####

on18 %>% 
  mutate(Social_Use=case_when(
    socialmedia==1 ~ freqsocialmedia,
    socialmedia==2 ~ 0
  ))->on18
on18 %>% 
  add_value_labels(Social_Use=c(Never=0))->on18
table(as_factor(on18$Social_Use))
on18$Social_Use<-as_factor(on18$Social_Use)
on18$Social_Use2<-car::Recode(on18$Social_Use, "'Never'='Never' ; 
'Several times in a year'='Less than once a week' ;
                              'About once a month'='Less than once a week' ;
                              'Several times a month'='Less than once a week';
                              'Never'='Less than once a week'", 
                              levels=c('Never', 
                                       'Less than once a week', 
                                       'About once a week', 
 'Several times a week', 
'About once a day', 
'Several times a day'))
table(on18$Social_Use2)

#### Export to SPSS File ####
names(on18)
#write_sav(on18, path=here("data/on18_with_emotion_responses.sav"))


