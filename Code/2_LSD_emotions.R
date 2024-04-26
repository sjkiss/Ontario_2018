source("Code/1_LSDprep_dec2017.R")
source("Code/2_add_emotion_variables.R")
library(here)
on18 %>% 
  ungroup() ->on18

on18 %>%   select(imm.y) %>% 
  slice(285) %>% 
  View()
#### LSD ####
library(quanteda)
on18$fin.y.LSD<-LSDprep_contr(on18$fin.y)
on18$imm.y.LSD<-LSDprep_contr(on18$imm.y)
on18$fin.y.LSD1<-LSDprep_dict_punct(on18$fin.y.LSD)
on18$imm.y.LSD1<-LSDprep_dict_punct(on18$imm.y.LSD)

#Check that this worked
on18 %>% 
  filter(str_detect(imm.y.LSD1, "xtoo")) %>% 
  select(imm.y.LSD1)

on18$fin.y.LSD2<-LSDprep_punctspace(on18$fin.y.LSD1)
on18$imm.y.LSD2<-LSDprep_punctspace(on18$imm.y.LSD1)
#Check that this works
on18 %>% 
  select(ends_with("LSD2")) %>% 
  print(n=100)

on18$fin.y.LSD3<-LSDprep_negation(on18$fin.y.LSD2)
on18$imm.y.LSD3<-LSDprep_negation(on18$imm.y.LSD2)
on18 %>% 
  select(ends_with("LSD3")) %>% 
  print(n=100)

on18 %>% 
  filter(str_detect(fin.y, pattern = " (N|n)ot much " )) %>% 
  select(fin.y, fin.y.LSD3)

# 
on18$fin.y.LSD4<-LSDprep_dict(on18$fin.y.LSD3)
on18$imm.y.LSD4<-LSDprep_dict(on18$imm.y.LSD3)
# on18 %>% 
#   select(ends_with("LSD4")) %>% 
#   View()
library(quanteda)
<<<<<<< HEAD
=======

>>>>>>> 85b0a28075816087ea0a3851127dc2354717b2e9
tokens_lookup(tokens(on18$fin.y.LSD4), 
              dictionary=data_dictionary_LSD2015)
tokens_lookup(tokens(on18$imm.y.LSD4), 
              dictionary=data_dictionary_LSD2015)
# Note row 4 is showing two words that are returning negative and negative positive
# Row 5 is showing two words that are showing positive and positive. 
# What are those words
kwic(tokens(on18$fin.y.LSD4), pattern=data_dictionary_LSD2015)
# kwic(tokens(on18$imm.y.LSD4), pattern=data_dictionary_LSD2015) %>% 
#   View()

#Get proportion counts
tokens (on18$fin.y.LSD4) %>% 
  dfm() %>% 
  dfm_lookup(., dictionary=data_dictionary_LSD2015) ->fin_dfm
tokens (on18$imm.y.LSD4) %>% 
  dfm() %>% 
  dfm_lookup(., dictionary=data_dictionary_LSD2015) ->imm_dfm
#Why is Item 4 showing now only one negative
colSums(fin_dfm)# Somehow dfm_lookup is not finding the neg_positive and neg_negative.
# #chck this. 
#Repeat but get proportions
fin_dfm
tokens(on18$fin.y.LSD4) %>% 
  dfm() %>%
  dfm_weight(., scheme="prop") %>% 
  dfm_lookup(., dictionary=data_dictionary_LSD2015) ->fin_dfm
tokens(on18$imm.y.LSD4) %>% 
  dfm() %>%
  dfm_weight(., scheme="prop") %>% 
  dfm_lookup(., dictionary=data_dictionary_LSD2015) ->imm_dfm
fin_dfm
imm_dfm
# Take the fin_dfm of proportions of negative and positive words about personal financial sentiment
fin_dfm %>% 
  #Convert to data frame
  quanteda::convert(., to="data.frame") %>% 
  #Subtract negative from positive as per Young and Soroka (2012) p. 215
  mutate(fin_sentiment=positive-negative) %>% 
  #Select only fin_sentement
  select(fin_sentiment) %>% 
  #bind cols to on18
  bind_cols(on18, .)->on18
#Repeat with immigration sentiment
imm_dfm %>% 
  quanteda::convert(., to="data.frame") %>% 
  mutate(imm_sentiment=positive-negative) %>% 
  select(imm_sentiment) %>% 
  bind_cols(on18, .)->on18


#Create Discrete emotions dictionary - Emotions are too scarce to do anything

# Discrete_emotions <- dictionary(
#                     list(
#                         anger = scan("Data/DED.Mar22.anger.txt", character(), quote = ""),
#                         aniexty = scan("Data/DED.Mar22.anxiety.txt", character(), quote = ""), 
#                         sadness = scan("Data/DED.Mar22.sadness.txt", character(), quote = ""), 
#                         optimism = scan("Data/DED.Mar22.optimism.txt", character(), quote = "")
#                     ))
# 
# tokens(on18$fin.y.LSD4) %>% 
#   dfm() %>%
#   dfm_weight(., scheme="prop") %>% 
#   dfm_lookup(., dictionary=Discrete_emotions) ->fin_des_dfm
# tokens(on18$imm.y.LSD4) %>% 
#   dfm() %>%
#   dfm_weight(., scheme="prop") %>% 
#   dfm_lookup(., dictionary=Discrete_emotions) ->imm_des_dfm

#Correlate
cor(on18$fin_sentiment, on18$imm_sentiment)


names(on18)
on18$Social_Use2
on18$Social_Use
table(on18$Social_Use, on18$Social_Use2)
table(on18$Social_Use2)
table(on18$Social_Use)

on18 %>% 
  select(Social_Use2, ends_with("sentiment")) %>% 
  pivot_longer(-1) %>% 
  group_by(Social_Use2, name) %>% 
  summarize(Average=mean(value, na.rm=T)) %>% 
  filter(!is.na(Social_Use2)) %>% 
  ggplot(., aes(x=Social_Use2, y=Average))+geom_point()+facet_wrap(~name)
<<<<<<< HEAD
  
#pivot_wider(., names_from = name, values_from=Average)
=======
 # pivot_wider(., names_from = name, values_from=Average)
>>>>>>> 85b0a28075816087ea0a3851127dc2354717b2e9

  
lm(fin_sentiment~Social_Use2, data=on18)
summary(lm(imm_sentiment~Social_Use2, data=on18))

names(on18)
on18$Media
summary(lm(fin_sentiment~Media, data=on18))
summary(lm(imm_sentiment~Media, data=on18))
summary(lm(imm_sentiment ~ WAP, data = on18))
summary(lm(fin_sentiment ~ WAP, data = on18))

