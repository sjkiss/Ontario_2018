library(here)
source(here("Code/1_load_on18.R"))

#Now read in the spellchecked emotions file. 
library(rio)
library(openxlsx)
out<-read.csv(here("Data/feelings_workfile_spellcheck.csv"))
#out<-import(here("Data/feelings_workfile_spellcheck_Aug27.xlsx"))
head(out)
out$indivfinfeel[14]
out$indivfinfeel[38]


on18$indivfinfeel %>%
  map(., nchar) %>% 
  unlist() %>% 
  summary()
nrow(out)
nrow(on18)
mean(out$id)
mean(on18$id)
summary(on18$id)
summary(out$id)
length(on18$id)
length(out$id)
#Adjust the id#s

out$id<-out$id+37
head(out$id)
nrow(out)
nrow(on18)

#there is one extra row in on18 that did not get spellchecked. It doesn't look like a quality response, but it should be kept. 
names(on18)
names(out)
summary(on18$id)
summary(out$id)
on18$immfeel
on18$indivfinfeel

#This file adds in spell-checked and piloted emotions files
on18$indivfinfeel[14]
on18$indivfinfeel[38]


#Note after this join indivfinfeel.x and immfeel.x ARE NOT SPELLCHECKED
#indivfinfeel.y and immfeel.y ARE SPELLCHECKED
on18 %>% 
  left_join(., out, by='id') -> on18

on18 %>% 
  rename(imm.x=immfeel.x, imm.y=immfeel.y, fin.x=indivfinfeel.x, fin.y=indivfinfeel.y)->on18
# on18$fin.y<-str_to_lower(on18$fin.y)
# on18$imm.y<-str_to_lower(on18$imm.y)
names(on18)
# #Read in pilot study
# pilot<-import(file = here("Data/qualitative_text_analysis_pilot_complete.xlsx"), format='Excel', which=1)
# head(pilot$id)
# head(pilot$id+37)
# head(on18$id)
# 
# #Adjust pilot id numbers for merging
# pilot$id<-pilot$id+37
# names(on18)
# names(pilot)
# #Merge
# on18 %>%
#   left_join(., pilot, by='id') %>% 
#   select(-starts_with('notes'), -ends_with('feel1')) ->on18

# ##Change emotion variable names
# names(on18)
# on18$emotion_imm
# on18 %>% 
#   mutate_at(., vars(contains('emotion_')), labelled, c('anger'=1, 'anticipation'=2, 'disgust'=3, 'fear'=4, 'joy'=5, 'sadness'=6, 'surprise'=7, 'trust'=8)) %>% 
#   modify_at(., vars(contains('emotion')), as_factor)->on18
# names(on18)
# on18$emotion_fin
# library(tidytext)
# library(textdata)
#This line imports nrc from the web
#nrc<-get_sentiments('nrc')


#write.csv(nrc, "/Users/skiss/OneDrive - Wilfrid Laurier University/Coding/sentiment-lexicons/nrc.csv")
#This line reads my local copy in
#nrc<-read.csv("/Users/skiss/OneDrive - Wilfrid Laurier University/Coding/sentiment-lexicons/nrc.csv")
#write.csv(nrc, "Data/nrc.csv")
#nrc<-read.csv(file="Data/nrc.csv")
# nrc %>% 
#   filter(.,sentiment!='negative'&sentiment!='positive')-> nrc
# 

#

#### LSD ####
library(quanteda)
on18$fin.y
on18$indivfin2
library(stringr)
names(on18)
# 
# ## n-gram modification
# # nrc %>% 
# #   filter(str_detect(word,"^ok")|str_detect(word, "okay"))
# names(on18)
# on18$words_fin
# on18 %>% 
#   mutate(fin.y=str_replace_all(fin.y, "okay", "ok"), 
#          imm.y=str_replace_all(imm.y, "okay", "ok")) %>% 
#   mutate(imm.y=str_replace_all(imm.y, "immigrants", "immigrant"))->on18
# 
# names(on18)
# #Get counts of financial emotion words
# 
# on18 %>% 
#   select(id, fin.y) %>% 
#   unnest_tokens(word, fin.y, token='words') %>% 
#   inner_join(., nrc, by='word') %>% 
#   count(id, sentiment) %>% 
#   spread(., key=sentiment, value=n)-> financial_emotions_count 
# financial_emotions_count
# 
# #Get counts of immigration words
# 
# on18 %>% 
#   select(id, imm.y) %>% 
#   unnest_tokens(word, imm.y, token='words') %>% 
#   inner_join(., nrc, by='word') %>% 
#   count(id, sentiment) %>% 
#   spread(., key=sentiment, value=n)-> immigration_emotions_count 
# immigration_emotions_count
# 
# financial_emotions_count %>% 
#   select(-id, -negative, -positive) %>% 
#   cor(., use='pairwise.complete.obs')->financial_emotions_correlation
# 
# immigration_emotions_count %>% 
#   select(-id, -negative, -positive) %>% 
#   cor(., use='pairwise.complete.obs') -> immigration_emotions_correlation
# 
# 
# financial_emotions_correlation %>% 
#   psych::scree(., factors=F, pc=T)->financial_emotions_scree
# 
# 
# immigration_emotions_correlation %>% 
#   psych::scree(., factors=T, pc=T)->immigration_emotions_scree
# immigration_emotions_correlation
# 
# 
# library(psych)
# 
# 
# on18 %>% 
#   select(id, fin.y) %>% 
#   unnest_tokens(word, fin.y, token='words') %>% 
#   inner_join(., nrc, by='word') %>% 
#   count(id, sentiment) %>% 
#   spread(., key=sentiment, value=n) %>% 
#   select(-id, -negative, -positive) %>% 
#   cor(., use='pairwise.complete.obs') %>% 
#   psych::principal(., nfactors=2, rotate='varimax')->financial_pca_raw_count
# financial_pca_raw_count
# 
# on18 %>% 
#   select(id, imm.y) %>% 
#   unnest_tokens(word, imm.y, token='words') %>% 
#   inner_join(., nrc, by='word') %>% 
#   count(id, sentiment) %>% 
#   spread(., key=sentiment, value=n) %>% 
#   select(-id, -negative, -positive) %>% 
#   cor(., use='pairwise.complete.obs') %>% 
#   psych::principal(., nfactors=2, rotate='varimax')->immigration_pca_raw_count
# immigration_pca_raw_count
# 
# 
# #Rejoint the term frequency counts back into on18
# names(on18)
# 
# on18 %>% 
#   # select(id, fin.y) %>% 
#   unnest_tokens(word, fin.y, token='words') %>% 
#   count(id, word) %>% 
#   bind_tf_idf(., term='word', document='id', n='n') %>% 
#   inner_join(., nrc, by='word') %>% 
#   select(-word) %>% 
#   select(-X) %>% 
#   pivot_wider(., names_from=c("sentiment"),id_cols=c("id"), values_from=c("n", "tf", "idf", "tf_idf"), values_fill=0, values_fn=max)->fin_emotions_count_wgt
# 
# 
# on18 %>%
#   select(id, imm.y) %>% 
#   unnest_tokens(word, imm.y, token='words') %>% 
#   count(id, word) %>% 
#   bind_tf_idf(., term='word', document='id', n='n') %>% 
#   inner_join(., nrc, by='word') %>% 
#   select(-word) %>% 
#   select(-X) %>% 
#   pivot_wider(., names_from=c("sentiment"),id_cols=c("id"), values_from=c("n", "tf", "idf", "tf_idf"), values_fill=0, values_fn=max)->imm_emotions_count_wgt
# 
# ### on18.out includes multiple rows per respondent and includes the actual dictionary words thatwere found. 
# on18 %>% 
#   left_join(., imm_emotions_count_wgt, by="id") %>% 
#   left_join(., fin_emotions_count_wgt,suffix=c("_imm", "_fin") ,by="id")->on18
# names(on18)
# on18 %>% 
#   select(matches("^idf_"))
# on18 %>% 
#   mutate_at(vars(matches("^idf_")), ~set_variable_labels(., .labels=rep("Inverse document frequency of emotion from NRC dictionary")))->on18
# on18 %>% 
#   mutate_at(vars(matches("^tf_")), ~set_variable_labels(., .labels=rep("Weighted term frequency of emotion from NRC dictionary")))->on18
# on18 %>% 
#   mutate_at(vars(matches("^n_")), ~set_variable_labels(., .labels=rep("raw count of emotion from NRC dictionary")))->on18
# on18 %>% 
#   mutate_at(vars(matches("^tf_idf_")), ~set_variable_labels(., .labels=rep("term frequency - inverse document frequency of emotion from NRC dictinoary")))->on18
# 
# # on18 %>% 
# #   mutate(imm_sum=rowSums(select(., anger_imm:trust_imm)),
# #          imm_pos=rowSums(select(., joy_imm, surprise_imm, trust_imm, anticipation_imm)), 
# #          imm_neg=rowSums(select(., anger_imm, disgust_imm, fear_imm, sadness_imm)),
# #          fin_sum=rowSums(select(., anger_fin:trust_fin)), 
# #          fin_pos=rowSums(select(., joy_fin, surprise_fin, trust_fin, anticipation_fin)),
# #          fin_neg=rowSums(select(., anger_fin, disgust_fin, fear_fin, sadness_fin)),
# #          imm_net=scales::rescale(imm_pos-imm_neg),
# #          fin_net=scales::rescale(fin_pos-fin_neg)
# #          ) ->on18
# 
# 
# 
