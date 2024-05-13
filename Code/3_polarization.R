#### LOAD SCRIPTS ####
library(here)
source(here("Code/1_load_on18.R")) #clean and load dataset
source(here("Code/0_functions.R")) #load custom functions and packages needed for these analyses
#source("Code/2_LSD_emotions.R")
<<<<<<< HEAD
nrow(on18)
=======
library(estimatr)
>>>>>>> 9cd867d37dfe59154474f9c7c3ec82b62eccf811
#### CHECK THE DATASET ####
# head(on18)
# names(on18)
# glimpse(on18)





#### CREATE VARIABLES FOR POLARIZATION ANALYSIS ####

#### Primary News Source ####
#Create a variable that classifies individuals based on the primary media type they use
on18 %>%
  mutate(Primary_media=case_when(
    (primarynews_7 == 1 | primarynews_6 == 1) & (primarynews_5 == 0 & primarynews_4 == 0 & primarynews_3 == 0 & primarynews_2 == 0 & primarynews_1 == 0) ~ "Social_Media", #For individuals who only use Social Media
    (primarynews_4 == 1 | primarynews_5 == 1) & (primarynews_6 == 0 & primarynews_7 == 0 & primarynews_3 == 0 & primarynews_2 == 0 & primarynews_1 == 0) ~ "Online", #For individuals who only use online sources that are not social media
    (primarynews_3 == 1 | primarynews_2 == 1 | primarynews_1 == 1) & (primarynews_5 == 0 & primarynews_4 == 0 & primarynews_6 == 0 & primarynews_7 == 0) ~ "Legacy", #For individuals who only receive news from television or newspapers 
    TRUE ~ "Mixed"
    
  ))->on18



#table(on18$Primary_)

# on18 <- on18 %>% 
#   mutate(
#     Primary_media = relevel(as.factor(Primary_media), ref = "Mixed")
#   )

#### Political Knowledge #### 

#Create a political knowledge using three political knowledge questions.
# R gets 1 if they got each response right
# sum to 3 divide by 3 to get one variable indicating their knowledge

on18 %>% 
  select(unsg:nhse) %>% 
  glimpse()
#This prints the variable labels of each item to see what each measures
on18 %>% 
  select(unsg:nhse) %>% 
  var_label()
#this prints the value lablels to see what each value label contains
on18 %>% 
  select(unsg:nhse) %>% 
  val_labels()

on18 %>% 
  mutate(
    across(.cols=unsg:nhse, 
           binaryrecode, #recode function from 0_functions
           .names="{.col}_correct")
  )->on18

on18 %>% 
  select(ends_with("_correct"))


lookfor(on18, "seats in the House of Commons")

#political knowledge variable
on18$pol_knowledge <- (on18$unsg_correct + on18$financename_correct +
                         on18$ggname_correct + on18$nhse_correct)/4

table(on18$pol_knowledge)


# Defensive coding for political knowledge variable
on18 %>% 
  ggplot(., aes(x=pol_knowledge))+geom_histogram()

on18 %>% 
  select(ends_with("correct")| "pol_knowledge") #%>% 
  #View()
on18$pol_knowledge
on18 %>%
  group_by(Social_Use2) %>%
  summarise(mean = mean(pol_knowledge, na.rm = T), sd = sd(pol_knowledge, na.rm = T)) 
stopifnot((isTRUE(all.equal(on18$pol_knowledge, 
                            ((on18$unsg_correct + on18$financename_correct +
                                on18$ggname_correct + on18$nhse_correct)/4))))) #Check to ensure the math is correct


#### POLARIZATION MEASURES ####
#### policy polarization ####
on18$climate_views <- (on18$climate_bin + on18$paris_bin)/2


#create a dataframe with all the policies
on18 %>% 
  select(starts_with("agreement_"))-> policies 

#rename policies with more descriptive names
policy_names <- c("help_racial_minorities", "help_women", "more_coporate_tax", "more_personal_tax", "private_health_care", 
                  "minimum_wage_to_high_prices", "income_inequality", "business_benefits_everyone", 
                  "drug_benefit_u25", "free_post_secondary", "inappropriate_sex_ed")
names(policies)<-policy_names

#recombine
policies <- mutate_all(policies, \(x) as.numeric(as.character(x)))
policies <- range01(policies, na.rm = T)

on18 <- policies %>%  
  bind_cols(., on18)

#Create a separate data set for descriptive statistics

policies %>%  
  bind_cols(., on18$Social_Use2)->policies_social_use

names(policies_social_use)<-c(policy_names, "Social_Use2")

#### POLICY DISTRIBUTIONS ####

#Create Policy Polarization Distribution
on18$agreement_1

#Recode policy polarization variables so that 0 represents the most left-wing position and 1 represents the most right-wing position
on18 <- on18 %>% 
  mutate(
    lr_private_health_care = case_match(private_health_care,
                                        0 ~ 1, 0.25 ~ 0.75, 0.5 ~ 0.5, 0.75 ~ 0.25, 1 ~ 0, NA ~ NA),
    Lr_minimum_wage_to_high_prices = case_match(minimum_wage_to_high_prices,
                                                0 ~ 1, 0.25 ~ 0.75, 0.5 ~ 0.5, 0.75 ~ 0.25, 1 ~ 0, NA ~ NA),
    lr_business_benefits_everyone = case_match(business_benefits_everyone,
                                               0 ~ 1, 0.25 ~ 0.75, 0.5 ~ 0.5, 0.75 ~ 0.25, 1 ~ 0, NA ~ NA), 
    lr_inappropriate_sex_ed = case_match(inappropriate_sex_ed, 
                                         0 ~ 1, 0.25 ~ 0.75, 0.5 ~ 0.5, 0.75 ~ 0.25, 1 ~ 0, NA ~ NA)
    
  )

#Create a variable for the policy positions of all individuals 
on18 <- on18 %>%  
  mutate(policy_polarization = (help_racial_minorities + help_women + more_coporate_tax +
                                  more_personal_tax + income_inequality + drug_benefit_u25 + free_post_secondary +
                                 lr_private_health_care + Lr_minimum_wage_to_high_prices + lr_business_benefits_everyone + 
                                  lr_inappropriate_sex_ed))  


#### Distributions for distinctiveness coefficients ####

# Primary Media

on18$partyvote2018


#legacy primary media - left-wing
policy_left_legacy <- on18 %>% 
  filter(Primary_media == "Legacy" & (partyvote2018 == 1 | partyvote2018 == 3)) %>% 
  select(policy_polarization)

#online primary media - left-wing
policy_left_online <- on18 %>% 
  filter(Primary_media == "Online" & (partyvote2018 == 1 | partyvote2018 == 3)) %>% 
  select(policy_polarization)

#social media primary media - left-wing
policy_left_social_media <- on18 %>% 
  filter(Primary_media == "Social_Media" & (partyvote2018 == 1 | partyvote2018 == 3)) %>% 
  select(policy_polarization)

#mixed primary media - left-wing
policy_left_mixed <- on18 %>% 
  filter(Primary_media == "Mixed" & (partyvote2018 == 1 | partyvote2018 == 3)) %>% 
  select(policy_polarization)

#on18$partyvote2018

#legacy primary media - right-wing
policy_conservatives_legacy <- on18 %>% 
  filter(Primary_media == "Legacy" & partyvote2018 == 2) %>% 
  select(policy_polarization)

#online primary media - right-wing
policy_conservatives_online <- on18 %>% 
  filter(Primary_media == "Online" & partyvote2018 == 2) %>% 
  select(policy_polarization)

#social media primary media - right-wing
policy_conservatives_media <- on18 %>% 
  filter(Primary_media == "Social_Media" & partyvote2018 == 2) %>% 
  select(policy_polarization)

# mixed primary media - right-wing
policy_conservatives_mixed <- on18 %>% 
  filter(Primary_media == "Mixed" & partyvote2018 == 2) %>% 
  select(policy_polarization)

# Social Media Use

#unique(on18$Social_Use)

# High social use - left-wing
policy_left_often <- on18 %>% 
  filter((Social_Use2 == "Several times a day" | 
            Social_Use2 == "About once a day" | 
            Social_Use2 == "Several times a week" | 
            Social_Use2 == "About once a week"  | 
            Social_Use2 == "Several times a month") &
           (partyvote2018 == 1 | partyvote2018 == 3)) %>% 
  select(policy_polarization) 

# low social use - left-wing
policy_left_rarely <- on18 %>% 
  filter((Social_Use2 == "Several times in a year" |
            Social_Use2 == "Never" | 
            Social_Use2 == "About once a month") &
           (partyvote2018 == 1 | partyvote2018 == 3)) %>% 
  select(policy_polarization)



# high social use - right-wing
policy_conservative_often <- on18 %>% 
  filter((Social_Use2 == "Several times a day" |
            Social_Use2 == "About once a day" |
            Social_Use2 == "Several times a week" |
            Social_Use2 == "About once a week" | 
            Social_Use2 == "Several times a month") & partyvote2018 == 2) %>% 
  select(policy_polarization)

# low social use - right-wing
policy_conservative_social_rarely <- on18 %>% 
  filter((Social_Use2 == "Several times in a year" | 
            Social_Use2 == "Never" |
            Social_Use2 == "About once a month" ) & partyvote2018 == 2) %>% 
  select(policy_polarization)

#### Distributions for bimodality coefficients ####

### Primary media 
bimodalities <- on18 %>% 
  select(c(Primary_media, policy_polarization)) %>% 
  rename("mt_id" = "policy_polarization", "Condition" = "Primary_media") %>% 
  list(data = .)
# primary media legacy
policy_legacy <- on18 %>% 
  filter(Primary_media == "Legacy") %>% 
  select(policy_polarization)
bimodality_coefficient(policy_legacy, na.rm=T)
# primary media online
policy_online <- on18 %>% 
  filter(Primary_media == "Online") %>% 
  select(policy_polarization)

#primary media social media 
policy_social_media <- on18 %>% 
  filter(Primary_media == "Social_Media") %>% 
  select(policy_polarization)

#primary media mixed
policy_mixed <- on18 %>% 
  filter(Primary_media == "Mixed") %>% 
  select(policy_polarization) 


### Social Use

#high social use
policy_often <- on18 %>% 
  filter((Social_Use2 == "Several times a day" |
            Social_Use2 == "About once a day" |
            Social_Use2 == "Several times a week" |
            Social_Use2 == "About once a week"  | 
            Social_Use2 == "Several times a month")) %>% 
  select(policy_polarization) 

# low social use
policy_rarely <- on18 %>% 
  filter((Social_Use2 == "Several times in a year" |
            Social_Use2 == "Never" | Social_Use2 == "About once a month")) %>% 
  select(policy_polarization)

bimodality_coefficient(policy_often, na.rm = T)
bimodality_coefficient(policy_rarely, na.rm = T)

#### Affective Polarization Wagner (2021) ####

#Extract feelings towards parties scores
# on18 %>% 
#   select(starts_with("partyeval")) %>% 
#   glimpse()
# so it looks like I took the step sometime of 
# scaling these variables to 0 and 1, and they end in 
# _out.
# on18 %>% 
#   select(starts_with("partyeval")) %>% 
#   summary()
#Looks like the originals run from 0 to 5
# on18 %>% 
#   select(starts_with("partyeval")) %>% 
#   val_labels()
# on18 %>% 
#   select(starts_with("partyeval")) %>% 
#   var_label()
#Party Numbers
#4 is Green
# 1 is Liberal
# 3 is NDP
# 2 is OPC
# 0 (0) is Really dislike and 5 (1) is really like. 

#Extract _out and create a new dataset to calculate affective polarization scores
on18 %>% 
  select(id,starts_with("partyeval")&ends_with("out"))->affect

<<<<<<< HEAD
=======

>>>>>>> 9cd867d37dfe59154474f9c7c3ec82b62eccf811
on18 <- on18 %>% 
  rename(Green_Like = partyeval_4_out, 
         Liberal_Like = partyeval_1_out, 
         NDP_Like = partyeval_3_out,
         PC_Like = partyeval_2_out)


<<<<<<< HEAD



=======
>>>>>>> 9cd867d37dfe59154474f9c7c3ec82b62eccf811
names(affect)

affect %>% 
  rename(Green_Like=2, Liberal_Like=3, NDP_Like=4, PC_Like=5)->affect

#Create a new dataset for the weighted score
affect_wt <- affect

#For each respondent, we will need an average like for the parties
# This is mean(like)_i
#So we need to effectively calculate a mean for each row. 
#Each row is one R. 

# This is a billion times faster
# Never use c_across(). Banish it. 
affect %>% 
  mutate(mean_like=rowMeans(across(2:5), na.rm=T))->affect
# affect %>% 
#   rowwise() %>% 
#   mutate(mean_like=mean(c_across(2:5), na.rm=T))->affect

#Now we need to take each R's like score for each party
# And subtract the mean from it. 

affect_pol_cal <- affect %>%
  pivot_longer(cols = !c(id, mean_like),
               names_to = "Party", 
               values_to = "Party_like_score")

affect_pol_cal
affect_pol_cal$like_mean <- (affect_pol_cal$Party_like_score - affect_pol_cal$mean_like)^2


affect_pol_cal %>%
  group_by(id) %>%
  summarise(Soc_dis = sum(like_mean)) -> Soc_dis_scores

Soc_dis_scores$Soc_dis <- sqrt(Soc_dis_scores$Soc_dis/4)


full_join(on18, Soc_dis_scores, by = join_by(id)) -> on18

PARTIES <- c("Green_Like", "Liberal_Like", "NDP_Like", "PC_Like")

on18$Spread <- unweighted_like_scores(PARTIES, on18)


PARTIES <- c("Green_Like", "Liberal_Like", "NDP_Like", "PC_Like")

on18$Spread <- unweighted_like_scores(PARTIES, on18)

on18 <- on18 %>% 
  mutate(Spread_sd = scale(Spread))

#### Weighted Affective Polarization (WAP) Scores ####

affect_wt$Green_Like_wt <- (affect$Green_Like * 0.046)
affect_wt$PC_Like_wt <- (affect$PC_Like * 0.405)
affect_wt$Liberal_Like_wt <- (affect$Liberal_Like * 0.196)
affect_wt$NDP_Like_wt <- (affect$NDP_Like * 0.336)

affect_wt %>% 
  mutate(mean_like=rowMeans(across(6:9), na.rm=T))->affect_wt
# affect_wt %>% 
#   rowwise() %>% 
#   mutate(mean_like=mean(c_across(6:9), na.rm=T))->affect_wt

affect_pol_cal_wt <- affect_wt %>%
  select(., !(6:9)) %>%
  pivot_longer(cols = !c(id, mean_like),
               names_to = "Party", 
               values_to = "Party_like_score")

affect_pol_cal_wt %>%
mutate(vote_share=case_when(Party == "Green_Like" ~ 0.046, Party == "Liberal_Like" ~ 0.196, Party == "NDP_Like" ~ 0.336, 
                            Party == "PC_Like" ~ 0.405
  
)) -> affect_pol_cal_wt

affect_pol_cal_wt$like_mean <- (affect_pol_cal_wt$Party_like_score - affect_pol_cal_wt$mean_like)^2
affect_pol_cal_wt$like_mean_wt <- (affect_pol_cal_wt$like_mean * affect_pol_cal_wt$vote_share)

affect_pol_cal_wt %>%
  group_by(id) %>%
  summarise(WAP = sum(like_mean_wt)) -> WAP_scores

WAP_scores$WAP <- sqrt(WAP_scores$WAP)

full_join(on18, WAP_scores, by = join_by(id)) -> on18

on18 <- on18 %>% 
  mutate(
    WAP_sd = as.numeric(scale(WAP))
  )

#Get interest by media consunption
on18 %>%
  group_by(Primary_media) %>%
  summarise(mean = mean(pointerst_ONint, na.rm = T), sd = sd(pointerst_ONint, na.rm = T))



on18 %>%
  group_by(Social_Use2) %>%
  summarise(mean = mean(WAP, na.rm = T), sd = sd(WAP, na.rm = T))

#### WAP Leaders ####

#leader 1 Ford, leader 2 Horwath, leader 3 Schreiner, leader 4 Wynne

leader_affect <- on18 %>% 
  select(c(id, leadereval_1_out:leadereval_4_out)) %>% 
  rename(
    Ford = leadereval_1_out,
    Horwath = leadereval_2_out,
    Schreiner = leadereval_3_out,
    Wynne = leadereval_4_out
  )

leader_affect$Schreiner_Like_wt <- (leader_affect$Schreiner * 0.046)
leader_affect$Ford_Like_wt <- (leader_affect$Ford * 0.405)
leader_affect$Wynne_Like_wt <- (leader_affect$Wynne * 0.196)
leader_affect$Horwath_Like_wt <- (leader_affect$Horwath * 0.336)

#
leader_affect %>% 
  mutate(mean_like=rowMeans(across(6:9), na.rm=T))->leader_affect
# leader_affect %>% 
#   rowwise() %>% 
#   mutate(mean_like=mean(c_across(6:9), na.rm=T))->leader_affect

leader_affect <- leader_affect %>%
  select(., !(6:9)) %>%
  pivot_longer(cols = !c(id, mean_like),
               names_to = "Leader", 
               values_to = "Leader_like_score")

leader_affect %>%
  mutate(vote_share=case_when(Leader == "Schreiner" ~ 0.046, Leader == "Wynne" ~ 0.196, Leader == "Horwath" ~ 0.336, 
                              Leader == "Ford" ~ 0.405
                              
  )) -> leader_affect

leader_affect$like_mean <- (leader_affect$Leader_like_score - leader_affect$mean_like)^2
leader_affect$like_mean_wt <- (leader_affect$like_mean * leader_affect$vote_share)

leader_affect %>%
  group_by(id) %>%
  summarise(WAP_lead = sum(like_mean_wt)) -> leader_affect

leader_affect$WAP_lead <- sqrt(leader_affect$WAP_lead)

full_join(on18, leader_affect, by = join_by(id)) -> on18

on18$wap_difference <- on18$WAP - on18$WAP_lead

on18 <- on18 %>% 
  mutate(
    WAP_lead_sd = as.numeric(scale(WAP_lead))
  )

mean(on18$wap_difference, na.rm = T)


#### DESCRIPTIVE STATISTICS ####


#### Primary Media ####

#Descriptive Statistics for political knowledge by Primary media
table(on18$Social_Use2, on18$pol_knowledge)
on18 %>%
  group_by(Primary_media) %>%
  summarise(mean = mean(pol_knowledge, na.rm = T), sd = sd(pol_knowledge, na.rm = T)) 

#### Primary Media and Media source ####

on18$HuffingtonPostonline

media_Sources_by_primarymedia <- on18 %>% 
  group_by(Primary_media) %>% 
  summarise(across(CBCTV:HuffingtonPostonline, \(x)mean(x))) %>% 
  filter(Primary_media == "Online") %>% 
  pivot_longer(cols = 2:22)


#see the most read sources for online news users
media_Sources_by_primarymedia %>% 
  ggplot(aes(y = name, x = value)) + geom_point() 
  


#### Affective Polarization ####

on18 %>%
  group_by(Social_Use2) %>%
  summarise(mean = mean(WAP, na.rm = T), sd = sd(WAP, na.rm = T), n=n(), se=sd/sqrt(n))->mean_affect_scores 

(on18 %>%
  group_by(partyvote2018) %>%
  summarise(Mean = mean(WAP, na.rm = T), Standard_Deviation = sd(WAP, na.rm = T), Minimum = min(WAP, na.rm = T), Maximum = max(WAP, na.rm = T), N=n()) %>% 
  rename(Group = partyvote2018) -> affect_scores_party)

(on18 %>%
    group_by(Primary_media) %>%
    summarise(Mean = mean(WAP, na.rm = T), Standard_Deviation = sd(WAP, na.rm = T), Minimum = min(WAP, na.rm = T), Maximum = max(WAP, na.rm = T), N=n()) %>% 
    rename(Group = Primary_media) -> affect_scores_primary_media)

mean(on18$WAP, na.rm = T); sd(on18$WAP, na.rm = T)

descripts_WAP <- rbind(tibble(Group = "Ontarians", Mean = mean(on18$WAP, na.rm = T),
             Standard_Deviation = sd(on18$WAP, na.rm = T),
             Minimum = min(on18$WAP, na.rm = T),
             Maximum = max(on18$WAP, na.rm = T),
             N = (length(!is.na(on18$WAP)))), 
on18 %>%
  group_by(Social_Use2) %>%
  summarise(Mean = mean(WAP, na.rm = T), Standard_Deviation = sd(WAP, na.rm = T), Minimum = min(WAP, na.rm = T), Maximum = max(WAP, na.rm = T), N=n()) %>% 
  rename(Group = Social_Use2), affect_scores_party, affect_scores_primary_media)

#kableExtra::kable(descripts_WAP, format = "pipe", digits = 3)

tibble(Group = "Ontarians", Mean = mean(on18$WAP, na.rm = T),
       Standard_Deviation = sd(on18$WAP, na.rm = T),
       Minimum = min(on18$WAP, na.rm = T),
       Maximum = max(on18$WAP, na.rm = T),
       N = (length(!is.na(on18$WAP))))


mean_affect_scores

mean_affect_scores %>% 
  filter(complete.cases(.)) %>% 
  ggplot(., aes(x = mean, 
                y = Social_Use2, 
                colour = Social_Use2)) + 
  geom_point()+
  geom_errorbar(aes (xmin = mean - (1.96*se), xmax = mean + (1.96*se)), width =.2) + 
  labs(x = "Mean", y= "Social Media Use") + 
  theme_bw()  + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank(), 
        strip.text = element_text(size=8))+
  guides(color=guide_legend(reverse=T))+xlim(c(0,0.5)) -> affect_scores_social_use; affect_scores_social_use

on18 %>%
  group_by(Primary_media) %>%
  summarise(mean = mean(Soc_dis, na.rm = T), sd = sd(Soc_dis, na.rm = T))

#### RE-RUN MAIN ANALYSES WITHOUT STRAIGHT LINERS ####

COVARS <- c("Interest", "age3", "degree", "income3", "pol_knowledge")

WAP_primary_media <- list()
for(i in 1:length(COVARS)){
  data <- on18 |> filter(straightliner == 0)
  WAP_primary_media[[i]] <-  lm_robust(reformulate(c("Primary_media", COVARS[1:i]),
                                                   response = "WAP_sd"), data = data, se_type = "HC0")
}
lm_robust(reformulate(c("Primary_media"),
                      response = "WAP_sd"), data = data, se_type = "HC0")
modelsummary(WAP_primary_media, stars = T)



#### Policy Position Distribution ####

#Exploratory factor analysis
cor_policies <- on18 %>% 
  dplyr::select(c(help_racial_minorities, help_women, more_coporate_tax,
                    more_personal_tax, income_inequality, drug_benefit_u25, free_post_secondary,
                    lr_private_health_care, Lr_minimum_wage_to_high_prices, lr_business_benefits_everyone, 
                    lr_inappropriate_sex_ed
  )) %>% 
  cor(., use = "complete.obs") 


eigen(cor_policies)$values
(policy_fa_analysis <- psych::fa(cor_policies, nfactors = 3))

policy_issues <- 'policy =~ help_racial_minorities + help_women + more_coporate_tax +
                                  more_personal_tax + income_inequality + drug_benefit_u25 + free_post_secondary +
                                 lr_private_health_care + Lr_minimum_wage_to_high_prices + lr_business_benefits_everyone + 
                                  lr_inappropriate_sex_ed'
cfa_policies <- cfa(policy_issues, data = on18)
summary(cfa_policies, fit.measures = T, standardized = T)



#### REGRESION MODELS ####

#Create regression models for affective polarization using the weighted affective polarization (WAP) measure
#**** Start thinking about control variables 
WAP_reg <- list()
WAP_graph <- list()

WAP_reg[[1]] <- lm(WAP_sd ~ Primary_media, data = on18, na.action = na.omit);summary(WAP_reg[[1]]) 
WAP_graph[[1]] <- graph_regression(WAP_reg[[1]]); WAP_graph[[1]]

WAP_reg[[2]] <- lm(WAP_sd ~ Interest, data = on18, na.action = na.omit); summary(WAP_reg[[2]]) 
WAP_graph[[2]] <- graph_regression(WAP_reg[[2]]); WAP_graph[[2]]


WAP_reg[[3]] <- lm(WAP_sd ~ Interest + Primary_media, data = on18, na.action = na.omit); summary(WAP_reg[[3]]) 
WAP_graph[[3]] <- graph_regression(WAP_reg[[3]]); WAP_graph[[3]]

WAP_reg[[4]] <- lm(WAP_sd ~ Interest + Primary_media + age3, data = on18, na.action = na.omit); summary(WAP_reg[[4]]) 
WAP_graph[[4]] <- graph_regression(WAP_reg[[4]]); WAP_graph[[4]]


WAP_reg[[5]] <- lm(WAP_sd ~ Interest + Primary_media + age3 + degree, data = on18, na.action = na.omit); summary(WAP_reg[[5]]) 
WAP_graph[[5]] <- graph_regression(WAP_reg[[5]]); WAP_graph[[5]]

WAP_reg[[6]] <- lm(WAP_sd ~ Interest + Primary_media + age3 + degree + income3, data = on18, na.action = na.omit); summary(WAP_reg[[6]]) 
WAP_graph[[6]] <- graph_regression(WAP_reg[[6]]); WAP_graph[[6]]

WAP_reg[[7]] <- lm(WAP_sd ~ Interest + Primary_media + age3 + degree + income3 + pol_knowledge, data = on18, na.action = na.omit); summary(WAP_reg[[7]]) 
WAP_graph[[7]] <- graph_regression(WAP_reg[[7]]); WAP_graph[[7]]
modelsummary::modelsummary(WAP_reg, stars = T)

WAP_Interact <- lm(WAP_sd ~ Primary_media*Interest, data = on18, na.action = na.omit)
summary(WAP_Interact)

WAP_Interact2 <- lm(WAP_sd ~ Primary_media*Interest  + age3 + degree + income3 + pol_knowledge, data = on18, na.action = na.omit)
summary(WAP_Interact2)

WAP_regdf <- map(WAP_reg, tidy, conf.int = TRUE) 
WAP_regdf <- bind_rows(WAP_regdf, .id = "model_num")

WAP_regdf <- WAP_regdf %>% 
  filter(!term == "(Intercept)") %>% 
  mutate(term = case_match(term, 
                             "Primary_mediaSocial_Media" ~ "Social Media (ref. Legacy News)", 
                            "Primary_mediaOnline" ~ "Online News",
                             "Primary_mediaMixed" ~ "Mixed News",
                           "Interest" ~ "Political Interest",
                            "age3" ~  "Age",
                             "degree" ~ "University Degree",
                             "income3" ~ "Income",
                            "pol_knowledge" ~ "Political Knowledge"
                           ),
         term = as.factor(term),
         term = fct_relevel(term, 
                            "Political Knowledge", "Income",
                            "University Degree", "Age", "Political Interest", 
                            "Mixed News", "Online News", 
                            "Social Media (ref. Legacy News)"
                             ),
         model_num = paste0("Model ", model_num)
         )




WAP_regs_PM <- WAP_regdf %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) + 
  geom_vline(xintercept = 0, col = "red", lty = 4) +
  geom_point() + geom_linerange() + facet_wrap(~model_num) +
  labs(x = "Estimates with 95% Confidence Intervals", y = NULL) +
  theme_bw() 




#Visualize the marginal effects from the interaction effects
marginaleffects::plot_slopes(WAP_Interact, variables = "Interest", condition = "Primary_media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(WAP_Interact2, variables = "Interest", condition = "Primary_media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()

#Display Models
modelsummary(WAP_reg, stars = T, vcov = "HC0") #As numbers
WAP_reg_df <- map(WAP_reg, tidy) 
WAP_reg_df_ci <- map(WAP_reg, confint, level = 0.95, HC_type = "HC0")
WAP_reg_df_ci <- map(WAP_reg_df_ci, as_tibble)
WAP_reg_df <- bind_rows(WAP_reg_df, .id = "model_num")  
WAP_reg_df_ci <- bind_rows(WAP_reg_df_ci) %>% 
  rename("conf.low" = "2.5 %", "conf.high" = "97.5 %")
WAP_reg_df <- bind_cols(WAP_reg_df, WAP_reg_df_ci)


COVARS <- c("Primary_media", "Interest", "age3", "degree", "income3", "pol_knowledge")

Spread_primary_media <- list()
for(i in 1:length(COVARS)){
  Spread_primary_media[[i]] <-  lm_robust(reformulate(COVARS[1:i],
                                                   response = "Spread_sd"), data = on18, se_type = "HC0")
}
modelsummary(Spread_primary_media, stars = T)

model_names <- list(
  "1" = "Model 1",
  "2" = "Model 2",
  "3" = "Model 3",
  "4" = "Model 4",
  "5" = "Model 5",
  "6" = "Model 6",
  "7" = "Model 7"
)
model_labeller <- function(variable, value){
  return(model_names[value])
}



WAP_reg_df %>% 
  filter(!term == "(Intercept)") %>% 
  ggplot(aes(x = estimate, y = term, xmax = conf.high, xmin = conf.low)) + geom_point() +
  geom_linerange() + geom_vline(xintercept = 0, lty = 4) + 
  facet_wrap(~model_num, ncol = 4, labeller = model_labeller) + theme_bw()

#Investigating the age coefficient
summary(lm(WAP_sd ~ age, data = on18))
summary(lm(WAP_sd ~ age + I(age^2), data = on18))
age_poly <- lm(WAP_sd ~ Interest + Primary_media  + age + I(age^2), data = on18, na.action = na.omit); summary(age_poly)
summary(lm(WAP_sd ~ Interest + Primary_media + age + I(age^2) + degree + income3 + pol_knowledge, data = on18, na.action = na.omit))


age_legacy <- data.frame(age = seq(18, 100, 1), 
           Primary_media = "Legacy",
           Interest = mean(on18$Interest, na.rm = T))
age_legacy[, c("predicted_WAP",
           "conf_up",
           "conf_low")] <- predict(age_poly, age_legacy, interval = "confidence")

age_legacy$media <- "Legacy"

age_mixed <- data.frame(age = seq(18, 100, 1), 
                         Primary_media = "Mixed",
                         Interest = mean(on18$Interest, na.rm = T))
age_mixed[, c("predicted_WAP",
               "conf_up",
               "conf_low")] <- predict(age_poly, age_mixed, interval = "confidence")

age_mixed$media <- "Mixed"

age_online <- data.frame(age = seq(18, 100, 1), 
                        Primary_media = "Online",
                        Interest = mean(on18$Interest, na.rm = T))
age_online[, c("predicted_WAP",
              "conf_up",
              "conf_low")] <- predict(age_poly, age_online, interval = "confidence")

age_online$media <- "Online"

age_social_media <- data.frame(age = seq(18, 100, 1), 
                         Primary_media = "Social_Media",
                         Interest = mean(on18$Interest, na.rm = T))
age_social_media[, c("predicted_WAP",
               "conf_up",
               "conf_low")] <- predict(age_poly, age_social_media, interval = "confidence")

age_social_media$media <- "Social Media"

age_predicted <- rbind(age_legacy, age_mixed, age_online, age_social_media)
age_predicted_graph <- age_predicted %>% 
  ggplot(aes(x = age, y = predicted_WAP, ymin = conf_low, ymax = conf_up)) +
  geom_line(colour = "#008080") +  facet_wrap(~media) +
  geom_ribbon(alpha = 0.2, fill = "aquamarine3") +
  labs(x = "Age", y = "Predicted Level of Affective Polarization \n (WAP Score)") + 
  theme_bw()
age_predicted_graph
#### Replicate models with the social use variable ####

WAP_reg_soc_use <- list()

WAP_reg_soc_use[[1]] <- lm(WAP_sd ~ Social_Use2, data = on18, na.action = na.omit);summary(WAP_reg_soc_use[[1]]) 
#WAP_graph[[1]] <- graph_regression(WAP_reg_soc_use[[1]]); WAP_graph[[1]]

WAP_reg_soc_use[[2]] <- lm(WAP_sd ~ Interest, data = on18, na.action = na.omit); summary(WAP_reg_soc_use[[2]]) 
#WAP_graph[[2]] <- graph_regression(WAP_reg_soc_use[[2]]); WAP_graph[[2]]


WAP_reg_soc_use[[3]] <- lm(WAP_sd ~ Interest + Social_Use2, data = on18, na.action = na.omit); summary(WAP_reg_soc_use[[3]]) 
#WAP_graph[[3]] <- graph_regression(WAP_reg_soc_use[[3]]); WAP_graph[[3]]

WAP_reg_soc_use[[4]] <- lm(WAP_sd ~ Interest + Social_Use2 + age3, data = on18, na.action = na.omit); summary(WAP_reg_soc_use[[4]]) 
#WAP_graph[[4]] <- graph_regression(WAP_reg_soc_use[[4]]); WAP_graph[[4]]


WAP_reg_soc_use[[5]] <- lm(WAP_sd ~ Interest + Social_Use2 + age3 + degree, data = on18, na.action = na.omit); summary(WAP_reg_soc_use[[5]]) 
#WAP_graph[[5]] <- graph_regression(WAP_reg_soc_use[[5]]); WAP_graph[[5]]

WAP_reg_soc_use[[6]] <- lm(WAP_sd ~ Interest + Social_Use2 + age3 + degree + income3, data = on18, na.action = na.omit); summary(WAP_reg_soc_use[[6]]) 
#WAP_graph[[6]] <- graph_regression(WAP_reg_soc_use[[6]]); WAP_graph[[6]]

WAP_reg_soc_use[[7]] <- lm(WAP_sd ~ Interest + Social_Use2 + age3 + degree + income3 + pol_knowledge, data = on18, na.action = na.omit); summary(WAP_reg_soc_use[[7]]) 
#WAP_graph[[7]] <- graph_regression(WAP_reg_soc_use[[7]]); WAP_graph[[7]]
modelsummary(WAP_reg_soc_use, stars = T, vcov = "HC0")
#ggarrange(plotlist = list(WAP_primarymedia_graph, WAP_Interest_Graph, WAP_Interact_graph))


WAP_reg_soc_usedf <- map(WAP_reg_soc_use, tidy, conf.int = TRUE) 
WAP_reg_soc_usedf <- bind_rows(WAP_reg_soc_usedf, .id = "model_num")

WAP_reg_soc_usedf <- WAP_reg_soc_usedf %>% 
  filter(!term == "(Intercept)") %>% 
  mutate(term = case_match(term, 
                           "Social_Use2Less than once a week" ~ "Less than once a week (ref. Never)", 
                           "Social_Use2About once a week" ~ "Once a week",
                           "Social_Use2Several times a week" ~ "Several Times a Week",
                           "Social_Use2About once a day" ~ "Once a Day",
                           "Social_Use2Several times a day" ~ "Several Times a Day",
                           "Interest" ~ "Political Interest",
                           "age3" ~  "Age",
                           "degree" ~ "University Degree",
                           "income3" ~ "Income",
                           "pol_knowledge" ~ "Political Knowledge"
  ),
  term = as.factor(term),
  term = fct_relevel(term, 
                     "Political Knowledge", "Income",
                     "University Degree", "Age", "Political Interest", 
                     "Several Times a Day", "Once a Day", 
                     "Several Times a Week", 
                     "Once a week",
                     "Less than once a week (ref. Never)"
  ),
  model_num = paste0("Model ", model_num)
  )


WAP_regs_SU <- WAP_reg_soc_usedf %>% 
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) + 
  geom_vline(xintercept = 0, col = "red", lty = 4) +
  geom_point() + geom_linerange() + facet_wrap(~model_num) +
  labs(x = "Estimates with 95% Confidence Intervals", y = NULL) +
  theme_bw() 

#### Affective polarization models ####

#Create regression models for affective polarization using the weighted affective polarization (WAP) measure
WAP_lead_reg <- list()
WAP_lead_graph <- list()

WAP_lead_reg[[1]] <- lm(WAP_lead_sd ~ Primary_media, data = on18, na.action = na.omit);summary(WAP_lead_reg[[1]]) 

WAP_lead_graph[[1]] <- graph_regression(WAP_lead_reg[[1]]); WAP_lead_graph[[1]]

WAP_lead_reg[[2]] <- lm(WAP_lead_sd ~ Interest, data = on18, na.action = na.omit); summary(WAP_lead_reg[[2]]) 
WAP_lead_graph[[2]] <- graph_regression(WAP_lead_reg[[2]]); WAP_lead_graph[[2]]


WAP_lead_reg[[3]] <- lm(WAP_lead_sd ~ Interest + Primary_media, data = on18, na.action = na.omit); summary(WAP_lead_reg[[3]]) 
WAP_lead_graph[[3]] <- graph_regression(WAP_lead_reg[[3]]); WAP_lead_graph[[3]]

WAP_lead_reg[[4]] <- lm(WAP_lead_sd ~ Interest + Primary_media + age3, data = on18, na.action = na.omit); summary(WAP_lead_reg[[4]]) 
WAP_lead_graph[[4]] <- graph_regression(WAP_lead_reg[[4]]); WAP_lead_graph[[4]]

WAP_lead_reg[[5]] <- lm(WAP_lead_sd ~ Interest + Primary_media + age3 + degree, data = on18, na.action = na.omit); summary(WAP_lead_reg[[5]]) 
WAP_lead_graph[[5]] <- graph_regression(WAP_lead_reg[[5]]); WAP_lead_graph[[5]]

WAP_lead_reg[[6]] <- lm(WAP_lead_sd ~ Interest + Primary_media + age3 + degree + income3, data = on18, na.action = na.omit); summary(WAP_lead_reg[[6]]) 
WAP_lead_graph[[6]] <- graph_regression(WAP_lead_reg[[6]]); WAP_lead_graph[[6]]

WAP_lead_reg[[7]] <- lm(WAP_lead_sd ~ Interest + Primary_media + age3 + degree + income3 + pol_knowledge, data = on18, na.action = na.omit); summary(WAP_lead_reg[[7]]) 
WAP_lead_graph[[7]] <- graph_regression(WAP_lead_reg[[7]]); WAP_lead_graph[[7]]

WAP_lead_Interact <- lm(WAP_lead_sd ~ Primary_media*Interest, data = on18, na.action = na.omit)
summary(WAP_lead_Interact)

WAP_lead_Interact2 <- lm(WAP_lead_sd ~ Primary_media*Interest + age3 + degree + income3 + pol_knowledge, data = on18, na.action = na.omit)
summary(WAP_lead_Interact2)

#Visualize the marginal effects from the interaction effects
marginaleffects::plot_slopes(WAP_lead_Interact, variables = "Interest", condition = "Primary_media") +
  geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + coord_flip() + theme_bw()
marginaleffects::plot_slopes(WAP_lead_Interact2, variables = "Interest", condition = "Primary_media") +
  geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + coord_flip() + theme_bw()

#Display Models
modelsummary(WAP_lead_reg, stars = T, vcov = "HC0") #As numbers


#### BIMODALITY COEFFICENT ####
  
#### Primary Media ####
bimod_online <- bimodality_coefficient(policy_online, na.rm = T); bimod_online
bimod_leacgy <- bimodality_coefficient(policy_legacy, na.rm = T); bimod_leacgy
bimodality_coefficient(policy_social_media, na.rm = T)
bimodality_coefficient(policy_mixed, na.rm = T)

bimod_legacy <- policy_legacy %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Legacy Media", x = "Policy Positions", y = NULL) +
 theme_bw()

bimod_online <- policy_online %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Online Media", x = "Policy Positions", y = NULL) +
  theme_bw()

bimod_social <- policy_social_media %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Social Media", x = "Policy Positions", y = NULL) +
  theme_bw()

bimod_mixed <- policy_mixed %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Mixed media", x = "Policy Positions", y = NULL) +
  theme_bw()

#Nice to have fake distributions of the different kinds of distribution

gridExtra::grid.arrange(bimod_legacy, bimod_online, bimod_social, bimod_mixed) 


#### Social Use ####

bimodality_coefficient(policy_often, na.rm = T)
bimodality_coefficient(policy_rarely, na.rm = T)

bimod_often <- policy_often %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Uses Social Media Often", x = "Policy Positions", y = NULL) +
  theme_bw()

bimod_rarely <- policy_rarely %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Uses Social Media Rarely", x = "Policy Positions", y = NULL) +
  theme_bw()

gridExtra::grid.arrange(bimod_often, bimod_rarely) 


#### OVERLAP COEFFICENTS GRAPHS ####

on18$partyvote2018
left_wing <- on18 %>% 
  filter(partyvote2018 == 1 | partyvote2018 == 3) %>% 
  select(policy_polarization) %>% 
  na.omit() 

right_wing <- on18 %>% 
  filter(partyvote2018 == 2) %>% 
  select(policy_polarization) %>% 
  na.omit()  


overlapping::overlap(list(as.double(left_wing$policy_polarization), 
                          as.double(right_wing$policy_polarization)))

policy_left_legacy_vector <- as.double(policy_left_legacy$policy_polarization) %>% 
  na.omit() 
policy_conservatives_legacy_vector <- as.double(policy_conservatives_legacy$policy_polarization) %>% 
  na.omit() 


overlapping::overlap(list(policy_left_legacy_vector, policy_conservatives_legacy_vector))
#overlapping::boot.overlap(list(policy_left_legacy_vector, policy_conservatives_legacy_vector), B = 1000)


#bayestestR::overlap(policy_left_legacy_vector, policy_conservatives_legacy_vector)


overlap_legacy <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_legacy, col = "red", fill = "red", alpha = 0.4) + 
  labs(title = "Legacy Media", subtitle = "Overlap Coefficent (0.66)", x = "Policy Positions", y = NULL) +
  geom_density(aes(policy_polarization), data = policy_conservatives_legacy, col = "blue", fill = "blue", alpha = 0.4) + theme_bw()


policy_left_online_vector <- as.double(policy_left_online$policy_polarization) %>% 
  na.omit() 
policy_conservatives_online_vector <- as.double(policy_conservatives_online$policy_polarization) %>% 
  na.omit() 

overlapping::overlap(list(policy_left_online_vector, policy_conservatives_online_vector))
overlapping::boot.overlap(list(policy_left_online_vector, policy_conservatives_online_vector), B = 1000)

#bayestestR::overlap(policy_left_online, policy_conservatives_online)

overlap_online <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_online, col = "red", fill = "red", alpha = 0.4) + 
  labs(title = "Online Media", subtitle = "Overlap Coefficent (0.59)", x = "Policy Positions", y = NULL) +
  geom_density(aes(policy_polarization), data = policy_conservatives_online, col = "blue", fill = "blue", alpha = 0.4) + theme_bw()

policy_left_media_vector <- as.double(policy_left_social_media$policy_polarization) %>% 
  na.omit() 
policy_conservatives_media_vector <- as.double(policy_conservatives_media$policy_polarization) %>% 
  na.omit() 

overlapping::overlap(list(policy_left_media_vector, policy_conservatives_media_vector))
overlapping::boot.overlap(list(policy_left_media_vector, policy_conservatives_media_vector), B = 1000)

#bayestestR::overlap(policy_left_social_media, policy_conservatives_media)

overlap_smedia <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_social_media, col = "red", fill = "red", alpha = 0.4) + 
  labs(title = "Social Media", subtitle = "Overlap Coefficent (0.74)", x = "Policy Positions", y = NULL) +
  geom_density(aes(policy_polarization), data = policy_conservatives_media, col = "blue", fill = "blue", alpha = 0.4) + theme_bw()

policy_left_mixed_vector <- as.double(policy_left_mixed$policy_polarization) %>% 
  na.omit() 
policy_conservatives_mixed_vector <- as.double(policy_conservatives_mixed$policy_polarization) %>% 
  na.omit() 



overlapping::overlap(list(policy_left_mixed_vector, policy_conservatives_mixed_vector))
set.seed(1234); overlapping::boot.overlap(list(policy_left_mixed_vector,
                                               policy_conservatives_mixed_vector), B = 1000)
#bayestestR::overlap(policy_left_mixed, policy_conservatives_mixed)

overlap_mixed <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_mixed, col = "red", fill = "red", alpha = 0.4) +
  labs(title = "Mixed Media", subtitle = "Overlap Coefficent (0.70)", x = "Policy Positions", y = NULL) +
  geom_density(aes(policy_polarization), data = policy_conservatives_mixed, col = "blue", fill = "blue", alpha = 0.4) + theme_bw()

ggpubr::ggarrange(overlap_legacy, overlap_online, overlap_smedia, overlap_mixed)

#### OVERLAP FOR SOCIAL USE ####

policy_left_often_vector <- as.double(policy_left_often$policy_polarization) %>% 
  na.omit() 
policy_conservative_often_vector <- as.double(policy_conservative_often$policy_polarization) %>% 
  na.omit() 


overlapping::overlap(list(policy_left_often_vector, policy_conservative_often_vector))
set.seed(1234); overlapping::boot.overlap(list(policy_left_often_vector,
                                               policy_conservative_often_vector), B = 1000)

#bayestestR::overlap(policy_left_often, policy_conservative_often)

overlap_often <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_often, col = "red", fill = "red", alpha = 0.4) + 
  geom_density(aes(policy_polarization), data = policy_conservative_often, col = "blue", fill = "blue", alpha = 0.4) +
  labs(title = "Uses Social Media Often", subtitle = "Overlap Coefficent (0.53)", x = "Policy Positions", y = NULL) + theme_bw()

policy_left_rarely_vector <- as.double(policy_left_rarely$policy_polarization) %>% 
  na.omit() 
policy_conservative_social_rarely_vector <- as.double(policy_conservative_social_rarely$policy_polarization) %>% 
  na.omit() 



overlapping::overlap(list(policy_left_rarely_vector, policy_conservative_social_rarely_vector))
set.seed(1234); overlapping::boot.overlap(list(policy_left_rarely_vector,
                               policy_conservative_social_rarely_vector), B = 1000)
#bayestestR::overlap(policy_left_rarely_vector, policy_conservative_social_rarely_vector)

overlap_rarely <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_rarely, col = "red", fill = "red", alpha = 0.4) + 
  geom_density(aes(policy_polarization), data = policy_conservative_social_rarely, col = "blue", fill = "blue", alpha = 0.4) +
  labs(title = "Uses Social Media Rarely", subtitle = "Overlap Coefficent (0.70)", x = "Policy Positions", y = NULL) + theme_bw()


ggpubr::ggarrange(overlap_often, overlap_rarely, ncol = 1)


#report the null of the social media use
#make the positive finding more interesting
#statistical tests with Overlap coefficients 

#### POLITICAL INTEREST AND AFFECTIVE POLARIZATION ####

Interest_pm <- lm(Interest ~ Primary_media, data = on18, na.action = na.omit); summary(Interest_pm) 
Interest_su <- lm(Interest ~ Social_Use2, data = on18, na.action = na.omit); summary(Interest_su)

WAP_Interact <- lm(WAP_sd ~ Primary_media*Interest, data = on18, na.action = na.omit)
summary(WAP_Interact)

WAP_Interact2 <- lm(WAP_sd ~ as.numeric(Social_Use2)*Interest  + age3 + degree + income3 + pol_knowledge, data = on18, na.action = na.omit)
summary(WAP_Interact2)

#Visualize the marginal effects from the interaction effects
marginaleffects::plot_slopes(WAP_Interact, variables = "Primary_media", condition = "Interest") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + labs(y = "Marginal Effect of Primary Media Variable") + theme_bw()

marginaleffects::plot_slopes(WAP_Interact2, variables = "Social_Use2", condition = "Interest") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
gam_model <- mgcv::gam(WAP_sd ~ s(age) + Primary_media + Interest + degree + income3 + pol_knowledge, data = on18)
#draw(gam_model, residuals = T)

#Theoretical distributions

set.seed(1234)
unimodal_data1 <- rnorm(1000, 0, 2)
unimodal_data2 <- rnorm(1000, 0, 1)
unimodal_data3 <- rnorm(1000, 0, 3)
unimodal_data4 <- rnorm(1000, 0, 1)
unimodal_data5 <- rnorm(1000, 0, 1)
unimodal_data <- c(unimodal_data1, unimodal_data2, unimodal_data3, unimodal_data4, unimodal_data5)
bimodality_coefficient(unimodal_data)

uni_modal <- as_tibble(unimodal_data) %>% 
  ggplot(aes(x = value)) +
  geom_density(col = "forestgreen", fill = "forestgreen", alpha = 0.4) +
  labs(subtitle = paste0("Bimodality Coefficent ",
                      round(bimodality_coefficient(unimodal_data), 2))) +
  theme_bw()


semimodal_data1 <- rnorm(1000, -2, 1)
semimodal_data2 <- rnorm(100, 0, 1)
semimodal_data3 <- rnorm(1000, 2, 1)
semimodal_data <- c(semimodal_data1, semimodal_data2, semimodal_data3)
bimodality_coefficient(semimodal_data)
semi_modal <- as_tibble(semimodal_data) %>% 
  ggplot(aes(x = value)) +
  geom_density(col = "forestgreen", fill = "forestgreen", alpha = 0.4) +
  labs(subtitle = paste0("Bimodality Coefficent ",
                      round(bimodality_coefficient(semimodal_data), 2))) +
  theme_bw()


bimod1 <- rnorm(1000, -1, 0.1)
bimod2 <- rnorm(1000, 1, 0.1)
bimodal_data <- c(bimod1, bimod2)
bimodality_coefficient(bimodal_data)
bimodal <- as_tibble(bimodal_data) %>% 
  ggplot(aes(x = value)) + 
  geom_density(col = "forestgreen", fill = "forestgreen", alpha = 0.4) + 
  labs(subtitle = paste0("Bimodality Coefficent ",
                         round(bimodality_coefficient(bimodal_data),2))) +
  theme_bw()
