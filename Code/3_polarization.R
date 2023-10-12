##Load needed scripts to complete analyses
source("Code/1_load_on18.R")
source("Code/0_functions.R")
#Check the variables in the dataset.
names(on18)
glimpse(on18)
#### Primary News Source ####

on18 %>%
  mutate(Primary_media=case_when(
    (primarynews_7 == 1 | primarynews_6 == 1) & (primarynews_5 == 0 & primarynews_4 == 0 & primarynews_3 == 0 & primarynews_2 == 0 & primarynews_1 == 0) ~ "Social_Media", #For individuals who only use Social Media
    (primarynews_4 == 1 | primarynews_5 == 1) & (primarynews_6 == 0 & primarynews_7 == 0 & primarynews_3 == 0 & primarynews_2 == 0 & primarynews_1 == 0) ~ "Online", #For individuals who only use online sources that are not social media
    (primarynews_3 == 1 | primarynews_2 == 1 | primarynews_1 == 1) & (primarynews_5 == 0 & primarynews_4 == 0 & primarynews_6 == 0 & primarynews_7 == 0) ~ "Legacy", #For individuals who only receive news from television or newspapers 
    TRUE ~ "Mixed"
    
  ))->on18

# on18 %>%
#   mutate(Primary_media2=case_when(
#     (primarynews_7 == 1 | primarynews_6 == 1 | primarynews_4 == 1 | primarynews_5 == 1) & (primarynews_3 == 0 & primarynews_2 == 0 & primarynews_1 == 0) ~ "Online",
#     (primarynews_3 == 1 | primarynews_2 == 1 | primarynews_1 == 1) & (primarynews_5 == 0 & primarynews_4 == 0 & primarynews_6 == 0 & primarynews_7 == 0) ~ "Legacy",
#     TRUE ~ "Mixed"
#     
#   ))->on18

table(on18$Primary_media)
#### Political Knowledge #### 
#Create one political knowledge variable using the variables on political knowledge.
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
#Interesting, in this one it looks like the survey team coded some 
# close responses. A person who responds with Bill Morneau (even though wrong)
# Might know more than someone who just flat out doesn't know. 
# For know let's keep it the way you have it. Responses are correct only if 1

#### Automatic way to recode several variables####
# I'm going to show you how to recode several variables the same way at the same time.

# Always start with the data frame
on18 %>% 
  #We are transforming variables so we use mutate
  # We are going to be applying a function across several columns, so we use
  # across()
  #Importantly, we can use the same select() semantics inside across()
  #in this case we can select the variables from unsg to nhse, including the ones in between
  #We only know this really through poking and actually looking at the data columns. Even in SPSS if necessary.
  mutate(
    across(.cols=unsg:nhse, 
           #Now add in the function we are going to apply.
           #Note that if_else is just a dplyr version of ifelse()
           binaryrecode, 
           # A cool feature of across() is that you can add a suffix or a prefix
           #This will paste the name of the column with _correct
           #Exactly like you did above.
           .names="{.col}_correct")
  )->on18
#Now you can quickly check that it worked
on18 %>% 
  select(ends_with("_correct"))


lookfor(on18, "seats in the House of Commons")
#This is great
on18$pol_knowledge <- (on18$unsg_correct + on18$financename_correct + on18$ggname_correct + on18$nhse_correct)/4



# Add in a check
on18 %>% 
  ggplot(., aes(x=pol_knowledge))+geom_histogram()
#But we should also check the math.
on18 %>% 
  select(ends_with("correct")| "pol_knowledge") %>% 
  View()
on18$pol_knowledge
on18 %>%
  group_by(Social_Use2) %>%
  summarise(mean = mean(pol_knowledge, na.rm = T), sd = sd(pol_knowledge, na.rm = T)) 
stopifnot((isTRUE(all.equal(on18$pol_knowledge, 
                            ((on18$unsg_correct + on18$financename_correct + on18$ggname_correct + on18$nhse_correct)/4)))))

table(on18$Social_Use2, on18$pol_knowledge)

on18 %>%
  group_by(Primary_media) %>%
  summarise(mean = mean(pol_knowledge, na.rm = T), sd = sd(pol_knowledge, na.rm = T)) 


#### Policy polarization ####
# R social media users frothing at the mouth extremists
# Pick good policy measures from data 
# Use pre-recoded climate change questions
# Use batteries on policy items
# Rename each agreement item with a useful variable name. 
# new_names<-c("help_racial_minorities")
#climate change

on18$climate_views <- (on18$climate_bin + on18$paris_bin)/2



on18 %>% 
  select(starts_with("agreement_"))-> policies 

policy_names <- c("help_racial_minorities", "help_women", "more_coporate_tax", "more_personal_tax", "private_health_care", 
                  "minimum_wage_to_high_prices", "income_inequality", "business_benefits_everyone", 
                  "drug_benefit_u25", "free_post_secondary", "inappropriate_sex_ed")
names(policies)<-policy_names
#recombine
policies <- mutate_all(policies, function(x) as.numeric(as.character(x)))
range01 <- function(x, ...){(x-min(x, ...))/(max(x, ...)-min(x, ...))} 
policies <- range01(policies, na.rm = T)

policies %>%  
  bind_cols(., on18$Social_Use2)->policies_social_use

names(policies_social_use)<-c(policy_names, "Social_Use2")

policies_social_use %>%
  group_by(Social_Use2) %>%
  summarise(across(everything(), sd, na.rm = T), .groups = "drop") -> policies_sd_social_use


#Calculate standard deviation for each. 
# Take on18
# pivot down the agreement variables after rename
policies_sd_social_use_down <- policies_sd_social_use %>%
  pivot_longer(cols = !Social_Use2,
               names_to = "Policy_Issue", 
               values_to = "Standard_Deviation")
#This is what we have now
# social_use | agreement_1 | agreement_2 |
# Heavy | 4 | 3|
#What we want is 

# social_use | Variable | Value |
# Heavy | agreement_1 | 4|
# Heavy | agreement_2 | 3 |

# Wide (current) versus (tidy or long ) data

# https://towardsdatascience.com/coding-in-r-pivot-painlessly-32e40a0b6c3d

# Then Group by Social_Use and variable
# Summarize by sd()


#View(policies_sd_down)

policies_sd_social_use_down$Policy_Issue <- factor(policies_sd_social_use_down$Policy_Issue, levels = c(policy_names, "climate_views"), 
                                        labels = c("The Government Should Help Racial Minorities", "The Government Should Help Women", 
                                                   "Higher Corporate Taxes are Good", "Higher Personal Taxes are Good",  "Health Care Should be Privatized", 
                                                   "Increasing Minimum Wage Increases Prices", "The Government Should Reduce Income Inequality", "More Business Benefits Everyone", 
                                                   "Youth Should Receive Drug Benefits", "Post Secondary Education Should be Free", "Sex-Ed Curriculum is Inappropriate", "Climate Change"))

policies %>%  
  bind_cols(., on18$Primary_media)->policies_media

names(policies_media)<-c(policy_names, "Primary_media")

policies_media %>%
  group_by(Primary_media) %>%
  summarise(across(everything(), sd, na.rm = T), .groups = "drop") -> policies_sd_media


#Calculate standard deviation for each. 
# Take on18
# pivot down the agreement variables after rename
policies_media_down <- policies_sd_media %>%
  pivot_longer(cols = !Primary_media,
               names_to = "Policy_Issue", 
               values_to = "Standard_Deviation")

#This is what we have now
# social_use | agreement_1 | agreement_2 |
# Heavy | 4 | 3|
#What we want is 

# social_use | Variable | Value |
# Heavy | agreement_1 | 4|
# Heavy | agreement_2 | 3 |

# Wide (current) versus (tidy or long ) data

# https://towardsdatascience.com/coding-in-r-pivot-painlessly-32e40a0b6c3d

# Then Group by Social_Use and variable
# Summarize by sd()


#View(policies_sd_down)

policies_media_down$Policy_Issue <- factor(policies_media_down$Policy_Issue, levels = c(policy_names, "climate_views"), 
                                                   labels = c("The Government Should Help Racial Minorities", "The Government Should Help Women", 
                                                              "Higher Coporate Taxes are Good", "Higher Personal Taxes are Good",  "Health Care Should be Privatized", 
                                                              "Increasing Minimum Wage Increases Prices", "The Government Should Reduce Income Inequality", "More Business Benefits Everyone", 
                                                              "Youth Should Receive Drug Benefits", "Post Secondary Education Should be Free", "Sex-Ed Curriculum is Inappropriate", "Climate Change"))


#### GRAPH STANDARD DEVIATIONS BY POLICY ISSUE ####

# I am trying to understand why you are excluding these rows
#policies_sd_down [-c(97:108), ]
#policies_sd_down[97:108,]
#OK, these seme to come from respondents who did not respond to their social media use
#So we can exclude them. 
#Here is a quicker way to exclude them
policies_sd_social_use_down
policies_sd_social_use_down %>% 
  filter(complete.cases(.)) %>% 
  #instead of setting x to be zero I want x to be the Standard Deviation
  ggplot(., aes(x = Standard_Deviation, 
                #And I want y to be the Social Media Use
                #You did this lower down with coord_flip()
                #Which is an old way of switching x and y. 
                #Now, however, ggplot() lets you just specify whichever
                # one you want to be horizontal and vertical with x and y
                y = Social_Use2, 
                colour = Social_Use2)) + 
  geom_point()+
  # geom_errorbar(aes (ymin = 0 - Standard_Deviation, ymax = 0 + Standard_Deviation), width =.2, 
  #position = position_dodge(.8)) + 
  facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) +
  labs(x = "Standard Deviation", y= "Social Media Use") + 
  theme_bw()  + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank(), 
        #I actually don't like the legend on the bottom for this reason.
        # We actually have a categorical variable (Social_Use2)
        # That has an intuitive order to it, never to a lot
        # I would like to capture that order in a legend that goes from low tohigh
        #But I am going to uncomment poutting the legend in the bottom.
        # legend.position ="bottom", 
        strip.text = element_text(size=8))+
  # guides(colour = guide_legend(nrow = 3, byrow = T, title.position = "top", hjust = 0.5, reverse = T), linetype = guide_legend(nrow = 3, byrow = T, title.position = "top", hjust = 0.5, reverse = T)) +
  #But this introduces a new problem which is that the 
  # Category "never" is at the top of the legend
  #Try making the graph, stopping the code at line 176
  # by deleting the +
  #Then add it back in and see the difference
  guides(color=guide_legend(reverse=T))->policy_issue_sd_graph
#scale_colour_manual(guide=guide_legend(reverse=T))
# scale_linetype_manual(values = c("twodash","twodash","longdash", "longdash", "dashed", "dashed", "solid", "solid")) -> policy_issue_sd_graph
ggsave(plot = policy_issue_sd_graph, "Plots/policy_issues_sd.png", width = 12, height = 7)


policies_media_down %>% 
  filter(complete.cases(.)) %>% 
  ggplot(., aes(x = Standard_Deviation, 
                y = Primary_media, 
                colour = Primary_media)) + 
  geom_point()+
  facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) +
  labs(x = "Standard Deviation", y= "Primary Media Source") + 
  theme_bw()  + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank(), 
        strip.text = element_text(size=8))+
  guides(color=guide_legend(reverse=T))

#### Affective Polarization ####
# We need the party feeling scores

on18 %>% 
  select(starts_with("partyeval")) %>% 
  glimpse()
# so it looks like I took the step sometime of 
# scaling these variables to 0 and 1, and they end in 
# _out.
on18 %>% 
  select(starts_with("partyeval")) %>% 
  summary()
#Looks like the originals run from 0 to 5
on18 %>% 
  select(starts_with("partyeval")) %>% 
  val_labels()
on18 %>% 
  select(starts_with("partyeval")) %>% 
  var_label()
#Looks like 4 is Green
# 1 is Liberal
# 3 is NDP
# 2 isPC
# 0 is Really dislike and 5 is really like. 

#Let's use the _out ones.

on18 %>% 
  select(id,starts_with("partyeval")&ends_with("out"))->affect
names(affect)
affect %>% 
  rename(Green_Like=2, Liberal_Like=3, NDP_Like=4, PC_Like=5)->affect

affect_wt <- affect
#For each respondent, we will need an average like for the parties
# This is mean(like)_i
#So we need to effectively calculate a mean for each row. 
#Each row is one R. 
affect %>% 
  rowwise() %>% 
  mutate(mean_like=mean(c_across(2:5), na.rm=T))->affect

#Now we need to take each R's like score for each party
# And subtract the mean from it. 
#my guit is that we should pivot this down
#Could you:
# 1) pivot down the party like scores (not the respondent id)
# and not the mean like
affect_pol_cal <- affect %>%
  pivot_longer(cols = !c(id, mean_like),
               names_to = "Party", 
               values_to = "Party_like_score")
# 2) Then, in my mind's eye, we will have a column
# of party like scores and a column of mean like values
# It should be easy to just subtract the mean like column
affect_pol_cal
affect_pol_cal$like_mean <- (affect_pol_cal$Party_like_score - affect_pol_cal$mean_like)^2
# From the party like scores. 
# 3) Then squre that last column

affect_pol_cal %>%
  group_by(id) %>%
  summarise(Soc_dis = sum(like_mean)) -> Soc_dis_scores

Soc_dis_scores$Soc_dis <- sqrt(Soc_dis_scores$Soc_dis/4)

# 4) Then we should just sum those up for each respondent.

full_join(on18, Soc_dis_scores, by = join_by(id)) -> on18

#Check this out

on18 %>%
  group_by(Social_Use2) %>%
  summarise(mean = mean(Soc_dis, na.rm = T), sd = sd(Soc_dis, na.rm = T), n=n(), se=sd/sqrt(n))->mean_affect_scores 

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
  guides(color=guide_legend(reverse=T))+xlim(c(0,0.5)) -> affect_scores_social_use
affect_scores_social_use

on18 %>%
  group_by(Primary_media) %>%
  summarise(mean = mean(Soc_dis, na.rm = T), sd = sd(Soc_dis, na.rm = T))

#### Weighted Affective Polarization (WAP) Scores ####

affect_wt$Green_Like_wt <- (affect$Green_Like * 0.046)
affect_wt$PC_Like_wt <- (affect$PC_Like * 0.405)
affect_wt$Liberal_Like_wt <- (affect$Liberal_Like * 0.196)
affect_wt$NDP_Like_wt <- (affect$NDP_Like * 0.336)

affect_wt %>% 
  rowwise() %>% 
  mutate(mean_like=mean(c_across(6:9), na.rm=T))->affect_wt

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

on18 %>%
  group_by(Primary_media) %>%
  summarise(mean = mean(pointerst_ONint, na.rm = T), sd = sd(pointerst_ONint, na.rm = T))


on18 %>%
  group_by(Social_Use2) %>%
  summarise(mean = mean(WAP, na.rm = T), sd = sd(WAP, na.rm = T))

#create number of standard deviations from mean variable
policies_social_use
policies_social_use_sd_dis <- do.call(cbind, lapply(policies_social_use[, 1:11], sd_mean, na.rm = T)) 
#Policies_social_use_sd_dis is now each R's distance from the mean in SD. 
head(policies_social_use_sd_dis)
policies_social_use_sd_dis %>%
  bind_cols(., on18$Social_Use2)->policies_social_use_sd_dis
names(policies_social_use_sd_dis)<-c(policy_names, "Social_Use2")


policies_social_use_sd_dis %>%
  group_by(Social_Use2) %>%
  summarise( across(everything(), list(.mean = mean, .se = se), na.rm = T )) -> policies_social_use_sd_dis_mean
policies_social_use_sd_dis_mean
on18 <- cbind(on18, policies_social_use_sd_dis)

policies_sd_social_dis_down <- policies_social_use_sd_dis_mean %>%
  pivot_longer(cols = !Social_Use2,
               names_to = c("Policy_Issue", ".value"), 
              names_sep = "\\." )
policies_sd_social_dis_down
policies_sd_social_dis_down %>% 
  filter(complete.cases(.)) %>%
  ggplot(., aes(x = mean, 
                y = Social_Use2, 
                colour = Social_Use2)) + 
  geom_point()+ facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) +
  geom_errorbar(aes (xmin = mean - (1.96*se), xmax = mean + (1.96*se)), width =.2) + 
  labs(x = "Mean", y= "Social Media Use") + 
  theme_bw()  + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y = element_blank(), 
        strip.text = element_text(size=8))+
  guides(color=guide_legend(reverse=T))
# Note no pattern



policies_social_use_sd_dis %>%
  bind_cols(., on18$Primary_media) %>%
  bind_cols(., on18$Interest) ->policies_social_use_sd_dis2

#check to make sure function worked properly
mean(policies$help_racial_minorities, na.rm = T)
sd(policies$help_racial_minorities, na.rm = T)
#observation #1
# 0.95 = (x-x̄)/σ
# 0.95 = (0.75-0.481)/0.282 
# 0.95 = 0.95
stopifnot(round(policies_social_use_sd_dis2[1 , 1], 3) == round(((0.75-0.4809809)/ 0.2815165), 3)) 
#stop if function did not work properly 
on18$Primary_media
WAP_primarymedia <- lm(WAP ~ Primary_media, data = on18, na.action = na.omit) 
summary(WAP_primarymedia)

WAP_Interest <- lm(WAP ~ Interest, data = on18, na.action = na.omit)
summary(WAP_Interest)  

WAP_Interact <- lm(WAP ~ Primary_media*Interest, data = on18, na.action = na.omit)
summary(WAP_Interact)

names(policies_social_use_sd_dis2)<-c(policy_names, "Social_Use2", "Primary_Media", "Interest")

policies_social_use_sd_dis2 %>%
  pivot_longer(cols = 1:11, names_to = "Policy_Issue", values_to = "distance") -> policies_social_use_sd_dis2_down
policies_social_use_sd_dis2_down

mods_Interest <- policies_social_use_sd_dis2_down %>%
  group_by(Policy_Issue) %>%
  summarise(lm_mod = list(lm(distance ~ Interest))) %>% 
  mutate(tidied = map(lm_mod, tidy, conf.int = T)) 

mods_Interest
library(modelsummary)
modelsummary(mods_Interest$lm_mod, stars=T)

#Make graph
mods_Interest %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(., aes(x=estimate, y=fct_reorder(Policy_Issue, estimate)))+
  geom_pointrange(aes(xmin=estimate-(1.96*std.error), xmax=estimate+(1.96*std.error)))

table(on18$Primary_media)
mods_media <- policies_social_use_sd_dis2_down %>%
  group_by(Policy_Issue) %>%
  summarise(lm_mod = list(lm(distance ~ Primary_Media))) %>% 
  mutate(tidied = map(lm_mod, tidy, conf.int = T))

modelsummary(mods_media$lm_mod, stars=T)
mods_media$lm_mod
mods_media %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(., aes(x=estimate, y=fct_reorder(Policy_Issue, estimate), col=term))+
  geom_pointrange(position="jitter", aes(xmin=estimate-(1.96*std.error), xmax=estimate+(1.96*std.error)))


mods_all <- policies_social_use_sd_dis2_down %>%
  group_by(Policy_Issue) %>%
  summarise(lm_mod = list(lm(distance ~ Interest + Primary_Media))) %>% 
  mutate(tidied = map(lm_mod, tidy, conf.int = T)) 

modelsummary(mods_all$lm_mod, stars=T)

mods_interact <- policies_social_use_sd_dis2_down %>%
  group_by(Policy_Issue) %>%
  summarise(lm_mod = list(lm(distance ~ Interest*Primary_Media))) %>% 
  mutate(tidied = map(lm_mod, tidy, conf.int = T)) 

modelsummary(mods_interact$lm_mod, stars=T)
str(policies_social_use_sd_dis2_down)
#Marginal effects of interaction models
policies_social_use_sd_dis2_down %>% 
lm(distance ~ Interest*Primary_Media ,data = ., na.action = na.omit) -> inter



#graph models

mods_Interest %>%
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(aes(x = estimate, y = term)) + geom_point() + 
  geom_linerange(aes(xmin=conf.low, xmax = conf.high), position = position_dodge(width=.75)) +
  facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) + geom_vline(xintercept = 0) + theme_bw()

mods_media %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(aes(x = estimate, y = term)) + geom_point() + 
  geom_linerange(aes(xmin=conf.low, xmax = conf.high), position = position_dodge(width=.75)) +
  facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) + geom_vline(xintercept = 0) + theme_bw()

mods_all %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(aes(x = estimate, y = term)) + geom_point() + 
  geom_linerange(aes(xmin=conf.low, xmax = conf.high), position = position_dodge(width=.75)) +
  facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) + geom_vline(xintercept = 0) + theme_bw()


mods_interact %>% 
  unnest(tidied) %>% 
  filter(term!="(Intercept)") %>% 
  ggplot(aes(x = estimate, y = term)) + geom_point() + 
  geom_linerange(aes(xmin=conf.low, xmax = conf.high), position = position_dodge(width=.75)) +
  facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) + geom_vline(xintercept = 0) + theme_bw() -> interaction_models
ggsave("Plots/interaction_models.png", plot = interaction_models)

extract_model(1) %>% 
  graph_interaction() -> interact_help_racial_minorities
extract_model(2) %>% 
  graph_interaction() -> interact_help_women
extract_model(3) %>% 
  graph_interaction() -> interact_more_coporate_tax
extract_model(4) %>% 
  graph_interaction() -> interact_more_personal_tax
extract_model(5) %>% 
  graph_interaction() -> interact_private_health_care
extract_model(6) %>% 
  graph_interaction() -> interact_minimum_wage_to_high_prices
extract_model(7) %>% 
  graph_interaction() -> interact_income_inequality
extract_model(8) %>% 
  graph_interaction() -> interact_business_benefits_everyone
extract_model(9) %>% 
  graph_interaction() -> interact_drug_benefit_u25
extract_model(10) %>% 
graph_interaction() -> interact_free_post_secondary
extract_model(11) %>% 
  graph_interaction() -> interact_inappropriate_sex_ed

#policies_social_use %>%
#  group_by(Social_Use2) %>%
#  summarise(mean = mean(sd_mean_hrm, na.rm = T))

# Tasks for July 28
# did means of interest by primary media
# fit three OLS models for each policy variable with 
# SD of policy variable as DV
# Model 1 only legacy media
# Model 2 only interest
# Model 3 legacy media plus interest
# https://r4ds.had.co.nz/many-models.html
# check to see if anyone has ever used SD as a DV. 