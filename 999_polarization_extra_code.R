#### Additional primary media variable ####

# on18 %>%
#   mutate(Primary_media2=case_when(
#     (primarynews_7 == 1 | primarynews_6 == 1 | primarynews_4 == 1 | primarynews_5 == 1) & (primarynews_3 == 0 & primarynews_2 == 0 & primarynews_1 == 0) ~ "Online",
#     (primarynews_3 == 1 | primarynews_2 == 1 | primarynews_1 == 1) & (primarynews_5 == 0 & primarynews_4 == 0 & primarynews_6 == 0 & primarynews_7 == 0) ~ "Legacy",
#     TRUE ~ "Mixed"
#     
#   ))->on18


#### STANDARD DEVIATIONS FOR POLICY ISSUES ####
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

# policies_media_down$Policy_Issue <- factor(policies_media_down$Policy_Issue, levels = c(policy_names, "climate_views"), 
#                                            labels = c("The Government Should Help Racial Minorities", "The Government Should Help Women", 
#                                                       "Higher Coporate Taxes are Good", "Higher Personal Taxes are Good",  "Health Care Should be Privatized", 
#                                                       "Increasing Minimum Wage Increases Prices", "The Government Should Reduce Income Inequality", "More Business Benefits Everyone", 
#                                                       "Youth Should Receive Drug Benefits", "Post Secondary Education Should be Free", "Sex-Ed Curriculum is Inappropriate", "Climate Change"))
# 
# 
# #### GRAPH STANDARD DEVIATIONS BY POLICY ISSUE ####
# 
# # I am trying to understand why you are excluding these rows
# #policies_sd_down [-c(97:108), ]
# #policies_sd_down[97:108,]
# #OK, these seme to come from respondents who did not respond to their social media use
# #So we can exclude them. 
# #Here is a quicker way to exclude them
# policies_sd_social_use_down
# policies_sd_social_use_down %>% 
#   filter(complete.cases(.)) %>% 
#   #instead of setting x to be zero I want x to be the Standard Deviation
#   ggplot(., aes(x = Standard_Deviation, 
#                 #And I want y to be the Social Media Use
#                 #You did this lower down with coord_flip()
#                 #Which is an old way of switching x and y. 
#                 #Now, however, ggplot() lets you just specify whichever
#                 # one you want to be horizontal and vertical with x and y
#                 y = Social_Use2, 
#                 colour = Social_Use2)) + 
#   geom_point()+
#   # geom_errorbar(aes (ymin = 0 - Standard_Deviation, ymax = 0 + Standard_Deviation), width =.2, 
#   #position = position_dodge(.8)) + 
#   facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) +
#   labs(x = "Standard Deviation", y= "Social Media Use") + 
# #   theme_bw()  + 
# #   theme(axis.text.y=element_blank(),
# #         axis.ticks.y = element_blank(), 
# #         #I actually don't like the legend on the bottom for this reason.
# #         # We actually have a categorical variable (Social_Use2)
# #         # That has an intuitive order to it, never to a lot
# #         # I would like to capture that order in a legend that goes from low tohigh
# #         #But I am going to uncomment poutting the legend in the bottom.
# #         # legend.position ="bottom", 
# #         strip.text = element_text(size=8))+
# #   # guides(colour = guide_legend(nrow = 3, byrow = T, title.position = "top", hjust = 0.5, reverse = T), linetype = guide_legend(nrow = 3, byrow = T, title.position = "top", hjust = 0.5, reverse = T)) +
# #   #But this introduces a new problem which is that the 
# #   # Category "never" is at the top of the legend
# #   #Try making the graph, stopping the code at line 176
# #   # by deleting the +
# #   #Then add it back in and see the difference
# #   guides(color=guide_legend(reverse=T))->policy_issue_sd_graph
# # #scale_colour_manual(guide=guide_legend(reverse=T))
# # # scale_linetype_manual(values = c("twodash","twodash","longdash", "longdash", "dashed", "dashed", "solid", "solid")) -> policy_issue_sd_graph
# # ggsave(plot = policy_issue_sd_graph, "Plots/policy_issues_sd.png", width = 12, height = 7)
# # 
# # 
# # policies_media_down %>% 
# #   filter(complete.cases(.)) %>% 
# #   ggplot(., aes(x = Standard_Deviation, 
# #                 y = Primary_media, 
# #                 colour = Primary_media)) + 
# #   geom_point()+
# #   facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) +
# #   labs(x = "Standard Deviation", y= "Primary Media Source") + 
# #   theme_bw()  + 
# #   theme(axis.text.y=element_blank(),
# #         axis.ticks.y = element_blank(), 
# #         strip.text = element_text(size=8))+
# #   guides(color=guide_legend(reverse=T))
# 
# 
# # policies_sd_social_dis_down <- policies_social_use_sd_dis_mean %>%
# #   pivot_longer(cols = !Social_Use2,
# #                names_to = c("Policy_Issue", ".value"), 
# #                names_sep = "\\." )
# # policies_sd_social_dis_down
# # policies_sd_social_dis_down %>% 
# #   filter(complete.cases(.)) %>%
# #   ggplot(., aes(x = mean, 
# #                 y = Social_Use2, 
# #                 colour = Social_Use2)) + 
# #   geom_point()+ facet_wrap(~Policy_Issue, labeller = label_wrap_gen(width=30)) +
# #   geom_errorbar(aes (xmin = mean - (1.96*se), xmax = mean + (1.96*se)), width =.2) + 
# #   labs(x = "Mean", y= "Social Media Use") + 
# #   theme_bw()  + 
# #   theme(axis.text.y=element_blank(),
# #         axis.ticks.y = element_blank(), 
# #         strip.text = element_text(size=8))+
# #   guides(color=guide_legend(reverse=T))
# # # Note no pattern
# 
# 
# #### CPC AFFECT POLARIZATION ####
# on18$partyvote2018
# 
# on18 <- on18 %>% 
#   mutate(
#     Liberal_Like_wt = partyeval_1_out * 0.196,
#     NDP_Like_wt = partyeval_3_out * 0.336,
#     Green_Like_wt = partyeval_4_out * 0.046,
#     Con_Like_wt = partyeval_2_out * 0.405
#   )
# 
# cpc_high <- on18 %>% 
#   filter((Social_Use2 == "Several times a day" | Social_Use2 == "About once a day" |  Social_Use2 == "Several times a week" | Social_Use2 == "About once a week"  |  Social_Use2 == "Several times a month")) %>% 
#   select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt, partyvote2018)) %>% 
#   na.omit() %>% 
#   as.matrix()
# 
# cpclow <- on18 %>% 
#   filter((Social_Use2 == "Several times in a year" | Social_Use2 == "Never" | Social_Use2 == "About once a month")) %>% 
#   select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
#   as.matrix()
# 
# 
# cpc_legacy <- on18 %>% 
#   filter(Primary_media == "Legacy") %>% 
#   select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
#   as.matrix()
# 
# cpc_social <- on18 %>% 
#   filter(Primary_media == "Social_Media") %>% 
#   select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
#   as.matrix()
# 
# cpc_online <- on18 %>% 
#   filter(Primary_media == "Online") %>% 
#   select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
#   na.omit() %>% 
#   as.matrix()
# 
# cpc_mixed <- on18 %>% 
#   filter(Primary_media == "Mixed") %>% 
#   select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
#   as.matrix()
# 
# set.seed(178)
# CPC(cpc_high, k = 4, type = "kmeans")
# set.seed(178)
# CPC(cpc_high, "manual", cols = 1:4, clusters = 4)
# CPC(cpclow, k = 4, type = "kmeans")
# set.seed(178)
# CPC(cpc_legacy, k = 4, type = "kmeans")
# set.seed(178)
# CPC(cpc_social, k = 4, type = "kmeans")
# set.seed(178)
# CPC(cpc_online, k = 4, type = "kmeans")
# set.seed(178)
# CPC(cpc_mixed, k = 4, type = "kmeans")

#Create regression models for policy polarization



mods_Interest <- policies_social_use_sd_dis2_down %>%
  group_by(Policy_Issue) %>%
  summarise(lm_mod = list(lm(distance ~ Interest))) %>% 
  mutate(tidied = map(lm_mod, tidy, conf.int = T)) 

mods_Interest
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


#Create graphs of the marginal effects of the interaction models using the functions defined in "0_functions.R"
marginaleffects::plot_slopes(mods_interact[[2]][[1]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[2]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[3]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[4]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[5]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[6]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[7]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[8]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[9]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[10]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()
marginaleffects::plot_slopes(mods_interact[[2]][[11]], variables = "Interest", condition = "Primary_Media") + geom_hline(yintercept  = 0, lty = "dashed", col = "forestgreen") + theme_bw()


#### CPC AFFECT POLARIZATION ####
on18$partyeval_4_out

on18 <- on18 %>% 
  mutate(
    Liberal_Like_wt = partyeval_1_out * 0.196,
    NDP_Like_wt = partyeval_3_out * 0.336,
    Green_Like_wt = partyeval_4_out * 0.046,
    Con_Like_wt = partyeval_2_out * 0.405
  )

cpc_high <- on18 %>% 
  filter((Social_Use2 == "Several times a day" | Social_Use2 == "About once a day" |  Social_Use2 == "Several times a week" | Social_Use2 == "About once a week"  |  Social_Use2 == "Several times a month")) %>% 
  select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
  as.matrix()

cpclow <- on18 %>% 
  filter((Social_Use2 == "Several times in a year" | Social_Use2 == "Never" | Social_Use2 == "About once a month")) %>% 
  select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
  as.matrix()


cpc_legacy <- on18 %>% 
  filter(Primary_media == "Legacy") %>% 
  select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
  as.matrix()

cpc_social <- on18 %>% 
  filter(Primary_media == "Social_Media") %>% 
  select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
  as.matrix()

cpc_online <- on18 %>% 
  filter(Primary_media == "Online") %>% 
  select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
  na.omit() %>% 
  as.matrix()

cpc_mixed <- on18 %>% 
  filter(Primary_media == "Mixed") %>% 
  select(c(Liberal_Like_wt,NDP_Like_wt,Green_Like_wt ,  Con_Like_wt)) %>% 
  as.matrix()

set.seed(178)
CPC(cpc_high, k = 4, type = "kmeans")
set.seed(178)
CPC(cpclow, k = 4, type = "kmeans")
set.seed(178)
CPC(cpc_legacy, k = 4, type = "kmeans")
set.seed(178)
CPC(cpc_social, k = 4, type = "kmeans")
set.seed(178)
CPC(cpc_online, k = 4, type = "kmeans")
set.seed(178)
CPC(cpc_mixed, k = 4, type = "kmeans")


#### POLACKO POLICY POLARIZATION ####

#create number of standard deviations from mean variable
policies_social_use
policies_social_use_sd_dis <- do.call(cbind, lapply(policies_social_use[, 1:11], sd_mean, na.rm = T)) 
#Policies_social_use_sd_dis is now each R's distance from the mean in SD. 
head(policies_social_use_sd_dis)
policies_social_use_sd_dis %>%
  bind_cols(., on18$Social_Use2)->policies_social_use_sd_dis
names(policies_social_use_sd_dis)<-c(policy_names, "Social_Use2")


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

names(policies_social_use_sd_dis2)<-c(policy_names, "Social_Use2", "Primary_Media", "Interest")

policies_social_use_sd_dis2 %>%
  pivot_longer(cols = 1:11, names_to = "Policy_Issue", values_to = "distance") -> policies_social_use_sd_dis2_down
policies_social_use_sd_dis2_down

#### Policy Polarization ####

policies_social_use_sd_dis %>%
  group_by(Social_Use2) %>%
  summarise( across(everything(), list(.mean = mean, .se = se), na.rm = T )) -> policies_social_use_sd_dis_mean
policies_social_use_sd_dis_mean

#on18 <- cbind(on18, policies_social_use_sd_dis)

policies_social_use_sd_dis %>%
  bind_cols(., on18$Primary_media) %>%
  bind_cols(., on18$Interest) ->policies_social_use_sd_dis2
