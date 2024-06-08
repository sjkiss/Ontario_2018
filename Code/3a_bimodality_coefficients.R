#### BIMODALITY COEFFICENT ####

#### Primary Media ####
bimod_online <- bimodality_coefficient(policy_online, na.rm = T); bimod_online
bimod_leacgy <- bimodality_coefficient(policy_legacy, na.rm = T); bimod_leacgy
bc_policy_social_media<-bimodality_coefficient(policy_social_media, na.rm = T)
bc_policy_mixed<-bimodality_coefficient(policy_mixed, na.rm = T)
bc_policy_legacy<-bimodality_coefficient(policy_legacy, na.rm = T)
bc_policy_online<-bimodality_coefficient(policy_online, na.rm = T)

bimod_legacy <- policy_legacy %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Legacy Media", subtitle=paste("BC", round(bc_policy_legacy,2), sep=" "), x = "Policy Positions", y = NULL) +
  theme_bw()
bimod_legacy
bimod_online <- policy_online %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Online Media", subtitle=paste("BC", round(bc_policy_online,2), sep=" "),x = "Policy Positions", y = NULL) +
  theme_bw()

bimod_social <- policy_social_media %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Social Media", subtitle=paste("BC", round(bc_policy_social_media,2), sep=" "),x = "Policy Positions", y = NULL) +
  theme_bw()

bimod_mixed <- policy_mixed %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Mixed media", subtitle=paste("BC", round(bc_policy_mixed,2), sep=" "),x = "Policy Positions", y = NULL) +
  theme_bw()

#Nice to have fake distributions of the different kinds of distribution

gridExtra::grid.arrange(bimod_legacy, bimod_online, bimod_social, bimod_mixed) 
#### Social Use ####

bimodality_coefficient(policy_often, na.rm = T)
bimodality_coefficient(policy_rarely, na.rm = T)

bimod_often <- policy_often %>% 
  ggplot() + 
  geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Uses Social Media Often", x = "Policy Positions", y = NULL, subtitle=paste("BC =", round(bimodality_coefficient(policy_often, na.rm = T),2)))+
  theme_bw()
bimod_often
bimod_rarely <- policy_rarely %>% 
  ggplot() + geom_density(aes(policy_polarization), col = "seagreen", fill = "seagreen", alpha = 0.4) + 
  labs(title = "Uses Social Media Rarely", x = "Policy Positions", y = NULL,subtitle=paste("BC =", round(bimodality_coefficient(policy_rarely, na.rm = T),2))) +
  theme_bw()
bimod_rarely
gridExtra::grid.arrange(bimod_often, bimod_rarely) 
