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


ov_policy_legacy<-round(overlapping::overlap(list(policy_left_legacy_vector, policy_conservatives_legacy_vector))$OV,2)
#overlapping::boot.overlap(list(policy_left_legacy_vector, policy_conservatives_legacy_vector), B = 1000)


#bayestestR::overlap(policy_left_legacy_vector, policy_conservatives_legacy_vector)


overlap_legacy <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_legacy, col = "red", fill = "red", alpha = 0.4) + 
  labs(title = "Legacy Media", subtitle = paste("Overlap Coefficent", ov_policy_legacy, sep=" "), x = "Policy Positions", y = NULL) +
  geom_density(aes(policy_polarization), data = policy_conservatives_legacy, col = "blue", fill = "blue", alpha = 0.4) + theme_bw()
overlap_legacy

policy_left_online_vector <- as.double(policy_left_online$policy_polarization) %>% 
  na.omit() 
policy_conservatives_online_vector <- as.double(policy_conservatives_online$policy_polarization) %>% 
  na.omit() 

ov_policy_online<-round(overlapping::overlap(list(policy_left_online_vector, policy_conservatives_online_vector))$OV,2)
#overlapping::boot.overlap(list(policy_left_online_vector, policy_conservatives_online_vector), B = 1000)

#bayestestR::overlap(policy_left_online, policy_conservatives_online)

overlap_online <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_online, col = "red", fill = "red", alpha = 0.4) + 
  labs(title = "Online Media", subtitle = paste("Overlap Coefficent", ov_policy_online, sep=" "), x = "Policy Positions", y = NULL) +
  geom_density(aes(policy_polarization), data = policy_conservatives_online, col = "blue", fill = "blue", alpha = 0.4) + theme_bw()

policy_left_media_vector <- as.double(policy_left_social_media$policy_polarization) %>% 
  na.omit() 
policy_conservatives_media_vector <- as.double(policy_conservatives_media$policy_polarization) %>% 
  na.omit() 

ov_policy_media<-round(overlapping::overlap(list(policy_left_media_vector, policy_conservatives_media_vector))$OV,2)
#overlapping::boot.overlap(list(policy_left_media_vector, policy_conservatives_media_vector), B = 1000)

#bayestestR::overlap(policy_left_social_media, policy_conservatives_media)

overlap_smedia <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_social_media, col = "red", fill = "red", alpha = 0.4) + 
  labs(title = "Social Media", subtitle = paste("Overlap Coefficent", ov_policy_media, sep=" "), x = "Policy Positions", y = NULL) +
  geom_density(aes(policy_polarization), data = policy_conservatives_media, col = "blue", fill = "blue", alpha = 0.4) + theme_bw()
overlap_smedia
policy_left_mixed_vector <- as.double(policy_left_mixed$policy_polarization) %>% 
  na.omit() 
policy_conservatives_mixed_vector <- as.double(policy_conservatives_mixed$policy_polarization) %>% 
  na.omit() 



ov_policy_mixed<-round(overlapping::overlap(list(policy_left_mixed_vector, policy_conservatives_mixed_vector))$OV,2)
#set.seed(1234); overlapping::boot.overlap(list(policy_left_mixed_vector,
                                               #policy_conservatives_mixed_vector), B = 1000)
#bayestestR::overlap(policy_left_mixed, policy_conservatives_mixed)

overlap_mixed <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_mixed, col = "red", fill = "red", alpha = 0.4) +
  labs(title = "Mixed Media", subtitle = paste("Overlap Coefficent", ov_policy_mixed, sep=" "), x = "Policy Positions", y = NULL) +
  geom_density(aes(policy_polarization), data = policy_conservatives_mixed, col = "blue", fill = "blue", alpha = 0.4) + theme_bw()

ggpubr::ggarrange(overlap_legacy, overlap_online, overlap_smedia, overlap_mixed)

#### OVERLAP FOR SOCIAL USE ####

policy_left_often_vector <- as.double(policy_left_often$policy_polarization) %>% 
  na.omit() 
policy_conservative_often_vector <- as.double(policy_conservative_often$policy_polarization) %>% 
  na.omit() 


ov_policy_social_usage_often<-round(overlapping::overlap(list(policy_left_often_vector, policy_conservative_often_vector))$OV,2)
#set.seed(1234); overlapping::boot.overlap(list(policy_left_often_vector,
                                               #policy_conservative_often_vector), B = 1000)

#bayestestR::overlap(policy_left_often, policy_conservative_often)

overlap_often <- ggplot() + geom_density(aes(policy_polarization), data = policy_left_often, col = "red", fill = "red", alpha = 0.4) + 
  geom_density(aes(policy_polarization), data = policy_conservative_often, col = "blue", fill = "blue", alpha = 0.4) +
  labs(title = "Uses Social Media Often", subtitle = paste("Overlap Coefficent", ov_policy_social_usage_often, sep=" "), x = "Policy Positions", y = NULL) + theme_bw()

policy_left_rarely_vector <- as.double(policy_left_rarely$policy_polarization) %>% 
  na.omit() 
policy_conservative_social_rarely_vector <- as.double(policy_conservative_social_rarely$policy_polarization) %>% 
  na.omit() 



ov_policy_social_usage_rarely<-round(overlapping::overlap(list(policy_left_rarely_vector, policy_conservative_social_rarely_vector))$OV,2)
#set.seed(1234); overlapping::boot.overlap(list(policy_left_rarely_vector,
                                              # policy_conservative_social_rarely_vector), B = 1000)
#bayestestR::overlap(policy_left_rarely_vector, policy_conservative_social_rarely_vector)

overlap_rarely <- ggplot() + 
  geom_density(aes(policy_polarization), data = policy_left_rarely, col = "red", fill = "red", alpha = 0.4) + 
  geom_density(aes(policy_polarization), data = policy_conservative_social_rarely, col = "blue", fill = "blue", alpha = 0.4) +
  labs(title = "Uses Social Media Rarely", subtitle = paste("Overlap Coefficent", ov_policy_social_usage_rarely, sep=" "), x = "Policy Positions", y = NULL) + theme_bw()


#ggpubr::ggarrange(overlap_often, overlap_rarely, ncol = 1)


#report the null of the social media use
#make the positive finding more interesting
#statistical tests with Overlap coefficients 
