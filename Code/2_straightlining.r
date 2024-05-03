source("Code/3_polarization.R")
#install.packages("careless")
library(careless)
library(estimatr)
lookfor(on18, "feel")
on18 %>% 
  select(matches("partyeval_[0-9]$")) %>% 
  irv(.)->irv_party

on18$irv_party<-irv_party
on18 %>% 
  filter(irv_party==0)
on18 %>% 
  select(matches("leader_[0-9]$")) %>% 
  irv(.)->irv_leader
on18$irv_leader<-irv_leader
on18 %>% 
  filter(irv_party==0|irv_leader==0)
lookfor(on18, "taxes")
on18 %>% 
  select(matches("^agreement_[0-9]$")) %>% 
  irv(.)->irv_agreement
on18$irv_agreement<-irv_agreement
lookfor(on18, "spend")
on18 %>% 
  select(matches("^spending_[0-9]$")) %>% 
  irv(.)->irv_spending
on18$irv_spending<-irv_spending
on18 %>% 
  filter(if_any(contains("irv"), ~.==0))

on18 %>% 
  mutate(straightliner=case_when(
    irv_party==0~ 1,
    irv_agreement==0~1,
    irv_spending==0~1,
    irv_leader==0~1,
    TRUE~0
  ))->on18
table(on18$straightliner)

#### RE-RUN MAIN ANALYSES WITHOUT STRAIGHT LINERS ####

COVARS <- c("Interest", "age3", "degree", "income3", "pol_knowledge")
on18$Pr
WAP_primary_media <- list()
for(i in 1:length(COVARS)){
 data <- on18 |> filter(straightliner == 0)
 WAP_primary_media[[i]] <-  lm_robust(reformulate(c("Primary_media", COVARS[1:i]),
                                                  response = "WAP_sd"), data = data, se_type = "HC0")
}
lm_robust(reformulate(c("Primary_media"),
                      response = "WAP_sd"), data = data, se_type = "HC0")
modelsummary(WAP_primary_media, stars = T)

