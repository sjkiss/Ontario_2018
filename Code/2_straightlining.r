#source("Code/1_load_on18.R")
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
  select(contains("leader"))
on18 %>% 
  select(matches("^leadereval_[0-9]$")) %>% 
  irv(.)->irv_leader
on18$irv_leader<-irv_leader
irv_leader
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
  select(matches("privacy_[0-9]$")) %>% 
  irv(.)->irv_privacy
on18$irv_privacy<-irv_privacy
on18 %>% 
  filter(if_any(contains("irv"), ~.==0))
on18 %>% 
  mutate(straightliner=case_when(
    irv_party==0~ 1,
    irv_agreement==0~1,
    irv_spending==0~1,
    irv_leader==0~1,
    irv_privacy==0~1,
    TRUE~0
  ))->on18
table(on18$straightliner)

