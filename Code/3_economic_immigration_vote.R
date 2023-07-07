#correlations of economic voting variables
on18 %>% 
  select(indiv_stress, indiv_comfort, indiv_better, indiv_policies, income) %>% 
  cor(., use='pairwise.complete.obs')


#Check correlation of stress and ess variable
ggplot(on18, aes(x=indiv_stress, y=indiv_comfort))+geom_point()+geom_smooth(method='lm')

#Check stress by party

on18 %>% 
  ggplot(., aes(x=as_factor(partyvote2018), y=indiv_stress))+geom_boxplot()
#Test $50 with ESS Question
lookfor(on18, 'income')
on18$income

on18 %>% 
  group_by(income) %>% 
  summarize(average=mean(indiv_stress, na.rm=T)) %>% 
  ggplot(., aes(x=income, y=average))+geom_point()

#Check out stress by income


#Check correlation of immigration
on18 %>% 
  select(starts_with('imm_')) %>% 
  cor(., use='pairwise.complete.obs')
lookfor(on18, 'vote')
#
on18 %>% 
  select(partyvote2018, starts_with('imm_')) %>% 
  gather(Immigration, Value, -partyvote2018) %>% 
  filter(partyvote2018!=0, partyvote2018!=5) %>% 
  ggplot(., aes(x=as_factor(partyvote2018), y=Value))+geom_boxplot()+labs(title='immigration sentiment by party')

#Vote model Economics
econ1<-glm(pc~indiv_stress, data=on18, family='binomial')
econ2<-glm(pc~indiv_stress+indiv_comfort, data=on18)
econ3<-update(econ2, ~.+indiv_better)
econ4<-update(econ3, ~.+pc14)
summary(econ1)
summary(econ2)
summary(econ3)
summary(econ4)

#Vote model Immigration

imm1<-glm(pc~imm_refugees, data=on18, family='binomial')
summary(imm1)
imm2<-update(imm1,~+imm_criminals, data=on18)
imm3<-update(imm1, ~+imm_jobs, data=on18)
imm4<-update(imm1, ~+imm_netbenefit, data=on18)
imm5<-update(imm1, ~+imm_rev, data=on18)

#Combined
econimm1<-glm(pc~indiv_stress+imm_rev, data=on18)



