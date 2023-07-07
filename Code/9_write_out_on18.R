## Variable labels
var_label(on18$ethnicity1)<-'What is your ethnicity (coded)?'
var_label(on18$ethnicity2)<-'What is your ethnicity (coded)?'
var_label(on18$regions)<-'Ontario region based on FSA provided'
var_label(on18$anger_fin)<-'Count of anger words about individual financial situation'
var_label(on18$anticipation_fin)<-'Count of anticipation words about individual financial situation'
var_label(on18$disgust_fin)<-'Count of disgust words about individual financial situation'
var_label(on18$fear_fin)<-'Count of fear words about individual financial situation'
var_label(on18$joy_fin)<-'Count of joy words about individual financial situation'
var_label(on18$sadness_fin)<-'Count of sadness words about individual financial situation'
var_label(on18$surprise_fin)<-'Count of surprise words about individual financial situation'
var_label(on18$trust_fin)<-'Count of trust words about individual financial situation'
nrow(on18)


names(on18)
names(on18)
on18 %>% 
  write_sav('Data/file_with_recodes.sav')
# on18 %>% 
#    #select(1:286, 325:347, 352:353) %>% 
#   select(-ends_with('out'),  -contains('econ_'), -starts_with('imm_'), -caseid:-fsaopes, -gov_satisfaction:-wynne_satisfaction, -voteswing:-age3) %>% 
#   write_sav(.,path='/Users/Simon/Dropbox/Ontario 2018/Data/OPES/Opes_v_2.sav')
