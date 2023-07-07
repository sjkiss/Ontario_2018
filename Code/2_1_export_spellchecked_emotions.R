source('2_separate_text_answers.R')
names(on18)
set.seed(1234)
#Select first batch
on18[sample(nrow(on18), 700),c('id', 'indivfinfeel','immfeel' )]-> out
#Check
out %>% 
complete.cases() %>% 
  table()
#Save with new variables
out<-data.frame(id=out$id, 
           indivfinfeel1=out$indivfinfeel1, 
           emotion_econ=rep('', nrow(out)), 
           intensity_econ=rep('', nrow(out)), 
           words_econ=rep('', nrow(out)), 
           issues_econ=rep('', nrow(out)), 
           immfeel1=out$immfeel1, 
           emotion_imm=rep('', nrow(out)), 
           intensity_imm=rep('', nrow(out)), 
           words_imm=rep('', nrow(out)), 
           issues_imm=rep('', nrow(out)))
#Save codes
codes<-data.frame(emotion_code=seq(1, 8, 1), Emotion=c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust'), intensity_code=c(seq(1,3,1), rep('', 5)), Intensity=c('low', 'medium', 'high', rep('', 5)))
codes
l<-list(out, codes)
table(out$id)
table(out2$id)
#export(l, 'feelings_text_qualitative_analysis.xlsx')

#Save batch two
out$id
on18 %>% 
  filter(!id %in% out$id) %>% 
  select(id, indivfinfeel1, immfeel1) -> out2

out2<-data.frame(id=out2$id, 
                 indivfinfeel1=out2$indivfinfeel1, 
                 emotion_econ=rep('', nrow(out2)), 
                 intensity_econ=rep('', nrow(out2)), 
                 words_econ=rep('', nrow(out2)), 
                 issues_econ=rep('', nrow(out2)), 
                 immfeel1=out2$immfeel1, 
                 emotion_imm=rep('', nrow(out2)), 
                 intensity_imm=rep('', nrow(out2)), 
                 words_imm=rep('', nrow(out2)), 
                 issues_imm=rep('', nrow(out2)))
# export(out2, 'second_feelings_text_qualitative_analysis.csv')
