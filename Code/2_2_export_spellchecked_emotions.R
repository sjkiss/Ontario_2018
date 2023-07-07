source('2_separate_text_answers.R')
names(on18)
set.seed(1234)
on18[sample(nrow(on18), 700),c('id', 'indivfinfeel1','immfeel1' )]-> out

out %>% 
complete.cases() %>% 
  table()

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
codes<-data.frame(emotion_code=seq(1, 8, 1), Emotion=c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust'), intensity_code=c(seq(1,3,1), rep('', 5)), Intensity=c('low', 'medium', 'high', rep('', 5)))
codes
l<-list(out, codes)
#export(l, 'feelings_text_qualitative_analysis.xlsx')
