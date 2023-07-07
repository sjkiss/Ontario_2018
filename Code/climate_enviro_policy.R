source('1_load_on18.R')
source('1_load_ipsos.R')
look('climate', on18.labs)
look('warm', on18.labs)
look('green', on18.labs)
#View(on18.labs)
look('paris', on18.labs)
look('rural', on18.labs)
on18$area
theme_set(theme_bw())

on18 %>% 
  select('climate', 'area', 'pc') %>% 
mutate(pc=dplyr::recode(pc, `1`="pc", `0`="other")) %>% 
  group_by(pc, area, climate) %>% 
  summarize(freq=n()) %>% 
  filter(is.na(pc)==F, is.na(area)==F, is.na(climate)==F& area!='Other') %>% 
  ggplot(., aes(x=pc, y=freq, group=climate))+geom_col(aes(fill=climate), position='dodge')+facet_wrap(~area)


on18 %>% 
  select('area', 'climate', 'paris', 'pc') %>% 
  #mutate_all(as_factor) %>% 
  mutate(pc=dplyr::recode(pc, `1`="pc", `0`="other")) %>% 
  group_by(pc, area, climate, paris) %>% 
  summarize(freq=n()) %>% 
  filter(area!='Other'&is.na(area)==F& is.na(climate)==F& is.na(pc)==F) %>% 
  ggplot(., aes(x=climate, y=freq, group=paris))+geom_col(aes(fill=paris), position='dodge')+facet_wrap(pc~area)
  



library(forcats)

leader_approval %>% 
  #gather into an issue variable and a leader variable
  gather(Issue, Leader) %>% 
  #count
  count(Issue, Leader) %>% 
  #form groups for counting percent
  group_by(Issue) %>% 
  #calculate percent
  mutate(pct=n/sum(n)) %>%
  #ungroup
  ungroup-> plot_df
plot_df
issue_order<-filter(plot_df, Leader == "Doug Ford") %>% 
  arrange(desc(pct)) %>% 
  pull(Issue) %>%
  as.character
length(issue_o)
leader_approval_issues<-plot_df %>% 
  mutate(Issue=factor(Issue, levels=issue_order)) %>% 
  ggplot(., aes(x=Leader, y=pct, fill=Leader))+geom_col()+facet_wrap(~Issue,labeller = label_wrap_gen())+scale_fill_manual(values=c('orange' ,'blue', 'red', 'black'))+theme(axis.text.x=element_blank())

#Most important issue
#MIP overall
ipsos %>% 
  select(332:354) %>% 
gather(Issue, Rank) %>% 
  count(Issue, Rank) %>% 
  group_by(Issue) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  filter(Rank!=0) %>% 
  ungroup() ->out

filter(out, Rank==1) %>% 
  arrange(desc(pct)) %>% 
  pull(Issue)-> mip_order

out %>% 
  mutate(Issue=factor(Issue, levels=mip_order)) %>% 
  ggplot(., aes(x=Issue, y=pct, fill=as.factor(Rank)))+geom_col(position='dodge')+facet_wrap(~Issue, labeller=label_wrap_gen(), scales='free_x')+theme(axis.text.x=element_blank())
ggsave('plots/most_important_problems.png', width=8, height=4)

