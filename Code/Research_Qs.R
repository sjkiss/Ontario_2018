#### Poltical Knowledge #### 
#Create one political knowledge variable using the variables on poitical knowledge.
# R gets 1 if they got each response right
# sum to 3 divide by 3 to get one variable indicating their knowledge

#### Policy polarization ####
# R social media users frothing at the moth extremists
# Pick good policy measures from data 
# Use pre-recoded climate change questions
# Use batteries on policy items
# Rename each agreement item with a useful variable name. 
# new_names<-c("help_racial_minorities")

# 
lookfor(on18, "corporate taxes")
on18 %>% 
  select(starts_with("agreement_"))-> junk
names(junk)<-new_names
#recombine
# on18 %>% 
#   bind_cols(., junk)->on18

#Calculate standard deviation for each. 
# Take on18
# pivot down the agreement variables after rename
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


