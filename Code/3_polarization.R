#Add in a line that sources the first script 1_load_on18
source("Code/1_load_on18.R")
#### Political Knowledge #### 
#Create one political knowledge variable using the variables on political knowledge.
# R gets 1 if they got each response right
# sum to 3 divide by 3 to get one variable indicating their knowledge

on18$unsg_correct <- ifelse(on18$unsg == 1, 1, 0)
on18$financename_correct <- ifelse(on18$financename == 1, 1, 0)
on18$ggname_correct <- ifelse(on18$ggname == 1, 1, 0)
on18$nhse_correct <- ifelse(on18$nhse == 1, 1, 0)
# Here is a quick way to look at what you are dealing with in the original variables

on18 %>% 
  select(unsg:nhse) %>% 
  glimpse()
#This prints the variable labels of each item to see what each measures
on18 %>% 
  select(unsg:nhse) %>% 
  var_label()
#this prints the value lablels to see what each value label contains
on18 %>% 
  select(unsg:nhse) %>% 
  val_labels()
#Interesting, in this one it looks like the survey team coded some 
# close responses. A person who responds with Bill Morneau (even though wrong)
# Might know more than someone who just flat out doesn't know. 
# For know let's keep it the way you have it. Responses are correct only if 1

#### Automatic way to recode several variables####
# I'm going to show you how to recode several variables the same way at the same time.

# Always start with the data frame
on18 %>% 
  #We are transforming variables so we use mutate
  # We are going to be applying a function across several columns, so we use
  # across()
  #Importantly, we can use the same select() semantics inside across()
  #in this case we can select the variables from unsg to nhse, including the ones in between
  #We only know this really through poking and actually looking at the data columns. Even in SPSS if necessary.
  mutate(
    across(.cols=unsg:nhse, 
           #Now add in the function we are going to apply.
           #Note that if_else is just a dplyr version of ifelse()
           function(x) if_else(x==1, 1, 0), 
           # A cool feature of across() is that you can add a suffix or a prefix
           #This will paste the name of the column with _correct
           #Exactly like you did above.
           .names="{.col}_correct")
    )->on18
#Now you can quickly check that it worked
on18 %>% 
  select(ends_with("_correct"))


lookfor(on18, "seats in the House of Commons")
#This is great
on18$pol_knowledge <- (on18$unsg_correct + on18$financename_correct + on18$ggname_correct + on18$nhse_correct)/4

# Add in a check
on18 %>% 
  ggplot(., aes(x=pol_knowledge))+geom_histogram()
#But we should also check the math.
on18 %>% 
  select(ends_with("correct")| "pol_knowledge") %>% 
  View()
on18$pol_knowledge
on18 %>%
  group_by(Social_Use) %>%
  summarise(mean = mean(pol_knowledge, na.rm = T), sd = sd(pol_knowledge, na.rm = T)) 

table(on18$Social_Use, on18$pol_knowledge)

#### Policy polarization ####
# R social media users frothing at the mouth extremists
# Pick good policy measures from data 
# Use pre-recoded climate change questions
# Use batteries on policy items
# Rename each agreement item with a useful variable name. 
# new_names<-c("help_racial_minorities")
#climate change

on18$climate_views <- (on18$climate_bin + on18$paris_bin)/2



on18 %>% 
  select(starts_with("agreement_"))-> policies 

policy_names <- c("help_racial_minorities", "help_women", "more_coporate_tax", "more_personal_tax", "private_health_care", 
                  "minimum_wage_to_high_prices", "income_inequality", "business_benefits_everyone", 
                  "drug_benefit_u25", "free_post_secondary", "inappropriate_sex_ed")
names(policies)<-policy_names
#recombine
policies <- mutate_all(policies, function(x) as.numeric(as.character(x)))
range01 <- function(x, ...){(x-min(x, ...))/(max(x, ...)-min(x, ...))}
policies <- range01(policies, na.rm = T)
policies %>%  
 bind_cols(., on18$Social_Use)->policies

names(policies)<-c(policy_names, "Social_Use")

policies %>%
  group_by(Social_Use) %>%
  summarise(across(everything(), sd, na.rm = T), .groups = "drop") -> policies_sd


#Calculate standard deviation for each. 
# Take on18
# pivot down the agreement variables after rename
policies_sd_down <- policies_sd %>%
  pivot_longer(cols = !Social_Use,
               names_to = "Policy Issue", 
               values_to = "Standard Deviation")
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

View(policies_sd_down)


