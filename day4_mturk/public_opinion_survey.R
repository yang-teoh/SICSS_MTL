library(tidyverse)
library(rstudioapi)
library(lme4)

path = dirname(getSourceEditorContext()$path)
setwd(path)



mturk<-read.csv("POS-G3.csv", header=T)

## Do computer algorithms exhibit biases?

mturk$ALG1<-as.numeric(mturk$ALG1)-1 #1: computers will have bias
table(mturk$ALG1)

##In general, would you say that the content posted on social media 
#provides an accurate picture of how society as a whole feels about important issues?
mturk$SM3<-as.numeric(mturk$SM3)-1 #1: provide an accurate picture
table(mturk$SM3)

#How fair is it to companies to calculate an automated financial score to determine whether to offer financial products?
mturk$alg_consumers<-as.numeric(mturk$alg_consumers)-1 # Higher Values indicate More Fairness
table(mturk$alg_consumers)

#How acceptable is it for the criminal justice system to use computer algorithms to determine criminal risk?
mturk$alg_criminaljustice<-as.numeric(mturk$alg_criminaljustice)*-1+2 # 1 indicates Acceptable
table(mturk$alg_criminaljustice)

#How acceptable is it for the companies to use computer algorithms to screen video recorded behavior to determine hiring decisions
mturk$alg_job<-as.numeric(mturk$alg_job)*-1+2 # 1 indicates Acceptable
table(mturk$alg_job)

#How acceptable is it for the companies to use computer algorithms to screen resumes to determine hiring decisions
mturk$alg_job2<-ifelse(mturk$alg_job2=="Acceptable",1,ifelse(mturk$alg_job2=="Not Acceptable",0,NA)) # 1 indicates Acceptable
table(mturk$alg_job2)


# load cleaned data file for survey results
data <- read_csv("https://raw.githubusercontent.com/compsocialscience/summer-institute/master/2020/materials/day4-surveys/activity/2020_06_clean_mturk_data.csv")

# load external information -- in this case, population info
census <- read_csv("https://raw.githubusercontent.com/compsocialscience/summer-institute/master/2020/materials/day4-surveys/activity/2017_acs_data_clean.csv")

# load pew benchmarks
pew <- read_csv("https://raw.githubusercontent.com/compsocialscience/summer-institute/master/2020/materials/day4-surveys/activity/pew_benchmark_question_source_sicss_2020.csv")
pew <- pew %>% select(qid, pew_estimate)


##MTURK ESTIMATES
pew_rep_all = mturk %>%
 select('ALG1','SM3','alg_consumers','alg_criminaljustice','alg_job','alg_job2') %>%
  summarise_all(~mean(.,na.rm = T))

#PEW ESTIMAES
pewplot = pew %>%
  filter(grepl('ALG.1|SOCIALMEDIA_LOOKFEEL.1|ALG_PAROLE.1|ALG_JOBCANDIDATE.1|ALG_SCREENRESUME', qid)) %>%
  filter(qid != 'ALG_PERSONALFINANCE_ALG.1')

pewplot$sample = 'pew'

pewplot2 = pewplot

pewplot2$pew_estimate = c(pew_rep_all$ALG1,pew_rep$SM3, 
                          pew_rep_all$alg_criminaljustice,
                          pew_rep_all$alg_job,
                          pew_rep_all$alg_job2)
pewplot2$sample = 'mturk'

pplot = rbind(pewplot,pewplot2)
#COMPARISON PLOT
pplot %>%
  ggplot(.) +
  geom_col(aes(x = qid, y= pew_estimate, fill = sample), position = position_dodge())


#RECODING DEMOGRAPHICS
mturk$gender[mturk$sex=="Male"]<-0
mturk$gender[mturk$sex=="Female"]<-1
table(mturk$gender) # Women coded as 1
mturk$hispanic<-as.numeric(mturk$hispanic)-1 # 1 is Hispanic heritage.
table(mturk$hispanic)
table(mturk$race)
table(mturk$age_cat)
mturk$educ2[mturk$educ=="High school graduate (including GED or alternative credential)"]<-1
mturk$educ2[mturk$educ=="Some college or Associate's Degree (for example: AA, AS)"]<-2
mturk$educ2[mturk$educ=="Bachelor's degree (for example: BA, BS)"]<-3
mturk$educ2[mturk$educ=="Postgraduate or professional degree, including Master's, 
            Doctorate, medical, or law degree (for example: MA, MS, MEng, MEd, MSW, 
            MBA, MD, DDS, DVM, LLB, JD, PhD, EdD)"]<-4
table(mturk$educ2)
mturk$age_cat2<-ifelse(mturk$age_cat<30,1,ifelse(mturk$age_cat>29 & mturk$age_cat<50,2,ifelse(mturk$age_cat>49 & mturk$age_cat<65,3,4)))
mturk$age_cat2<-factor(mturk$age_cat2, levels = c(1,2,3), labels = c("18 - 29", "30 - 49", "50 - 64"))
table(mturk$age_cat2)

table(mturk$race)
mturk$race2<-ifelse(mturk$race=="White" & mturk$hispanic==0,"white",
                    ifelse(mturk$race=="Asian or Asian-American", "asian",
                           ifelse(mturk$race=="Black or African American" & mturk$hispanic==0 | mturk$race=="White, Black or African American, Other" & mturk$hispanic==0,"black"," hispanic")))
table(mturk$race2)

mturk$region<-ifelse(mturk$state=="Rhode Island" | mturk$state=="Massachusetts" | mturk$state=="New Jersey" | mturk$state=="New York","northeast",
                     ifelse(mturk$state=="Ohio" | mturk$state=="Illinois" | mturk$state=="Pennsylvania","midwest",
                            ifelse(mturk$state=="Alabama" | mturk$state=="Texas" | mturk$state=="Kentucky" | mturk$state=="Louisiana"| mturk$state=="Florida" | mturk$state=="North Carolina" | mturk$state=="Tennessee" | mturk$state=="Oklahoma","south","west")))
table(mturk$region)

# get total census population
N <- sum(census$POP)



# calculate group weights 
## group population data by sex and race
## get the sum for each cell and divide by total pop
population_counts <- census %>% 
  group_by(sex,race) %>%
  summarise(group_weight = sum(POP)/N)

# check that weights sum to one
if (sum(population_counts$group_weight) != 1) {
  print("weights don't sum to one")
}

head(population_counts)

# calculate group means for each question response
## group data by sex and race
## remove non-numeric variables (demographic vars)
## calculate group means for each column


pew_rep = mturk %>%
  select('ALG1','SM3','alg_criminaljustice','alg_job','alg_job2','gender','race2') %>%
  mutate(gender = abs(gender -1),
         race2 = factor(race2, levels = c(" hispanic", "asian","white","black"),
                        labels = c("hispanic", "asian","white","black"))) %>%
  group_by(gender,race2) %>%
  summarise_all(~mean(.,na.rm = T))

pew_rep$race2 = levels(pew_rep$race2)[as.numeric(pew_rep$race2)]

add = cbind(0,'other',pew_rep_all[,-3])
add2= cbind(1,'other', pew_rep_all[,-3])
colnames(add) = colnames(pew_rep)
colnames(add2) = colnames(pew_rep)

pew_rep = rbind(data.frame(pew_rep), data.frame(add),data.frame(add2)) %>%
  mutate(gender =factor(gender),
         race2 = factor(race2, levels = c("hispanic","asian","white","black",
                                          "other"),
                        labels = c("hispanic", "asian","white","black",
                                   "other"))) %>%
  arrange(gender,race2)

population_counts$race = factor(population_counts$race, 
                                levels = c("hispanic", "asian","white","black","other"))
population_counts = population_counts %>%
  arrange(sex,race) 
  
population_counts
pew_rep


pew_rep[,3:7] =pew_rep[,3:7] * replicate(5,population_counts$group_weight)

pew_rep_corrected = colSums(pew_rep[,3:7])

#corrected for race and gender
pewplot2_corrected = pewplot2
pewplot2_corrected$pew_estimate = pew_rep_corrected
pewplot2_corrected$sample = 'mturk_corrected_race_gender' 


#correcting for gender only

population_counts <- census %>% 
  group_by(sex) %>%
  summarise(group_weight = sum(POP)/N)

pew_rep = mturk %>%
  select('ALG1','SM3','alg_criminaljustice','alg_job','alg_job2','gender') %>%
  mutate(gender = abs(gender -1)) %>%
  group_by(gender) %>%
  summarise_all(~mean(.,na.rm = T))
population_counts = population_counts %>%
  arrange(sex)

pew_rep[,2:6] =pew_rep[,2:6] * replicate(5,population_counts$group_weight)

pew_rep_corrected_sex = colSums(pew_rep[,2:6])

pewplot2_corrected_sex = pewplot2
pewplot2_corrected_sex$pew_estimate = pew_rep_corrected_sex
pewplot2_corrected_sex$sample = 'mturk_corrected_gender'

#Comparing Pew data to Mturk, mturk correced for gender and mturk corrected for race and gender
pplot_corr = rbind(pewplot,pewplot2,pewplot2_corrected_sex,pewplot2_corrected)

pplot_corr$sample = factor(pplot_corr$sample, levels = c('pew', 
                                                         'mturk',
                                                         'mturk_corrected_gender',
                                                         'mturk_corrected_race_gender'),
                           labels = c('pew', 
                                      'mturk',
                                      'mturk_corrected\n_gender',
                                      'mturk_corrected\n_race_gender'))
pplot_corr %>%
  ggplot(.) +
  geom_col(aes(x = qid, y= pew_estimate, fill = sample), position = position_dodge()) + 
  theme_minimal()


