library(weights)
library(tidyverse)
library(ipumsr)
library(stats)
library(reldist)

ipums_file <- read_ipums_micro("C:/Users/theto/Documents/ATUS/12.4.substack/ATUS_DDI3.xml",vars=NULL,n_max=Inf,data_file=NULL,verbose=TRUE,var_attrs=c("val_labels","var_label","var_desc"),lower_vars=FALSE)
data <- ipums_file
rm(ipums_file)

data_household <- data %>% filter(RECTYPE==1)
data_person <- data %>% filter(RECTYPE==2)
data_activity <- data %>% filter(RECTYPE==3)
rm(data)
data_household <- data_household[,colSums(is.na(data_household))<nrow(data_household)]
data_person <- data_person[,colSums(is.na(data_person))<nrow(data_person)]
data_activity <- data_activity[,colSums(is.na(data_activity))<nrow(data_activity)]

grocery_act <- data_activity %>% filter(ACTIVITY==070101)
grocery <- left_join(data_person,grocery_act,by="CASEID") %>%
  filter(!is.na(ACTIVITY)) %>%
  filter(!is.na(WT06))
weighted.mean(grocery$DURATION,grocery$WT06) #average grocery trip duration, weighted
df <- grocery %>% group_by(YEAR.x,CASEID) %>% summarize(total_time = sum(DURATION))
a <- data_person %>% select(c("CASEID","WT06","SEX"))
b <- data_household %>% select(c("CASEID","HH_CHILD"))
a <- left_join(a,b,by="CASEID")
df <- left_join(df,a,by="CASEID")
weighted.mean(df$total_time,df$WT06)

#annual grocery shopping by sex
x <- df %>% group_by(SEX) %>% summarize(yearly_hrs = weighted.mean(total_time,WT06)*365/60)
samp_a <- df %>% filter(SEX==1)
samp_b <- df %>% filter(SEX==2)
t.test(samp_a$total_time,samp_b$total_time,var.equal=FALSE)

#annual grocery shopping by parental status
x <- df %>% group_by(HH_CHILD) %>% summarize(yearly_hrs = weighted.mean(total_time,WT06)*365/60)
samp_a <- df %>% filter(HH_CHILD==0)
samp_b <- df %>% filter(HH_CHILD==1)
t.test(samp_a$total_time,samp_b$total_time,var.equal=FALSE)

#grocery shopping by parents of children under 13
df <- grocery %>% filter(KIDUND13==1) %>%
  mutate(with_kids = ifelse(SCC_OWN_LN>0,1,0))
df %>% group_by(SEX,with_kids) %>% summarize(wtd.total = sum(WT06,na.rm=TRUE))
df %>% group_by(with_kids) %>% summarize(wtd.avg = weighted.mean(DURATION,WT06))
samp_a <- df %>% filter(with_kids==1)
samp_b <- df %>% filter(with_kids==0)
t.test(samp_a$DURATION,samp_b$DURATION)

#grocery shopping by day
df <- grocery %>%
  group_by(DAY) %>%
  summarize(total_visits = sum(WT06,na.rm=TRUE),
            wtd.avg = weighted.mean(DURATION,WT06))
a <- sum(df$total_visits)
df$pct <- df$total_visits/a
samp_a <- grocery %>% filter(DAY==1|DAY==7)
samp_b <- grocery %>% filter(DAY>1&DAY<7)
wtd.t.test(samp_a$DURATION,samp_b$DURATION,weight=samp_a$WT06,weighty = samp_b$WT06,samedata = FALSE,drops="")

#travel time to grocery store
t_grocery_act <- data_activity %>% filter(ACTIVITY==180701)
t_grocery <- left_join(data_person,t_grocery_act,by="CASEID") %>%
  filter(!is.na(ACTIVITY)) %>%
  filter(!is.na(WT06))
weighted.mean(t_grocery$DURATION,t_grocery$WT06) #average grocery trip duration, weighted
df <- t_grocery %>% group_by(YEAR.x,CASEID) %>% summarize(total_time = sum(DURATION))
a <- data_person %>% select(c("CASEID","WT06","SEX"))
b <- data_household %>% select(c("CASEID","HH_CHILD"))
a <- left_join(a,b,by="CASEID")
df <- left_join(df,a,by="CASEID")
weighted.mean(df$total_time,df$WT06)

