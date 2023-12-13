library(tidyverse)
library(haven)
library(stats)

state_fips <- read.csv("state_fips.csv")

#load UCC data
q1 <- read_sas("intrvw19/intrvw19/mtbi191x.sas7bdat")
q2 <- read_sas("intrvw19/intrvw19/mtbi192.sas7bdat")
q3 <- read_sas("intrvw19/intrvw19/mtbi193.sas7bdat")
q4 <- read_sas("intrvw19/intrvw19/mtbi194.sas7bdat")
q5 <- read_sas("intrvw19/intrvw19/mtbi201.sas7bdat")

ucc_data <- rbind(q1,q2,q3,q4,q5)
rm(q1,q2,q3,q4,q5)

#load family data for 4 quarters of 2019 FMLI interview files

q1 <- read_sas("fmli191x.sas7bdat")
q2 <- read_sas("fmli192.sas7bdat")
q3 <- read_sas("fmli193.sas7bdat")
q4 <- read_sas("fmli194.sas7bdat")
q5 <- read_sas("fmli201.sas7bdat")

#change all column names to uppercase
names(q1) <- toupper(names(q1))
names(q2) <- toupper(names(q2))
names(q3) <- toupper(names(q3))
names(q4) <- toupper(names(q4))
names(q5) <- toupper(names(q5))

#combine quarterly data
data_cu <- rbind(q1,q2,q3,q4,q5)
rm(q1,q2,q3,q4,q5)

data_cu_select <- data_cu %>%
  select(c("NEWID","INC_RANK","STATE","SMSASTAT","FINLWT21","FINCBTXM"))

#charitable UCCs
charitity_other <- ucc_data %>%
  filter(UCC==800821) %>%
  group_by(NEWID) %>%
  summarize(total_other = sum(COST))

charitity_relig <- ucc_data %>%
  filter(UCC==800831) %>%
  group_by(NEWID) %>%
  summarize(total_relig = sum(COST))

charitity_educ <- ucc_data %>%
  filter(UCC==800841) %>%
  group_by(NEWID) %>%
  summarize(total_educ = sum(COST))

charitable <- merge(charitity_other,charitity_relig,by="NEWID",all=TRUE)
charitable <- merge(charitable,charitity_educ,by="NEWID",all=TRUE)

#political UCCs
political <- ucc_data %>%
  filter(UCC==800851)

#charitable linked
df_charitable <- left_join(data_cu_select,charitable,by="NEWID")
df_charitable$total_other <- replace(df_charitable$total_other, is.na(df_charitable$total_other), 0)
df_charitable$total_relig <- replace(df_charitable$total_relig, is.na(df_charitable$total_relig), 0)
df_charitable$total_educ <- replace(df_charitable$total_educ, is.na(df_charitable$total_educ), 0)
df_charitable$total <- df_charitable$total_educ+df_charitable$total_other+df_charitable$total_relig
df_charitable <- df_charitable %>%
  mutate(inc_quint = cut(.$INC_RANK,breaks=c(0,.2,.4,.6,.8,1),right=FALSE,labels=c("q1","q2","q3","q4","q5")),
         inc_dec = cut(.$INC_RANK,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),right=FALSE,labels=c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
         pct_relig = total_relig/total,
         pct_other = total_other/total,
         pct_educ = total_educ/total,
         pct_of_income = total/(FINCBTXM/4),
         pct_relig_of_income = total_relig/(FINCBTXM/4),
         pct_educ_of_income = total_educ/(FINCBTXM/4),
         pct_other_of_income = total_other/(FINCBTXM/4))

#average and median of all charitable giving by income quintile
df_charitable %>% group_by(inc_quint) %>%
  summarize(avg = mean(total),
            wtd.avg = weighted.mean(total,FINLWT21),
            wtd.median = wtd.quantile(total,q=.5,na.rm=TRUE,weight=FINLWT21),
            wtd.avg.pct = weighted.mean(pct_of_income,FINLWT21))

#average and median of religious giving by income quintile
df_charitable %>% group_by(inc_quint) %>%
  summarize(avg = mean(total_relig),
            wtd.avg = weighted.mean(total_relig,FINLWT21),
            wtd.median = wtd.quantile(total_relig,q=.5,na.rm=TRUE,weight=FINLWT21),
            avg.pct = mean(pct_relig,na.rm=TRUE))

#average and median of educational giving by income quintile
df_charitable %>% group_by(inc_quint) %>%
  summarize(avg = mean(total_educ),
            wtd.avg = weighted.mean(total_educ,FINLWT21),
            wtd.median = wtd.quantile(total_educ,q=.5,na.rm=TRUE,weight=FINLWT21),
            avg.pct = mean(pct_educ,na.rm=TRUE))

#average and median of other giving by income quintile
df_charitable %>% group_by(inc_quint) %>%
  summarize(avg = mean(total_other),
            wtd.avg = weighted.mean(total_other,FINLWT21),
            wtd.median = wtd.quantile(total_other,q=.5,na.rm=TRUE,weight=FINLWT21),
            avg.pct = mean(pct_other,na.rm=TRUE))

#weighted average of giving categories by income quintile
df <- df_charitable %>% group_by(inc_quint) %>%
  summarize(
    all_giving = weighted.mean(total,FINLWT21)*4,
    relig_giving = weighted.mean(total_relig,FINLWT21)*4,
    educ_giving = weighted.mean(total_educ,FINLWT21)*4,
    other_giving = weighted.mean(total_other,FINLWT21)*4,
    avg_inc = weighted.mean(FINCBTXM,FINLWT21)
  )

df <- df_charitable %>% filter(FINCBTXM>0) %>% 
  filter(!is.na(inc_quint)) %>%
  filter(pct_of_income<2) %>%
  group_by(inc_quint) %>%
  summarize(
    pct_relig = weighted.mean(pct_relig_of_income,FINLWT21)*100,
    pct_educ = weighted.mean(pct_educ_of_income,FINLWT21)*100,
    pct_other = weighted.mean(pct_other_of_income,FINLWT21)*100
    #pct_all = weighted.mean(pct_of_income,FINLWT21)
  )

#does top fifth of income give less of their money to charitable causes than the 2-4th quintiles
samp_a <- df_charitable %>% filter(inc_quint=="q5")
samp_b <- df_charitable %>% filter(inc_quint=="q2"|inc_quint=="q3"|inc_quint=="q4")
t.test(samp_a$pct_of_income,samp_b$pct_of_income,var.equal = FALSE)

#percentage of CUs reporting charitable giving
df <- df_charitable %>%
  #filter(FINCBTXM>0) %>%
  filter(!is.na(inc_quint)) %>%
  mutate(giving = ifelse(total>0,TRUE,FALSE))
a <- df %>% group_by(giving) %>% summarize(sum(FINLWT21))

#percentage of CUs reporting charitable giving by income bucket
a <- df %>% mutate(bucket = cut(.$FINCBTXM,breaks=c(-50000,1,10000,25000,50000,75000,100000,200000,1000000),right=FALSE)) %>%
  group_by(bucket,giving) %>%
  summarize(total = sum(FINLWT21)) %>%
  pivot_wider(id_cols=bucket,names_from = giving,values_from=total)
a$pct <- a$`TRUE`/(a$`FALSE`+a$`TRUE`)*100

#december giving in december
dec_charitity_other <- ucc_data %>%
  filter(UCC==800821) %>%
  group_by(NEWID) %>%
  summarize(total_other = sum(COST))

dec_charitity_relig <- ucc_data %>%
  filter(UCC==800831&REF_MO==12) %>%
  group_by(NEWID) %>%
  summarize(total_relig = sum(COST))

dec_charitity_educ <- ucc_data %>%
  filter(UCC==800841&REF_MO==12) %>%
  group_by(NEWID) %>%
  summarize(total_educ = sum(COST))

dec_charitable <- merge(dec_charitity_other,dec_charitity_relig,by="NEWID",all=TRUE)
dec_charitable <- merge(dec_charitable,dec_charitity_educ,by="NEWID",all=TRUE)

df_dec <- left_join(data_cu_select,dec_charitable,by="NEWID")
df_dec$total_other <- replace(df_dec$total_other, is.na(df_dec$total_other), 0)
df_dec$total_relig <- replace(df_dec$total_relig, is.na(df_dec$total_relig), 0)
df_dec$total_educ <- replace(df_dec$total_educ, is.na(df_dec$total_educ), 0)
df_dec$total <- df_dec$total_educ+df_dec$total_other+df_dec$total_relig
df_dec <- df_dec %>%
  mutate(inc_quint = cut(.$INC_RANK,breaks=c(0,.2,.4,.6,.8,1),right=FALSE,labels=c("q1","q2","q3","q4","q5")),
         inc_dec = cut(.$INC_RANK,breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1),right=FALSE,labels=c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")),
         pct_relig = total_relig/total,
         pct_other = total_other/total,
         pct_educ = total_educ/total,
         pct_of_income = total/(FINCBTXM/4),
         pct_relig_of_income = total_relig/(FINCBTXM/4),
         pct_educ_of_income = total_educ/(FINCBTXM/4),
         pct_other_of_income = total_other/(FINCBTXM/4))

a <- df_dec %>% 
  mutate(giving = ifelse(total>0,TRUE,FALSE)) %>% 
  mutate(bucket = cut(.$FINCBTXM,breaks=c(-50000,1,10000,25000,50000,75000,100000,200000,1000000),right=FALSE)) %>%
  group_by(bucket,giving) %>%
  summarize(total = sum(FINLWT21)) %>%
  pivot_wider(id_cols=bucket,names_from = giving,values_from=total)
a$pct <- a$`TRUE`/(a$`FALSE`+a$`TRUE`)*100
  
#giving by state
state_charitable <- df_charitable %>%
  mutate(giving = ifelse(total>0,TRUE,FALSE)) %>% 
  group_by(STATE,giving) %>%
  summarize(total = sum(FINLWT21)) %>%
  pivot_wider(id_cols=STATE,names_from=giving,values_from=total)
state_charitable$pct <- state_charitable$`TRUE`/(state_charitable$`TRUE`+state_charitable$`FALSE`)*100

#giving percentage by state
state_charitable <- df_charitable %>%
  filter(!is.na(pct_of_income)&is.finite(pct_of_income)&pct_of_income>=0&pct_of_income<2) %>%
  mutate(giving = ifelse(total>0,TRUE,FALSE),
         STATE = as.integer(STATE)) %>% 
  group_by(STATE) %>%
  summarize(avg.pct = weighted.mean(pct_of_income,FINLWT21)*100)
state_charitable <- left_join(state_fips,state_charitable,by="STATE") %>%
  select(c("s_names","avg.pct"))

#average charitable giving for households making more than $200k
a <- df_charitable %>%
  filter(FINCBTXM>=200000)
weighted.mean(a$pct_of_income,a$FINLWT21)

#hypothesis - is there a significant difference between average giving percentage of metropolitan and non-metropolitan areas?
samp_a <- df_charitable %>% filter(SMSASTAT==1) %>% filter(is.finite(pct_of_income))
samp_b <- df_charitable %>% filter(SMSASTAT==2) %>% filter(is.finite(pct_of_income))
t.test(samp_a$pct_of_income,samp_b$pct_of_income,var.equal = FALSE)
#no, no significant difference

#non-church giving percentages by state
state_charitable <- df_charitable %>%
  filter(pct_educ_of_income>=0&pct_educ_of_income<2) %>%
  filter(pct_other_of_income>=0&pct_other_of_income<2) %>%
  mutate(pct_non_church = pct_educ_of_income+pct_other_of_income,
         STATE = as.integer(STATE)) %>% 
  group_by(STATE) %>%
  summarize(avg.pct = weighted.mean(pct_non_church,FINLWT21)*100)
state_charitable <- left_join(state_fips,state_charitable,by="STATE") %>%
  select(c("s_names","avg.pct"))

#non church giving percentage
a <- df_charitable %>%
  filter(pct_educ_of_income>=0&pct_educ_of_income<2) %>%
  filter(pct_other_of_income>=0&pct_other_of_income<2) %>%
  mutate(pct_non_church = pct_educ_of_income+pct_other_of_income,
         STATE = as.integer(STATE))
weighted.mean(a$pct_non_church,a$FINLWT21)*100
