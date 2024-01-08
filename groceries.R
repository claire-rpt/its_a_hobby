library(tidyverse)
library(haven)
library(stats)
library(reldist)

state_fips <- read.csv("state_fips.csv")

#load UCC data
q1 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw21/intrvw21/mtbi221.sas7bdat")
q2 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw22/intrvw22/mtbi222.sas7bdat")
q3 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw22/intrvw22/mtbi223.sas7bdat")
q4 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw22/intrvw22/mtbi224.sas7bdat")
q5 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw22/intrvw22/mtbi231.sas7bdat")

ucc_data <- rbind(q1,q2,q3,q4,q5) %>% filter(REF_YR==2022)
rm(q1,q2,q3,q4,q5)

#load family data for 4 quarters of 2019 FMLI interview files

q1 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw21/intrvw21/fmli221.sas7bdat")
q2 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw22/intrvw22/fmli222.sas7bdat")
q3 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw22/intrvw22/fmli223.sas7bdat")
q4 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw22/intrvw22/fmli224.sas7bdat")
q5 <- read_sas("C:/Users/theto/Documents/Consumer_Expenditure/intrvw22/intrvw22/fmli231.sas7bdat")

#change all column names to uppercase
names(q1) <- toupper(names(q1))
names(q2) <- toupper(names(q2))
names(q3) <- toupper(names(q3))
names(q4) <- toupper(names(q4))
names(q5) <- toupper(names(q5))

#combine quarterly data
data_cu <- rbind(q1,q2,q3,q4)
rm(q1,q2,q3,q4,q5)

data_cu_select <- data_cu %>%
  select(c("NEWID","INC_RANK","STATE","SMSASTAT","FINLWT21","FINCBTXM","FAM_SIZE","CHILDAGE","FAM_TYPE","INC_HRS1","INC_HRS2","AGE2")) %>%
  mutate(FAM_SIZE_grp = ifelse(FAM_SIZE>7,7,FAM_SIZE)) %>%
  mutate(INC_quint = ifelse(INC_RANK<.2,"q1",ifelse(INC_RANK<.4,"q2",ifelse(INC_RANK<.6,"q3",ifelse(INC_RANK<.8,"q4","q5"))))) %>%
  mutate(PPL = FINCBTXM/(13590+4720*(FAM_SIZE-1))) %>%
  mutate(PPL_rank = percent_rank(PPL)) %>%
  mutate(PPL_quint = cut(.$PPL_rank,breaks=c(0,.2,.4,.6,.8,1.01),right=FALSE,labels=c("q1","q2","q3","q4","q5")))

#UCC 190904 - groceries while traveling
groc_travel <- ucc_data %>%
  filter(UCC==190904) %>%
  group_by(NEWID,REF_MO) %>%
  summarize(groc_travel = sum(COST)) %>%
  mutate(ID = paste(NEWID,REF_MO,sep=""))

#UCC 790240 - food & non-alcoholic beverage expense
groc_home <- ucc_data %>%
  filter(UCC==790240) %>%
  group_by(NEWID,REF_MO) %>%
  summarize(groc_home = sum(COST))%>%
  mutate(ID = paste(NEWID,REF_MO,sep=""))

#UCC 190901 - food away from home, other
food_away_from_home <- ucc_data %>%
  filter(UCC==190901|UCC==190902|UCC==190903|UCC==790410|UCC==790430) %>%
  group_by(NEWID,REF_MO) %>%
  summarize(fah = sum(COST,na.rm=TRUE))%>%
  mutate(ID = paste(NEWID,REF_MO,sep=""))

#groceries
groceries <- merge(groc_travel,groc_home,by="ID",all=TRUE)
groceries <- groceries %>% mutate(NEWID = ifelse(is.na(NEWID.x),NEWID.y,NEWID.x),REF_MO = ifelse(is.na(REF_MO.x),REF_MO.y,REF_MO.x)) %>%
  select(-c("NEWID.x","NEWID.y","REF_MO.x","REF_MO.y"))

food <- merge(groceries,food_away_from_home,by="ID",all=TRUE)
food <- food %>% mutate(NEWID = ifelse(is.na(NEWID.x),NEWID.y,NEWID.x),REF_MO = ifelse(is.na(REF_MO.x),REF_MO.y,REF_MO.x)) %>%
  select(-c("NEWID.x","NEWID.y","REF_MO.x","REF_MO.y"))
food$fah[is.na(food$fah)] <- 0
food$groc_travel[is.na(food$groc_travel)] <- 0
food$groc_home[is.na(food$groc_home)] <- 0

df_food <- left_join(data_cu_select,food,by="NEWID")
df_food <- df_food %>% filter(!is.na(REF_MO)) %>% filter(!is.na(FINLWT21))
wtd.quantile(df_food$groc_home,q=.5,na.rm=TRUE,weight=df_food$FINLWT21)
wtd.mean(df_food$groc_home,weight=df_food$FINLWT21)
df_food %>% group_by(FAM_SIZE_grp) %>% summarize(wtd.quantile(groc_home,q=.5,na.rm=TRUE,weight=FINLWT21),wtd.mean(groc_home,weight=FINLWT21))
df_food %>% group_by(FAM_SIZE_grp) %>% summarize(wtd.quantile(groc_travel,q=.5,na.rm=TRUE,weight=FINLWT21),wtd.mean(groc_home,weight=FINLWT21))
df_food %>% group_by(FAM_SIZE_grp) %>% summarize(wtd.quantile(fah,q=.5,na.rm=TRUE,weight=FINLWT21),wtd.mean(groc_home,weight=FINLWT21))
df_food$pct_groceries <- df_food$groc_home*12/df_food$FINCBTXM
wtd.quantile(df_food$pct_groceries,q=.5,na.rm=TRUE,weight=df_food$FINLWT21)
df_food %>% group_by(INC_quint) %>% summarize(wtd.quantile(groc_home,q=.5,na.rm=TRUE,weight=FINLWT21),wtd.quantile(pct_groceries,q=.5,na.rm=TRUE,weight=FINLWT21))
df_food %>% group_by(PPL_quint) %>% summarize(wtd.quantile(groc_home,q=.5,na.rm=TRUE,weight=FINLWT21),wtd.quantile(pct_groceries,q=.5,na.rm=TRUE,weight=FINLWT21))
df_food %>% group_by(PPL_quint) %>% summarize(wtd.quantile(fah,q=.5,na.rm=TRUE,weight=FINLWT21))

#percent of food spent on groceries by average combined work week hours, presence of children
df <- df_food %>% filter(FAM_TYPE==2|FAM_TYPE==3) %>% rowwise %>% mutate(HRS_combined = sum(INC_HRS1,INC_HRS2,na.rm=TRUE),
                                                                         all_food = sum(groc_travel,groc_home,fah,na.rm=TRUE),
                                                                         SAHP = ifelse(is.na(INC_HRS1),TRUE,ifelse(is.na(INC_HRS2),TRUE,FALSE))) %>% ungroup() %>%
  mutate(HRS_rank = percent_rank(HRS_combined),
         pct_of_food_on_groc = groc_home/all_food,
         pct_of_food_on_fah = fah/all_food,
         pct_fah = fah*12/FINCBTXM
         )
df2 <- df_food %>% filter(FAM_TYPE==6|FAM_TYPE==7)
df %>% group_by(SAHP) %>% summarize(wtd.mean(fah,weight=FINLWT21))
df %>% group_by(SAHP,FAM_SIZE_grp) %>% 
  summarize(wtd.quantile(fah,q=.5,na.rm=TRUE,weight=FINLWT21),wtd.mean(fah,weight=FINLWT21))

#does t test different hold for family size
c <- df %>% filter(FAM_SIZE==3&SAHP==TRUE)
d <- df %>% filter(FAM_SIZE==3&SAHP==FALSE)
t.test(c$pct_of_food_on_fah,d$pct_of_food_on_fah)

c <- df %>% filter(FAM_SIZE==4&SAHP==TRUE)
d <- df %>% filter(FAM_SIZE==4&SAHP==FALSE)
t.test(c$pct_of_food_on_fah,d$pct_of_food_on_fah)

c <- df %>% filter(FAM_SIZE==5&SAHP==TRUE)
d <- df %>% filter(FAM_SIZE==5&SAHP==FALSE)
t.test(c$pct_of_food_on_fah,d$pct_of_food_on_fah)

c <- df %>% filter(FAM_SIZE==6&SAHP==TRUE)
d <- df %>% filter(FAM_SIZE==6&SAHP==FALSE)
t.test(c$pct_of_food_on_fah,d$pct_of_food_on_fah)

c <- df %>% filter(FAM_SIZE==7&SAHP==TRUE)
d <- df %>% filter(FAM_SIZE==7&SAHP==FALSE)
t.test(c$pct_of_food_on_fah,d$pct_of_food_on_fah)

c <- df %>% filter(SAHP==TRUE)
d <- df %>% filter(SAHP==FALSE)
t.test(c$pct_fah,d$pct_fah)
t.test(c$FINCBTXM,d$FINCBTXM)
cor(df$FINCBTXM,df$HRS_combined)
