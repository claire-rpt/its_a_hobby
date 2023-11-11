library(tidyverse)
library(haven)
library(stats)
library(reldist)


#read in 2019 data
df <- read_sas("C:/Users/theto/Documents/NEHS_ECPP/2019.sas7bdat")
df$YEAR <- 2019
df <- df %>% rename("AGE"="AGE2018")
data_2019 <- df

#select columns
select_data <- data_2019 %>% select(c("YEAR","AGE","RCNOW","RCWEEK","RCHRS","RCDAYS","NCNOW","NCWEEK","NCHRS","NCDAYS","NCALKNE","NCPLACE","CPNNOWX","CPWEEKX","CPHRS","FEWT","P1REL","P2REL","P1SEX","P2SEX","PAR1EDUC","PAR2EDUC","PAR1EMPL","PAR2EMPL","P1HRSWK","P2HRSWK","RCFEE","RCCOST","NCFEE","NCCOST","CPFEE","CPCOST","TTLHHINC","RCUNIT","NCUNIT","CPUNIT","CPDAYS","CPHEADST","CPPLACEX","PAR1TYPE","PAR2TYPE","PAR1MARST","PAR2MARST"))

select_data <- select_data %>% filter(AGE<6)

#define relative, nonrelative, and center care into binary values
select_data <- select_data %>% 
  mutate(
    Relative = ifelse(RCNOW==1&RCWEEK==1,TRUE,FALSE),
    Nonrelative = ifelse(NCNOW==1&NCWEEK==1,TRUE,FALSE),
    Center = ifelse(CPNNOWX==1&CPWEEKX==1,TRUE,FALSE)) %>% 
  mutate(ParentOnly = ifelse(Relative==FALSE&Nonrelative==FALSE&Center==FALSE,TRUE,FALSE)) #define parent care into binary value

#set zero hours in care types to zero
select_data$RCHRS <- replace(select_data$RCHRS,select_data$RCHRS<0,0)
select_data$NCHRS <- replace(select_data$NCHRS,select_data$NCHRS<0,0)
select_data$CPHRS <- replace(select_data$CPHRS,select_data$CPHRS<0,0)

select_data <- select_data %>% 
  mutate(CenterFee = ifelse(CPUNIT==1,CPCOST*CPHRS,ifelse(CPUNIT==2,CPCOST*CPDAYS,ifelse(CPUNIT==3,CPCOST,ifelse(CPUNIT==4,CPCOST/2,ifelse(CPUNIT==5,CPCOST*12/52,ifelse(CPUNIT==6,CPCOST/52,NA))))))) %>% #calculate center fee
  mutate(PublicCenter = ifelse(CPHEADST==1,TRUE,ifelse(CPPLACEX==2,TRUE,FALSE))) %>% #binary value for public center
  mutate(AgeGrp = cut(.$AGE,breaks=c(0,1,3,6,7),right=FALSE)) %>% #age groups
  mutate(RelativeFee = ifelse(RCUNIT==1,RCCOST*RCHRS,ifelse(RCUNIT==2,RCCOST*RCDAYS,ifelse(RCUNIT==3,RCCOST,ifelse(RCUNIT==4,RCCOST/2,ifelse(RCUNIT==5,RCCOST*12/52,ifelse(NCUNIT==6,RCCOST/52,NA))))))) %>% #calculate relative fee
  mutate(NonRelativeFee = ifelse(NCUNIT==1,NCCOST*NCHRS,ifelse(NCUNIT==2,NCCOST*NCDAYS,ifelse(NCUNIT==3,NCCOST,ifelse(NCUNIT==4,NCCOST/2,ifelse(NCUNIT==5,NCCOST*12/52,ifelse(NCUNIT==6,RCCOST/52,NA))))))) %>% #calculate nonrelative fee
  mutate(NonRelativeType = ifelse(Nonrelative==TRUE,ifelse(NCPLACE==1,"Nanny",ifelse(NCALKNE==1,"FFN","FCCH")),NA)) %>% #impute type of nonrelative care
  rowwise() %>%
  mutate(Total_Fees = round(sum(CenterFee,RelativeFee,NonRelativeFee,na.rm=TRUE),0)) %>% #calculate total weekly amount paid
  ungroup() %>%
  mutate(Income_Group = cut(.$TTLHHINC,breaks=c(0,6,8,9,10,99),labels=c("Under $50k","$50k-75k","75k-100k","100k-150k","150k+"),right=FALSE)) %>% #define income groups
  mutate(Nanny = ifelse(NonRelativeType=="Nanny",TRUE,FALSE))

select_data$Nanny[is.na(select_data$Nanny)] <- FALSE

#how many children have a paid childcare arrangement
select_data <- select_data %>% mutate(Has_Fee = ifelse(Total_Fees==0,"No","Yes"),Has_Care = ifelse(ParentOnly==TRUE,"No","Yes"))
df <- select_data %>% group_by(Has_Care,Has_Fee) %>% summarize(total_wt = sum(FEWT))

#how many children are in center or nanny care
df <- select_data %>% group_by(Center,Nanny) %>% summarize(total_wt = sum(FEWT))
df2 <- select_data %>% filter(CPHEADST!=1&CPPLACEX!=2) %>% group_by(Center) %>% summarize(total_wt = sum(FEWT))

#how much are parents paying for center and nanny care
df <- select_data %>% filter(Center==TRUE&CenterFee>0) %>% group_by(Center) %>% summarize(
  Total_Fee = wtd.mean(Total_Fees,weight=FEWT),
  Center_Fee = wtd.mean(CenterFee,weight=FEWT)
)

df <- select_data %>% filter(Nanny==TRUE&NonRelativeFee>0) %>% group_by(Nanny) %>% summarize(
  Total_Fee = wtd.mean(Total_Fees,weight=FEWT),
  NR_Fee = wtd.mean(NonRelativeFee,weight=FEWT)
)

#how much are parents paying for care
df <- select_data %>% filter(Total_Fees>0) %>% group_by(ParentOnly) %>% summarize(Total_Fee = wtd.mean(Total_Fees,weight=FEWT))

#how many parents are paying more than $18,000 per year in 2023 dollars
df <- select_data %>% mutate(More_Than_18k = ifelse(Total_Fees>290,TRUE,FALSE)) %>% group_by(More_Than_18k) %>%
  summarize(total = sum(FEWT),
            avg_fees = wtd.mean(Total_Fees,weight=FEWT),
            median_fees = wtd.quantile(Total_Fees,q=.5,na.rm=TRUE,weight=FEWT))

df <- select_data %>% filter(Center==TRUE|Nanny==TRUE) %>% mutate(More_Than_18k = ifelse(Total_Fees>290,TRUE,FALSE)) %>% 
  group_by(More_Than_18k) %>%
  summarize(total = sum(FEWT),
            avg_fees = wtd.mean(Total_Fees,weight=FEWT),
            median_fees = wtd.quantile(Total_Fees,q=.5,na.rm=TRUE,weight=FEWT))

#csv of weekly care costs
df <- select_data %>% mutate(type = ifelse(ParentOnly==TRUE,"None",ifelse(Center==TRUE&Nonrelative==FALSE&Relative==FALSE,"Center",ifelse(Nonrelative==TRUE&Relative==FALSE&Center==FALSE,"Nonrelative",ifelse(Relative==TRUE&Center==FALSE&Nonrelative==FALSE,"Relative","Multiple"))))) %>% select(c("Total_Fees","FEWT","type"))
write.csv(df,"df.csv")

#weekly care costs by income
df <- select_data %>% group_by(TTLHHINC) %>%
  summarize(wtd.quantile(Total_Fees,q=.5,na.rm=TRUE,weight=FEWT))

df <- select_data %>% filter(Center==TRUE|Nanny==TRUE) %>% group_by(TTLHHINC) %>%
  summarize(wtd.quantile(Total_Fees,q=.5,na.rm=TRUE,weight=FEWT))

#csv of weekly care costs by income, filtered on center and nanny
df <- select_data %>% filter(Center==TRUE|Nanny==TRUE) %>% select(c("TTLHHINC","FEWT","Total_Fees"))
write.csv(df,"df.csv")

#csv of care types
df <- select_data %>%
  mutate(type = ifelse(Center==TRUE,"Center",ifelse(Nanny==TRUE,"Nanny",ifelse(Relative==TRUE,"Relative",ifelse(ParentOnly==TRUE,"Parent only","Other"))))) %>%
  mutate(Has_Fee = ifelse(Total_Fees>0,"Fee","No fee")) %>%
  group_by(type,Has_Fee) %>%
  summarize(wtd.total = sum(FEWT,na.rm=TRUE))
a <- sum(df$wtd.total)
df$pct <- round(df$wtd.total/a,3)*100
df$wtd.total <- df$wtd.total/1000
write.csv(df,"df.csv")

#csv of care types by income
df <- select_data %>%
  mutate(type = ifelse(Center==TRUE,"Center",ifelse(Nanny==TRUE,"Nanny",ifelse(Relative==TRUE,"Relative",ifelse(ParentOnly==TRUE,"Parent only","Other"))))) %>%
  mutate(Has_Fee = ifelse(Total_Fees>0,"Fee","No fee")) %>%
  mutate(Income = cut(.$TTLHHINC,breaks=c(0,4,6,8,9,10,13),labels = c("Under $30,000","$30,001-$50,000","$50,000-$75,000","$75,001-$100,000","$100,001-$150,000","$150,001 or more"),right=FALSE)) %>%
  group_by(Income,type) %>%
  summarize(wtd.total = sum(FEWT,na.rm=TRUE))
df <- pivot_wider(df,id_cols=Income,names_from=type,values_from = wtd.total)
df[is.na(df)] <- 0
df[2:6] <- df[2:6]/rowSums(df[2:6])*100
#t(df)
#df = df[-1,]
write.csv(df,"df.csv")

#care as percent of income, category minimum
df <- select_data %>% select(c("TTLHHINC","Total_Fees","ParentOnly","FEWT","Nanny","Center"))
df2 <- data.frame(
  TTLHHINC = c(1,2,3,4,5,6,7,8,9,10,11,12),
  imputed_income = c(1,10001,20001,30001,40001,50001,60001,75001,100001,150001,200001,250001)
)
df <- left_join(df,df2,by="TTLHHINC")
df <- df %>%
  mutate(Pct_Fees = round(Total_Fees*52/imputed_income,3)) %>%
  filter(ParentOnly==FALSE&Total_Fees>0) %>%
  filter(Nanny==TRUE|Center==TRUE)
a <- wtd.quantile(df$Pct_Fees,q=.5,na.rm=TRUE,weight=df$FEWT)
df2 <- df %>% mutate(Over_20 = ifelse(Pct_Fees>.2,TRUE,FALSE)) %>%
  filter(Nanny==TRUE|Center==TRUE) %>%
  group_by(Over_20) %>%
  summarize(wtd.total = sum(FEWT,na.rm=TRUE))
df2$pct = df2$wtd.total/sum(df2$wtd.total)
