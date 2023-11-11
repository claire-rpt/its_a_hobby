library(tidyverse)
library(ipumsr)
library(reldist)
library(stats)

ipums_file <- read_ipums_micro("C:/Users/theto/Documents/CPS_SPM/CPS_Childcare/cps_00021ddi.xml",vars=NULL,n_max=Inf,data_file=NULL,verbose=TRUE,var_attrs=c("val_labels","var_label","var_desc"),lower_vars=FALSE)

YEAR <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)
Inflation_adj <- c(1.402,1.359,1.331,1.312,1.291,1.29,1.274,1.247,1.217,1.196,1.181,1.128,1.045)
c <- data.frame(YEAR,Inflation_adj)

ipums_file <- left_join(ipums_file,c,by="YEAR")
rm(c)
rm(YEAR)
rm(Inflation_adj)

data_2022 <- ipums_file %>% filter(YEAR==2022) %>%
  mutate(PPL = FTOTVAL/(13590+4720*(FAMSIZE-1))) %>%
  mutate(PPL_ptile = percent_rank(PPL))

childcare_costs <- data_2022 %>% filter(SPMCHXPNS>0) %>%
  mutate(pct_childcare = (SPMCHXPNS/FTOTVAL)) %>%
  mutate(over_20 = ifelse(pct_childcare>.2,TRUE,FALSE))

df <- childcare_costs %>% filter(NCHLT5>0) %>% select(c("ASECWTH","PPL","PPL_ptile","pct_childcare")) %>% filter(PPL>0) %>%
  mutate(ptile_grp = round(PPL_ptile,2)) %>%
  group_by(ptile_grp) %>%
  summarize(wtd.median = wtd.quantile(pct_childcare,q=.5,na.rm=TRUE,weight=ASECWTH)) %>%
  mutate(wtd.median = round(wtd.median,3)*100)
write.csv(df,"df.csv")

df <- data_2022 %>% filter(NCHLT5>0&FTOTVAL>0) %>%
  mutate(pct_childcare = (SPMCHXPNS/FTOTVAL),
         income_quint = cut(.$PPL_ptile,breaks=c(0,.2,.4,.6,.8,1),right=TRUE,labels=c("First quintile","Second quintile","Third quintile","Fourth quintile","Fifth quintile")))
df$pct_childcare[is.na(df$pct_childcare)] <- 0 
df <- df %>%
  mutate(childcare_grp = cut(.$pct_childcare,breaks=c(-1,0,.05,.1,.15,.2,9999999999),labels=c("None","Less than 5%","5.01%-10%","10.01%-15%","15.01%-20%","Greater than 20%"),right=TRUE)) %>%
  group_by(income_quint,childcare_grp) %>%
  summarize(wtd.total = sum(SPMWT,na.rm=TRUE))
df <- pivot_wider(df,id_cols=income_quint,names_from = childcare_grp,values_from = wtd.total)
df[is.na(df)] <- 0 
df[2:7] <- round(df[2:7]/rowSums(df[2:7]),3)*100
write.csv(df,"df.csv",row.names=FALSE)