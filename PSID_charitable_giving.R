library(tidyverse)
library(stats)
library(reldist)
library(readxl)

data <- read_excel("PISD_giving.xlsx")

data_2017 <- data %>% filter(!is.na(ER66001))
data_2017 <- data_2017[,colSums(is.na(data_2017))<nrow(data_2017)]
data_2019 <- data %>% filter(!is.na(ER72001))
data_2019 <- data_2019[,colSums(is.na(data_2019))<nrow(data_2019)]
data_2021 <- data %>% filter(!is.na(ER78001))
data_2021 <- data_2021[,colSums(is.na(data_2021))<nrow(data_2021)]

data_all <- data %>% filter(!is.na(ER66001)) %>% filter(!is.na(ER72001)) %>% filter(!is.na(ER78001))
data_all <- data_all[,colSums(is.na(data_all))<nrow(data_all)]

#2017 data
data_2017 <- data_2017 %>%
  mutate(Relig_Identity = ifelse(ER70941==0,"None",ifelse(ER70941==99,NA,"Religious")),
         Any_Giving = ifelse(ER71040==1,TRUE,ifelse(ER71040==5,FALSE,NA)),
         Relig_Giving = ifelse(ER71040==5,FALSE,ifelse(ER71041==5,FALSE,ifelse(ER71041==1,TRUE,NA)))) %>%
  mutate(ER71041a = ifelse(ER71040==5,5,ER71041),
         ER71043a = ifelse(ER71040==5,5,ER71043),
         ER71045a = ifelse(ER71040==5,5,ER71045),
         ER71047a = ifelse(ER71040==5,5,ER71047),
         ER71049a = ifelse(ER71040==5,5,ER71049),
         ER71051a = ifelse(ER71040==5,5,ER71051),
         ER71053a = ifelse(ER71040==5,5,ER71053),
         ER71055a = ifelse(ER71040==5,5,ER71055),
         ER71057a = ifelse(ER71040==5,5,ER71057),
         ER71059a = ifelse(ER71040==5,5,ER71059),
         ER71061a = ifelse(ER71040==5,5,ER71061)) %>%
  mutate(Non_Relig_Giving = ifelse(ER71043a==1|ER71045a==1|ER71047a==1|ER71049a==1|ER71051a==1|ER71053a==1|ER71055a==1|ER71057a==1|ER71059a==1|ER71061a==1,"Reported","Not_reported"))
data_2017$ER71042[data_2017$ER71042==0] <- NA
data_2017$ER71042[data_2017$ER71042>999998] <- NA
data_2017$ER71044[data_2017$ER71044==0] <- NA
data_2017$ER71044[data_2017$ER71044>999998] <- NA
data_2017$ER71046[data_2017$ER71046==0] <- NA
data_2017$ER71046[data_2017$ER71046>999998] <- NA
data_2017$ER71048[data_2017$ER71048==0] <- NA
data_2017$ER71048[data_2017$ER71048>999998] <- NA
data_2017$ER71050[data_2017$ER71050==0] <- NA
data_2017$ER71050[data_2017$ER71050>999998] <- NA
data_2017$ER71052[data_2017$ER71052==0] <- NA
data_2017$ER71052[data_2017$ER71052>999998] <- NA
data_2017$ER71054[data_2017$ER71054==0] <- NA
data_2017$ER71054[data_2017$ER71054>999998] <- NA
data_2017$ER71056[data_2017$ER71056==0] <- NA
data_2017$ER71056[data_2017$ER71056>999998] <- NA
data_2017$ER71058[data_2017$ER71058==0] <- NA
data_2017$ER71058[data_2017$ER71058>999998] <- NA
data_2017$ER71060[data_2017$ER71060==0] <- NA
data_2017$ER71060[data_2017$ER71060>999998] <- NA
data_2017$ER71063[data_2017$ER71063==0] <- NA
data_2017$ER71063[data_2017$ER71063>999998] <- NA

data_2017$total_giving <- ifelse(data_2017$ER71040==5,0,rowSums(data_2017[,c("ER71042","ER71044","ER71046","ER71048","ER71050","ER71052","ER71054","ER71056","ER71058","ER71060","ER71063")],na.rm=TRUE))
data_2017$non_relig_giving <- ifelse(data_2017$ER71040==5,0,rowSums(data_2017[,c("ER71044","ER71046","ER71048","ER71050","ER71052","ER71054","ER71056","ER71058","ER71060","ER71063")],na.rm=TRUE))
data_2017 <- data_2017 %>%
  mutate(giving_pct = round(ifelse(ER71426==0,NA,total_giving/ER71426)*100,1),
         non_relig_pct = round(ifelse(ER71426==0,NA,non_relig_giving/ER71426)*100,1))

df <- data_2017 %>%
  filter(giving_pct>=0&giving_pct<200)
mean(df$giving_pct)
mean(df$non_relig_pct)
df %>% group_by(Relig_Identity) %>%
  summarize(mean(giving_pct),mean(non_relig_pct))