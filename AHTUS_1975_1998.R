#code to calculate average hours worked by employed mothers in 1975 and 1998 samples
#unweighted average but removed low quality responses via RECWGHT>0
library(tidyverse)
library(ipumsr)

ipums_file <- read_ipums_micro("ATUS_DDI.xml",vars=NULL,n_max=Inf,data_file=NULL,verbose=TRUE,var_attrs=c("val_labels","var_label","var_desc"),lower_vars=FALSE)
data <- ipums_file %>% mutate(YEAR = ifelse(YEAR<1998,1975,2000))

df <- data %>% filter(RECWGHT>0) %>% filter(SEX==2) %>% filter(FAMSTAT==1|FAMSTAT==2) %>% 
  mutate(WKHRS = ifelse(WKHRS==-4,10,ifelse(WKHRS==61,80,ifelse(WKHRS==81,81,WKHRS))),
         EMPSTAT = ifelse(EMPSTAT==2,1,EMPSTAT)) %>% filter(WKHRS>=0)
df2 <- df %>% group_by(YEAR,EMPSTAT) %>% summarize(avg_weekly_work = mean(WKHRS))
