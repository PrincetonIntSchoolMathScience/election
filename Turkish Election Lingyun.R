#Read and summarize the data 
ankara <- read.csv("ankara.csv")
head(ankara)
summary(ankara)
names(ankara)

#Select and reshape the dataset
library(dplyr)
df <- select(ankara, 9, 12, 15, 21, 29, 31)

library(reshape2)
df_l <- melt(df, id=c("itirazsiz_gecerli_oy", "gecersiz_oy"))
head(df_l)

df_n <- group_by(df_l, variable) %>% 
        summarize(s = sum(value))

#Make the bar plot that represents the votes from major parties
library(ggplot2)
ggplot(df_n, aes(x=variable, y=s)) + 
  geom_bar(stat="sum") +
  xlab("party name") +
  ylab("votes") +
  ggtitle("Turkish election vote summary")

#Subtract akp and chp number of votes
ankara$akp_chp=(ankara$akp_oy-ankara$chp_oy)

ankara$turn_out_rates=(ankara$itirazsiz_gecerli_oy/ankara$oy_kullanan_kayitli_secmen)
ankara$turn_out_rates
