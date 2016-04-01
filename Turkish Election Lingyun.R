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
pl <- ggplot(df_n, aes(x=variable, y=s)) + 
  geom_bar(stat="sum") +
  xlab("party name") +
  ylab("votes") +
  ggtitle("Turkish election vote summary") +
  scale_x_discrete(breaks=c("oy_kullanan_kayitli_secmen", "akp_oy", "chp_oy", "mhp_oy"),
                   labels=c("Total Votes", "Justice and Development Party", "Republican People's Party", "Nationalist Movement Party"))
pl <- pl + theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) + theme(legend.position="none")

print(pl)

#Get the sum of votes from all the parties
total_vote <- select(ankara, 16:32)
colSums(total_vote, na.rm=T)
ankara$total_vote <- rowSums(total_vote, na.rm=T)

#Calculate akp total vote share
ankara$akp_total_vote_share <- (ankara$akp_oy/ankara$total_vote)

#Add akp and chp number of votes
ankara$akp_plus_chp=(ankara$akp_oy+ankara$chp_oy)

#Substract chp from akp votes
ankara$akp_minus_chp=(ankara$akp_oy-ankara$chp_oy)

#Calculate the AKP vote share
ankara$akp_vote_share=(ankara$akp_oy/ankara$akp_plus_chp)
na.omit(ankara$akp_vote_share)

#Divide valid votes and registered votes gives the turnout rate
ankara <- mutate(ankara, turn_out_rates = gecerli_oy/kayitli_secmen)
ankara <- mutate(ankara, turn_out_rates2 = kullanilan_toplam_oy/kayitli_secmen)

#Divide invalid votes and actual votes gives the invalid ballot share
ankara$ballot=(ankara$gecersiz_oy/ankara$kullanilan_toplam_oy)

#Make a scatter plot regarding akp vote share and turnout rates
library(ggplot2)
pl2 <- ggplot(ankara, aes(x=turn_out_rates, y=akp_vote_share),
              ggtitle="AKP Vote Share vs turnout")
pl2 + geom_point(size=2, colour="blue") +xlim(0.2, 1.2) +ylim(0, 1) +
  xlab("Turnout") +
  ylab("AKP Vote Share") +
  stat_smooth(color="red")

#Make the scatter plot of akp-chp vote share and invalid ballot share
par(pch=16)
plot(ankara$ballot, ankara$akp_minus_chp)
abline(lm(ankara$akp_minus_chp~ankara$ballot), col="red")

#Make the scatter plot of akp total vote share and turnout rates
pl3 <- ggplot(ankara, aes(x=turn_out_rates, y=akp_total_vote_share),
              ggtitle="AKP Total Vote Share vs turnout")
pl3 + geom_point(size=2, colour="blue") +xlim(0.2, 1.2) +ylim(0, 1) +
  xlab("Turnout") +
  ylab("AKP Total Vote Share") +
  stat_smooth(color="red")

#Make the scatter plot of akp total vote share and invalid ballot share
par(pch=20)
plot(ankara$ballot, ankara$akp_total_vote_share)
abline(lm(ankara$akp_total_vote_share~ankara$ballot), col="red")

library(dplyr)
ankara_complete <- ankara %>%
  select(akp_total_vote_share, turn_out_rates) %>%
  na.omit

#Bootstrap to estimate the significance level of the fit line 
set.seed(123)
library(boot)
boot.fn=function(data, index)
return(coef(lm(akp_total_vote_share~turn_out_rates, data=data, subset=index)))
boot(ankara_complete, boot.fn, 1000)

#Difference between bootstrapping for different turn out rates, also omitted NA values

boot(filter(ankara_complete, turn_out_rates <= 0.96), boot.fn, 1000)

boot(filter(ankara_complete, turn_out_rates > 0.96), boot.fn, 1000)

boot(filter(ankara_complete, turn_out_rates <= 1.0), boot.fn, 1000)

boot(filter(ankara_complete, turn_out_rates > 1.0), boot.fn, 1000)

#The linear regression shows statistical significance in the 0.95 case, but not in 1.0 case

#Correlation between two variables and signifance of the coefficient
#Positive/Negative relationship, quantifying the confidence interval

cor(ankara_complete$akp_total_vote_share, method = "pearson", ankara_complete$turn_out_rates)

#Correlation with bootstrap
library(boot)
bootTau <- function(ankara_complete, i)
  cor(ankara_complete$akp_total_vote_share[i], ankara_complete$turn_out_rates[i], method="pearson")

boot_pearson<-boot(filter(ankara_complete, turn_out_rates > 1.0), bootTau, 2)
boot_pearson

bootTau <- function(ankara_complete, i)
  cor(ankara_complete$akp_total_vote_share[i], ankara_complete$turn_out_rates[i], method="pearson")

boot_pearson<-boot(filter(ankara_complete, turn_out_rates <= 1.0), bootTau, 2)
boot_pearson

###############
pl3 <- ggplot(ankara, aes(x=turn_out_rates, y=akp_total_vote_share),
              ggtitle="AKP Total Vote Share vs turnout")
pl3 + geom_point(size=2, colour="blue") +xlim(0.5, 1.2) +ylim(0, 1) +
  xlab("Turnout") +
  ylab("AKP Total Vote Share") +
  stat_smooth(data=filter(ankara), method = "lm", aes(color = turn_out_rates >1))