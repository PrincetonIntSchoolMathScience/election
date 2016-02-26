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
pl + theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) + theme(legend.position="none")

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

#Linear Regression between AKP-CHP and Turnout Rate
fit <- lm(ankara$akp_vote_share ~ ankara$ballot)
summary(fit)
abline(fit)
confint(fit)
fit

library(boot)
# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 

library(dplyr)
ankara_complete <- ankara %>%
  select(akp_vote_share, ballot) %>%
  na.omit

#Bootstrap to estimate the significance level of fit line 
boot.fn=function(data, index)
return(coef(lm(akp_vote_share~ballot, data=data, subset=index)))
boot(ankara_complete, boot.fn, 1000)