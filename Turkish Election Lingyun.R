#Read and summarize the data 
ankara <- read.csv("ankara.csv")
head(ankara)
summary(ankara)
names(ankara)

names(ankara)[1] <- "County"
names(ankara)[2] <- "District"
names(ankara)[3] <- "Box Number"
names(ankara)[4] <- "Voting Station"
names(ankara)[8] <- "Registered Votes"
names(ankara)[9] <- "Registered Voters Voting"
names(ankara)[10] <- "Using the law should vote"
names(ankara)[11] <- "Used total votes"
names(ankara)[12] <- "Unargued Valid Votes"
names(ankara)[13] <- "Argued Valid Votes"
names(ankara)[14] <- "Valid Votes"
names(ankara)[15] <- "Invalid Votes"

#Select and reshape the dataset
library(dplyr)
df <- select(ankara, 9, 12, 15, 21, 29, 31)

library(reshape2)
df_l <- melt(df, id=c("Argued Valid Votes", "Invalid Votes"))
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

#Calculate the AKP vote share
ankara$akp_vote_share=(ankara$akp_oy/ankara$akp_plus_chp)

library(dplyr)

#Divide valid votes and registered votes gives the turnout rate
ankara <- mutate(ankara, turn_out_rates = gecerli_oy/kayitli_secmen)
ankara <- mutate(ankara, turn_out_rates2 = kullanilan_toplam_oy/kayitli_secmen)

#Make a scatter plot regarding akp-chp and turnout rates
pl2 <- ggplot(ankara, aes(x=turn_out_rates, y=akp_vote_share),
              ggtitle="AKP Vote Share vs turnout")
pl2 + geom_point(size=2, colour="blue") +xlim(0.2, 1.2) +ylim(0, 1) +
                 xlab("Turnout") +
                 ylab("AKP Vote Share") +
  stat_smooth(color="red")

#Linear regression: AKP-CHP vs. invalid vote shares
