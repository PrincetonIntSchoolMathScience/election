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

#Make the scatter plot of akp-chp vote share and invalid ballot share
par(pch=16)
plot(ankara$ballot, ankara$akp_minus_chp)
abline(lm(ankara$akp_minus_chp~ankara$ballot), col="red")

library(dplyr)
ankara_complete <- ankara %>%
  select(akp_total_vote_share, turn_out_rates) %>%
  na.omit(ankara_complete)

#Make the scatter plot of akp total vote share and turnout rates
library(ggplot2)
pl3 <- ggplot(ankara, aes(x=turn_out_rates, y=akp_total_vote_share),
              ggtitle="AKP Total Vote Share vs turnout")
endpoint1 <- data.frame(x1 = 0.2889, x2 = 0.96 , y1 = 0.63482135561, y2 = 0.35475368)
endpoint2 <- data.frame(x3 = 0.96, x4 = 1.2, y3 = 0.495029788 , y4 = 0.85494815)

pl3 <- pl3 + geom_point(size=2, colour="blue") +xlim(0.2, 1.2) +ylim(0, 1) +
  xlab("Turnout") +
  ylab("AKP Total Vote Share") +
  #stat_smooth(color="red", data = subset(ankara, turn_out_rates < 0.96)) +
  #stat_smooth(color="green", data = subset(ankara, turn_out_rates >= 0.96)) +
  geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2), data = endpoint1, color="yellow") +
  geom_segment(aes(x=x3, y=y3, xend=x4, yend=y4), data = endpoint2, color="purple") 

pl3

#Plotting the confidence interval of the correlations for turn out rates > 0.96
library(dplyr)
ankara_complete <- ankara %>%
  select(akp_total_vote_share, turn_out_rates) %>%
  na.omit(ankara_complete)

library(simpleboot)
sq <- seq(0.50, 1.0, by = 0.01)

t_res <- data.frame(p = c(), index = c())

for(j in sq){
  ankara_1_j <- filter(ankara_complete, turn_out_rates > j)
  print(j)
  attach(ankara_1_j)
  lmodel_j<-lm(akp_total_vote_share~turn_out_rates)
  lboot_j<-lm.boot(lmodel_j,R=200)
  
  all_fits_j<-lboot_j$boot.list
  all_coef_j<-sapply(all_fits_j, function(x) x["coef"])
  
  intercepts_j<-sapply(all_coef_j, function(x) x["(Intercept)"])
  slopes_j<-sapply(all_coef_j, function(x) x["turn_out_rates"])
  
  #Plotting the confidence interval of the correlations for turn out rates <= j
  ankara_2_j <- filter(ankara_complete, turn_out_rates <= j)
  attach(ankara_2_j)
  lmodel2_j<-lm(akp_total_vote_share~turn_out_rates)
  lboot2_j<-lm.boot(lmodel2_j,R=200)
  summary(lboot2_j)
  
  all_fits2_j<-lboot2_j$boot.list
  all_coef2_j<-sapply(all_fits2_j, function(x) x["coef"])
  
  intercepts2_j<-sapply(all_coef2_j, function(x) x["(Intercept)"])
  
  slopes2_j<-sapply(all_coef2_j, function(x) x["turn_out_rates"])
  
  #Non-parametric t-test
  p_value <- wilcox.test(slopes_j, slopes2_j)$p.value
  
  t_res <- rbind(t_res, data.frame(p_value, index = j))
  
  #write.table(j, file = "out.csv", append = T,  quote = F, col.names = F)
}

t_res

plot(t_res)

ankara_complete %>% filter(turn_out_rates < 0.74) %>% summarize(n())
#From the result, when turnout rates is less than 0.73 when less than 1%
#of the data is on the left of the division point, the p-value is significantly larger

library(boot)
library(simpleboot)
ankara_1 <- filter(ankara_complete, turn_out_rates > 0.7)
attach(ankara_1)
lmodel<-lm(akp_total_vote_share~turn_out_rates)
lboot<-lm.boot(lmodel,R=200)
summary(lboot)

all_fits<-lboot$boot.list
all_coef<-sapply(all_fits, function(x) x["coef"])

intercepts<-sapply(all_coef, function(x) x["(Intercept)"])
slopes<-sapply(all_coef, function(x) x["turn_out_rates"])

#Plotting the confidence interval of the correlations for turn out rates <= 0.96
ankara_2 <- filter(ankara_complete, turn_out_rates <= 0.7)
attach(ankara_2)
lmodel2<-lm(akp_total_vote_share~turn_out_rates)
lboot2<-lm.boot(lmodel2,R=200)
summary(lboot2)

all_fits2<-lboot2$boot.list
all_coef2<-sapply(all_fits2, function(x) x["coef"])

intercepts2<-sapply(all_coef2, function(x) x["(Intercept)"])

slopes2<-sapply(all_coef2, function(x) x["turn_out_rates"])

#Plotting the whole scatter plot separated by the threshold value. 

x1_s <- 0.7
x1_e <- 1.20
y1_s <- intercepts + x1_s*slopes
y1_e <- intercepts + x1_e*slopes
names(y1_s) <- NULL
names(y1_e) <- NULL

x2_s <- 0.3
x2_e <- 0.7
y2_s <- intercepts2 + x2_s*slopes2
y2_e <- intercepts2 + x2_e*slopes2
names(y2_s) <- NULL
names(y2_e) <- NULL

pl6 <- ggplot(aes(x=turn_out_rates, y=akp_total_vote_share), data=ankara_complete) + 
  geom_point(size=2, colour="blue") +
  geom_vline(xintercept=0.7, color="red")
for(abc in 1:20)
  pl6 <- pl6 + geom_segment(x=x1_s, y=y1_s[abc], xend=x1_e, yend=y1_e[abc], color="purple", alpha=1/20) + 
         geom_segment(x=x2_s, y=y2_s[abc], xend=x2_e, yend=y2_e[abc], color="yellow", alpha=1/20)
pl6

#Non-parametric t-test
wilcox.test(slopes, slopes2)

#Make the scatter plot of akp total vote share and invalid ballot share
par(pch=20)
plot(ankara$ballot, ankara$akp_total_vote_share)
abline(lm(ankara$akp_total_vote_share~ankara$ballot), col="red")

## New Work

- Another graph about the p-value >= 0.73

```{r, echo = F, cache=TRUE, message=FALSE, warning=FALSE}
library(simpleboot)
sq <- seq(0.73, 1.0, by = 0.01)

t_res <- data.frame(p = c(), index = c())

for(j in sq){
  ankara_1_j <- filter(ankara_complete, turn_out_rates > j)
  attach(ankara_1_j)
  lmodel_j<-lm(akp_total_vote_share~turn_out_rates)
  lboot_j<-lm.boot(lmodel_j,R=200)
  
  all_fits_j<-lboot_j$boot.list
  all_coef_j<-sapply(all_fits_j, function(x) x["coef"])
  
  intercepts_j<-sapply(all_coef_j, function(x) x["(Intercept)"])
  slopes_j<-sapply(all_coef_j, function(x) x["turn_out_rates"])
  
  #Plotting the confidence interval of the correlations for turn out rates <= j
  ankara_2_j <- filter(ankara_complete, turn_out_rates <= j)
  attach(ankara_2_j)
  lmodel2_j<-lm(akp_total_vote_share~turn_out_rates)
  lboot2_j<-lm.boot(lmodel2_j,R=200)
  
  all_fits2_j<-lboot2_j$boot.list
  all_coef2_j<-sapply(all_fits2_j, function(x) x["coef"])
  
  intercepts2_j<-sapply(all_coef2_j, function(x) x["(Intercept)"])
  
  slopes2_j<-sapply(all_coef2_j, function(x) x["turn_out_rates"])
  
  #Non-parametric t-test
  p_value <- wilcox.test(slopes_j, slopes2_j)$p.value
  
  t_res <- rbind(t_res, data.frame(p_value, index = j))
}
plot(t_res$index, t_res$p_value)
```

## New Work
```{r echo=FALSE, message=FALSE, cache=TRUE, warning=FALSE}
library(boot)
library(simpleboot)
ankara_1 <- filter(ankara_complete, turn_out_rates > 0.88)
attach(ankara_1)
lmodel<-lm(akp_total_vote_share~turn_out_rates)
lboot<-lm.boot(lmodel,R=200)

all_fits<-lboot$boot.list
all_coef<-sapply(all_fits, function(x) x["coef"])

intercepts<-sapply(all_coef, function(x) x["(Intercept)"])
slopes<-sapply(all_coef, function(x) x["turn_out_rates"])

#Plotting the confidence interval of the correlations for turn out rates <= 0.96
ankara_2 <- filter(ankara_complete, turn_out_rates <= 0.88)
attach(ankara_2)
lmodel2<-lm(akp_total_vote_share~turn_out_rates)
lboot2<-lm.boot(lmodel2,R=200)

all_fits2<-lboot2$boot.list
all_coef2<-sapply(all_fits2, function(x) x["coef"])

intercepts2<-sapply(all_coef2, function(x) x["(Intercept)"])

slopes2<-sapply(all_coef2, function(x) x["turn_out_rates"])
x1_s <- 0.88
x1_e <- 1.20
y1_s <- intercepts + x1_s*slopes
y1_e <- intercepts + x1_e*slopes
names(y1_s) <- NULL
names(y1_e) <- NULL

x2_s <- 0.3
x2_e <- 0.88
y2_s <- intercepts2 + x2_s*slopes2
y2_e <- intercepts2 + x2_e*slopes2
names(y2_s) <- NULL
names(y2_e) <- NULL

pl6 <- ggplot(aes(x=turn_out_rates, y=akp_total_vote_share), data=ankara_complete) + 
  geom_point(size=2, colour="blue") +
  geom_vline(xintercept=0.88, color="red")
for(abc in 1:20)
  pl6 <- pl6 + geom_segment(x=x1_s, y=y1_s[abc], xend=x1_e, yend=y1_e[abc], color="purple", alpha=1/20) + 
  geom_segment(x=x2_s, y=y2_s[abc], xend=x2_e, yend=y2_e[abc], color="yellow", alpha=1/20)
pl6
```

#Results: 

#Visualization of the correlation & the confidence intervals

#Correlation between two variables and significance of the coefficients
#Positive/Negative relationship, quantifying the confidence interval

#Comparing correlations
zdifference <- function(z_r1, z_r2, N1, N2){
  z_difference <- (z_r1-z_r2)/(sqrt(1/(N1-3))+sqrt(1/(N2-3)))
  return(z_difference)}

zdifference(0.2649014, -0.075, 81 ,12149)

#z=2.157187, statistically significant