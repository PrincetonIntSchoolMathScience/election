ankara <- read.csv("ankara.csv")
head(ankara)
summary(ankara)
names(ankara)

library(dplyr)
df <- select(ankara, 9, 12, 15, 21, 29, 31)

library(reshape2)
df_l <- melt(df, id=c("itirazsiz_gecerli_oy", "gecersiz_oy"))
head(df_l)

df_n <- group_by(df_l, variable) %>% 
        summarize(s = sum(value))

library(ggplot2)
ggplot(df_n, aes(x=variable, y=s)) + geom_bar(stat="sum")


