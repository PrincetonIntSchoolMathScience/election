ankara <- read.csv("ankara.csv")
head(ankara)
summary(ankara)
names(ankara)

library(dplyr)
df <- select(ankara, 9, 12, 15, 21, 29, 31)

library(reshape2)
df_l <- melt(df, id=c("oy_kullanan_kayitli_secmen", "itirazsiz_gecerli_oy", "gecersiz_oy"))


df_n <- group_by(df_l, variable) %>% summarize(s = sum(value))
barplot(df_n$s, by=variable)

