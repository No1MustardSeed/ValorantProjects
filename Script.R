#importing libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(kableExtra)
library(psych)
install.packages("jtools")
library(jtools)
install.packages("stargazer")
library(stargazer)

#imported throguh clicking on file in the bottom left of the screen and pressed import
#save the working data sparely for wrangling
valstat <- valstat_Sheet1_

#describing
describe(valstat$`K/D`)
describe(valstat$`DD?/Round`)

#wrangling -need to make ranks ordinal
#recoding ranks as number
valstat$`Rank (Tier)` <- recode(valstat$`Rank (Tier)`, 'Iron'= 1, 'Bronze' = 2, 'Silver' = 3, 'Gold' = 4, 'Platnium' = 5, 'Diamond' = 6, 'Ascendant' = 7, 'Immortal' = 8, 'Radiant' = 9)

#making graphs and linear regression
KD <- valstat$`K/D`
DD <- valstat$`DD?/Round`
Ranking <- valstat$`Rank (Tier)`
plot(KD, Ranking, main = "K/D Effect on Rank", 
     xlab = "K/D", ylab = "Rank")
abline(lm(Ranking ~ KD, data = valstat), col = "red")
plot(DD, Ranking, main = "DD?/Round Effect on Rank", 
     xlab = "DD?/Round", ylab = "Rank")
abline(lm(Ranking ~ DD, data = valstat), col = "blue")

#linear regression table thing
RegressionKD <- lm(Ranking ~ KD, data = valstat)
RegressionDDRound <- lm(Ranking ~ DD, data = valstat)
stargazer(RegressionKD, RegressionDDRound, type = 'text')