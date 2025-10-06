install.packages("dplyr")
install.packages(c("haven", "readr", "pacman"))
install.packages("tidyr")
install.packages("readstata13")
install.packages("lm.beta")
install.packages("magrittr")
library(magrittr)
library(lm.beta)
library(tidyr)
library(pacman)
library(dplyr)
library(haven)
library(readr)
library(readstata13)
setwd("/Users/Musa/Downloads/")
wage.data <- read.dta13("card_krueger_1994.dta")
wage1992NJ.data <- wage.data[ which(wage.data$year == 1992
                                  & wage.data$state == 1), ]
wage1993NJ.data <- wage.data[ which(wage.data$year == 1993
                                    & wage.data$state == 1), ]
wage1992PN.data <- wage.data[ which(wage.data$year == 1992
                                    & wage.data$state == 0), ]
wage1993PN.data <- wage.data[ which(wage.data$year == 1993
                                    & wage.data$state == 0), ]
wage1992.data <- wage.data[ which(wage.data$year == 1992), ]
wage1993.data <- wage.data[ which(wage.data$year == 1993), ]
wageNJ.data <- wage.data[ which(wage.data$state == 1), ]
wagePN.data <- wage.data[ which(wage.data$state == 0), ]
mean(wage1992NJ.data$fte, na.rm=T)
mean(wage1993NJ.data$fte, na.rm=T)
mean(wage1992PN.data$fte, na.rm=T)
mean(wage1993PN.data$fte, na.rm=T)
mean(wage1992.data$fte, na.rm=T)
mean(wage1993.data$fte, na.rm=T)
mean(wageNJ.data$fte, na.rm=T)
mean(wagePN.data$fte, na.rm=T)
(21.08558 - 20.75668) - (22.17411 - 23.67273)
wage2.data <- wage.data %<>% mutate(., year = year - 1992)
estimation <- lm(formula= fte ~ year + state + I(year*state),
                 data= wage2.data)
summary(estimation)
estimation.empft <- lm(formula= empft ~ year + state + I(year*state),
                 data= wage2.data)
summary(estimation.empft)
mean(wage1992NJ.data$empft, na.rm=T)
mean(wage1993NJ.data$empft, na.rm=T)
8.458015 - 7.838298
t.test(wage1992NJ.data$empft, wage1993NJ.data$empft)
