library(readxl)
df <- read_excel("C:\\Users\\user\\Desktop\\LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022.xlsx", sheet = "NWS", skip = 0)
#df$`Date (GMT)` <- as.character(df$`Date (GMT)`)

#install.packages("pracma")

#kolona ema prozor 30
# 1----------------------------------------------------------------------------------------------------------------                   
library(pracma)
head(df)
df$ema <- movavg(df$Last, n=30, type='e')
print(df)


library(dplyr)
emaTrend <- function(df) {
  df <- df %>% mutate(emaTrend = 0)
  for (i in 2:length(df$ema)) {
    if (df$ema[i] > df$ema[i - 1]) {
      df$emaTrend[i] <- 1
    } else if (df$ema[i] < df$ema[i - 1]) {
      df$emaTrend[i] <- -1
    }
  }
  return(df)
}

emaTrend(df)
#print(df, n=30)
#  2-------------------------------------------------------------------------------------------------------------
# +day -day da je zbir 1, kontrolna promenljiva
df$`+day` <- -2
df$`-day`<- -3
trendBrDana <- function(df, n) {
  df$dayChange <- df$Last - df$Open
  for (i in 1:nrow(df)) {
    df$`+day`[i] <- ifelse(df$dayChange[i] > 0, 1, 0)
    df$`-day`[i] <- ifelse(df$dayChange[i] < 0, 1, 0)
  }
  df$trendBrDana <- 0
  for (i in (n):nrow(df)) {
    if (df$Last[i] > df$Last[i - n+1] && sum(df$`+day`[(i - n+1):i]) / n >= 0.66) {
      df$trendBrDana[i] <- 1
    } else if (df$Last[i] < df$Last[i - n+1] && sum(df$`-day`[(i - n+1):i]) / n >= 0.66) {
      df$trendBrDana[i] <- -1
    }
  }
  return(df)
}
#za 30 dana
n=30
trendBrDana <- function(df, n) {
  df$dayChange <- df$Last - df$Open
  for (i in 1:nrow(df)) {
    df$`+day`[i] <- ifelse(df$dayChange[i] > 0, 1, 0)
    df$`-day`[i] <- ifelse(df$dayChange[i] < 0, 1, 0)
  }
  df$trendBrDana <- 0
  for (i in (n+1):nrow(df)) {
    if (df$Last[i] > df$Last[i - n] && sum(df$`+day`[(i - n):i]) / n >= 0.66) {
      df$trendBrDana[i] <- 1
    } else if (df$Last[i] < df$Last[i - n] && sum(df$`-day`[(i - n):i]) / n >= 0.66) {
      df$trendBrDana[i] <- -1
    }
  }
  return(df)
}
trendBrDana(df,30)

# KORELACIJA--------------------------------------------------------------------------------------------------------
#install.packages("ggpubr")
library("ggpubr")
cor(df$emaTrend, df$trendBrDana, method = "pearson")
cor.test(df$emaTrend, df$trendBrDana, method = "pearson")
#------------------ ZAKLJUCAK - SLABA
