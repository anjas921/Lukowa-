
library(SciViews)

# UCITAVANJE TABELE

library(readxl)
PARA <- read_excel("C:\\Users\\user\\Desktop\\LUGR Data OHLC Daily Gaming and Entertainment ENG 2013-2022.xlsx", sheet="PARA")
names(PARA)[1] <- "Date"

# DESKRIPTIVNA STATISTIKA

PARA_descriptive_statistics <- data.frame(open=mean(PARA$Open),high=mean(PARA$High),low=mean(PARA$Low),last=mean(PARA$Last)) 
PARA_descriptive_statistics[2,] <- c(median(PARA$Open),median(PARA$High),median(PARA$Low),median(PARA$Last))
library(moments)
PARA_descriptive_statistics[3,] <- c(skewness(PARA$Open),skewness(PARA$High),skewness(PARA$Low),skewness(PARA$Last))
PARA_descriptive_statistics[4,] <- c(kurtosis(PARA$Open),kurtosis(PARA$High),kurtosis(PARA$Low),kurtosis(PARA$Last))

rownames(PARA_descriptive_statistics) <- c("MPARAN","MEDIAN", "SKEWNESS", "KURTOSIS")

# DNEVNI LOG RETURN (ovo proveriti posto nisam siguran jer sam to radio ranije)
#prvo moraju da se naprave kolone u dataframe-u
PARA$LogReturn_Open<-0
PARA$LogReturn_High<-0
PARA$LogReturn_Low<-0
PARA$LogReturn_Last<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(PARA)){
  PARA$LogReturn_Open[i]<-log(PARA$Open[i]/PARA$Open[i-1])
  PARA$LogReturn_High[i]<-log(PARA$High[i]/PARA$High[i-1])
  PARA$LogReturn_Low[i]<-log(PARA$Low[i]/PARA$Low[i-1])
  PARA$LogReturn_Last[i]<-log(PARA$Last[i]/PARA$Last[i-1])
}

# ///////////////////////////

# grupisanje podataka po nedeljama

library(dplyr)
library(lubridate)

# Kreiranje novih kolona za oznaku nedelje i godine
PARA$week <- week(PARA$Date)
PARA$year <- year(PARA$Date)

# Kreiranje nedeljnih podataka po godinama iz dnevnih podataka
PARA_weekly <- PARA %>%
  group_by(year, week) %>%
  summarize(weekly_open = first(Open),
            weekly_high = max(High),
            weekly_low = min(Low),
            weekly_close = last(Last))

# Ispisivanje nedeljnih podataka po godinama
print(PARA_weekly)


# /////////////////////////


# NEDELJNI LOG RETURN 

############## NOVO RESENJE (nedeljni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u

PARA_weekly$LogReturn_Open<-0
PARA_weekly$LogReturn_High<-0
PARA_weekly$LogReturn_Low<-0
PARA_weekly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(PARA_weekly)){
  PARA_weekly$LogReturn_Open[i]<-log(PARA_weekly$weekly_open[i]/PARA_weekly$weekly_open[i-1])
  PARA_weekly$LogReturn_High[i]<-log(PARA_weekly$weekly_high[i]/PARA_weekly$weekly_high[i-1])
  PARA_weekly$LogReturn_Low[i]<-log(PARA_weekly$weekly_low[i]/PARA_weekly$weekly_low[i-1])
  PARA_weekly$LogReturn_Close[i]<-log(PARA_weekly$weekly_close[i]/PARA_weekly$weekly_close[i-1])
}

# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Dodaj kolonu za godinu i mesec
PARA$month <- month(PARA$Date)
PARA$year <- year(PARA$Date)

# Grupisi po godini i mesecu, izracunaj mesece podatke
PARA_monthly <- PARA %>%
  group_by(year, month) %>%
  summarise(monthly_open = first(Open),
            monthly_high = max(High),
            monthly_low = min(Low),
            monthly_close = last(Last))

# Prikazi rezultate
PARA_monthly

# /////////////////////////


# MESECNI LOG RETURN


############## NOVO RESENJE (mesecni) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
PARA_monthly$LogReturn_Open<-0
PARA_monthly$LogReturn_High<-0
PARA_monthly$LogReturn_Low<-0
PARA_monthly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(PARA_monthly)){
  PARA_monthly$LogReturn_Open[i]<-log(PARA_monthly$monthly_open[i]/PARA_monthly$monthly_open[i-1])
  PARA_monthly$LogReturn_High[i]<-log(PARA_monthly$monthly_high[i]/PARA_monthly$monthly_high[i-1])
  PARA_monthly$LogReturn_Low[i]<-log(PARA_monthly$monthly_low[i]/PARA_monthly$monthly_low[i-1])
  PARA_monthly$LogReturn_Close[i]<-log(PARA_monthly$monthly_close[i]/PARA_monthly$monthly_close[i-1])
}

# ///////////////////////////

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
PARA_yearly <- PARA %>%
  group_by(year) %>%
  summarise(yearly_open = first(Open),
            yearly_high = max(High),
            yearly_low = min(Low),
            yearly_close = last(Last))

# Prikazi rezultate
PARA_yearly

# /////////////////////////

# GODISNJI LOG RETURN

############## NOVO RESENJE (godisnji) ################

library(SciViews)

#prvo moraju da se naprave kolone u dataframe-u
PARA_yearly$LogReturn_Open<-0
PARA_yearly$LogReturn_High<-0
PARA_yearly$LogReturn_Low<-0
PARA_yearly$LogReturn_Close<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(PARA_yearly)){
  PARA_yearly$LogReturn_Open[i]<-log(PARA_yearly$yearly_open[i]/PARA_yearly$yearly_open[i-1])
  PARA_yearly$LogReturn_High[i]<-log(PARA_yearly$yearly_high[i]/PARA_yearly$yearly_high[i-1])
  PARA_yearly$LogReturn_Low[i]<-log(PARA_yearly$yearly_low[i]/PARA_yearly$yearly_low[i-1])
  PARA_yearly$LogReturn_Close[i]<-log(PARA_yearly$yearly_close[i]/PARA_yearly$yearly_close[i-1])
}

#--------------------------------------------------------------------------------------------------------------
# Calculating yearly volatility using standard deviation of log returns (volatilnost po godinama)
#--------------------------------------------------------------------------------------------------------------


############## NOVO RESENJE ################
#PARA<-PARA[,10]
#install.packages("quantmod")
library(quantmod)

PARA<-PARA[,-10]
PARA_yearly$Volatility_Open <- "/"
PARA_yearly$Volatility_Open[1] <- sd(PARA_yearly$LogReturn_Open)
PARA_yearly$Volatility_High <- "/"
PARA_yearly$Volatility_High[1] <- sd(PARA_yearly$LogReturn_High)
PARA_yearly$Volatility_Low <- "/"
PARA_yearly$Volatility_Low[1] <- sd(PARA_yearly$LogReturn_Low)
PARA_yearly$Volatility_Last <- "/"
PARA_yearly$Volatility_Last[1] <- sd(PARA_yearly$LogReturn_Close)

# Ucitaj pakete
library(dplyr)
library(lubridate)

# Grupisi po godini i mesecu, izracunaj mesece podatke
PARA_yearly_volatility <- PARA %>%
  group_by(year) %>%
  summarise(open_volatility = sd(Open),
            high_volatility = sd(High),
            low_volatility = sd(Low),
            close_volatility = sd(Last))

# Prikazi rezultate
PARA_yearly_volatility


#--------------------------------------------------------------------------------------------------------------
# Visualizing both returns and raw prices on graphs and charts
#--------------------------------------------------------------------------------------------------------------

colnames(PARA)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)
library(ggplot2)

colnames(PARA[1])<-"Date"

# Convert date to character for better labeling
PARA$Date<- as.character(PARA$Date)

# Create a candlestick chart
fig_PARA <- plot_ly(data = PARA, type = "candlestick",
                     x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                     increasing = list(fillcolor = "green", line = list(color = "green")),
                     decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA <- fig_PARA %>% layout(title = "PARA Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# PARAplay the chart
fig_PARA

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
PARA$Date<- as.character(PARA$Date)

# Create a candlestick chart
fig_PARA_lr_d <- plot_ly(data = PARA, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Last,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA_lr_d <- fig_PARA_lr_d %>% layout(title = "PARA Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# PARAplay the chart
fig_PARA_lr_d


# WEEKLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
PARA$Date<- as.character(PARA$Date)

# Create a candlestick chart
fig_PARA_lr_w <- plot_ly(data = PARA_weekly, type = "candlestick",
                          x = ~Date, open = ~LogReturn_Open, high = ~LogReturn_High, low = ~LogReturn_Low, close = ~LogReturn_Close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA_lr_w <- fig_PARA_lr_w %>% layout(title = "PARA Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# PARAplay the chart
fig_PARA_lr_w

# MONTHLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better label
PARA_monthly$month<- as.character(PARA_monthly$month)

# Create a candlestick chart
fig_PARA_lr_m <- plot_ly(data = PARA_monthly, type = "candlestick",
                          x = ~month, open = ~monthly_open, high = ~monthly_high, low = ~monthly_low, close = ~monthly_close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA_lr_m <- fig_PARA_lr_m %>% layout(title = "PARA Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# PARAplay the chart
fig_PARA_lr_m

# YPARARLY

# Load libraries
library(plotly)
library(quantmod)

# Convert date to character for better labeling
PARA_yearly$year<- as.character(PARA_yearly$year)

# Create a candlestick chart
fig_PARA_lr_y <- plot_ly(data = PARA_yearly, type = "candlestick",
                          x = ~year, open = ~yearly_open, high = ~yearly_high, low = ~yearly_low, close = ~yearly_close,
                          increasing = list(fillcolor = "green", line = list(color = "green")),
                          decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_PARA_lr_y <- fig_PARA_lr_y %>% layout(title = "PARA Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# PARAplay the chart
fig_PARA_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------
#+252,+126
library(zoo)
PARA$MA5 <- rollmean(PARA$Last, k = 5, fill = NA)
PARA$MA21 <- rollmean(PARA$Last, k = 21, fill = NA)
PARA$MA63 <- rollmean(PARA$Last, k = 63, fill = NA)
PARA$MA126 <- rollmean(PARA$Last, k = 126, fill = NA)
PARA$MA252 <- rollmean(PARA$Last, k = 252, fill = NA)

ggplot(PARA, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(PARA, aes(x = Date, y = Last,group = 1)) +
  geom_line() +
  geom_line(aes(y = MA5,group = 1), color = "blue", linetype = "dashed") +
  geom_line(aes(y = MA21,group = 1), color = "green", linetype = "dashed") +
  geom_line(aes(y = MA63,group = 1), color = "red", linetype = "dashed") +
  geom_line(aes(y = MA126,group = 1), color = "yellow", linetype = "dashed") +
  geom_line(aes(y = MA252,group = 1), color = "magenta", linetype = "dashed") +
  labs(x = "Date", y = "Price", title = "Moving Averages") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

#--------------------------------------------------------------------------------------------------------------
# Using all the gathered information from descriptive measures, returns and moving averages,
# rating companies based on price levels of their stock
#--------------------------------------------------------------------------------------------------------------












