for (i in nrow(ATVI)) {
  ATVI$LogReturn.Open.w<-"/"
  ATVI$LogReturn.High.w<-"/"
  ATVI$LogReturn.Low.w<-"/"
  ATVI$LogReturn.Last.w<-"/"
}


library(SciViews)

# DNEVNI LOG RETURN
#prvo moraju da se naprave kolone u dataframe-u
ATVI$LogReturn.Open[i]<-0
ATVI$LogReturn.High[i]<-0
ATVI$LogReturn.Low[i]<-0
ATVI$LogReturn.Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(ATVI)){
  ATVI$LogReturn.Open[i]<- log(ATVI$Open[i]/ATVI$Open[i-1])
  ATVI$LogReturn.High[i]<- log(ATVI$High[i]/ATVI$High[i-1])
  ATVI$LogReturn.Low[i]<- log(ATVI$Low[i]/ATVI$Low[i-1])
  ATVI$LogReturn.Last[i]<- log(ATVI$Last[i]/ATVI$Last[i-1])
  
}

# NEDELJNI LOG RETURN 
for(i in 9:nrow(ATVI)){
  if(weekdays.POSIXt(ATVI$'Date (GMT)'[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(ATVI$'Date (GMT)'[i-j])=="Monday"){
        ATVI$LogReturn.Open.w[i]<-log(ATVI$Open[i]/ATVI$Open[j])
        ATVI$LogReturn.High.w[i]<-log(ATVI$High[i]/ATVI$High[j])
        ATVI$LogReturn.Low.w[i]<-log(ATVI$Low[i]/ATVI$Low[j])
        ATVI$LogReturn.Last.w[i]<-log(ATVI$Last[i]/ATVI$Last[j])
      }
    }
  }
}



# MESECNI LOG RETURN

#NASDAQ$LogReturn.Open <- as.numeric(NASDAQ$LogReturn.Open)
#NASDAQ$LogReturn.High <- as.numeric(NASDAQ$LogReturn.High)
#NASDAQ$LogReturn.Low <- as.numeric(NASDAQ$LogReturn.Low)
#NASDAQ$LogReturn.Last <- as.numeric(NASDAQ$LogReturn.Last)

monthly_returns_ATVI_Open <- aggregate(ATVI$LogReturn.Open, by = list(format(ATVI$'Date (GMT)' , "%Y-%m")), sum)
monthly_returns_ATVI_High <- aggregate(ATVI$LogReturn.High, by = list(format(ATVI$'Date (GMT)', "%Y-%m")), sum)
monthly_returns_ATVI_Low <- aggregate(ATVI$LogReturn.Low, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)
monthly_returns_ATVI_Last <- aggregate(ATVI$LogReturn.Last, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(monthly_returns_ATVI_Open,monthly_returns_ATVI_High,by="Group.1")
low_last<-merge(monthly_returns_ATVI_Low,monthly_returns_ATVI_Last,by="Group.1")
monthly_returns_ATVI<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_ATVI)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_ATVI_Open)
remove(monthly_returns_ATVI_High)
remove(monthly_returns_ATVI_Low)
remove(monthly_returns_ATVI_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_ATVI_Open <- aggregate(ATVI$LogReturn.Open, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)
yearly_returns_ATVI_High <- aggregate(ATVI$LogReturn.High, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)
yearly_returns_ATVI_Low <- aggregate(ATVI$LogReturn.Low, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)
yearly_returns_ATVI_Last <- aggregate(ATVI$LogReturn.Last, by = list(format(ATVI$`Date (GMT)`, "%Y-%m")), sum)

open_high<-merge(yearly_returns_ATVI_Open,yearly_returns_ATVI_High,by="Group.1")
low_last<-merge(yearly_returns_ATVI_Low,yearly_returns_ATVI_Last,by="Group.1")
yearly_returns_ATVI<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_ATVI)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_ATVI_Open)
remove(yearly_returns_ATVI_High)
remove(yearly_returns_ATVI_Low)
remove(yearly_returns_ATVI_Last)
remove(open_high)
remove(low_last)
#------------------------------------------------------------------------------
library(quantmod)

volatility_open_A <- sd(yearly_returns_ATVI$Open[-1])
volatility_high_A <- sd(yearly_returns_ATVI$High[-1])
volatility_low_A <- sd(yearly_returns_ATVI$Low[-1])
volatility_last_A <- sd(yearly_returns_ATVI$Last[-1])

yearly_returns_ATVI$Volatility_Open[1] <- "/"
yearly_returns_ATVI$Volatility_Open[1] <- sd(yearly_returns_ATVI$Open)
yearly_returns_ATVI$Volatility_High[1] <- "/"
yearly_returns_ATVI$Volatility_High[1] <- sd(yearly_returns_ATVI$High)
yearly_returns_ATVI$Volatility_Low[1] <- "/"
yearly_returns_ATVI$Volatility_Low[1] <- sd(yearly_returns_ATVI$Low)
yearly_returns_ATVI$Volatility_Last[1] <- "/"
yearly_returns_ATVI$Volatility_Last[1] <- sd(yearly_returns_ATVI$Last)

#-------------------------------------------------------------------------------
colnames(ATVI)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries
#install.packages("plotly")
library(plotly)
library(quantmod)

# Convert date to character for better labeling
ATVI$Date<- as.character(ATVI$Date)

# Create a candlestick chart
fig <- plot_ly(data = ATVI, type = "candlestick",
               x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
               increasing = list(fillcolor = "green", line = list(color = "green")),
               decreasing = list(fillcolor = "red", line = list(color = "red")))
fig <- fig %>% layout(title = "ATVI Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries


# Convert date to character for better labeling


# Create a candlestick chart
fig_lr_d <- plot_ly(data = ATVI, type = "candlestick",
                    x = ~Date, open = ~LogReturn.Open, high = ~LogReturn.High, low = ~LogReturn.Low, close = ~LogReturn.Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_lr_d <- fig_lr_d %>% layout(title = "ATVI Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_lr_d
#--------------------------------------------------------------------------
fig_lr_w <- plot_ly(data = ATVI, type = "candlestick",
                    x = ~Date, open = ~LogReturn.Open.w, high = ~LogReturn.High.w, low = ~LogReturn.Low.w, close = ~LogReturn.Last.w,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_lr_w <- fig_lr_w %>% layout(title = "ATVI Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_lr_w

# MONTHLY

# Load libraries


# Convert date to character for better labeling
monthly_returns_ATVI$Month<- as.character(monthly_returns_ATVI$Month)

# Create a candlestick chart
fig_lr_m <- plot_ly(data = monthly_returns_ATVI, type = "candlestick",
                    x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_lr_m <- fig_lr_m %>% layout(title = "ATVI Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_lr_m

# YEARLY

# Load libraries

# Convert date to character for better labeling
yearly_returns_ATVI$Year<- as.character(yearly_returns_ATVI$Year)

# Create a candlestick chart
fig_lr_y <- plot_ly(data = yearly_returns_ATVI, type = "candlestick",
                    x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_lr_y <- fig_lr_y %>% layout(title = "ATVI Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
ATVI$MA5 <- rollmean(ATVI$Last, k = 5, fill = NA)
ATVI$MA21 <- rollmean(ATVI$Last, k = 21, fill = NA)
ATVI$MA63 <- rollmean(ATVI$Last, k = 63, fill = NA)

ggplot(ATVI, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(ATVI, aes(x = Date, y = Last,group = 1)) +
  geom_line() +
  geom_line(aes(y = MA5,group = 1), color = "blue", linetype = "dashed") +
  geom_line(aes(y = MA21,group = 1), color = "green", linetype = "dashed") +
  geom_line(aes(y = MA63,group = 1), color = "red", linetype = "dashed") +
  labs(x = "Date", y = "Price", title = "Moving Averages") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

#--------------------------------------------------------------------------------------------------------------
# Using all the gathered information from descriptive measures, returns and moving averages,
# rating companies based on price levels of their stock
#--------------------------------------------------------------------------------------------------------------




                   
                      
                    