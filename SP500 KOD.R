for (i in nrow(SP500)) {
  SP500$LogReturn.Open.w<-"/"
  SP500$LogReturn.High.w<-"/"
  SP500$LogReturn.Low.w<-"/"
  SP500$LogReturn.Last.w<-"/"
}

library(SciViews)
# DNEVNI LOG RETURN
#prvo moraju da se naprave kolone u dataframe-u

SP500$LogReturn.Open[i]<-0
SP500$LogReturn.High[i]<-0
SP500$LogReturn.Low[i]<-0
SP500$LogReturn.Last[i]<-0

#sad se dodaju vrednosti tim kolonama
for(i in 2:nrow(SP500)){
  SP500$LogReturn.Open[i]<- log(SP500$Open[i]/SP500$Open[i-1])
  SP500$LogReturn.High[i]<- log(SP500$High[i]/SP500$High[i-1])
  SP500$LogReturn.Low[i]<- log(SP500$Low[i]/SP500$Low[i-1])
  SP500$LogReturn.Last[i]<- log(SP500$Last[i]/SP500$Last[i-1])
}
# NEDELJNI LOG RETURN 
for(i in 9:nrow(SP500)){
  if(weekdays.POSIXt(SP500$`Date (GMT)`[i])=="Monday"){
    for(j in 1:5){
      if(weekdays.POSIXt(SP500$`Date (GMT)`[i-j])=="Monday"){
        SP500$LogReturn.Open.w[i]<-log(SP500$Open[i]/SP500$Open[j])
        SP500$LogReturn.High.w[i]<-log(SP500$High[i]/SP500$High[j])
        SP500$LogReturn.Low.w[i]<-log(SP500$Low[i]/SP500$Low[j])
        SP500$LogReturn.Last.w[i]<-log(SP500$Last[i]/SP500$Last[j])
      }
    }
  }
}



# MESECNI LOG RETURN

SP500$LogReturn.Open <- as.numeric(SP500$LogReturn.Open)
SP500$LogReturn.High <- as.numeric(SP500$LogReturn.High)
SP500$LogReturn.Low <- as.numeric(SP500$LogReturn.Low)
SP500$LogReturn.Last <- as.numeric(SP500$LogReturn.Last)

monthly_returns_SP500_Open <- aggregate(SP500$LogReturn.Open[-1], by = list(format(SP500$'Date (GMT)'[-1], "%Y-%m")), sum)
monthly_returns_SP500_High <- aggregate(SP500$LogReturn.High[-1], by = list(format(SP500$'Date (GMT)'[-1], "%Y-%m")), sum)
monthly_returns_SP500_Low <- aggregate(SP500$LogReturn.Low[-1], by = list(format(SP500$`Date (GMT)`[-1], "%Y-%m")), sum)
monthly_returns_SP500_Last <- aggregate(SP500$LogReturn.Last[-1], by = list(format(SP500$`Date (GMT)`[-1], "%Y-%m")), sum)

open_high<-merge(monthly_returns_SP500_Open,monthly_returns_SP500_High,by="Group.1")
low_last<-merge(monthly_returns_SP500_Low,monthly_returns_SP500_Last,by="Group.1")
monthly_returns_SP500<-merge(open_high,low_last,by="Group.1")

colnames(monthly_returns_SP500)<-c("Month","Open","High","Low","Last")

remove(monthly_returns_SP500_Open)
remove(monthly_returns_SP500_High)
remove(monthly_returns_SP500_Low)
remove(monthly_returns_SP500_Last)
remove(open_high)
remove(low_last)

# GODISNJI LOG RETURN

yearly_returns_SP500_Open <- aggregate(SP500$LogReturn.Open[-1], by = list(format(SP500$`Date (GMT)`[-1], "%Y-%m")), sum)
yearly_returns_SP500_High <- aggregate(SP500$LogReturn.High[-1], by = list(format(SP500$`Date (GMT)`[-1], "%Y-%m")), sum)
yearly_returns_SP500_Low <- aggregate(SP500$LogReturn.Low[-1], by = list(format(SP500$`Date (GMT)`[-1], "%Y-%m")), sum)
yearly_returns_SP500_Last <- aggregate(SP500$LogReturn.Last[-1], by = list(format(SP500$`Date (GMT)`[-1], "%Y-%m")), sum)

open_high<-merge(yearly_returns_SP500_Open,yearly_returns_SP500_High,by="Group.1")
low_last<-merge(yearly_returns_SP500_Low,yearly_returns_SP500_Last,by="Group.1")
yearly_returns_SP500<-merge(open_high,low_last,by="Group.1")

colnames(yearly_returns_SP500)<-c("Year","Open","High","Low","Last")

remove(yearly_returns_SP500_Open)
remove(yearly_returns_SP500_High)
remove(yearly_returns_SP500_Low)
remove(yearly_returns_SP500_Last)
remove(open_high)
remove(low_last)

#----------------------------------------------

volatility_open_SP <- sd(yearly_returns_SP500$Open)
volatility_high_SP <- sd(yearly_returns_SP500$High)
volatility_low_SP<- sd(yearly_returns_SP500$Low)
volatility_last_SP <- sd(yearly_returns_SP500$Last)


yearly_returns_SP500$Volatility_Open[1] <- "/"
yearly_returns_SP500$Volatility_Open[1] <- sd(yearly_returns_SP500$Open)
yearly_returns_SP500$Volatility_High[1] <- "/"
yearly_returns_SP500$Volatility_High[1] <- sd(yearly_returns_SP500$High)
yearly_returns_SP500$Volatility_Low[1] <- "/"
yearly_returns_SP500$Volatility_Low[1] <- sd(yearly_returns_SP500$Low)
yearly_returns_SP500$Volatility_Last[1] <- "/"
yearly_returns_SP500$Volatility_Last[1] <- sd(yearly_returns_SP500$Last)

#---------------------------------------------------------------------------
colnames(SP500)[1]<-"Date"

# RAW PRICES

# CANDLESTICK CHART PATTERN

# Load libraries

# Convert date to character for better labeling
SP500$Date<- as.character(SP500$Date)

# Create a candlestick chart
fig <- plot_ly(data = SP500, type = "candlestick",
               x = ~Date, open = ~Open, high = ~High, low = ~Low, close = ~Last,
               increasing = list(fillcolor = "green", line = list(color = "green")),
               decreasing = list(fillcolor = "red", line = list(color = "red")))
fig <- fig %>% layout(title = "SP500 Raw Prices Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig

# RETURNS

# CANDLESTICK CHART PATTERN

# DAILY

# Load libraries


# Convert date to character for better labeling
SP500$Date<- as.character(SP500$Date)

# Create a candlestick chart
fig_lr_d <- plot_ly(data = SP500, type = "candlestick",
                    x = ~Date, open = ~LogReturn.Open, high = ~LogReturn.High, low = ~LogReturn.Low, close = ~LogReturn.Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_lr_d <- fig_lr_d %>% layout(title = "SP500 Daily Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_lr_d
#--------------------------------------------------------------------------
fig_lr_w <- plot_ly(data = SP500, type = "candlestick",
                    x = ~Date, open = ~LogReturn.Open.w, high = ~LogReturn.High.w, low = ~LogReturn.Low.w, close = ~LogReturn.Last.w,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_lr_w <- fig_lr_w %>% layout(title = "SP500 Weekly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_lr_w

# MONTHLY

# Load libraries


# Convert date to character for better labeling
monthly_returns_SP500$Month<- as.character(monthly_returns_SP500$Month)

# Create a candlestick chart
fig_lr_m <- plot_ly(data = monthly_returns_SP500, type = "candlestick",
                    x = ~Month, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_lr_m <- fig_lr_m %>% layout(title = "SP500 Monthly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_lr_m

# YEARLY

# Load libraries

# Convert date to character for better labeling
yearly_returns_SP500$Year<- as.character(yearly_returns_SP500$Year)

# Create a candlestick chart
fig_lr_y <- plot_ly(data = yearly_returns_SP500, type = "candlestick",
                    x = ~Year, open = ~Open, high = ~High, low = ~Low, close = ~Last,
                    increasing = list(fillcolor = "green", line = list(color = "green")),
                    decreasing = list(fillcolor = "red", line = list(color = "red")))
fig_lr_y <- fig_lr_y %>% layout(title = "SP500 Yearly Return Candlestick Chart", xaxis = list(title = "Date"), yaxis = list(title = "Price"))

# Display the chart
fig_lr_y


#--------------------------------------------------------------------------------------------------------------
# On visual plots introducing moving averages on raw prices with windows 5, 21 and 63 
# for weekly, monthly and quarterly averages respectively
#--------------------------------------------------------------------------------------------------------------

library(zoo)
SP500$MA5 <- rollmean(SP500$Last, k = 5, fill = NA)
SP500$MA21 <- rollmean(SP500$Last, k = 21, fill = NA)
SP500$MA63 <- rollmean(SP500$Last, k = 63, fill = NA)

ggplot(SP500, aes(x = Date, y = Last,group = 1)) + geom_line() + labs(x = "Date", y = "Price", title = "Raw Prices")

ggplot(SP500, aes(x = Date, y = Last,group = 1)) +
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
