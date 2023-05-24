# Example data
#ATVI
returns_total <- data.frame(ATVI_yearly$LogReturn_rtns,DIS_yearly$LogReturn_rtns,EA_yearly$LogReturn_rtns,
             NASDAQ_yearly$LogReturn_rtns, NFLX_yearly$LogReturn_rtns, NTDOY_yearly$LogReturn_rtns, NWS_yearly$LogReturn_rtns,
             PARA_yearly$LogReturn_rtns, SP500_yearly$LogReturn_rtns, TCEHY_yearly$LogReturn_rtns, TTWO_yearly$LogReturn_rtns,
             WBD_yearly$LogReturn_rtns)
colnames(returns_total)<- c("R.ATVI", "R.DIS", "R.EA", 'R.NASDAQ', 'R.NFLX', 'R.NTDOY','R.NWS', 'R.PARA', 'R.SP500', 'R.TCEHY','R.TTWO', 'R.WBD')

returns_ATVI <- ATVI_yearly$LogReturn_rtns
returns_DIS <- DIS_yearly$LogReturn_rtns
returns_EA <- EA_yearly$LogReturn_rtns
returns_NASDAQ <- NASDAQ_yearly$LogReturn_rtns
returns_NFLX <- NFLX_yearly$LogReturn_rtns
returns_NTDOY <- NTDOY_yearly$LogReturn_rtns
returns_NWS <- NWS_yearly$LogReturn_rtns
returns_PARA <- PARA_yearly$LogReturn_rtns
returns_SP500 <- SP500_yearly$LogReturn_rtns
returns_TCEHY <- TCEHY_yearly$LogReturn_rtns
returns_TTWO <- TTWO_yearly$LogReturn_rtns
returns_WBD <- WBD_yearly$LogReturn_rtns


risk_free_rate <- c(0.0235, 0.0254, 0.0214, 0.0184, 0.0233,0.0291, 0.0214, 0.0089, 0.0145,0.0295)
years <- c(2013:2022)


#------------------------------------------------------------------------


# Calculate excess returns
excess_returns_ATVI <- returns_ATVI - risk_free_rate
excess_returns_DIS <- returns_DIS - risk_free_rate
excess_returns_EA <- returns_EA - risk_free_rate
excess_returns_NASDAQ <- returns_NASDAQ - risk_free_rate
excess_returns_NFLX <- returns_NFLX - risk_free_rate
excess_returns_NTDOY <- returns_NTDOY - risk_free_rate
excess_returns_NWS <- returns_NWS - risk_free_rate
excess_returns_PARA <- returns_PARA - risk_free_rate
excess_returns_SP500 <- returns_SP500 - risk_free_rate
excess_returns_TCEHY <- returns_TCEHY - risk_free_rate
excess_returns_TTWO <- returns_TTWO - risk_free_rate
excess_returns_WBD <- returns_WBD - risk_free_rate

# Calculate average excess return
average_excess_return_ATVI <- mean(excess_returns_ATVI)
average_excess_return_DIS <- mean(excess_returns_DIS)
average_excess_return_EA <- mean(excess_returns_EA)
average_excess_return_NASDAQ <- mean(excess_returns_NASDAQ)
average_excess_return_NFLX <- mean(excess_returns_NFLX)
average_excess_return_NTDOY <- mean(excess_returns_NTDOY)
average_excess_return_NWS <- mean(excess_returns_NWS)
average_excess_return_PARA <- mean(excess_returns_PARA)
average_excess_return_SP500 <- mean(excess_returns_SP500)
average_excess_return_TCEHY <- mean(excess_returns_TCEHY)
average_excess_return_TTWO <- mean(excess_returns_TTWO)
average_excess_return_WBD <- mean(excess_returns_WBD)

# Calculate standard deviation of excess returns
std_dev_excess_returns <- sd(excess_returns)

std_dev_excess_returns_ATVI <- sd(excess_returns_ATVI)
std_dev_excess_returns_DIS <- sd(excess_returns_DIS)
std_dev_excess_returns_EA <- sd(excess_returns_EA)
std_dev_excess_returns_NASDAQ <- sd(excess_returns_NASDAQ)
std_dev_excess_returns_NFLX <- sd(excess_returns_NFLX)
std_dev_excess_returns_NTDOY <- sd(excess_returns_NTDOY)
std_dev_excess_returns_NWS <- sd(excess_returns_NWS)
std_dev_excess_returns_PARA <- sd(excess_returns_PARA)
std_dev_excess_returns_SP500 <- sd(excess_returns_SP500)
std_dev_excess_returns_TCEHY <- sd(excess_returns_TCEHY)
std_dev_excess_returns_TTWO <- sd(excess_returns_TTWO)
std_dev_excess_returns_WBD <- sd(excess_returns_WBD)
# Calculate Sharpe ratio




sharpe_ratio_ATVI <- average_excess_return_ATVI/std_dev_excess_returns_ATVI
sharpe_ratio_DIS<- average_excess_return_DIS/std_dev_excess_returns_DIS 
sharpe_ratio_EA<- average_excess_return_EA/std_dev_excess_returns_EA 
sharpe_ratio_NASDAQ<- average_excess_return_NASDAQ/std_dev_excess_returns_NASDAQ
sharpe_ratio_NFLX<- average_excess_return_NFLX/std_dev_excess_returns_NFLX 
sharpe_ratio_NTDOY<- average_excess_return_NTDOY/std_dev_excess_returns_NTDOY
sharpe_ratio_NWS<- average_excess_return_NWS/std_dev_excess_returns_NWS
sharpe_ratio_PARA<- average_excess_return_PARA/std_dev_excess_returns_PARA 
sharpe_ratio_SP500<- average_excess_return_SP500/std_dev_excess_returns_SP500 
sharpe_ratio_TCEHY<- average_excess_return_TCEHY/std_dev_excess_returns_TCEHY 
sharpe_ratio_TTWO<- average_excess_return_TTWO/std_dev_excess_returns_TTWO 
sharpe_ratio_WBD<- average_excess_return_WBD/std_dev_excess_returns_WBD 

# Print the Sharpe ratio
SHAPRE_RATIO <- data.frame(SR= c(sharpe_ratio_ATVI, sharpe_ratio_DIS, sharpe_ratio_EA, sharpe_ratio_NASDAQ,sharpe_ratio_NFLX, sharpe_ratio_NTDOY, sharpe_ratio_NWS, sharpe_ratio_PARA,
                             sharpe_ratio_SP500, sharpe_ratio_TCEHY, sharpe_ratio_TTWO, sharpe_ratio_WBD), 
                           comp =  c("R.ATVI", "R.DIS", "R.EA", 'R.NASDAQ', 'R.NFLX', 'R.NTDOY','R.NWS', 'R.PARA', 'R.SP500', 'R.TCEHY','R.TTWO', 'R.WBD'))

library(ggplot2)

#print(sharpe_ratio)
