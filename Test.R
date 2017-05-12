library(xts)
library(IBrokers)
library(TTR)
library(quantmod)
#library(blotter)
library(quantstrat)
rm(list = ls())

#Data
symbol='SPY'
tws <- twsConnect(port=7497)  
tws
spy <- twsEquity("SPY")
hstSpy <- reqHistoricalData(tws, spy, barSize = "30 mins", duration = "1 Y", endDateTime="20170331 23:59:59")
hstSpy <- hstSpy[,1:5]
head(hstSpy,3)
tail(hstSpy,3)
assign(symbol,hstSpy)

#Setup
currency("USD")
stock(symbol, currency="USD", multiplier=1)
fast <- 10   
slow <- 30   
stratName <- "LuxorJrSpy"
portName <- "LuxorJrSpy"
acctName <- "LuxorJrSpy"
nShs <- 1000   # number of shares in one transaction 
initEq <- 1000000
initDate <- '2016-3-31'



#Strategy
strategy_setup <- function()
{
  rm.strat(stratName)
  
  initPortf(name = portName, symbols = symbol, initDate = initDate, currency = 'USD')
  initAcct(name = acctName, portfolios = portName, initDate=initDate, initEq=initEq)
  initOrders(portfolio = portName, initDate = initDate)
  strategy(name = stratName, store=TRUE)
  
  add.indicator(strategy = stratName, name = "SMA", arguments = list(x = quote(Cl(SPY)), n=fast), label="SMA_fast")
  add.indicator(strategy = stratName, name = "SMA", arguments = list(x = quote(Cl(SPY)), n=slow), label="SMA_slow")
  
  # There are two signals:
  # The first is when fast SMA crosses above the slow SMA curve
  add.signal(strategy = stratName, name = "sigCrossover", 
             arguments = list(columns=c("SMA_fast","SMA_slow"),relationship="gte"),
             label="SMA_fast.gt.SMA_Slow")
  # The second is when fast SMA crosses below the slow SMA curve
  add.signal(strategy = stratName, name="sigCrossover",
             arguments = list(columns=c("SMA_fast","SMA_slow"),relationship="lt"),
             label="SMA_fast.lt.SMA_slow")
  
  # There are two rules:
  # The first is to buy 1000 shares when the fast SMA crosses above the slow SMA curve
  add.rule(strategy = stratName, name='ruleSignal', 
           arguments = list(sigcol="SMA_fast.gt.SMA_Slow", sigval=TRUE, orderqty=nShs, 
                            ordertype='market', orderside='long', pricemethod='market',
                            TxnFees = 0), type = 'enter', path.dep = TRUE)
  # The second is to exit when the fast SMA crosses below the slow SMA curve
  add.rule(strategy = stratName, name='ruleSignal', 
           arguments = list(sigcol="SMA_fast.lt.SMA_slow", sigval=TRUE, orderqty='all', 
                            ordertype='market', orderside='long', pricemethod='market',
                            TxnFees = 0), type = 'exit', path.dep = TRUE)
}


# Process the indicators and generate trades
strategy_setup()       # function call to set up strategy
applyStrategy(strategy = stratName, portfolios = portName)


# Analysis

updatePortf(Portfolio = portName)
updateAcct(name = acctName)
updateEndEq(Account = acctName)

txns <- as.xts(getTxns(Portfolio = portName, Symbol = symbol))
pts <- perTradeStats(Portfolio = portName)

# 1. Trade Statistics (in transposed form)
tstats <- tradeStats(Portfolios = portName)
t(tstats)

# 2. chart.Posn() plots showing fast SMA and slow SMA
# (a) Entire year time frame
chart.Posn(Portfolio = portName, Symbol = symbol,
           TA = "add_SMA(n = fast, col = 'blue'); add_SMA(n = slow, col = 'red');")

# (b) Only data in 2017
chart.Posn(Portfolio=portName, Symbol=symbol, Dates="2017",  
           TA = "add_SMA(n = fast, col = 'blue'); add_SMA(n = slow, col = 'red');")

# 3. Plot of equity curve for the strategy
a <- getAccount(stratName)
equity <- a$summary$End.Eq
plot(equity,main="Luxor Jr SPY 30-min Bar Strategy Equity Curve")

# A plot of the log returns for the strategy; 
ret <- Return.calculate(equity,method="log")
charts.PerformanceSummary(ret, colorset = bluefocus, main="Luxor Jr SPY 30-min Bar Strategy Return Performance")

# A plot of MAE
chart.ME(Portfolio = portName, Symbol = symbol, type = 'MAE', scale = 'percent')




# Examine the effect of a change in SMA Parameter
fast <- 10   
slow <- 25

strategy_setup()
# Process the indicators and generate trades
applyStrategy(strategy = stratName, portfolios = portName)

updatePortf(Portfolio = portName)
updateAcct(name = acctName)
updateEndEq(Account = acctName)

tstats <- tradeStats(Portfolios = portName)
t(tstats)
