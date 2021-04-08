#acciones estudiadas: 
#SPY (S&P500 fund) w:25%
#EFA (a non-US equities fund) w:25%
#IJS (small-cap value fund) W:20%
#EEM (an emerging-mkts fund) w:20%
#AGG (bond fund) w:10%



###package loading------

library(tidyverse)
library(quantmod)
library(TTR)
library(tidyquant)
library(timetk)

###importing daily prices---------

#The symbols vector holds our tickers.
symbols <- c("SPY","EFA","IJS","EEM","AGG")

#The prices object hols our raw price data
prices <- getSymbols(symbols, src = "yahoo", from= "2005-01-01", 
                     auto.assign = TRUE, warnings = FALSE ) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

#converting daily adjusted prices to monthly log returns
prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))
# Remarks: using xts dates must be accessed via index() + excludes NA + wide format

###cronstructing the portfolio----------

w <- c(0.25,0.25,0.2,0.2,0.1)

#wights must line up with assets.checked by:
asset_weights_sanity_check <- tibble(w, symbols)

#assign each weight from w to a variable
W_1 <- w[1]
W_2 <- w[2]
W_3 <- w[3]
W_4 <- w[4]
W_5 <- w[5]

#assign asset's return stored in asset_returns_xts to a variable

asset1 <- asset_returns_xts[,1]
asset2 <- asset_returns_xts[,2]
asset3 <- asset_returns_xts[,3]
asset4 <- asset_returns_xts[,4]
asset5 <- asset_returns_xts[,5]

#return equation
portfolio_returns_byhand <- (W_1*asset1)+(W_2*asset2)+(W_3*asset3)+(W_4*asset4)+(W_5*asset5)

#insert variables in the equation
names(portfolio_returns_byhand) <- "returns"

#same as be4
portfolio_returns_xts_rebalanced_monthly <- Return.portfolio(asset_returns_xts,weights = w, rebalance_on = "months") %>%
  `colnames<-`("retuns")

#more realistic adjustment
portfolio_returns_xts_rebalanced_year <- Return.portfolio(asset_returns_xts, weights = w, rebanlance_on = "years")%>%
  `colnames<-`("returns")

##FF model final returns
asset_returns_log <- prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(returns)-log(lag(returns)))) %>%
  na.omit()
portfolio_returns_tq_rebalanced_monthly <- asset_returns_log %>%
  tq_portfolio(assets_col = asset, returns_col = returns, weights = w,
               col_rename = "returns", rebalance_on = "months")
