library(quantmod)
library(lubridate)

start_years <- 3

getSymbols(tickers)

pxs <- NULL
rets <- NULL

for(ticker in tickers){
  pxs <- ticker %>% get %>% Ad %>% merge(pxs)
  rets <- ticker %>% get %>% dailyReturn %>% merge(rets)
}

colnames(pxs) <- tickers
colnames(rets) <- tickers

# Preview Data
plot(pxs)
plot(rets)

date_start <- pxs %>% index %>% first + years(start_years)

cov_start <- cov(rets[paste0("/", date_start)]) * 100
ret_start <- rets[paste0("/", date_start)] %>% apply(2, mean)


