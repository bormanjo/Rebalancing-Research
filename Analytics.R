library(PortfolioAnalytics)
library(PerformanceAnalytics)

rebalances <- portfolio_data$rebalance %>% subset( (.) %>% equals(1))

returns <- portfolio_data$return

# Performance Charts
charts.PerformanceSummary(returns, main = "Dynamic Rebalancing")
charts.RollingPerformance(returns, main = "Dynamic Rebalancing")

# Returns Analysis
chart.ECDF(returns)
chart.QQPlot(returns, distribution = "norm")

kurtosis(returns)
skewness(returns)
SkewnessKurtosisRatio(returns)

# Comparison to Minimum Cost Weights

lines(min_cost_weights)
plot(min_cost_weights)

plot(portfolio_data$eu)

chart.CumReturns(returns, main = "Cumulative Return")
addEventLines(events = rebalances, lwd = 0.3)
addSeries(returns, main = "Daily Return")
addEventLines(events = rebalances, lwd = 0.3)
addSeries(weights, main = "Weights")
addEventLines(events = rebalances, lwd = 0.3)

plot(portfolio_data$opt_var)
lines(portfolio_data$var, col = "red")

hist(portfolio_data$opt_var - portfolio_data$var)
hist(returns)

plot(portfolio_data$costs)

View(weights)
