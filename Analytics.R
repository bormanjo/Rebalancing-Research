library(PortfolioAnalytics)
library(PerformanceAnalytics)

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
