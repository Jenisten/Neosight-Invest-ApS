rm(list = ls())

#install.packages(c("quantmod", "PerformanceAnalytics", "PortfolioAnalytics", "ROI.plugin.glpk", "Rglpk", "ROI.plugin.quadprog", "ROI", "quadprog", "fPortfolio","skedastic", "ggplot2"))

lapply(c("car", "lmtest", "sandwich", "tseries", "quantmod", "PortfolioAnalytics", "ROI.plugin.quadprog", "zoo", "magrittr", "fPortfolio","skedastic", "ggplot2"), library, character.only = TRUE)

# Stocks
symbols <- c("NVDA", "NOVO-B.CO", "GOOGL", "RYAAY", "AAPL", "ADBE", "AMD", "QCOM", "PG", "WMT", "LMT", "RTX", "T", "TSLA")

stockPrices <- NULL
for (ticker in symbols) {
  stockPrices <- cbind(
    stockPrices,
    getSymbols.yahoo(ticker, from = "2018-06-07", to = "2021-12-31", periodicity = "daily", auto.assign = FALSE)[, 4]
  )
}

# Log returns
stockReturns <- na.omit(diff(log(stockPrices)))
head(stockReturns)

# Mean-Variance Optimization
{
  portf <- portfolio.spec(colnames(stockReturns))
  portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
  portf <- add.constraint(portf, type="box", min=, max=)
  portf <- add.objective(portf, type = "return", name = "mean")
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  optPort <- optimize.portfolio(stockReturns, portf, optimize_method = "quadprog", maxSR=TRUE, trace=TRUE)
  print(optPort)
}

# Sharpe Ratio
{
  weights <- extractWeights(optPort)
  portReturn <- sum(weights * colMeans(stockReturns))
  portRisk <- sqrt(t(weights) %*% cov(stockReturns) %*% weights)
  riskfreerate <- 0.0
  sharpeRatio <- (portReturn*252 - riskfreerate) / (portRisk*sqrt(252))
  cat("\nSharpe Ratio: ", sharpeRatio, "\n")
}

# Plotting the weights
{
  weights_df <- data.frame(weights)
  weights_df$stocks <- colnames(stockReturns)
  weights_df %>%
    ggplot(aes(x = stocks, y = weights)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Mean-Variance Portfolio Weights",
         x = "Stocks",
         y = "Weights")
}