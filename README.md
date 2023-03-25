# Portfolio Optimization

Load the necessary libraries.
```
library(tidyquant)
library(forcats)
library(plotly)
library(timetk)
library(tidyr)
```

Define the parameters of the analysis.

The data will be annualized, considering 252 trading days in a year.
Additionally, the analysis will carry out a simulation of 1000 portfolios.

```
trading_days <- 252
num_port <- 1000
```

Download 10 years of historical price data of the ETFs selected for the portfolio.
```
tickers <- c('SPY', 'VTI', 'TLT', 'LQD', 'GLD')

prices <- tq_get(tickers,
                 from = '2012-01-01',
                 to = '2022-12-31',
                 get = 'stock.prices')
```

Calculate daily logarithmic returns of the selected assets.
```
ret <- prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log') %>%
  pivot_wider(names_from = symbol, values_from = ret) %>%
  tk_xts()
```

Generate random weights for each portfolio, they have to sum to 1 for each portfolio.
```
weights <- matrix(runif(num_port * length(tickers)), nrow = num_port)
weights <- weights / rowSums(weights)
```

Calculate the annualized returns for each portfolio.
```
mean_ret <- colMeans(ret)
port_ret <- (weights %*% mean_ret + 1) ^ trading_days - 1
```

Calculate the risk for each portfolio.
```
cov_mat <- cov(ret) * trading_days
port_risk <- sqrt(diag(weights %*% cov_mat %*% t(weights)))
```

Calculate the Sharpe Ratio, assuming 0% risk free rate.
```
sharpe_ratio <- port_ret / port_risk
```

Aggregate all values together.
```
colnames(weights) <- colnames(ret)
port_values <- tibble(Return = port_ret, Risk = port_risk, SharpeRatio = sharpe_ratio)
port_values <- cbind(weights, port_values)
```

Define the function to plot the weights in a portfolio.
```
save_barplot <- function(data, title, file) {
  p <- data %>%
    pivot_longer(cols = all_of(tickers), names_to = 'Asset', values_to = 'Weights') %>%
    ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
    geom_bar(stat = 'identity') +
    theme_classic() +
    labs(x = 'Assets', y = 'Weights', title = title) +
    scale_y_continuous(labels = scales::percent) 

  ggplotly(p)
  ggsave(file)
}
```

Plot the minimum variance and the tangency portfolio.
```
min_var <- port_values[which.min(port_values$Risk),]
max_sr <- port_values[which.max(port_values$SharpeRatio),]
min_var_plot <- save_barplot(min_var, 'Minimum Variance Portfolio Weights', 'min_var.png')
max_sr_plot <- save_barplot(max_sr, 'Tangency Portfolio Weights', 'max_sr.png')
```
<p align="middle">
  <img src="https://user-images.githubusercontent.com/38300176/227746749-a4e7dacb-dcc6-4393-b5c7-1db1a16da5cc.png" alt="Minimum Variance Portfolio Weights" width=47% height=47%>
  <img src="https://user-images.githubusercontent.com/38300176/227746752-8408d48e-0427-4071-bc14-a5f5fda3ac9f.png" alt="Tangency Portfolio Weights" width=47% height=47%>
</p>

Visualize the efficient frontier.
```
p <- port_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Risk", y = "Return", title = "Efficient Frontier")
ggplotly(p)
ggsave('efficient_frontier.png')
```
<p align="middle">
  <img src="https://user-images.githubusercontent.com/38300176/227746758-e4c836ad-28d3-4056-9f6d-ae5a18bcc5a6.png" alt="Efficient Frontier" width=52% height=52%>
</p>

