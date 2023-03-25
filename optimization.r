library(tidyquant)
library(forcats)
library(plotly)
library(timetk)
library(tidyr)

# Set seed for reproducibility
set.seed(123)

# Define the assets to include in the portfolio
tickers <- c('SPY', 'VTI', 'TLT', 'LQD', 'GLD')

# Retrieve the historical daily close prices for the assets
prices <- tq_get(tickers,
                 from = '2012-01-01',
                 to = '2022-12-31',
                 get = 'stock.prices')

# Calculate daily log returns for each asset
ret <- prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log') %>%
  pivot_wider(names_from = symbol, values_from = ret) %>%
  tk_xts()

# Set the parameters for the portfolio optimization
trading_days <- 252
num_port <- 1000

# Generate random weights that sum to 1 for each portfolio
weights <- matrix(runif(num_port * length(tickers)), nrow = num_port)
weights <- weights / rowSums(weights)

# Calculate the returns for each portfolio
mean_ret <- colMeans(ret)
port_ret <- (weights %*% mean_ret + 1) ^ trading_days - 1

# Calculate the risk for each portfolio
cov_mat <- cov(ret) * trading_days
port_risk <- sqrt(diag(weights %*% cov_mat %*% t(weights)))

# Calculate the sharpe ratio for each portfolio
sharpe_ratio <- port_ret / port_risk

# Aggregate all values together
colnames(weights) <- colnames(ret)
port_values <- tibble(Return = port_ret, Risk = port_risk, SharpeRatio = sharpe_ratio)
port_values <- cbind(weights, port_values)

# Define a function to save a barplot of portfolio weights
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

# Save the plots for the portfolio with minimum risk and the portfolio with maximum Sharpe Ratio
min_var <- port_values[which.min(port_values$Risk),]
max_sr <- port_values[which.max(port_values$SharpeRatio),]
min_var_plot <- save_barplot(min_var, 'Minimum Variance Portfolio Weights', 'min_var.png')
max_sr_plot <- save_barplot(max_sr, 'Tangency Portfolio Weights', 'max_sr.png')

# Save a scatterplot of portfolio returns vs risks
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
