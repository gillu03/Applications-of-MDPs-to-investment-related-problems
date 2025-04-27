# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Applications of Markov Decision Processes to Investment-Related Problems
# Code for Chapter 6 - Numerical Example
# Julian Pace
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(dplyr)
library(abind)
library(tictoc)

set.seed(333)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ----- Simulating interest rate - CIR -----
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# CIR sim parameters
kappa = 0.5 # Speed of mean reversion
theta = 0.05 # Long-term mean level
sigma = 0.05 # Volatility
I0 = 0.05 # Initial value of the process
T = 1 # Time horizon (1 year)
days = 365 # Number of time steps (days in a yr)
dt = T / days # Time step size

# Pre-allocate the I vector
I = numeric(days + 1)
I[1] = I0

# Simulate the CIR process using Euler-Maruyama
for (i in 1:days) {
  dW = sqrt(dt) * rnorm(1)
  I[i + 1] = I[i] + kappa * (theta - I[i]) * dt + sigma * sqrt(abs(I[i])) * dW
  
  # Ensure I stays positive (CIR is supposed to stay ≥ 0)
  I[i + 1] = max(I[i + 1], 0)
}

# Plot the simulated CIR path
time = seq(0, T, by = dt)
plot(time, I, type = "l", col = "blue", lwd = 2,
     main = "CIR Process Simulation",
     xlab = "Time", ylab = "I(t)")
grid()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ---- Simulating asset prices using binomial model ----
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Set up parameters
d = 2                        # Number of risky assets
gamma = 0.1                  # Risk aversion parameter (0 < gamma < 1)
delta = 1 / (1 - gamma)
X_N = 1000                    # Initial wealth
N = 13                       # Number of decision stages (4 week period)
S0 = c(2, 1.5)               # Initial stock prices for asset 1 and 2
sigma = c(0.5, 0.75)         # Volatility
Delta_t = 1 / N              # Time step size
M = 1000                     # Number of paths of price process to be simulated

# Extract interest rate at end of month
indices = seq(28, length(I), by = 28)
#Interest_rates = c(I0, I[indices])

Interest_rates = c(I0, sapply(seq_along(indices), function(i) {
  start_idx = if (i == 1) 1 else (indices[i-1] + 1)
  end_idx = indices[i]
  mean(I[start_idx:end_idx])
}))

# convert interest rates to monthly 
monthly_interest_rates = (1 + Interest_rates)^(1/N) - 1

# Simulating Riskless asset 
B = numeric(N+1)
B[1] = 1

for (n in 1:N) {
  B[n+1] = B[n] * (1 + Interest_rates[n+1])^Delta_t
}

# Initialize storage arrays
S = array(0, dim = c(N + 1, d, M))  # Stock prices
# Set initial prices: Asset 1 -> 1.5, Asset 2 -> 2
S[1, 1, ] = 1.5  # Asset 1 initial price
S[1, 2, ] = 2.0  # Asset 2 initial price

up_factors = array(0, dim = c(N, d, M))  # Up factors
down_factors = array(0, dim = c(N, d, M))  # Down factors
probabilities = array(0, dim = c(N, d, M))

# Simulate M random paths
for (m in 1:M) {  # Loop over simulations
  for (n in 1:N) {  # Loop over time steps
    r_n = Interest_rates[n]  # Interest rate at step n
    
    for (k in 1:d) {  # Loop over each asset
      up_n = exp(sigma[k] * sqrt(Delta_t))  # Up factor
      down_n = 1 / up_n  # Down factor
      
      # Check arbitrage condition 
      if (! (down_n < (1 + r_n)^Delta_t && (1 + r_n)^Delta_t < up_n)) {
        cat("Arbitrage condition violated at step", n, "for asset", k, "\n")
      }
      
      # Store factors
      up_factors[n, k, m] = up_n
      down_factors[n, k, m] = down_n
      
      # Risk-neutral probability
      p_n = (exp(r_n * Delta_t) - down_n) / (up_n - down_n)
      
      # Ensure p_n is valid
      p_n = max(0, min(1, p_n))
      
      probabilities[n,k,m] = p_n
      
      # Simulate stock movement
      S[n + 1, k, m] = S[n, k, m] * ifelse(runif(1) < p_n, up_n, down_n)
    }
  }
}

# Example: Print first simulation factors
print(up_factors[, , 1])    # Up factors for first simulation
print(down_factors[, , 1])  # Down factors for first simulation
print(probabilities[, , 1]) # Probabilities from first simulation

# Extract mean probabilities across 1000 simulations for each asset
mean_probabilities = apply(probabilities, c(1, 2), mean)

# Example: Print first simulation
print(S[, , 1])  # First simulation (all time steps & assets)

r_1_u = up_factors[1,1,1]
r_1_d = down_factors[1,1,1]
r_2_u = up_factors[1,2,1]
r_2_d = down_factors[1,2,1]

p = p_n # risk neutral probability

# Select a subset of simulations to plot (e.g., first 5 simulations)
num_simulations_to_plot = 5  
sim_indices = 1:num_simulations_to_plot  

# Convert the 3D array into a long-format dataframe
price_process = expand.grid(
  time = 0:N, 
  Simulation = sim_indices,
  Asset = c("Asset 1", "Asset 2")
)
# Extract prices for selected simulations and reshape
# Correctly interleave asset prices
price_process$Price = as.vector(aperm(S[, , sim_indices], perm = c(1, 3, 2)))


# Plot multiple simulations
ggplot(price_process, aes(x = time, y = Price, color = as.factor(Simulation), linetype = as.factor(Simulation), group = Simulation)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  facet_wrap(~Asset, scales = "fixed") +  # One plot per asset
  theme_minimal() +
  labs(
    title = "Simulated Binomial Price Paths per Asset",
    x = "Time Step",
    y = "Stock Price",
    color = "Simulation"
  ) +
  scale_color_manual(values = c("red", "blue", "darkgreen", "purple", "orange")) +  # 5 colors
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "twodash")) +
  guides(
    color = guide_legend(title = "Simulation"),
    linetype = guide_legend(title = "Simulation")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Initialize arrays for relative price change and relative risk process
relative_price_change = array(0, dim = c(N, d, M))  # (n, assets, simulations)
relative_risk_process = array(0, dim = c(N, d, M))  # (n, assets, simulations)

# Compute the values for all simulations
for (m in 1:M) {
  for (n in 1:N) {
    for (k in 1:d) {
      # Compute relative price change
      relative_price_change[n, k, m] = S[n + 1, k, m] / S[n, k, m]
      
      # Compute relative risk process
      relative_risk_process[n, k, m] = (relative_price_change[n, k, m] / (1 + Interest_rates[n])^Delta_t) - 1
    }
  }
}

# Create zero arrays with shape (1, d, M)
zero_row = array(0, dim = c(1, d, M))

# Prepend the zero row
relative_price_change = abind::abind(zero_row, relative_price_change, along = 1)
relative_risk_process = abind::abind(zero_row, relative_risk_process, along = 1)

# Example: Print the first simulation's results
print(relative_price_change[, , 1])  # Relative price changes for first simulation
print(relative_risk_process[, , 1])  # Relative risk process for first simulation

# Extract mean of relative risks over 1000 simulations for each asset
relative_risk_mean = apply(relative_risk_process, c(1, 2), mean)
print(relative_risk_mean)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ----- Power Utility - Investment Only -----
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tic()

compute_optimal_alpha_PI = function(i, u, d, p, gamma) {
  # Compute delta
  delta = 1 / (1 - gamma)
  
  # Compute numerator and denominator of the alpha* formula
  num = ((u - (1 + i)^Delta_t)^delta) * (p^delta) - (((1 + i)^Delta_t - d)^delta) * ((1 - p)^delta)
  denom = ((u - (1 + i)^Delta_t)^(delta * gamma)) * (p^delta) + (((1 + i)^Delta_t - d)^(delta * gamma)) * ((1 - p)^delta)
  
  # Compute alpha*
  alpha_star = ((1 + i)^Delta_t / (((1 + i)^Delta_t - d) * (u - (1 - i)^Delta_t))) * (num / denom)
  
  return(alpha_star)
}

alpha_star_PI = mat.or.vec(N,d)
for (n in 1:N) {
  # Extract mean probabilities for each asset at time step n
  p1 = mean_probabilities[n, 1]
  p2 = mean_probabilities[n, 2]
  
  # Compute optimal alpha for each asset and assign to matrix
  alpha_star_PI[n, 1] = compute_optimal_alpha_PI(Interest_rates[n + 1], r_1_u, r_1_d, p1, gamma)
  alpha_star_PI[n, 2] = compute_optimal_alpha_PI(Interest_rates[n + 1], r_2_u, r_2_d, p2, gamma)
}
print(alpha_star_PI)


# Expected value function computation for d_n
v_k_PI = function(alpha_row, R_n1, gamma) {
  M = dim(R_n1)[2]  # Number of simulations
  
  # Compute dot product of alpha_row (1xd) with each column of R_n1 (dxM)
  term = 1 + colSums(alpha_row * R_n1)  # Result: Vector of length M
  
  # Handle negative values to avoid NaN issues
  
  
  # Compute expectation over M simulations
  return(mean(term^gamma, na.rm = TRUE))
}

v_n_PI = numeric(N)  # Initialize vector for v_n values

for (n in 1:N) {
  v_n_PI[n] = v_k_PI(alpha_star_PI[n, ], relative_risk_process[n , , ], gamma)
}

print(v_n_PI)  


# Compute d_n values
compute_d_n_PI = function(N, gamma, i, v_values) {
  d_N = 1 / gamma  # d_N is given
  d_n = numeric(N)
  d_n[N] = d_N  # Set d_N in the array
  
  for (n in (N-1):1) {
    d_n[n] = (1 / gamma) * prod((1 + i[n+1])^(Delta_t*gamma) * v_values[(n+1):N])
  }
  
  return(d_n)
}


# Computing the value function
value_fn_PI = function(x, d_n, gamma) {
  return(d_n * x^gamma)
}

# Calculate Recusion
d_n_PI = compute_d_n_PI(N, gamma, Interest_rates, v_n_PI)

# Wealth process
X_PI = numeric(N)
X_PI[N] = X_N


for(n in (N-1):1){
  X_PI[n] = X_PI[n+1] / ((1 + Interest_rates[n+1])^Delta_t * (1 + alpha_star_PI[n,] %*%  relative_risk_mean[n + 1, ] ))
}


# Calculate Value Process using V_n(x) = 1/gamma * x^gamma
V_n_PI = value_fn_PI(X_PI, d_n_PI, gamma)


# Find optimal portfolio f_n(x) = alpha_n * x_n
optimal_portfolio_PI = mat.or.vec(N,2)
for(n in 1:N){
  optimal_portfolio_PI[n,] = X_PI[n] %*% alpha_star_PI[n,]
}

# Combine everything into a single data frame
portfolio_results_PI = data.frame(
  Time = 1:N,
  Wealth = X_PI,
  v_n = v_n_PI,
  d_n = d_n_PI,
  `Portfolio Asset 1` = optimal_portfolio_PI[, 1],
  `Portfolio Asset 2` = optimal_portfolio_PI[, 2],
  `Optimal Alpha Asset 1` = alpha_star_PI[, 1],
  `Optimal Alpha Asset 2` = alpha_star_PI[, 2]
)

colnames(portfolio_results_PI) = c("Time", "Wealth", "v_n", "d_n", "Optimal Allocation Asset 1", "Optimal Allocation Asset 2",
                                   "Optimal Alpha Asset 1", "Optimal Alpha Asset 2")

print(portfolio_results_PI)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Plot Wealth Process
ggplot(portfolio_results_PI, aes(x = Time, y = Wealth)) +
  geom_line(color = "blue", linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Wealth Process Over Time - Power Utility Investment Only",
    x = "Time Step",
    y = "Wealth"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Pivot data for asset allocation
allocation_data_PI = portfolio_results_PI %>%
  dplyr::select(Time, `Optimal Allocation Asset 1`, `Optimal Allocation Asset 2`) %>%
  pivot_longer(cols = -Time, names_to = "Asset", values_to = "Allocation")

ggplot(allocation_data_PI, aes(x = Time, y = Allocation, color = Asset)) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Optimal Asset Allocation Over Time - Power Utility Investment Only",
    x = "Time Step",
    y = "Allocation",
    color = "Asset"
  ) +
  scale_color_manual(values = c("Optimal Allocation Asset 1" = "darkgreen", "Optimal Allocation Asset 2" = "darkred")) +
  theme(plot.title = element_text(hjust = 0.5))

toc()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ----- Power Utility - Consumption Investment -----
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tic()

compute_optimal_alpha_PCI = function(i, u, d, p, gamma) {
  # Compute delta
  delta = 1 / (1 - gamma)
  
  # Compute numerator and denominator of the alpha* formula
  num = (u - (1 + i)^Delta_t)^delta * p^delta - ((1 + i)^Delta_t - d)^delta * (1 - p)^delta
  denom = (u - (1 + i)^Delta_t)^(delta * gamma) * p^delta + ((1 + i)^Delta_t - d)^(delta * gamma) * (1 - p)^delta
  
  # Compute alpha*
  alpha_star = (1 + i)^Delta_t / (((1 + i)^Delta_t - d) * (u - (1 + i)^Delta_t)) * (num / denom)
  
  return(alpha_star)
}

compute_optimal_a_PCI = function(x, d_n, gamma, alpha) {
  delta = 1 / (1 - gamma)
  gd_n = (gamma * d_n)^delta
  # optimal allocation
  a_star = x * ((gd_n - 1) / gd_n) * alpha
  
  return(a_star)
}

compute_optimal_c_PCI = function(x, d_n, gamma) {
  delta = 1 / (1 - gamma)
  # optimal consumption
  c_star = x * (gamma * d_n)^(-delta)
  
  return(c_star)
}


v_k_PCI = function(alpha_row, R_n1, gamma) {
  M = dim(R_n1)[2]  # Number of simulations
  
  # Compute dot product of alpha_row (1xd) with each column of R_n1 (dxM)
  term = 1 + colSums(alpha_row * R_n1)  # Result: Vector of length M
  
  # Handle negative values to avoid NaN issues
  
  
  # Compute expectation over M simulations
  return(mean(term^gamma, na.rm = TRUE))
}

# Computing the value function
value_fn_PCI = function(x, d_n, gamma) {
  return(d_n * x^gamma)
}

# Compute d_n values
compute_d_n_PCI = function(N, gamma, i, v_values) {
  d_N = 1 / gamma  # d_N is given
  d_n = numeric(N)
  d_n[N] = d_N  # Set d_N in the array
  
  delta = 1 / (1 - gamma)
  
  for (n in (N-1):1) {
    d_n[n] = ((gamma^(-delta)) + (( (1 + i[n+1])^(Delta_t*gamma) * v_values[n] )^delta * d_n[n+1]^delta))^(1 / delta)
  }
  
  return(d_n)
}

# optimal alpha
alpha_star_PCI = mat.or.vec(N,d)
for (n in 1:N) {
  # Extract mean probabilities for each asset at time step n
  p1 = mean_probabilities[n, 1]
  p2 = mean_probabilities[n, 2]
  
  # Compute optimal alpha for each asset and assign to matrix
  alpha_star_PCI[n, 1] = compute_optimal_alpha_PCI(Interest_rates[n + 1], r_1_u, r_1_d, p1, gamma)
  alpha_star_PCI[n, 2] = compute_optimal_alpha_PCI(Interest_rates[n + 1], r_2_u, r_2_d, p2, gamma)
}
print(alpha_star_PCI)


# v_n for d_n computation
v_n_PCI = numeric(N)
for(n in 1:N){
  v_n_PCI[n] = v_k_PCI(alpha_star_PCI[n,], relative_risk_process[n , , ], gamma)
}
print(v_n_PCI)

# d_n 
d_n_PCI = compute_d_n_PCI(N, gamma, Interest_rates, v_n_PCI)
print(d_n_PCI)

# optimal consumption and investment as well as wealth process
c_star_PCI = numeric(N)
a_star_PCI = mat.or.vec(N,d)
X_PCI = numeric(N)

c_star_PCI[N] = X_N
X_PCI[N] = X_N

for (n in (N-1):1){
  X_PCI[n] = (1 / (1 - (gamma * d_n_PCI[n])^-delta)) * ((X_PCI[n+1] / (1 + Interest_rates[n+1])^Delta_t) - a_star_PCI[n,] %*% relative_risk_mean[n+1,])
}
print(X_PCI)

for (n in (N-1):1) {
  c_star_PCI[n] = compute_optimal_c_PCI(X_PCI[n], d_n_PCI[n], gamma)
  a_star_PCI[n,] = compute_optimal_a_PCI(X_PCI[n], d_n_PCI[n], gamma, alpha_star_PCI[n,])
  
}

print(c_star_PCI)
print(a_star_PCI)


# value functions V_n
V_n_PCI = value_fn_PCI(X_PCI, d_n_PCI, gamma)
print(V_n_PCI)

# optimal portfolio
optimal_portfolio_PCI = cbind(c_star_PCI, a_star_PCI[,1], a_star_PCI[,2])
colnames(optimal_portfolio_PCI) = c("Optimal Consumtpion", "Optimal Allocation Asset 1", 
                                    "Optimal Allocation Asset 2")

# Combine everything into a single data frame
portfolio_results_PCI = data.frame(
  Time = 1:N,
  Wealth = X_PCI,
  v_n = v_n_PCI,
  d_n = d_n_PCI,
  `Optimal Consumption` = optimal_portfolio_PCI[, 1],
  `Portfolio Asset 1` = optimal_portfolio_PCI[, 2],
  `Portfolio Asset 2` = optimal_portfolio_PCI[, 3],
  `Optimal Alpha Asset 1` = alpha_star_PCI[, 1],
  `Optimal Alpha Asset 2` = alpha_star_PCI[, 2]
)

colnames(portfolio_results_PCI) = c("Time", "Wealth", "v_n", "d_n", "Optimal Consumption", "Optimal Allocation Asset 1", "Optimal Allocation Asset 2",
                                    "Optimal Alpha Asset 1", "Optimal Alpha Asset 2")

portfolio_results_PCI$`Cumulative Consumption` <- cumsum(portfolio_results_PCI$`Optimal Consumption`)

print(portfolio_results_PCI)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Reshape data to long format for easy plotting
plot_wealth_cons_PCI <- portfolio_results_PCI %>%
  select(Time, Wealth, `Cumulative Consumption`) %>%
  pivot_longer(cols = c(Wealth, `Cumulative Consumption`), names_to = "Variable", values_to = "Value")

# Plot
ggplot(plot_wealth_cons_PCI, aes(x = Time, y = Value, color = Variable)) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Wealth and Cumulative Consumption Over Time - Power Utility",
    x = "Time Step",
    y = "Value"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("red", "blue"))  # Custom colors for the two lines


# Pivot data for asset allocation
allocation_data_PCI = portfolio_results_PCI %>%
  dplyr::select(Time, `Optimal Allocation Asset 1`, `Optimal Allocation Asset 2`) %>%
  pivot_longer(cols = -Time, names_to = "Asset", values_to = "Allocation")

ggplot(allocation_data_PCI, aes(x = Time, y = Allocation, color = Asset)) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Optimal Asset Allocation Over Time - Power Utility Consumption-Investment",
    x = "Time Step",
    y = "Allocation",
    color = "Asset"
  ) +
  scale_color_manual(values = c("Optimal Allocation Asset 1" = "darkgreen", "Optimal Allocation Asset 2" = "darkred")) +
  theme(plot.title = element_text(hjust = 0.5))



toc()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ----- Exponential Utility - Investment Only -----
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tic()

compute_optimal_atilde_EI = function(i, u, d, p) {
  # Compute q
  q = ((1 + i)^Delta_t - d) / (u - d)
  
  # Compute numerator and denominator of the alpha* formula
  num = log((1 - q) / (1 - p)) - log(q / p)
  denom = u - d
  
  # Compute alpha*
  atilde_star = (1 + i)^Delta_t * (num / denom)
  
  return(atilde_star)
}

a_tilde_EI = mat.or.vec(N,d)
for (n in 1:N) {
  # Extract mean probabilities for each asset at time step n
  p1 = mean_probabilities[n, 1]
  p2 = mean_probabilities[n, 2]
  
  # Compute optimal alpha for each asset and assign to matrix
  a_tilde_EI[n, 1] = compute_optimal_atilde_EI(Interest_rates[n + 1], r_1_u, r_1_d, p1)
  a_tilde_EI[n, 2] = compute_optimal_atilde_EI(Interest_rates[n + 1], r_2_u, r_2_d, p2)
  
}
print(a_tilde_EI)


compute_optimal_a_EI = function(gamma, S_n, S_N, atilde) {
  a_star = (1 / gamma) * (S_n / S_N) * atilde
  
  return(a_star)
}

a_star_EI = mat.or.vec(N,d)
for (n in 1:N) {
  a_star_EI[n,] = compute_optimal_a_EI(gamma, B[n], B[N], a_tilde_EI[n,])
}
print(a_star_EI)


# Expected value function computation for d_n - change for exponential utility
v_k_EI = function(a_row, R_n1, gamma, S_n_0, S_N_0) {
  M = dim(R_n1)[2]  # Number of simulations
  
  # Compute the term inside the expectation (exponential utility)
  exp_term = exp(-gamma * (S_N_0 / S_n_0) * colSums(a_row * R_n1))  # Vector of length M
  
  # Compute the expectation (mean) over M simulations
  return(mean(exp_term, na.rm = TRUE))
}

v_n_EI = numeric(N)  # Initialize vector for v_n values
for (n in 1:N) {
  v_n_EI[n] = v_k_EI(a_star_EI[n, ], relative_risk_process[n , , ], gamma, B[n], B[N])
}
print(v_n_EI)  

# Compute d_n values - change for exponential utility
compute_d_n_EI = function(N, gamma, v_values) {
  d_N = - 1 / gamma  # d_N is given
  d_n = numeric(N)
  d_n[N] = d_N  # Set d_N in the array
  
  for (n in (N-1):1) {
    # Compute the product of v_k values from n to N-1
    v_product = prod(v_values[n:(N-1)])  # Product of v_k values from n to N-1
    
    # Compute d_n using the new definition
    d_n[n] = -1 / gamma * v_product
  }
  
  return(d_n)
}

d_n_EI = compute_d_n_EI(N, gamma, v_n_EI)

# Wealth process
X_EI = numeric(N)
X_EI[N] = X_N

for(n in (N-1):1){
  X_EI[n] = X_EI[n+1] / (1 + Interest_rates[n+1])^Delta_t - (a_star_EI[n,] %*%  relative_risk_mean[n + 1, ] )
}

# Computing the value function - change for exponential utility
value_fn_EI = function(x, d_n, gamma, S_n_0, S_N_0) {
  return(d_n * exp(- gamma * (S_N_0 / S_n_0) * x))
}

# Calculate Value Process using V_n(x) = d_n * exp(- gamma * (S_N_0 / S_n_0) * x)
V_n_EI = numeric(N)
for (n in 1:N) {
  V_n_EI[n] = value_fn_EI(X_EI[n], d_n_EI[n], gamma, B[n], B[N])
}
print(V_n_EI)


# Find optimal portfolio f_n(x) = alpha_n * x_n
optimal_portfolio_EI = mat.or.vec(N,2)
for(n in 1:N){
  optimal_portfolio_EI[n,] = a_star_EI[n, ]
}

# Combine everything into a single data frame
portfolio_results_EI = data.frame(
  Time = 1:N,
  Wealth = X_EI,
  v_n = v_n_EI,
  d_n = d_n_EI,
  `Portfolio Asset 1` = optimal_portfolio_EI[, 1],
  `Portfolio Asset 2` = optimal_portfolio_EI[, 2],
  `atilde Asset 1` = a_tilde_EI[, 1],
  `atilde Asset 2` = a_tilde_EI[, 2]
)

colnames(portfolio_results_EI) = c("Time", "Wealth", "v_n", "d_n","Optimal Allocation Asset 1", "Optimal Allocation Asset 2",
                                   "Optimal a_tilde Asset 1", "Optimal a_tilde Asset 2")

print(portfolio_results_EI)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Plot Wealth Process
ggplot(portfolio_results_EI, aes(x = Time, y = Wealth)) +
  geom_line(color = "blue", linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Wealth Process Over Time - Exponential Utility Investment Only",
    x = "Time Step",
    y = "Wealth"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


# Pivot data for asset allocation
allocation_data_EI = portfolio_results_EI %>%
  dplyr::select(Time, `Optimal Allocation Asset 1`, `Optimal Allocation Asset 2`) %>%
  pivot_longer(cols = -Time, names_to = "Asset", values_to = "Allocation")

ggplot(allocation_data_EI, aes(x = Time, y = Allocation, color = Asset)) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Optimal Asset Allocation Over Time - Exponential Utility Investment Only",
    x = "Time Step",
    y = "Allocation",
    color = "Asset"
  ) +
  scale_color_manual(values = c("Optimal Allocation Asset 1" = "darkgreen", "Optimal Allocation Asset 2" = "darkred")) +
  theme(plot.title = element_text(hjust = 0.5))

toc()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ----- Exponential Utility - Consumption Investment -----
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tic()

compute_optimal_atilde_ECI = function(i, u, d, p) {
  # Compute q
  q = ((1 + i)^Delta_t - d) / (u - d)
  
  # Compute numerator and denominator of the alpha* formula
  num = log((1 - q) / (1 - p)) - log(q / p)
  denom = u - d
  
  # Compute alpha*
  atilde_star = (1 + i)^Delta_t * (num / denom)
  
  return(atilde_star)
}

a_tilde_ECI = mat.or.vec(N,d)
for (n in 1:N) {
  # Extract mean probabilities for each asset at time step n
  p1 = mean_probabilities[n, 1]
  p2 = mean_probabilities[n, 2]
  
  # Compute optimal alpha for each asset and assign to matrix
  a_tilde_ECI[n, 1] = compute_optimal_atilde_ECI(Interest_rates[n + 1], r_1_u, r_1_d, p1)
  a_tilde_ECI[n, 2] = compute_optimal_atilde_ECI(Interest_rates[n + 1], r_2_u, r_2_d, p2)
  
}
print(a_tilde_ECI)



compute_optimal_a_ECI = function(gamma, S_n, S_N, atilde) {
  a_star = (1 / gamma) * (S_n / S_N) * atilde
  
  return(a_star)
}

# Expected value function computation for d_n
v_k_ECI = function(a_row, R_n1, gamma, S_n_0, S_N_0) {
  M = dim(R_n1)[2]  # Number of simulations
  
  # Compute the term inside the expectation (exponential utility)
  exp_term = exp(-gamma * (S_N_0 / S_n_0) * colSums(a_row * R_n1))  # Vector of length M
  
  # Compute the expectation (mean) over M simulations
  return(mean(exp_term, na.rm = TRUE))
}



# Recursive function to compute b_n
compute_b_n = function(N, gamma, i) {
  b_n = numeric(N)
  b_n[N] = gamma  # b_N = gamma
  
  for (n in (N-1):1) {
    b_n1 = b_n[n + 1]
    i_n1 = i[n + 1]
    
    b_n[n] = (b_n1 * (1 + i_n1)^Delta_t * gamma) / (b_n1 * (1 + i_n1)^Delta_t + gamma)
  }
  
  return(b_n)
}

# Recursive function to compute k_n
compute_k_n = function(N, gamma, i, v_values, b_n) {
  k_n = numeric(N)
  k_n[N] = 1 / gamma  # k_N = 1 / gamma
  
  for (n in (N-1):1) {
    b_n1 = b_n[n + 1]
    i_n1 = i[n + 1]
    v_n = v_values[n]
    
    term1 = (k_n[n + 1] * b_n1 * (1 + i_n1)^Delta_t * v_n)^(gamma / (b_n1 * (1 + i_n1)^Delta_t + gamma))
    term2 = k_n[n + 1] * v_n * (k_n[n + 1] * b_n1 * (1 + i_n1)^Delta_t * v_n)^(-b_n1 * (1 + i_n1)^Delta_t / (b_n1 * (1 + i_n1)^Delta_t + gamma))
    
    k_n[n] = (1 / gamma) * (term1 + term2)
  }
  
  return(k_n)
}

compute_zeta_star_ECI = function(b_n, i, x, k_n, v_n, gamma) {
  num = b_n * (1 + i)^Delta_t * x - log(k_n * b_n * (1 + i)^Delta_t * v_n)
  den = (gamma + b_n * (1 + i)) * x
  
  zeta = num / den
  
  return(zeta)
}

compute_optimal_c_ECI = function(zeta, x) {
  return(zeta * x)
}


a_star_ECI = mat.or.vec(N,d)
for (n in (N-1):1) {
  a_star_ECI[n, ] = compute_optimal_a_ECI(gamma, B[n], B[N], a_tilde_ECI[n,])
}
print(a_star_ECI)

v_n_ECI = numeric(N)  # Initialize vector for v_n values
for (n in 1:N) {
  v_n_ECI[n] = v_k_ECI(a_star_ECI[n, ], relative_risk_process[n , , ], gamma, B[n], B[N])
}
print(v_n_ECI) 

b_n_ECI = compute_b_n(N, gamma, Interest_rates)
k_n_ECI = compute_k_n(N, gamma, Interest_rates, v_n_ECI, b_n_ECI)

print(b_n_ECI)
print(k_n_ECI)

# optimal consumption and investment as well as wealth process
c_star_ECI = numeric(N)
X_ECI = numeric(N)
zeta_star_ECI = numeric(N)

c_star_ECI[N] = X_N
X_ECI[N] = X_N

for (n in (N-1):1) {
  X_ECI[n] = ((gamma + b_n_ECI[n+1] *(1 + Interest_rates[n+1])^Delta_t) / gamma) * 
    ( (X_ECI[n+1] / (1 + Interest_rates[n+1])^Delta_t) - (a_star_ECI[n,] %*% relative_risk_mean[n+1,]) + 
       (log(k_n_ECI[n+1] * b_n_ECI[n+1] * (1 + Interest_rates[n+1])^Delta_t * v_n_ECI[n]) / (gamma + b_n_ECI[n+1] *(1 + Interest_rates[n+1])^Delta_t)) )
}

for (n in (N-1):1) {
  zeta_star_ECI[n] = compute_zeta_star_ECI(b_n_ECI[n+1], Interest_rates[n+1], X_ECI[n], k_n_ECI[n+1], v_n_ECI[n], gamma)
  c_star_ECI[n] = compute_optimal_c_ECI(zeta_star_ECI[n], X_ECI[n])
  
}

print(X_ECI)
print(zeta_star_ECI)
print(c_star_ECI)


# Computing the value function
value_fn_ECI = function(k_n, b_n, x) {
  return(-k_n * exp(-b_n * x))  # V_n(x) = -k_n * exp(-b_n * x)
}


# value functions V_n
V_n_ECI = value_fn_ECI(k_n_ECI, b_n_ECI, X_ECI)
print(V_n_ECI)

# optimal portfolio
optimal_portfolio_ECI = cbind(c_star_ECI, a_star_ECI[,1], a_star_ECI[,2])
colnames(optimal_portfolio_ECI) = c("Optimal Consumtpion", "Optimal Allocation Asset 1", 
                                    "Optimal Allocation Asset 2")

# Combine everything into a single data frame
portfolio_results_ECI = data.frame(
  Time = 1:N,
  Wealth = X_ECI,
  v_n = v_n_ECI,
  k_n = k_n_ECI,
  b_n = b_n_ECI,
  zeta_n = zeta_star_ECI,
  `Optimal Consumption` = optimal_portfolio_ECI[, 1],
  `Portfolio Asset 1` = optimal_portfolio_ECI[, 2],
  `Portfolio Asset 2` = optimal_portfolio_ECI[, 3],
  `atilde Asset 1` = a_tilde_EI[, 1],
  `atilde Asset 2` = a_tilde_EI[, 2]
)

colnames(portfolio_results_ECI) = c("Time", "Wealth", "v_n", "k_n", "b_n", "zeta_n", "Optimal Consumption", "Optimal Allocation Asset 1", "Optimal Allocation Asset 2",
                                    "Optimal a_tilde Asset 1", "Optimal a_tilde Asset 2")

portfolio_results_ECI$`Cumulative Consumption` <- cumsum(portfolio_results_ECI$`Optimal Consumption`)


print(portfolio_results_ECI)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Reshape data to long format for easy plotting
plot_wealth_cons_ECI <- portfolio_results_ECI %>%
  select(Time, Wealth, `Cumulative Consumption`) %>%
  pivot_longer(cols = c(Wealth, `Cumulative Consumption`), names_to = "Variable", values_to = "Value")

# Plot
ggplot(plot_wealth_cons_ECI, aes(x = Time, y = Value, color = Variable)) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Wealth and Cumulative Consumption Over Time - Exponential Utility",
    x = "Time Step",
    y = "Value"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("red", "blue"))  # Custom colors for the two lines



# Pivot data for asset allocation
allocation_data_ECI = portfolio_results_ECI %>%
  dplyr::select(Time, `Optimal Allocation Asset 1`, `Optimal Allocation Asset 2`) %>%
  pivot_longer(cols = -Time, names_to = "Asset", values_to = "Allocation")

ggplot(allocation_data_ECI, aes(x = Time, y = Allocation, color = Asset)) +
  geom_line(linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Optimal Asset Allocation Over Time - Exponential Utility Consumption-Investment",
    x = "Time Step",
    y = "Allocation",
    color = "Asset"
  ) +
  scale_color_manual(values = c("Optimal Allocation Asset 1" = "darkgreen", "Optimal Allocation Asset 2" = "darkred")) +
  theme(plot.title = element_text(hjust = 0.5))


toc()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#  ----- Additional Plots - For Comparison Section -----
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Wealth 

wealth_df = data.frame(Time = 1:N, 
                       `Wealth PI` = portfolio_results_PI$Wealth,
                       `Wealth PcI` = portfolio_results_PCI$Wealth,
                       `Wealth EI` = portfolio_results_EI$Wealth,
                       `Wealth ECI` = portfolio_results_ECI$Wealth)
colnames(wealth_df) = c("Time", "Wealth PI", "Wealth PCI", "Wealth EI", "Wealth ECI")

print(wealth_df)

ggplot(wealth_df, aes(x = Time)) +
  geom_line(aes(y = `Wealth PI`, color = "Wealth PI"), linewidth = 1.2) +
  geom_line(aes(y = `Wealth EI`, color = "Wealth EI"), linewidth = 1.2) +
  labs(title = "Wealth Dynamics for Different Utility Functions",
       x = "Time",
       y = "Wealth") +
  scale_color_manual(name = "Portfolio", values = c("Wealth PI" = "darkgreen", "Wealth EI" = "darkred")) +
  theme_minimal()

ggplot(wealth_df, aes(x = Time)) +
  geom_line(aes(y = `Wealth PCI`, color = "Wealth PCI"), linewidth = 1.2) +
  geom_line(aes(y = `Wealth ECI`, color = "Wealth ECI"), linewidth = 1.2) +
  labs(title = "Wealth Dynamics for Different Utility Functions",
       x = "Time",
       y = "Wealth") +
  scale_color_manual(name = "Portfolio", values = c("Wealth PCI" = "darkgreen", "Wealth ECI" = "darkred")) +
  theme_minimal()

# Allocations

allocation_df = data.frame(Time = 1:N,
                           PIA1 = portfolio_results_PI$`Optimal Allocation Asset 1`,
                           PIA2 = portfolio_results_PI$`Optimal Allocation Asset 2`,
                           PCIA1 = portfolio_results_PCI$`Optimal Allocation Asset 1`,
                           PCIA2 = portfolio_results_PCI$`Optimal Allocation Asset 2`,
                           EIA1 = portfolio_results_EI$`Optimal Allocation Asset 1`,
                           EIA2 = portfolio_results_EI$`Optimal Allocation Asset 2`,
                           ECIA1 = portfolio_results_ECI$`Optimal Allocation Asset 1`,
                           ECIA2 = portfolio_results_ECI$`Optimal Allocation Asset 2`
                           )

colnames(allocation_df) = c("Time", "PI - Optimal Allocation Asset 1", "PI - Optimal Allocation Asset 2", "PCI - Optimal Allocation Asset 1", "PCI - Optimal Allocation Asset 2",
                            "EI - Optimal Allocation Asset 1", "EI - Optimal Allocation Asset 2", "ECI - Optimal Allocation Asset 1", "ECI - Optimal Allocation Asset 2")
print(allocation_df)

ggplot(allocation_df, aes(x = Time)) +
  geom_line(aes(y = `PI - Optimal Allocation Asset 1`, color = "PI - Optimal Allocation Asset 1"), linewidth = 1.2) +
  geom_line(aes(y = `PI - Optimal Allocation Asset 2`, color = "PI - Optimal Allocation Asset 2"), linewidth = 1.2) +
  geom_line(aes(y = `EI - Optimal Allocation Asset 1`, color = "EI - Optimal Allocation Asset 1"), linewidth = 1.2) +
  geom_line(aes(y = `EI - Optimal Allocation Asset 2`, color = "EI - Optimal Allocation Asset 2"), linewidth = 1.2) +
  labs(title = "Optimal Allocations for Different Utility Functions",
       x = "Time",
       y = "Allocation") +
  scale_color_manual(name = "Portfolio", values = c("PI - Optimal Allocation Asset 1" = "darkgreen", "PI - Optimal Allocation Asset 2" = "darkred", 
                                                    "EI - Optimal Allocation Asset 1" = "darkblue", "EI - Optimal Allocation Asset 2" = "purple")) +
  theme_minimal()

ggplot(allocation_df, aes(x = Time)) +
  geom_line(aes(y = `PCI - Optimal Allocation Asset 1`, color = "PCI - Optimal Allocation Asset 1"), linewidth = 1.2) +
  geom_line(aes(y = `PCI - Optimal Allocation Asset 2`, color = "PCI - Optimal Allocation Asset 2"), linewidth = 1.2) +
  geom_line(aes(y = `ECI - Optimal Allocation Asset 1`, color = "ECI - Optimal Allocation Asset 1"), linewidth = 1.2) +
  geom_line(aes(y = `ECI - Optimal Allocation Asset 2`, color = "ECI - Optimal Allocation Asset 2"), linewidth = 1.2) +
  labs(title = "Optimal Allocations for Different Utility Functions",
       x = "Time",
       y = "Allocation") +
  scale_color_manual(name = "Portfolio", values = c("PCI - Optimal Allocation Asset 1" = "darkgreen", "PCI - Optimal Allocation Asset 2" = "darkred", 
                                                    "ECI - Optimal Allocation Asset 1" = "darkblue", "ECI - Optimal Allocation Asset 2" = "purple")) +
  theme_minimal()


# Consumption

consumption_df = data.frame(Time = 1:N,
                            PCIC = portfolio_results_PCI$`Optimal Consumption`,
                            ECIC = portfolio_results_ECI$`Optimal Consumption`)
colnames(consumption_df) = c("Time", "PCI - Optimal Consumption", "ECI - Optimal Consumption")
print(consumption_df)


ggplot(consumption_df, aes(x = Time)) +
  geom_line(aes(y = `PCI - Optimal Consumption`, color = "PCI - Optimal Consumption"), linewidth = 1.2) +
  geom_line(aes(y = `ECI - Optimal Consumption`, color = "ECI - Optimal Consumption"), linewidth = 1.2) +
  labs(title = "Optimal Consumption for Different Utility Functions",
       x = "Time",
       y = "Consumption") +
  scale_color_manual(name = "Portfolio", values = c("PCI - Optimal Consumption" = "darkgreen", "ECI - Optimal Consumption" = "darkred")) +
  theme_minimal()










