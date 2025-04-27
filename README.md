# Applications-of-MDPs-to-investment-related-problems


## Numerical Example: Consumption and Investment Optimization

## Description
This repository contains the code and documentation for implementing a numerical example related to consumption and investment optimization, focusing on the use of two utility functions (Power and Exponential) in Markov Decision Processes (MDPs). The goal is to showcase the results from Chapters **Investment Problems** and **Consumption-Investment Problems** of the dissertation.

### Objective
The numerical example aims to solve optimization problems for both an **investment-only problem** and a **consumption-investment problem**, using a binomial model for asset price evolution. The key features of the project include:
- Two utility functions: Power utility and Exponential utility.
- A financial market consisting of two risky assets, with prices modeled using a binomial tree.
- Optimization of consumption and investment strategies using dynamic programming.
- Scenarios are evaluated with constraints on portfolio weights and consumption, as well as transaction costs.

## Numerical Example Overview

This section applies the concepts discussed in Chapters **Investment Problems** and **Consumption-Investment Problems**. The numerical example incorporates two key stages:
1. **Investment-only Problem**: This problem is modeled and solved using the power and exponential utility functions.
2. **Consumption-Investment Problem**: The investment-only problem is extended to consider optimal consumption over time.

### Problem Setup
- The market consists of **two risky assets** (denoted as \(S^1\) and \(S^2\)) and a riskless asset (\(B\)).
- The price process for the **riskless asset** \(B_n\) follows the equation:
  \[
  B_{n+1} = (1 + i_{n+1}) B_n \quad \text{for} \quad n = 0, 1, \dots, 12
  \]
  where \(i_{n+1}\) is the interest rate.

- The price process for the **risky assets** \(S_n^1\) and \(S_n^2\) follows a **binomial model**:
  \[
  u = \exp(\sigma \sqrt{\Delta t}), \quad d = \frac{1}{u}, \quad p = \frac{\exp(\mu \Delta t) - d}{u - d}
  \]
  where \(\sigma^2\) represents the variance of returns and \(\mu\) is the interest rate over the period.

- **Decision Stages**: The investment decision is made over 13 decision stages, each corresponding to a 4-week period.

- The goal is to achieve a **terminal wealth** of €1,000 by time \(N = 13\).

### Simulation
The simulation involves 1000 paths for the risky assets, generated using the binomial model. The resulting price paths for the two assets are used to optimize investment strategies under the power and exponential utility functions.

## Setting up the Environment

### Prerequisites
To run the code, you will need the following tools:
- **R**: This implementation is done using R. You will need to have R installed on your machine.
- **R Packages**: Make sure you have the required R packages installed:
  - `ggplot2` for plotting
  - `dplyr` for data manipulation
  - `tidyverse` for data cleaning
  - `tictoc` for computational time
  - `abind` for handling large data efficiently




