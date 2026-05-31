# Probability and Statistics Project

This repository contains an academic project developed for the Probability and Statistics course at the Faculty of Mathematics and Computer Science (University of Bucharest). The project focuses on computational statistics, probabilistic modeling, and interactive data visualizations using **R**.

## Overview

The project is divided into two main computational problems:

### 1. Rare Events Detection and Simulation (`problema1_simulare.R` & `problema1_monte_carlo.R`)
Simulates an IT system that receives thousands of daily access requests, a very small fraction of which are suspicious (potentially malicious). The goal is to evaluate the efficiency of different manual verification strategies.

* **Probabilistic Model**: Uses the Poisson distribution for total daily traffic, Binomial distribution for the number of suspicious requests, and Hypergeometric distribution for detection (sampling without replacement).
* **Strategies Evaluated**:
  * **Strategy A (Fixed)**: A baseline approach that randomly verifies a fixed 10% of requests every day.
  * **Strategy B (Adaptive)**: Dynamically adjusts the verification rate (10%, 20%, or 30%) when daily traffic spikes above the mean by 1 or 2 standard deviations.
* **Cost Analysis & Monte Carlo**: Includes a 1,000-iteration Monte Carlo simulation to prove that the adaptive strategy is economically superior and statistically robust.

### 2. Random Variable Transformations (`problema2.R`)
An interactive web application built with **Shiny** to study and visualize random variable transformations.

* **1D Transformations**: Apply functions like $Y = X^2$, $Y = \ln(X)$, $Y = e^X$, or $Y = |X|$ to standard distributions (Normal, Exponential, Uniform, Gamma). Features side-by-side histograms, theoretical densities, and empirical statistics.
* **2D Transformations**: Generates independent or correlated pairs using a Bivariate Normal distribution (`mvtnorm`) to study operations like $Z = X + Y$ or $Z = X \cdot Y$.

## Tech Stack

* **Language**: R
* **Core Libraries**: `tidyverse` (specifically `ggplot2` and `dplyr`), `patchwork` (for plot layouts).
* **Shiny App Libraries**: `shiny`, `bslib` (for UI styling), `plotly` (for interactive plots), `mvtnorm` (for multivariate generation).

## How to Run

1. Make sure you have **R** and optionally **RStudio** installed.
2. Install the required dependencies:
   ```R
   install.packages(c("tidyverse", "patchwork", "shiny", "bslib", "plotly", "mvtnorm", "gganimate", "gifski"))
   ```
3. **For Problem 1**: Run `problema1_simulare.R` to generate the plots and efficiency metrics. Run `problema1_monte_carlo.R` to run the 1,000-iteration cost stability simulation.
4. **For Problem 2**: Open `problema2.R` and click **"Run App"** (in RStudio) or run `shiny::runApp("problema2.R")` in your console.

## Documentation
A full and detailed report, including mathematical foundations and detailed chart analyses, is available in the PDF file: `ProjectReport.pdf`.
