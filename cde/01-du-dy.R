# Program: 01-du-dy.R
# Purpose: Replicate figure 2 in Ljungqvist and Sargent's "The Fundamental
# Surplus," AER 2017 for different matching technologies.
# 
# The code uses functional programming. See, in particular, ch 10, "Function
# factories," in Advanced R, 2nd ed, by Hadley Wickham.
# 
# Date Started: 2021-10-06
# Date Revised: 2021-10-06

library(tidyverse)
library(ggthemes)
library(latex2exp)
library(assertr)
library(cowplot)
library(here)

file_prg <- "01-du-dy"
file_out <- c("out")

# Golden ratio plotting parameters
mywidth <- 6
golden <- 0.5*(1 + sqrt(5))
myheight <- mywidth / golden


# Deep parameters ---------------------------------------------------------

z <- 0.6
alpha <- 0.5
pgamma <- 1.27
phi <- 0.5
beta <- 0.95^(1/365) # = 1 / (1+r)
r <- 1 / beta - 1
s <- 0.001
c <- 0.1
TOL <- 0.000001

# Targeted unemployment rate
ur_target <- 0.05

# Auxiliary functions -----------------------------------------------------

fmake_find_NL <- function(A) {
  # Input: Matching-technology parameter, A 
  # Output: Function that takes as input tightness and returns the job-finding
  # probability
  force(A)
  function(theta) {
    A*theta/((1+theta^pgamma)^(1/pgamma))
  }
}

fmake_fill_NL <- function(A) {
  # Input: Matching-technology parameter, A 
  # Output: Function that takes as input tightness and returns the job-filling
  # probability
  force(A)
  function(theta) {
    A/((1+theta^pgamma)^(1/pgamma))
  }
}

fmake_find_CD <- function(A) {
  # Input: Matching-technology parameter, A 
  # Output: Function that takes as input tightness and returns the job-finding
  # probability
  force(A)
  function(theta) {
    A*theta^(1-alpha)
  }
}

fmake_fill_CD <- function(A) {
  # Input: Matching-technology parameter, A 
  # Output: Function that takes as input tightness and returns the job-filling
  # probability
  force(A)
  function(theta) {
    A*theta^(-alpha)
  }
}

fmake_zero <- function(A, y, fmake_find, fmake_fill) {
  # Inputs: 
  #   - Matching-technology parameter, A
  #   - productivity, y
  #   - function that makes fill function (takes theta as input)
  #   - function that makes find function (takes theta as input)
  # Output: Function that takes as input tightness and returns the zero
  # equilibrium condition.
  force(A)
  force(y)
  fun_fill <- fmake_fill(A)
  fun_find <- fmake_find(A)
  function(theta) {
    # Probability find and fill
    pfind <- fun_find(theta)
    pfill <- fun_fill(theta)
    # Zero condition: Equation 12 in the text, p 2635
    LHS <- y - z
    RHS <- (r + s + phi * pfind) / ((1 - phi) * pfill) * c
    zero_condition <- LHS - RHS
  }
}

fmake_get_A <- function(y, fmake_find, fmake_fill) {
  force(y)
  function(A) {
    force(A)
    fun_get_theta <- fmake_zero(A, y, fmake_find, fmake_fill)
    # Search over A, the first argument
    sol_eqm_tight <- uniroot(function(x)
      fun_get_theta(x)[[1]],
      c(0.0001, (1-phi)*(y-z)/c/phi),
      tol = TOL,
      extendInt = "yes",
      maxiter = 10000)
    eqm_tight <- sol_eqm_tight$root
    
    # Compute implied unemployment rate
    fun_find <- fmake_find(A)
    pfind <- fun_find(eqm_tight)
    ur <- s / (s + pfind)
    
    # Returns A and the associated level of tightness
    ret <- c(diff0 = ur - ur_target,
             eqm_tight = eqm_tight)
    return(ret)
  }
}


# Vary match efficiency to target steady-state unemployment rate ----------

# Productivity parameters
ECONOMY_Y <- c(0.61, 0.63, 0.65)
# Preallocate matching efficiency for Cobb--Douglas
ECONOMY_A_CD <- vector(mode = "double", length = length(ECONOMY_Y))
# Preallocate matching efficiency for nonlinear
ECONOMY_A_NL <- vector(mode = "double", length = length(ECONOMY_Y))

# For given y, make the function that takes A as input and and searches for the
# equilibrium unemployment rate
for (ii in 1:length(ECONOMY_Y)) {
  # Economy with Cobb-Douglas matching technology
  print(paste0("=== Solving Cobb-Douglas economy: ", ECONOMY_Y[[ii]]))
  fun_get_A <- fmake_get_A(ECONOMY_Y[[ii]], fmake_find_CD, fmake_fill_CD)
  # Look only over the first argument of fun_get_A
  sol_A <-
    uniroot(function(x)
      fun_get_A(x)[[1]], c(0.001, 2.0), tol = TOL)
  # A associated with 0.05 percent unemployment rate
  ECONOMY_A_CD[[ii]] <- sol_A$root

  # Economy with Nonlinear matching technology
  print(paste0("=== Solving nonlinear economy: ", ECONOMY_Y[[ii]]))
  fun_get_A <- fmake_get_A(ECONOMY_Y[[ii]], fmake_find_NL, fmake_fill_NL)
  # Look only over the first argument of fun_get_A
  sol_A <- uniroot(function(x) fun_get_A(x)[[1]], c(0.1, 10.0), tol = TOL, 
      extendInt = "yes")
  # A associated with 0.05 percent unemployment rate or ur_target
  ECONOMY_A_NL[[ii]] <- sol_A$root  
}

# Find equilibria as productivity varies ----------------------------------

# Figure 2 in LS spans different productivities for each economy
# Set up economiesq
econ1_CD <- tibble(Y = seq(from = 0.603, to = 0.63, by = 0.001),
                A = ECONOMY_A_CD[[1]],
                y = ECONOMY_Y[[1]])
econ2_CD <- tibble(Y = seq(from = 0.61, to = 0.65, by = 0.001),
                A = ECONOMY_A_CD[[2]],
                y = ECONOMY_Y[[2]])
econ3_CD <- tibble(Y = seq(from = 0.63, to = 0.67, by = 0.001),
                A = ECONOMY_A_CD[[3]],
                y = ECONOMY_Y[[3]])

econ1_NL <- tibble(Y = seq(from = 0.603, to = 0.63, by = 0.001),
                   A = ECONOMY_A_NL[[1]],
                   y = ECONOMY_Y[[1]])
econ2_NL <- tibble(Y = seq(from = 0.61, to = 0.65, by = 0.001),
                   A = ECONOMY_A_NL[[2]],
                   y = ECONOMY_Y[[2]])
econ3_NL <- tibble(Y = seq(from = 0.63, to = 0.67, by = 0.001),
                   A = ECONOMY_A_NL[[3]],
                   y = ECONOMY_Y[[3]])

econ_CD <- rbind(econ1_CD, econ2_CD, econ3_CD) %>% 
  group_by(A, y) %>% 
  nest() %>% 
  mutate(match_tech = "Cobb\u2013Douglas")

econ_NL <- rbind(econ1_NL, econ2_NL, econ3_NL) %>% 
  group_by(A, y) %>% 
  nest() %>% 
  mutate(match_tech = "Nonlinear")

fun_fig2data_CD <- function(A5, df) {
  # Inputs: 
  #  A5: the matching-efficiency parameter that generates 5 percent unemployment
  #  df: Data frame with variable Y of productivities
  # Output: Data frame with two variables:
  #  (1) Productivity
  #  (2) Steady-state unemployment rate
  
  # Make the data frame a vector
  Y <- pull(df, Y)
  THETA <- vector(mode = "double", length = length(Y))
  UR <- vector(mode = "double", length = length(Y))
  
  # Step over productivity levels
  for (ii in 1:length(Y)) {
    # Step over productivity
    yii <- Y[[ii]]
    # Make zero function associated with yii
    fun_eqm_theta <- fmake_zero(A5, yii, fmake_find_CD, fmake_fill_CD)
    solii <- uniroot(fun_eqm_theta, c(0.001, 2.0), tol = TOL)
    THETA[[ii]] <- solii$root
  }
  
  fun_find <- fmake_find_CD(A5)
  fun_fill <- fmake_fill_CD(A5)  
  FIND <- fun_find(THETA)
  FILL <- fun_fill(THETA)  
  
  UR <- s / (s + FIND)
  ETA_M_U <- alpha
  UPSILON <- 1 + (r + s) * (1 - ETA_M_U) / ((r + s) * ETA_M_U + phi * FIND)  
  BOUND <- 1 / ETA_M_U
  # Return vector of URs
  dat <- tibble(productivity = Y, tightness = THETA, unemployment_rate = UR, bound = BOUND, Upsilon = UPSILON, find = FIND, fill = FILL)
  return(dat)
}

fun_fig2data_NL <- function(A5, df) {
  # Inputs: 
  #  A5: the matching-efficiency parameter that generates 5 percent unemployment
  #  df: Data frame with variable Y of productivities
  # Output: Data frame with two variables:
  #  (1) Productivity
  #  (2) Steady-state unemployment rate
  
  # Make the data frame a vector
  Y <- pull(df, Y)
  THETA <- vector(mode = "double", length = length(Y))
  UR <- vector(mode = "double", length = length(Y))
  
  # Step over productivity levels
  for (ii in 1:length(Y)) {
    # Step over productivity
    yii <- Y[[ii]]
    # Make zero function associated with yii
    fun_eqm_theta <- fmake_zero(A5, yii, fmake_find_NL, fmake_fill_NL)
    solii <- uniroot(fun_eqm_theta, c(0.001, 2.0), tol = TOL)
    THETA[[ii]] <- solii$root
  }
  
  fun_find <- fmake_find_NL(A5)
  fun_fill <- fmake_fill_NL(A5)
  FIND <- fun_find(THETA)
  FILL <- fun_fill(THETA)
  UR <- s / (s + FIND)
  ETA_M_U <- (THETA ^ pgamma) / (1 + THETA ^ pgamma)
  UPSILON <- 1 + (r + s) * (1 - ETA_M_U) / ((r + s) * ETA_M_U + phi * FIND)
  BOUND <- 1 / ETA_M_U
  # Return vector of URs
  dat <- tibble(productivity = Y, tightness = THETA, unemployment_rate = UR, bound = BOUND, Upsilon = UPSILON, find = FIND, fill = FILL)
  return(dat)
}

econ_CD <- econ_CD %>% 
  mutate(dat = map2(A, data, fun_fig2data_CD)) %>% 
  unnest(dat) 

econ_NL <- econ_NL %>% 
  mutate(dat = map2(A, data, fun_fig2data_NL)) %>% 
  unnest(dat) 

econ <- rbind(econ_CD, econ_NL)

econ <- econ %>% 
  mutate(find_check = tightness * fill)
stopifnot(all.equal(econ$find, econ$find_check))

# Plot equilibrium unemployment versus productivity -----------------------

ggplot(data = econ) +
  geom_line(mapping = aes(x = productivity, y = unemployment_rate,
                          color = factor(y),
                          linetype = match_tech),
            linewidth = 1.0) +
  geom_hline(yintercept = ur_target) +
  labs(x = "Productivity", y = "Unemployment rate", linetype = "Matching\ntechnology") +
  guides(color = "none") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(breaks = seq(0.6, 0.66, by = 0.01)) + 
  scale_color_viridis_d(option = "cividis", end = 1.0) # magma, plasma, viridis, cividis

fig_out <- paste0("fig_", file_prg, ".pdf")
ggsave(here(file_out, fig_out),
       width = mywidth, height = myheight) 

# Plot monthly job-finding rate ---------------------------------------------------

ggplot(data = econ) +
  geom_line(mapping = aes(x = productivity, y = (1 - (1 - find)^30),
                          color = factor(y),
                          linetype = match_tech),
            linewidth = 1.0) +
  geom_hline(yintercept = 0.0) +
  labs(x = "Productivity", y = "Monthly job-finding rate", linetype = "Matching\ntechnology") +
  scale_x_continuous(breaks = seq(0.6, 0.66, by = 0.01)) +   
  guides(color = "none") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8)) +
  scale_color_viridis_d(option = "plasma") # magma, plasma, viridis, cividis

fig_out <- paste0("fig_", file_prg, "-job-finding.pdf")
ggsave(here(file_out, fig_out),
       width = mywidth, height = myheight) 

# Plot job-filling rate ---------------------------------------------------

ggplot(data = econ) +
  geom_line(mapping = aes(x = productivity, y = (1 - (1 - fill)^30),
                          color = factor(y),
                          linetype = match_tech),
            linewidth = 1.2) +
  geom_hline(yintercept = 0.0) +
  scale_x_continuous(breaks = seq(0.6, 0.66, by = 0.01)) +   
  labs(x = "Productivity", y = "Job-filling rate", linetype = "Matching\ntechnology") +
  guides(color = "none") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2)) +
  scale_color_viridis_d(option = "magma", begin = 0.1, end = 0.8) # magma, plasma, viridis, cividis

fig_out <- paste0("fig_", file_prg, "-job-filling.pdf")
ggsave(here(file_out, fig_out),
       width = mywidth, height = myheight) 


# Plot elasticities of tightness and wages wrt y --------------------------------

econ_elasticities <- econ %>% 
  mutate(eta_theta_y = Upsilon * productivity / (productivity - z),
         eta_m_u = 1 / bound,
         wage = z + phi * (productivity - z + tightness * c),
         eta_w_y = phi * (((r + s) * eta_m_u + find) / ((r + s) * eta_m_u + phi * find)) * (productivity / wage)) %>% 
  filter(y == productivity)

plot_theta_elasticity <- ggplot(data = econ_elasticities) +
  geom_col(mapping = aes(x = as_factor(y), y = eta_theta_y, fill = match_tech),
           position = "dodge") +
  geom_text(mapping = aes(x = as_factor(y), y = eta_theta_y, label = paste0(round(eta_theta_y, 1))), 
            vjust = 1.9, colour = "white",
            position = position_dodge2(0.9)) +
  labs(x = "Productivity", 
       y = TeX(r'($\theta$ elasticity)'), 
       fill = "Matching\ntechnology") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8)) +
  scale_fill_viridis_d(option = "viridis", end = 0.6) # magma, plasma, viridis, cividis
(plot_theta_elasticity)

plot_wage_elasticity <- ggplot(data = econ_elasticities) +
  geom_col(mapping = aes(x = as_factor(y), y = eta_w_y, fill = match_tech),
           position = "dodge") +
  geom_text(mapping = aes(x = as_factor(y), y = eta_w_y, label = paste0(round(eta_w_y, 3))), 
            vjust = 1.9, colour = "white",
            position = position_dodge2(0.9)) +
  labs(x = "Productivity", 
       y = "Wage elasticity", 
       fill = "Matching\ntechnology") +  
  theme_minimal() +
  scale_fill_viridis_d(option = "viridis", end = 0.6) # magma, plasma, viridis, cividis  

legend_b <- get_legend(
  plot_theta_elasticity + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plot_grid(plot_theta_elasticity + xlab(NULL) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()), 
          plot_wage_elasticity + theme(legend.position = "none") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()), 
          ncol = 1, labels = c("A", "B"), hjust = 0.1)

fig_out <- paste0("fig_", file_prg, "-elasticities.pdf")
ggsave(here(file_out, fig_out),
       width = myheight, height = mywidth) 
