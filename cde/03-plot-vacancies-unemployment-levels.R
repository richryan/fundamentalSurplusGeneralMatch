# Program: 03-plot-vacancies-unemployment-levels.R
# Purpose: Plot the number of job openings and number of unemployed people in
# the US economy.
# 
# Running 02-get-vacancies-unemployment.R will get the latest data on vacancies
# and unemployment. The latest data will be saved in a .csv file with a date.
# Either:
#  (A) use the code here to reproduce the figure in the paper
#  (B) update the input file to use the latest data.
# The input file can be changed by updating the parameter input_file.
# 
# The code produces two plots:
# 
# (1) unemployment and vacancies in levels
# (2) the inverse of the elasticity of matching with respect to unemployment
# 
# Date Started: 2023-06-18
# Date Revised: 2023-07-11
library(tidyverse)
library(tidyquant)
library(ggthemes)
library(viridis)
library(ggrepel)
library(here)

input_file <- "dat_02-get-vacancies-unemployment_2023-07-11.csv"
file_prg <- "03-plot-vacancies-unemployment-levels"
file_out <- c("out")

# Golden ratio plotting parameters
mywidth <- 6
golden <- 0.5*(1 + sqrt(5))
myheight <- mywidth / golden

# CSUB colors
csub_blue <- rgb(0, 26, 112, maxColorValue = 255)
csub_gray <- rgb(112, 115, 114, maxColorValue = 255)

# Data on recession dates -------------------------------------------------------

recess <- tq_get("USRECM", get = "economic.data", from = "1800-01-01")
recess_dat <- recess %>% 
  arrange(date) %>% 
  mutate(same = 1 - (price == lag(price))) %>% 
  # Remove first row, an NA, for cumulative sum
  filter(date > min(recess$date)) %>% 
  mutate(era = cumsum(same)) %>% 
  # Filter only recessions
  filter(price == 1)

recess_dat <- recess_dat %>% 
  group_by(era) %>% 
  # Unncessary, but to be sure...
  arrange(date) %>% 
  filter(row_number() == 1 | row_number() == n())

# Now reshape the data wide.
# Each row will contain the start and end dates of a recession.
recess_dat <- recess_dat %>% 
  mutate(junk = row_number()) %>% 
  mutate(begin_end = case_when(
    junk == 1 ~ "begin",
    junk == 2 ~ "end"
  ))

recess_wide <- recess_dat %>%
  ungroup() %>% 
  select(symbol, price, date, era, begin_end) %>% 
  pivot_wider(names_from = begin_end, values_from = date)

# Read in data on unemployment and vacancies ------------------------------

dat <- read_csv(here("dta", "cln", input_file))

# Get dates when both series start
dat_dates <- dat %>% 
  group_by(FRED_symbol) %>% 
  summarise(date = min(date)) %>% 
  pull(date)
dat_dates <- max(dat_dates)

# Plot unemployment and vacancies in levels -------------------------------

chart_begin <- dat_dates
chart_end <- max(dat$date)

dat <- dat %>%
  group_by(FRED_symbol) %>%
  mutate(series_begin_end = case_when(
    date == tail(date, n = 1) ~ paste0(round(lvl_millions, 1)),
    date == head(date, n = 1) ~ paste0(round(lvl_millions, 1))
  ),
  series_name = case_when(
    FRED_symbol == "UNEMPLOY" ~ "Unemployed persons",
    TRUE ~ "Job openings"
  ),
  series_label = case_when(
    series_name == "Unemployed persons" & date == ymd("2010-01-01") ~ series_name,
    series_name == "Job openings" & date == ymd("2010-01-01") ~ series_name
  ))

# Y-axis ticks and labels
ybreaks <- seq(from = 0, to = 25, by = 5)
ylabels <- paste0(ybreaks)

# Caption
mycap <- paste0("Data are monthly. The two series begin in ", 
               month(dat_dates, label = TRUE), " ",
               year(dat_dates), ".")

# Figure without caption
plt_vacancies_unemployment <- ggplot(data = dat) +
  geom_rect(
    data = filter(recess_wide,
                  begin >= chart_begin,
                  end <= chart_end),
    mapping = aes(
      xmin = begin,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "blue",
    alpha = 0.2
  ) +
  geom_line(mapping = aes(x = date, y = lvl_millions, color = series_name, linetype = series_name), 
            linewidth = 0.9) +
  geom_text_repel(mapping = aes(x = date, y = lvl_millions, label = series_begin_end, color = series_name), box.padding = 0.5) +  
  geom_text_repel(mapping = aes(x = date, y = lvl_millions, label = series_label, color = series_name),
                  max.overlaps = Inf, box.padding = 2.0) +
  labs(x = "", y = "Millions", color = "", linetype = "") +
  scale_y_continuous(breaks = ybreaks,
                     labels = ylabels,
                     limits = c(0, 25)) + 
  # Add 1-million line
  geom_hline(yintercept = 1.0, color = "blue") +
  guides(color = "none", linetype = "none") +
  theme_light()

fout_name <- paste0("fig_", file_prg, ".pdf")
fout <- here(file_out, fout_name)
ggsave(fout, plot = last_plot(), 
       width = mywidth, height = myheight)

# Add caption
(plt_vacancies_unemployment + labs(x = "", y = "Millions", color = "", linetype = "", caption = mycap))

# Plot bounds or inverse of elasticity of matching wrt unemployment --------------------------------------------------------------

ggamma <- 1.27
aalpha <- 0.5

dat_bound <- dat %>% 
  pivot_wider(id_cols = date, values_from = lvl_thousands, names_from = series_name) %>% 
  mutate(tightness = `Job openings` / `Unemployed persons`,
         eta_M_u = (tightness^ggamma) / (1 + tightness^ggamma),
         Upsilon_bound_nonlinear = 1 / eta_M_u,
         Upsilon_bound_cobb_douglas = 1 / aalpha,
         series_label_cobb_douglas = case_when(
           date == ymd("2011-01-01") ~ "Cobb\u2013Douglas", 
           TRUE ~ ""
         ),
         series_label_nonlinear = case_when(
           date == ymd("2009-01-01") ~ "Nonlinear",
           TRUE ~ ""
         ))

ggplot(data = dat_bound) +
  geom_rect(
    data = filter(recess_wide,
                  begin >= chart_begin,
                  end <= chart_end),
    mapping = aes(
      xmin = begin,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "blue",
    alpha = 0.2
  ) +  
  geom_line(mapping = aes(x = date, y = Upsilon_bound_cobb_douglas), color = csub_gray, linewidth = 1.0) +  
  geom_line(mapping = aes(x = date, y = Upsilon_bound_nonlinear), 
            linewidth = 1.0, color = csub_blue) +
  geom_text_repel(mapping = aes(x = date, y = Upsilon_bound_cobb_douglas + 0.1, label = series_label_cobb_douglas),
                  max.overlaps = Inf, color = csub_gray, box.padding = 1.5,
                  arrow = arrow(length = unit(0.02, "npc"), type = "open", ends = "last")) +
  geom_text_repel(mapping = aes(x = date, y = Upsilon_bound_nonlinear + 0.5, label = series_label_nonlinear),
                  vjust = 0.0, hjust = 0.0, nudge_x = -0.6,
                  max.overlaps = Inf, color = csub_blue, box.padding = 1.1,
                  arrow = arrow(length = unit(0.02, "npc"), type = "open", ends = "last")) +  
  labs(x = "", y = "Bound") +
  guides(color = "none", linetype = "none") +
  theme_light()

fout_name <- paste0("fig_", file_prg, "-bound.pdf")
fout <- here(file_out, fout_name)
ggsave(fout, plot = last_plot(), 
       width = mywidth, height = myheight)
