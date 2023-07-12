# fundamentalSurplusGeneralMatch

The paper is in top/fundamental-surplus-general-matching-technology.pdf.
Figures in the paper are generated with R code.
The code is contained in R files that are located in cde/.

  * 01-du-dy.R
  Computes responses of unemployment to changes in productivity.
  A comparison of two matching functions is made.
  One is the familiar Cobb--Douglas parameterization.
  The other is a nonlinear parameterization that generates larger volatility.
  * 02-get-vacancies-unemployment.R
  Retrieves data on unemployment and job openings.
  The code retrieves the latest data from [FRED](https://fred.stlouisfed.org/).
  This code does not need to be run if the aim is replicating figures in the paper.
  Data used to generate the figures are located in dta/cln/.
  * 03-plot-vacancies-unemployment-levels.R
  Plots unemployment and job openings in levels.
