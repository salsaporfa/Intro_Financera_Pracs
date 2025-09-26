rm(list=ls())
packages <- c("quantmod", "e1071")
to_install <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  install.packages(to_install)
}
library("quantmod")
library("e1071")
options("getSymbols.warning4.0"=FALSE)
cat("\f")

getSymbols("GOOG", from="2011-01-03", to="2014-12-31")
google_close <- GOOG$GOOG.Close

getSymbols("SPX", from="2011-01-03", to="2014-12-31")
spx_close <- SPX$SPX.Close

# 1.a
# Sample mean
mean(google_close)
mean(spx_close)

# Standard Deviation
sd(google_close)
sd(spx_close)

# skewness
skewness(google_close)
skewness(spx_close)

# excess kurtosis
kurtosis(google_close)
kurtosis(spx_close)

# minimum
min(google_close)
min(spx_close)

# max
max(google_close)
max(spx_close)

# 1.b
g_ret_simp <- na.omit(diff(google_close) / lag(google_close, k = -1)) # (P_i - P_(i-1)) / P_(i-1)

g_ret_simp_dens <- density(as.numeric(g_ret_simp))
plot(g_ret_simp_dens)
# gràficament, sembla que l'excés de kurtosis és significativament diferent de 0, el cual
# no és compatible a una distribució normal, que té un excés de kurtosis = 0.

shapiro.test(as.numeric(g_ret_simp))
# p-valor ~= 0 => rebutjem H0 => distribució no-normal.

tseries::jarque.bera.test(as.numeric(g_ret_simp))
# p-valor ~= 0 => rebutjem H0 => distribució no-normal.

# 1.c
g_ret_log <- na.omit(diff(log(google_close))) # log(P_i/P_(i-1))
s_ret_log <- na.omit(diff(log(spx_close)))

# mean
g_ret_log_mean <- mean(g_ret_log)
mean(s_ret_log)

# standard deviation
g_ret_log_sd <- sd(g_ret_log)
sd(s_ret_log)

# skewness
skewness(g_ret_log)
skewness(s_ret_log)

# excess kurtosis
kurtosis(g_ret_log)
kurtosis(s_ret_log)

# minimum
min(g_ret_log)
min(s_ret_log)

# maximum
max(g_ret_log)
max(s_ret_log)

# 1.d
shapiro.test(as.numeric(g_ret_log))
# p-valor ~= 0 => rebutjem H0 => distribució no-normal.

tseries::jarque.bera.test(as.numeric(g_ret_log))
# p-valor ~= 0 => rebutjem H0 => distribució no-normal.

# Encara que els retorns logarítmics no són normals, i el t-test requereix normalitat, com
# que la mostra és gran, per el teorema central del límit sabem que l'estadístic t s'apro-
# ximarà molt bé a una normal, llavors el podem fer servir.

t.test(as.numeric(g_ret_log), mu = 0)
# p-valor ~= 0.24 => no rebutjem H0 => La mitjana és 0.

# 1.e
g_ret_log_dens <- density(as.numeric(g_ret_log))
plot(g_ret_log_dens)
# Com en el cas dels retorns simples, l'excés de kurtosis sembla significativament diferent
# de 0, no és normal.
# A part, els test de 1.d ja han confirmat que no segueix una distribució normal

s_ret_log_dens <- density(as.numeric(s_ret_log))
plot(s_ret_log_dens)
# El mateix que en el cas de google, excés de kurtosis molt diferent de 0.

shapiro.test(as.numeric(s_ret_log))
# p-valor ~= 0 => rebutjem H0 => distribució no-normal.

tseries::jarque.bera.test(as.numeric(s_ret_log))
# p-valor ~= 0 => rebutjem H0 => distribució no-normal.

# 1.f
n <- length(g_ret_log)
c(g_ret_log_mean - g_ret_log_sd * qt(0.025, df = n-1),
  g_ret_log_mean - g_ret_log_sd * qt(0.975, df = n-1))
# [1]  0.03020819 -0.02908536