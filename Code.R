# STAT 408 – Gold Price Regression Project

library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)

gold <- read_csv("C:/Users/dell/Downloads/gld_price_data.csv")
str(gold)
summary(gold)

# Missing values
colSums(is.na(gold))

#Correlation Matrix
vars <- gold %>% select(GLD, SPX, USO, SLV, `EUR/USD`)
cor(vars, use = "complete.obs")

#Plots
# Time series of GLD
plot(gold$Date, gold$GLD, type = "l", col = "gold3", lwd = 2,
     main = "Time Series of Gold Prices (GLD)",
     xlab = "Date", ylab = "GLD Price")

# Scatterplot of GLD vs SLV
plot(gold$SLV, gold$GLD,
     pch = 16, col = "darkgray",
     main = "Scatterplot of GLD vs SLV",
     xlab = "SLV", ylab = "GLD")

# 5. Regression Models
# Simple Linear Regression
simple_mod <- lm(GLD ~ SLV, data = gold)
summary(simple_mod)

# Multiple Linear Regression
full_mod <- lm(GLD ~ SPX + USO + SLV + `EUR/USD`, data = gold)
summary(full_mod)

#Stepwise Selection (AIC)

library(MASS)
step_mod <- stepAIC(full_mod, direction = "both")
summary(step_mod)

# Residuals vs Fitted
plot(step_mod, which = 1,
     main = "Residuals vs Fitted")

# Normal Q–Q Plot
plot(step_mod, which = 2,
     main = "Normal Q–Q Plot")


# Cook’s Distance Plot
plot(step_mod, which = 5,
     main = "Residuals vs Leverage (Cook's Distance)")

bptest(step_mod)

