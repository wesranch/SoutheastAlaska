# Fitting weibull distribution SE AK light EST parms
# Wesley Rancher

#libs
library(ggplot2)
library(stats)
library(fitdistrplus)

#light est table
shade_probs <- data.frame(
  Spp_Shade_Class = 0:5,  
  Least_Tolerant = c(0.52, 0.01, 0.35, 0.07, 0.02, 0.03),
  Mid_Tolerant = c(0.17, 0.01, 0.45, 0.18, 0.07, 0.11),
  Most_Tolerant = c(0.24, 0.01, 0.48, 0.15, 0.05, 0.07)
)


fit_weibull <- function(data) {
  fit <- fitdistrplus::fitdist(data, "weibull")
  return(fit)
}

fit_least <- fit_weibull(shade_probs$Least_Tolerant)
fit_mid <- fit_weibull(shade_probs$Mid_Tolerant)
fit_most <- fit_weibull(shade_probs$Most_Tolerant)

# 5. Plotting the fitted distributions
# Generate values from the fitted Weibull distributions for each group
x_vals <- seq(0, 7, length.out = 100) 
test_shape <- 1.5
test_scale <- 4

mid_shape <- 1.5
mid_scale <- 1

most_shape <- 1.5
most_scale <- 1.35

least_vals <- dweibull(x_vals, test_shape, test_scale)
mid_vals <- dweibull(x_vals, shape = mid_shape, scale = mid_scale)
most_vals <- dweibull(x_vals, most_shape, most_scale)

# Plot the fitted Weibull distributions for each group
ggplot() +
  geom_line(aes(x = x_vals, y = least_vals), color = "red", size = 1.5, linetype = "solid")
  #geom_line(aes(x = x_vals, y = mid_vals), color = "blue", size = 1.5, linetype = "dashed")
  #geom_line(aes(x = x_vals, y = most_vals), color = "green", size = 1.5, linetype = "dotted")
