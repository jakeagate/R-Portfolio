# Assignment 4
# Statistics for Data Science
# Jake Agate

# Install / Load Packages:
install.packages("magrittr")
install.packages("ggplot2")

library(magrittr)
library(ggplot2)

# Read in data:
data <- read.csv("exposure_data.csv", header = F)
str(data)
View(data)

# Remove unnecessary rows and columns:
data <- data[-c(1:4),]
data[c("V4","V5","V6")] <- NULL
data[-c(15:17),]

# Rename columns:
colnames(data)[1] <- 'Business_Name'
colnames(data)[2] <- "Time_Ad_Aired"
colnames(data)[3] <- "Household_Exposure"

# Change variable classes:
data$Time_Ad_Aired %<>% as.numeric()
data$Household_Exposure %<>% as.numeric()

str(data)

# Create scatterplot:
data %>% 
  ggplot(aes(x = Time_Ad_Aired, y = Household_Exposure)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(
    title = "Time Ad Aired vs. Household Exposure",
    y = 'Household Exposure (million)',
    x = "Time Ad Aired"
  ) +
  geom_text(x = 20, y = 38, label = "R-Squared Value = 0.8936", aes(size = 20), col = 'dark green', show.legend = F) +
  geom_text(x = 20, y = 36, label = "Y = 0.58x + 6.69", aes(size = 20), col = 'dark green', show.legend = F) +
  theme_bw() +
  theme(plot.title = element_text(size = 17), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + 
  scale_x_continuous(breaks=seq(0, 50, by = 5)) + 
  scale_y_continuous(breaks=seq(0, 40, by = 5))

# Mean Statistics:
mean(data$Time_Ad_Aired, na.rm = T)
mean(data$Household_Exposure, na.rm = T)

# Standard Deviation Statistics:
sd(data$Time_Ad_Aired, na.rm = T)
sd(data$Household_Exposure, na.rm = T) 

# Linear Regression:
results <- lm(Household_Exposure ~ Time_Ad_Aired, data = data)
summary(results)
# Multiple R-Squared: 0.8936
# Regression Equation: Y = 0.58x + 6.69
# Standard Error: 2.283

# QQ-Plots:
qqplot(data$Time_Ad_Aired, data$Household_Exposure, 
       main = "QQ-Plot of Time Ad Aired vs. Household Exposure",
       xlab = 'Time Ad Aired',
       ylab = "Household Exposure (million)")


qqnorm(data$Time_Ad_Aired,
       main = "Time Ad Aired QQ-Plot")
qqline(data$Time_Ad_Aired)

qqnorm(data$Household_Exposure,
       main = "Household Exposure QQ-Plot")
qqline(data$Household_Exposure)

# If add aired 28 times:
# Y = 0.58x + 6.69
(.58*28) + 6.69 # Expected exposure of 22.93 million


# 95% Confidence Interval:
exposure_mean <- mean(data$Household_Exposure, na.rm = T)
time_mean <- mean(data$Time_Ad_Aired, na.rm = T)
Z <- (qnorm(0.95))/2
exposure_sd <- sd(data$Household_Exposure, na.rm = T)
time_sd <- sd(data$Time_Ad_Aired, na.rm = T)
n <- 28


CI_lower <- exposure_mean - Z*(exposure_sd/(sqrt(n))) # 21.79
CI_upper <- exposure_mean + Z*(exposure_sd/(sqrt(n))) # 23.85

# 95% confident that the mean lies in the interval (21.79, 23.85)


#############

par(mfrow = c(2,2))
plot(results, labels.id = data$Business_Name)
# Indicates Blockbuster Video and HBO are outliers

install.packages("FSA")
install.packages("predictmeans")
library(FSA)
library(predictmeans)

residplot(results)





