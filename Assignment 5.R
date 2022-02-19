# Assignment 5
# Statistics for Data Science
# Jake Agate


#Install / Load Packages:
install.packages("ggplot2")
install.packages("magrittr")
install.packages("gmodels")
install.package("dplyr")

library(ggplot2)
library(magrittr)
library(gmodels)
library(dplyr)



# Read in data:
heights <- read.csv("family_heights.csv", header = T)
str(heights)
View(heights)

# Filter out daughters height:
daughter_heights <- heights %>% 
  filter(gender == 'female') %>% 
  select(child_height, father_height, mother_height, avg_father_mother_height, children_number, family_number, child_number_based_on_age)

daughter_heights['daughter_height'] <- daughter_heights$child_height
daughter_heights$child_height <- NULL

# Convert non-numeric variables to numeric:
daughter_heights$family_number %<>% as.numeric() # 3 NA's
daughter_heights$children_number %<>% as.numeric()
daughter_heights$child_number_based_on_age %<>% as.numeric()

str(daughter_heights)
summary(daughter_heights)

write.csv(daughter_heights, file = "Daughter Height Data.csv", sep = ',')





######## Statistics #########

# Father Height:
mean(daughter_heights$father_height) # 69.26
sd(daughter_heights$father_height)   # 2.65

# Mother Height:
mean(daughter_heights$mother_height) # 64.15
sd(daughter_heights$mother_height)   # 2.26

# Average Father & Mother Height:
mean(daughter_heights$avg_father_mother_height) # 69.27
sd(daughter_heights$avg_father_mother_height)   # 1.83

# Children Number:
mean(daughter_heights$children_number) # 6.34
sd(daughter_heights$children_number)   # 2.78

# Child Number Based on Age:
mean(daughter_heights$child_number_based_on_age) # 5.00
sd(daughter_heights$child_number_based_on_age)   # 2.36

# Daughter Height:
mean(daughter_heights$daughter_height) # 64.10
sd(daughter_heights$daughter_height)   # 2.36

# Family Number:
mean(daughter_heights$family_number, na.rm = T) # 103.48
sd(daughter_heights$family_number, na.rm = T)   # 58.42





######## Scatterplots #########

# Father Height Scatterplot:
daughter_heights %>% 
  ggplot(aes(x = father_height, y = daughter_height)) + 
  geom_point(col = 'blue') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
        axis.title.x = element_text(size = 15, face = 'bold'),
        axis.title.y = element_text(size = 15, face = 'bold')) +
  ggtitle('Scatterplot of Father vs. Daughter Height') +
  xlab('Father Height (in)')+
  ylab('Daughter Height (in)') +
  geom_smooth(method = 'lm', col = 'red') +
  geom_text(x = 75, y = 59, label = "R-Squared Value = 0.1817", aes(size = 20), col = 'dark green', show.legend = F) +
  geom_text(x = 75, y = 58, label = "Y = 0.38x + 37.69", aes(size = 20), col = 'dark green', show.legend = F) 


# Mother Height Scatterplot:
daughter_heights %>% 
  ggplot(aes(x = mother_height, y = daughter_height)) + 
  geom_point(col = 'blue') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
        axis.title.x = element_text(size = 15, face = 'bold'),
        axis.title.y = element_text(size = 15, face = 'bold')) +
  ggtitle('Scatterplot of Mother vs. Daughter Height') +
  xlab('Mother Height (in)')+
  ylab('Daughter Height (in)') +
  geom_smooth(method = 'lm', col = 'red') +
  geom_text(x = 60, y = 70, label = "R-Squared Value = 0.09111", aes(size = 20), col = 'dark green', show.legend = F) +
  geom_text(x = 60, y = 69, label = "Y = 0.32x + 43.69", aes(size = 20), col = 'dark green', show.legend = F) 


# Average Father and Mother Height Scatterplot:
daughter_heights %>% 
  ggplot(aes(x = avg_father_mother_height, y = daughter_height)) + 
  geom_point(col = 'blue') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
        axis.title.x = element_text(size = 15, face = 'bold'),
        axis.title.y = element_text(size = 15, face = 'bold')) +
  ggtitle('Scatterplot of Averaged Father & Mother Height vs. Daughter Height') +
  xlab('Average Father & Mother Height (in)')+
  ylab('Daughter Height (in)') +
  geom_smooth(method = 'lm', col = 'red') +
  geom_text(x = 74, y = 59, label = "R-Squared Value = 0.2618", aes(size = 20), col = 'dark green', show.legend = F) +
  geom_text(x = 74, y = 58, label = "Y = 0.66x + 18.33", aes(size = 20), col = 'dark green', show.legend = F) 






############ Regression Analysis #############

# Father's Height:
father_regression <- lm(daughter_height ~ father_height, data = daughter_heights)
summary(father_regression)
# R-Squared Value: 0.1817
# Regression Equation: Y = 0.38x + 37.69
# Standard Error: 2.131

# Mother's Height:
mother_regression <- lm(daughter_height ~ mother_height, data = daughter_heights)
summary(mother_regression)
# R-Squared Value: 0.09111
# Regression Equation: Y = 0.32x + 43.69
# Standard Error: 2.246

# Average of Father & Mother Height:
avg_mother_father_reg <- lm(daughter_height ~ avg_father_mother_height, data = daughter_heights)
summary(avg_mother_father_reg)
# R-Squared Value: 0.2618
# Regression Equation: Y = 0.66x + 18.33
# Standard Error: 2.024





######### Scatterplots of residual values vs independent variables ###########

# Residual Averages:
mean(father_regression$residuals)
mean(mother_regression$residuals)
mean(avg_mother_father_reg$residuals)

par(mfrow = c(1,1))

# Father:
daughter_heights %>% 
  ggplot(aes(x = father_height, y = father_regression$residuals)) +
  geom_point(col = 'blue') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
        axis.title.x = element_text(size = 15, face = 'bold'),
        axis.title.y = element_text(size = 15, face = 'bold')) +
  ggtitle('Father Height Residual Plot') +
  xlab('Father Heights')+
  ylab('Residuals') +
  geom_smooth(method = 'lm')


# Mother:
daughter_heights %>% 
  ggplot(aes(x = mother_height, y = mother_regression$residuals)) +
  geom_point(col = 'blue') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
        axis.title.x = element_text(size = 15, face = 'bold'),
        axis.title.y = element_text(size = 15, face = 'bold')) +
  ggtitle('Mother Height Residual Plot') +
  xlab('Mother Heights')+
  ylab('Residuals') +
  geom_smooth(method = 'lm')


# Average Father & Mother:
daughter_heights %>% 
  ggplot(aes(x = avg_father_mother_height, y = avg_mother_father_reg$residuals)) +
  geom_point(col = 'blue') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
        axis.title.x = element_text(size = 15, face = 'bold'),
        axis.title.y = element_text(size = 15, face = 'bold')) +
  ggtitle('Averaged Father & Mother Height Residual Plot') +
  xlab('Averaged Father & Mother Heights')+
  ylab('Residuals') +
  geom_smooth(method = 'lm')








######## QQ-Plots ##########

# Father Height QQ-Plot:
qqnorm(daughter_heights$father_height,
       main = "Father Height QQ-Plot")
qqline(daughter_heights$father_height)

# Mother Height QQ-Plot:
qqnorm(daughter_heights$mother_height,
       main = "Mother Height QQ-Plot")
qqline(daughter_heights$mother_height)

# Average Father & Mother QQ-Plot:
qqnorm(daughter_heights$avg_father_mother_height,
       main = "Average Father & Mother Height QQ-Plot")
qqline(daughter_heights$avg_father_mother_height)





######## Confidence Intervals ###########

# Father Height:
father_height_mean <- mean(daughter_heights$father_height)
Z <- (qnorm(0.95))/2
father_height_sd <- sd(daughter_heights$father_height)
n <- 453

CI_father_lower <- father_height_mean - Z*(father_height_sd/(sqrt(n))) # 69.16
CI_father_upper <- father_height_mean + Z*(father_height_sd/(sqrt(n))) # 69.36

# 95% confident that the mean lies in the interval (69.16 , 69.36)

ci(daughter_heights$father_height, confidence = 0.95)
# CI: ( 69.02 , 69.50 )
# SE: 0.124359



# Mother Height:
mother_height_mean <- mean(daughter_heights$mother_height)
Z <- (qnorm(0.95))/2
mother_height_sd <- sd(daughter_heights$mother_height)
n <- 453

CI_mother_lower <- mother_height_mean  - Z*(mother_height_sd/(sqrt(n))) # 64.06
CI_mother_upper <- mother_height_mean  + Z*(mother_height_sd/(sqrt(n))) # 64.24

ci(daughter_heights$mother_height, confidence = 0.95)
# CI: ( 63.94 , 64.36 )
# SE: 0.1061292



# Average Father & Mother Height:
father_mother_height_mean <- mean(daughter_heights$avg_father_mother_height)
Z <- (qnorm(0.95))/2
father_mother_height_sd <- sd(daughter_heights$avg_father_mother_height)
n <- 453
  
  
CI_father_mother_lower <- father_mother_height_mean - Z*(father_mother_height_sd/(sqrt(n))) # 69.2
CI_father_mother_upper <- father_mother_height_mean + Z*(father_mother_height_sd/(sqrt(n))) # 69.34

ci(daughter_heights$avg_father_mother_height, confidence = 0.95)
# CI: ( 69.10 , 69.45 )
# SE: 0.08597511


####

y_hat <- # desired value to predict
s <- #standard error
t_.025 <- 1.96

CI_father_lower <- y_hat - (t_.025 * s)
CI_father_upper <- y_hat + (t_.025 * s)
  
# df = n-2
# 453 - 2 = 451








