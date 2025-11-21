## set working directory
setwd("/courses/STA145/schwamg1")

# Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(read_csv"/courses/STA145/schwamg1/data.csv")

# load dataset into environment
library(readr)
data <- read_delim("data.csv")
View(data)

##################################################################################
###############  Table 1: Descriptive Statistics  ################################  
##################################################################################

# generate statistics for word count
summary(data$word_count)
mean(data$word_count)
sd(data$word_count)

# generate statistics for title words
summary(data$title_words)
mean(data$title_words)
sd(data$title_words)

##################################################################################
####################   Figure 1: Scatter Plot  ###################################   
##################################################################################
linear_plot <- plot(data$word_count, data$title_words)
print(linear_plot)
meany <- mean(data$title_words)
meanx <- mean(data$word_count)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(word_count ~ title_words, data = data)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 2: Residual Plot  ###################################   
##################################################################################
# Plot the residuals
plot(data$title_words, residuals(linear_relationship))
plot(data$word_count, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: Regression/Correlation Table  #####################   
##################################################################################
table(data$title_words, data$word_count)
t.test(data$title_words, data$word_count, data = data, var.equal = TRUE)
