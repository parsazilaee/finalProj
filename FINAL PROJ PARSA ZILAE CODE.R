# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load dataset
boxing_data <- read.csv("C:/Users/Parsa/Downloads/archive/boxing_matches.csv")

# Cleaning: Fill missing numeric data with median
boxing_data <- boxing_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Cleaning: Fill missing categorical data with mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
boxing_data <- boxing_data %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), Mode(.), .)))

# Review data structure
summary(boxing_data)


# Plot Histograms for each variable
ggplot(boxing_data, aes(x = age_A)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Age (Fighter A)", x = "Age", y = "Count")

ggplot(boxing_data, aes(x = height_A)) +
  geom_histogram(binwidth = 2, fill = "green", color = "black") +
  labs(title = "Histogram of Height (Fighter A)", x = "Height", y = "Count")

ggplot(boxing_data, aes(x = reach_A)) +
  geom_histogram(binwidth = 2, fill = "red", color = "black") +
  labs(title = "Histogram of Reach (Fighter A)", x = "Reach", y = "Count")


# PMF for wins by stance
orthodox_pmf <- prop.table(table(boxing_data %>% filter(stance_A == "orthodox") %>% .$won_A))
southpaw_pmf <- prop.table(table(boxing_data %>% filter(stance_A == "southpaw") %>% .$won_A))

barplot(orthodox_pmf, main = "PMF: Wins (Orthodox Fighters)", xlab = "Wins", ylab = "Probability")
barplot(southpaw_pmf, main = "PMF: Wins (Southpaw Fighters)", xlab = "Wins", ylab = "Probability")

# CDF of Reach
reach_sorted <- sort(boxing_data$reach_A)
cdf <- ecdf(reach_sorted)

plot(cdf, main = "CDF of Reach (Fighter A)", xlab = "Reach (cm)", ylab = "Cumulative Probability")


hist(boxing_data$reach_A, probability = TRUE, main = "Reach Distribution")
curve(dnorm(x, mean = mean(boxing_data$reach_A), sd = sd(boxing_data$reach_A)), col = "red", add = TRUE)


# Scatter plot for Reach vs Wins
ggplot(boxing_data, aes(x = reach_A, y = won_A)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Scatter Plot: Reach vs Wins", x = "Reach (cm)", y = "Wins")

# Scatter plot for Height vs Age
ggplot(boxing_data, aes(x = height_A, y = age_A)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter Plot: Height vs Age", x = "Height", y = "Age")




t.test(won_A ~ stance_A, data = boxing_data)



# Multiple regression: Predict wins based on height, reach, and age
model <- lm(won_A ~ height_A + reach_A + age_A, data = boxing_data)
summary(model)