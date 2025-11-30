# Load necessary libraries
library(dplyr)
library(ggplot2)
library(caret)
library(readxl)

# Load the dataset
file_path <- "D:/stat/Data.xlsx"
data <- read_excel(file_path)

# Inspect the dataset
cat("Column Names:\n")
print(colnames(data))  # Display column names

cat("\nFirst Few Rows of Data:\n")
print(head(data))  # Display the first few rows

cat("\nSummary of Data:\n")
print(summary(data))  # Display a summary of the data

# Ensure 'students_enrolled' is numeric and 'difficulty' is a factor
data <- data %>%
  mutate(students_enrolled = as.numeric(enrollment),
         difficulty = factor(difficulty, levels = c("Beginner", "Intermediate", "Advanced")))

# Check for missing values
cat("\nNumber of Missing Values:\n")
print(sum(is.na(data)))  # Display the count of missing values

# Data Cleaning: Remove rows with NA values
data_clean <- data %>% filter(!is.na(students_enrolled) & !is.na(session_duration))

# Visualizing the distribution of session durations
ggplot(data_clean, aes(x = session_duration)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "darkgreen") +
  theme_minimal() +
  labs(title = "Distribution of Session Duration", x = "Session Duration", y = "Frequency") +
  theme(text = element_text(size = 12, color = "darkblue"),
        plot.title = element_text(size = 16, hjust = 0.5, color = "darkblue"))

# Create synthetic metric: Completion rate
data_clean <- data_clean %>%
  mutate(completion_rate = (rating / 5) * (1 - as.numeric(difficulty == "Advanced") * 0.2))

# Linear Regression Model to predict completion_rate based on session count and session duration
lm_model <- lm(completion_rate ~ session_count + session_duration + difficulty, data = data_clean)
cat("\nLinear Regression Summary:\n")
print(summary(lm_model))

# Logistic Regression: Predicting high engagement (completion_rate > 0.75)
data_clean <- data_clean %>%
  mutate(high_engagement = ifelse(completion_rate > 0.75, 1, 0))

logit_model <- glm(high_engagement ~ session_count + session_duration + difficulty, data = data_clean, family = "binomial")
cat("\nLogistic Regression Summary:\n")
print(summary(logit_model))

# K-fold Cross-validation for Linear Model
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(completion_rate ~ session_count + session_duration + difficulty,
                  data = data_clean, method = "lm", trControl = train_control)
cat("\nCross-Validation Model Results:\n")
print(cv_model)

# Save cleaned data
write.csv(data_clean, "Cleaned_User_Engagement_Data.csv")

# 1. Scatter plot of session_count vs. completion_rate with customized color
ggplot(data_clean, aes(x = session_count, y = completion_rate)) +
  geom_point(color = "darkorange", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE, formula = y ~ x) +
  theme_minimal() +
  labs(title = "Session Count vs Completion Rate", x = "Session Count", y = "Completion Rate") +
  theme(text = element_text(size = 12, color = "darkblue"),
        plot.title = element_text(size = 16, hjust = 0.5, color = "darkblue"))

# 2. Boxplot for session_duration by difficulty level with color and style
ggplot(data_clean, aes(x = difficulty, y = session_duration, fill = difficulty)) +
  geom_boxplot(color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("skyblue", "orange", "lightgreen")) +
  theme_minimal() +
  labs(title = "Session Duration by Difficulty Level", x = "Difficulty Level", y = "Session Duration") +
  theme(text = element_text(size = 12, color = "darkblue"),
        plot.title = element_text(size = 16, hjust = 0.5, color = "darkblue"))

# 3. Boxplot for completion_rate by difficulty level with color and style
ggplot(data_clean, aes(x = difficulty, y = completion_rate, fill = difficulty)) +
  geom_boxplot(color = "black", alpha = 0.5) +
  scale_fill_manual(values = c("lightblue", "salmon", "lightgreen")) +
  theme_minimal() +
  labs(title = "Completion Rate by Difficulty Level", x = "Difficulty Level", y = "Completion Rate") +
  theme(text = element_text(size = 12, color = "darkblue"),
        plot.title = element_text(size = 16, hjust = 0.5, color = "darkblue"))
