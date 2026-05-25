# ALY 6010 - Module 2
# Neeti Shah | May 31, 2025
# Mental Health Care in the Last 4 Weeks
# =======================

# -------------------------------------
# Load Required Libraries
# -------------------------------------
library(dplyr)      # For data wrangling
library(psych)      # For descriptive statistics
library(ggplot2)    # For additional plots (if needed)
library(stringr)    # For string manipulation

# -------------------------------------
# Read Dataset
# -------------------------------------
df <- read.csv("C:/Users/neeti/OneDrive/Desktop/NEU/ALY 6010/Shah_Project2/Mental_Health_Care.csv", stringsAsFactors = FALSE)

# -------------------------------------
# Data Cleaning & Recoding Categories
# -------------------------------------

# Clean and standardize 'Group' and 'Subgroup' columns
df$Group <- tolower(trimws(df$Group))
df$Subgroup <- trimws(df$Subgroup)

# ---- Categorize Age into 3 groups ----
df$Age_Category <- NA
df$Age_Category[df$Group == "by age" & df$Subgroup %in% c("18 - 29 years")] <- "0-30"
df$Age_Category[df$Group == "by age" & df$Subgroup %in% c("30 - 39 years", "40 - 49 years", "50 - 59 years")] <- "30-60"
df$Age_Category[df$Group == "by age" & df$Subgroup %in% c("60 - 69 years", "70 - 79 years", "80 years and above")] <- "60+"

# ---- Categorize Sex ----
df$Sex_Category <- NA
df$Sex_Category[df$Group == "by sex" & df$Subgroup == "Male"] <- "Male"
df$Sex_Category[df$Group == "by sex" & df$Subgroup == "Female"] <- "Female"
df$Sex_Category[df$Group == "by gender identity"] <- "Other"  # Includes LGBTQ identities

# ---- Categorize Race ----
df$Race_Category <- NA
df$Race_Category[df$Group == "by race/hispanic ethnicity"] <- df$Subgroup[df$Group == "by race/hispanic ethnicity"]

# ---- Categorize Education ----
df$Education_Category <- NA
df$Education_Category[df$Group == "by education" & df$Subgroup %in% c("Less than a high school diploma", "High school diploma or GED")] <- "High school diploma"
df$Education_Category[df$Group == "by education" & df$Subgroup %in% c("Bachelor's degree or higher")] <- "Bachelor’s degree"

# ---- Categorize Disability Status ----
df$Disability_Status <- NA
df$Disability_Status[df$Group == "by disability status"] <- df$Subgroup[df$Group == "by disability status"]

# ---- Categorize Symptom Presence ----
df$Symptom_Status <- NA
df$Symptom_Status[df$Group == "by presence of symptoms of anxiety/depression"] <- df$Subgroup[df$Group == "by presence of symptoms of anxiety/depression"]

# -------------------------------------
# Descriptive Statistics
# -------------------------------------

# Overall summary
summary(df$Value)
describe(df$Value)  # psych package gives detailed statistics

# Grouped statistics by age
age_stats <- df %>% filter(!is.na(Age_Category)) %>%
  group_by(Age_Category) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE),
            Min = min(Value, na.rm = TRUE), Max = max(Value, na.rm = TRUE), N = n())

# Grouped statistics by sex
sex_stats <- df %>% filter(!is.na(Sex_Category)) %>%
  group_by(Sex_Category) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE),
            Min = min(Value, na.rm = TRUE), Max = max(Value, na.rm = TRUE), N = n())

# Grouped statistics by race
race_stats <- df %>% filter(!is.na(Race_Category)) %>%
  group_by(Race_Category) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE),
            Min = min(Value, na.rm = TRUE), Max = max(Value, na.rm = TRUE), N = n())

# Grouped statistics by education
education_stats <- df %>% filter(!is.na(Education_Category)) %>%
  group_by(Education_Category) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE),
            Min = min(Value, na.rm = TRUE), Max = max(Value, na.rm = TRUE), N = n())

# Grouped statistics by disability
disability_stats <- df %>% filter(!is.na(Disability_Status)) %>%
  group_by(Disability_Status) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE),
            Min = min(Value, na.rm = TRUE), Max = max(Value, na.rm = TRUE), N = n())

# Grouped statistics by symptoms
symptom_stats <- df %>% filter(!is.na(Symptom_Status)) %>%
  group_by(Symptom_Status) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE),
            Min = min(Value, na.rm = TRUE), Max = max(Value, na.rm = TRUE), N = n())

# -------------------------------------
# Visualizations in RStudio Viewer
# -------------------------------------

# ---- Scatter Plot: Value vs Lower Confidence Interval ----
par(mar = c(5, 5, 4, 2))
plot(df$LowCI, df$Value, pch = 19, col = "darkorange",
     main = "Scatter: Value vs Lower CI", xlab = "Lower CI", ylab = "Value")
abline(lm(Value ~ LowCI, data = df), col = "blue", lwd = 2)

# ---- Boxplot: Value by Age Category ----
box_data <- df %>% filter(!is.na(Age_Category))
boxplot(Value ~ Age_Category, data = box_data, col = "skyblue",
        main = "Boxplot: Value by Age Category", ylab = "Value (%)")

# ---- Jitter Plot: Value by Sex Category ----
jitter_data <- df %>% filter(!is.na(Sex_Category))
stripchart(Value ~ Sex_Category, data = jitter_data,
           method = "jitter", jitter = 0.2, vertical = TRUE,
           pch = 16, col = "darkgreen", main = "Jitter: Value by Sex",
           ylab = "Value (%)", las = 1)

# ---- Barplot: Mean Value by Education ----
edu_data <- df %>% filter(!is.na(Education_Category)) %>%
  group_by(Education_Category) %>%
  summarise(Mean = mean(Value, na.rm = TRUE))
barplot(edu_data$Mean, names.arg = edu_data$Education_Category, col = "coral",
        main = "Barplot: Mean Value by Education", ylab = "Mean Value (%)")
