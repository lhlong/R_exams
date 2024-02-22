###
### Clear memory
###
rm(list = ls())

### load libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(lsr))
suppressPackageStartupMessages(library(olsrr))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(lm.beta))


### load data
brf <- read_csv("BRFSS2015_650.csv", show_col_types = FALSE)

# For this final section, I will make some analysis and explore data to find the relationship
# between 4 factors: Gender, marital status, mental health / physical health in the past 30 days
### Select 4 variables to explore in new ways
### 1. SEX
### 2. MARITAL
### 3. MENTHLTH
### 4. PHYSHLTH
data <- brf %>% select(SEX, MARITAL, MENTHLTH, PHYSHLTH)

### explore the data - overview
# get summary statistics for each columns
summary(data)
# show the structure of data including its type, length, contents
str(data)
# describe data
describe(data)
################################################################
################################################################
# Q10: Address the values of each of the variables
# Check the unique values of each variable
unique(data$SEX)
unique(data$MARITAL)
unique(data$PHYSHLTH)
unique(data$MENTHLTH)

# Check if there are any missing values
sum(is.na(data$SEX))
sum(is.na(data$MARITAL))
sum(is.na(data$PHYSHLTH))
sum(is.na(data$MENTHLTH))

# Check if "none" is equal to a value other than 0 for PHYSHLTH and MENTHLTH
data %>% filter(is.na(data$PHYSHLTH)) %>% summarise(count = n())
data %>% filter(is.na(data$MENTHLTH)) %>% summarise(count = n())

# Check if there are any extra decimals for PHYSHLTH and MENTHLTH
data %>% filter(!is.na(as.numeric(PHYSHLTH)) & floor(as.numeric(PHYSHLTH)) != as.numeric(PHYSHLTH)) %>% summarise(count = n())
data %>% filter(!is.na(as.numeric(MENTHLTH)) & floor(as.numeric(MENTHLTH)) != as.numeric(MENTHLTH)) %>% summarise(count = n())

################################################################
################################################################
# Q11: Remove any outliers for each applicable variable
# remove all NAN value and Refused answer and Don't know/Not sure answer
# I want to analyze to find out how factors of gender, marital status, mental health
# will make physical health not good, so I select the data with
# at least one day the physical health not good
df <- data %>%
  filter(!is.na(SEX)
         & (!is.na(MARITAL) & MARITAL != 9)
         & (!is.na(MENTHLTH) & MENTHLTH != 77 & MENTHLTH != 99)
         & (!is.na(PHYSHLTH) & PHYSHLTH >= 1 & PHYSHLTH <=30)
  )
df$MENTHLTH[df$MENTHLTH == 88] <- 0

# use boxplot function to identify the outliers for PHYSHLTH and MENTHLTH
# boxplot is a good way to identify outliers because they provide a visual representation
# of the distribution of the data, including the median, quartiles, and any outliers.
# The box in the plot represents the interquartile range (IQR), which is the range 
# between the first quartile (Q1) and the third quartile (Q3). 
# Any data points that fall outside the whiskers of the boxplot, 
# which extend 1.5 times the IQR from the edge of the box, are considered outliers.

# Remove outliers for MENTHLTH using the boxplot method
outliers <- boxplot(df$MENTHLTH, plot=FALSE)$out
df$MENTHLTH[which(df$MENTHLTH %in% outliers)] <- 0

# Remove outliers for PHYSHLTH using the boxplot method
outliers <- boxplot(df$PHYSHLTH, plot=TRUE)$out
df$PHYSHLTH[which(df$PHYSHLTH %in% outliers)] <- 0

################################################################
################################################################
# Q12: Complete exploratory analyses (for each variable) doing appropriate visualizations with ggplot2

# Create a bar chart of the counts for each gender
Q12a <- ggplot(df, aes(x = factor(SEX))) + 
  geom_bar() + 
  labs(x = "Gender", y = "Count", title = "Distribution of Gender")

# Create a bar chart of the counts for each marital status
Q12b <- ggplot(df, aes(x = factor(MARITAL))) + 
  geom_bar() + 
  labs(x = "Marital Status", y = "Count", title = "Distribution of Marital Status")

# Create a histogram of the number of days of poor mental health
Q12c <- ggplot(df, aes(x = MENTHLTH)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") + 
  labs(x = "Number of Days of Poor Mental Health",
       y = "Count", 
       title = "Distribution of Poor Mental Health")

# Create a histogram of the number of days of poor physical health
Q12d <- ggplot(df, aes(x = PHYSHLTH)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") + 
  labs(x = "Number of Days of Poor Physical Health",
       y = "Count", 
       title = "Distribution of Poor Physical Health")

# Create a box plot of PHYSHLTH by gender
Q12e <- ggplot(df, aes(x = factor(SEX), y = PHYSHLTH)) + 
  geom_boxplot() + 
  labs(x = "Gender",
       y = "Number of Days of Poor Physical Health", 
       title = "Distribution of Poor Physical Health by Gender")

# Create a box plot of MENTHLTH by gender
Q12f <- ggplot(df, aes(x = factor(SEX), y = MENTHLTH)) + 
  geom_boxplot() + 
  labs(x = "Gender", y = "Number of Days of Poor Mental Health", title = "Distribution of Poor Mental Health by Gender")


# Create a box plot of PHYSHLTH by marital status
Q12g <- ggplot(df, aes(x = factor(MARITAL), y = PHYSHLTH)) + 
  geom_boxplot() + 
  labs(x = "Marital status", y = "Number of Days of Poor Physical Health", title = "Distribution of Poor Physical Health by Marital status")

# Create a box plot of MENTHLTH by marital status
Q12h <- ggplot(df, aes(x = factor(MARITAL), y = MENTHLTH)) + 
  geom_boxplot() + 
  labs(x = "Marital status", y = "Number of Days of Poor Mental Health", title = "Distribution of Poor Mental Health by Marital status")

# Create a scatter plot of PHYSHLTH vs MENTHLTH
Q12i <- ggplot(df, aes(x = PHYSHLTH, y = MENTHLTH)) + 
  geom_point() + 
  labs(x = "Number of Days of Poor Physical Health", y = "Number of Days of Poor Mental Health", title = "Relationship between Poor Physical and Mental Health")

# Calculate the means and standard deviations of physical health and mental health by gender and marital status
health_summary <- df %>%
  group_by(SEX, MARITAL) %>%
  summarise(mean_physhlth = mean(PHYSHLTH),
            sd_physhlth = sd(PHYSHLTH),
            mean_menthlth = mean(MENTHLTH),
            sd_menthlth = sd(MENTHLTH))

# Plot the means of physical health and mental health by gender and marital status
Q12j <- ggplot(health_summary, aes(x = MARITAL, y = mean_physhlth, fill = SEX)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Physical Health by Gender and Marital Status",
       x = "Marital Status",
       y = "Mean Physical Health",
       fill = "Gender")
Q12k <- ggplot(health_summary, aes(x = MARITAL, y = mean_menthlth, fill = SEX)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Mental Health by Gender and Marital Status",
       x = "Marital Status",
       y = "Mean Mental Health",
       fill = "Gender")


################################################################
################################################################
# Q13: Run basic descriptive statistics

# Check the summary statistics of each variable
Q13a <- summary(df$SEX)
Q13b <- summary(df$MARITAL)
Q13c <- summary(df$PHYSHLTH)
Q13d <- summary(df$MENTHLTH)

# count the number of males and females
Q13e <- table(df$SEX)

# count the number of people in each marital status category
Q13f <- table(df$MARITAL)

# describe physical health
Q13g <- describe(df$PHYSHLTH, na.rm = TRUE)

# describe mental health
Q13h <- describe(df$MENTHLTH, na.rm = TRUE)

# Calculate the mean, median, standard deviation, and range of
# the number of days of poor mental health
Q13i <- mean(df$MENTHLTH, na.rm = TRUE)
Q13j <- median(df$MENTHLTH, na.rm = TRUE)
Q13k <- sd(df$MENTHLTH, na.rm = TRUE)
Q13l <- range(df$MENTHLTH, na.rm = TRUE)

# Calculate the mean, median, standard deviation, and range of
# the number of days of poor physical health
Q13m <- mean(df$PHYSHLTH, na.rm = TRUE)
Q13n <- median(df$PHYSHLTH, na.rm = TRUE)
Q13o <- sd(df$PHYSHLTH, na.rm = TRUE)
Q13p <- range(df$PHYSHLTH, na.rm = TRUE)
################################################################
################################################################
# Q14: Run an appropriate regression predicting one of those variables
### Find the relationship between factors: Gender, marital status, mental health in the past 1 month, physical health in the past 1 month
# Run a linear regression predicting physical health from other variables
model <- lm(PHYSHLTH ~ MENTHLTH + SEX + MARITAL, data = df)
Q14a <- summary(model)
# resid_qq_graph
Q14b <- ols_plot_resid_qq(model, print_plot = FALSE)
Q14c <- ols_plot_resid_fit(model, print_plot = FALSE)
Q14d <- ols_plot_resid_hist(model, print_plot = FALSE)
Q14e <- ols_plot_resid_stud(model, print_plot = FALSE)
# Identify the best model using stepwise regression
best_model <- step(model, direction = "both")
Q14f <- summary(best_model)
