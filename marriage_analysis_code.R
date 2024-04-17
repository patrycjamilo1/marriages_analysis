# Marital Couples in the USA: Observations on Work and Household

# Loading libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(graphics)
library(gridExtra)

# Loading dataset
USA_marriages <- read.csv("C:/Users/vanae/Desktop/marriage_analysis_project/data.txt")

# Checking for missing data
anyNA(USA_marriages)

# Displaying data structure
str(USA_marriages)

# Removing unnecessary column
data <- data.frame(USA_marriages[, -1])

# 1. Presenting the number of people working at home in 1975

# Counting the number of couples working at home
home_work <- sum(data$work == "yes")

# Calculating the percentage of couples working at home
percentage_home_work <- round((home_work / nrow(data)) * 100, digits = 2)

# Displaying the result
cat("Percentage of couples working at home:", percentage_home_work, "%\n")

# 2. Comparing the number of hours worked annually between men and women

# This variable should repeat the value "Female" and "Male" for the appropriate number of rows in the data frame
gender <- rep(c("Female", "Male"), each = nrow(data))

# Creating a data frame for comparing hours worked by women and men
hours_worked_data <- data.frame(Gender = gender, Hours = c(data$hoursw, data$hoursh))

# Creating a bar plot
barplot(tapply(hours_worked_data$Hours, hours_worked_data$Gender, mean), 
        names.arg = c("Female", "Male"),
        xlab = "Gender", ylab = "Average number of hours worked annually",
        main = "Comparison of hours worked between genders",
        col = c("pink", "green"))

# 3. Children

# Calculating the total number of children in the household
number_of_children <- data$child6 + data$child618

# Calculating the average number of children in the household
average_children <- round(mean(number_of_children), digits = 2)

# Calculating the percentage of households with children
percentage_with_children <- round((sum(number_of_children > 0) / nrow(data)) * 100)

# Calculating the minimum and maximum number of children in the household
min_children <- min(number_of_children)
max_children <- max(number_of_children)

# Creating a summary
children_summary <- data.frame(
  Average_Number_of_Children = average_children,
  Percentage_of_Households_with_Children = percentage_with_children,
  Minimum_Number_of_Children = min_children,
  Maximum_Number_of_Children = max_children
)

# Displaying the summary
print(children_summary)

# 4. Relationship between education level and men's earnings

# Comparing men's earnings by education level
education_income_aggregation <- aggregate(wageh ~ educh, data = data, FUN = mean)

# Sorting education levels in ascending order
education_income_aggregation <- education_income_aggregation[order(education_income_aggregation$educh), ]

# Creating a color gradient
color_gradient <- colorRampPalette(c("blue", "red"))

# Calculating colors for bars based on the gradient
bar_color <- color_gradient(length(education_income_aggregation$wageh))

# Bar plot showing the relationship between education level and men's earnings
barplot(education_income_aggregation$wageh, names.arg = education_income_aggregation$educh,
        xlab = "Years of Education", ylab = "Average earnings per hour in $",
        main = "Average men's earnings by education level",
        col = bar_color)

# 5. Relationship between family income and number of children

# Creating a new data frame with two variables: family income and number of children
income_and_children <- data.frame(Family_Income = data$income, Number_of_Children = data$child6 + data$child618)

# Line plot
ggplot(income_and_children, aes(x = Family_Income, y = Number_of_Children)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(color = "gray", size = 3, alpha = 0.5) +
  labs(title = "Relationship between family income and number of children",
       x = "Family Income",
       y = "Number of Children") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 6. Comparing reported and earned income of women

# Creating a data frame
income_comparison <- data.frame(
  Year = factor(c(rep("1975", nrow(data)), rep("1976", nrow(data)))),
  Earnings = c(data$hearnw, data$wagew)
)

# Plot
ggplot(income_comparison, aes(x = Year, y = Earnings)) +
  geom_point(color = "blue") +
  labs(title = "Comparison of hourly earnings of wives 
       in 1975 and reported in the 1976 interview",
       x = "Year",
       y = "Hourly Earnings") +
  theme_minimal()

# 7. Calculating the percentage of families in the lowest income brackets

percentile <- quantile(data$income, 0.1)

poverty <- subset(data, income <= percentile)

# Plot - number of children
children_plot <- ggplot(poverty, aes(x = "", fill = factor(child6 + child618))) +
  geom_bar(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Number of children in families 
       in poverty",
       fill = "Number of children",
       x = "") +
  theme_void()

# Plot - location
location_plot <- ggplot(poverty, aes(x = "", fill = city)) +
  geom_bar(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Location of families in poverty",
       fill = "City",
       x = "") +
  theme_void() +
  geom_text(aes(label = scales::percent(..count../sum(..count..))), stat = "count", position = position_stack(vjust = 0.5))

# Plot - mother's age
mother_age_plot <- ggplot(poverty, aes(x = "", fill = cut(agew, breaks = c(0, 20, 30, 40, 50, 60, Inf)))) +
  geom_bar(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Mother's age in families 
       in poverty",
       fill = "Mother's age",
       x = "") +
  theme_void() +
  geom_text(aes(label = scales::percent(..count../sum(..count..))), stat = "count", position = position_stack(vjust = 0.5))

# Plot - father's age
father_age_plot <- ggplot(poverty, aes(x = "", fill = cut(ageh, breaks = c(0, 20, 30, 40, 50, 60, Inf)))) +
  geom_bar(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Father's age in families in poverty",
       fill = "Father's age",
       x = "") +
  theme_void() +
  geom_text(aes(label = scales::percent(..count../sum(..count..))), stat = "count", position = position_stack(vjust = 0.5))

# Displaying plots in one window (pie charts)
grid.arrange(children_plot, location_plot, mother_age_plot, father_age_plot,
             ncol = 2, widths = c(1, 1), heights = c(1, 1))

# 8. Analysis of families with and without children

# Analysis for families with children
df_with_children <- subset(data, child6 + child618 > 0)

# Plot - mother's education
mother_education_plot <- ggplot(df_with_children, aes(x = educwm)) +
  geom_bar(fill = "red") +
  labs(title = 
         "Mother's education 
       in families with children",
       x = "Mother's education",
       y = "Number of people") +
  theme_minimal()

# Plot - father's education
father_education_plot <- ggplot(df_with_children, aes(x = educwf)) +
  geom_bar(fill = "green") +
  labs(title = 
         "Father's education 
       in families with children",
       x = "Father's education",
       y = "Number of people") +
  theme_minimal()

# Analysis for families without children
df_without_children <- subset(data, child6 + child618 == 0)

# Plot - wife's education
wife_education_plot <- ggplot(df_without_children, aes(x = educw)) +
  geom_bar(fill = "pink") +
  labs(title = 
         "Wife's education 
       in families without children",
       x = "Wife's education",
       y = "Number of people") +
  theme_minimal()

# Plot - husband's education
husband_education_plot <- ggplot(df_without_children, aes(x = educh)) +
  geom_bar(fill = "blue") +
  labs(title = 
         "Husband's education 
       in families without children",
       x = "Husband's education",
       y = "Number of people") +
  theme_minimal()

# Displaying plots in one window
grid.arrange(mother_education_plot, father_education_plot, wife_education_plot, husband_education_plot,
             ncol = 2, widths = c(1, 1), heights = c(1, 1))
