# Step 1: Dataset Description
# Load necessary libraries
install.packages(c("dplyr", "ggplot2", "readr", "tidyr","knitr"))
library(dplyr) 
library(ggplot2) 
library(readr)
library(tidyr)

# Load the dataset
covid_data <- read.csv("C:/Users/giaph/OneDrive/Máy tính/3rd semester/BDM300/Data mining project/Conditions_Contributing_to_COVID-19_Deaths__by_State_and_Age__Provisional_2020-2023.csv")

# View the structure of the dataset
str(covid_data)
head(covid_data)
tail(covid_data)

# Summarize the dataset
summary(covid_data)

# Step 2: Data Cleaning and Preprocessing
# Check for missing values
missing_values <- colSums(is.na(covid_data))
print(missing_values)

# Remove rows with missing values
covid_data_cleaned <- na.omit(covid_data)

# Convert Year and Month columns to integers 
covid_data_cleaned$Year <- as.integer(covid_data_cleaned$Year)
covid_data_cleaned$Month <- as.integer(covid_data_cleaned$Month)

# Check for remaining missing values
colSums(is.na(covid_data_cleaned))

# Calculate IQR for COVID-19 deaths
Q1 <- quantile(covid_data_cleaned$COVID.19.Deaths, 0.25, na.rm = TRUE)
Q3 <- quantile(covid_data_cleaned$COVID.19.Deaths, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Define outlier thresholds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Remove outliers
covid_data_cleaned <- covid_data_cleaned %>%
  filter(COVID.19.Deaths >= lower_bound & COVID.19.Deaths <= upper_bound)

# Confirm structure and summary after outlier removal
str(covid_data_cleaned)
summary(covid_data_cleaned)

# Step 3: Exploratory Data Analysis
# (a) Yearly deaths by age group
yearly_deaths_by_age <- covid_data_cleaned %>%
  group_by(Year, Age.Group) %>%
  summarise(Total_Deaths = sum(COVID.19.Deaths, na.rm = TRUE))

ggplot(yearly_deaths_by_age, aes(x = Year, y = Total_Deaths, color = Age.Group)) +
  geom_line(size = 1) +
  labs(title = "Yearly COVID-19 Deaths by Age Group",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal()

# (b) Top 10 states for respiratory diseases
state_condition_deaths <- covid_data_cleaned %>%
  filter(Condition.Group == "Respiratory diseases") %>%
  group_by(State) %>%
  summarise(Total_Deaths = sum(COVID.19.Deaths, na.rm = TRUE)) %>%
  arrange(desc(Total_Deaths))

top_states <- head(state_condition_deaths, 10)

ggplot(top_states, aes(x = reorder(State, Total_Deaths), y = Total_Deaths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 States for Respiratory Disease Deaths",
       x = "State",
       y = "Total Deaths") +
  theme_minimal()

# (c) Distribution of deaths by age group
ggplot(covid_data_cleaned, aes(x = Age.Group, y = COVID.19.Deaths)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Distribution of COVID-19 Deaths by Age Group",
    x = "Age Group",
    y = "COVID-19 Deaths"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (d) COVID-19 deaths by condition group
condition_group_deaths <- covid_data_cleaned %>%
  group_by(Condition.Group) %>%
  summarise(Total_Deaths = sum(COVID.19.Deaths, na.rm = TRUE)) %>%
  arrange(desc(Total_Deaths))

ggplot(condition_group_deaths, aes(x = reorder(Condition.Group, Total_Deaths), y = Total_Deaths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "COVID-19 Deaths by Condition Group",
    x = "Condition Group",
    y = "Total Deaths"
  ) +
  theme_minimal()


# Step 4: Correlation Analysis
ggplot(covid_data_cleaned, aes(x = Number.of.Mentions, y = COVID.19.Deaths)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  
  labs(title = "Relationship Between Mentions and COVID-19 Deaths",
       x = "Number of Mentions",
       y = "COVID-19 Deaths") +
  theme_minimal()

cor(covid_data_cleaned$Number.of.Mentions, covid_data_cleaned$COVID.19.Deaths, use = "complete.obs")


