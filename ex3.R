library(ggplot2)
library(patchwork)
library(dplyr)
library(knitr)


dataset <- read.csv("HW1_bike_sharing.csv", header = TRUE, sep = ',')


# ===== Item n.1 ===== 

# (classify the variables by type (categorical or numerical))
sprintf("Record index: Numerical, Discrete")
sprintf("Date of observation: Categorical")
sprintf("Season: Categorical, ordinal")
sprintf("Weather condition: Categorical, nominal")
sprintf("Temperature in C: Numerical, continuous")
sprintf("Number of casual users: Numerical, discrete")
sprintf("Number of registered users: Numerical, discrete")

# Identify the number of observations 
num_row <- nrow(dataset)

#Start and end dates
start_date <- dataset[1, "dteday"]
end_date <- dataset[731, "dteday"]

#===== Item n.2 =====

#Temperature
m_temp <- mean(dataset$temp) # Mean of the temperature inputs
q_temp <- quantile(dataset$temp, probs = c(0.25, 0.50, 0.75), na.rm = TRUE) # Gets all quantiles 
med_temp <- median(dataset$temp)

#Casual users
m_casual <- mean(dataset$casual) # Mean of the casual users inputs
q_casual <- quantile(dataset$casual, probs = c(0.25, 0.50, 0.75), na.rm = TRUE) # Gets all quantiles 
med_casual <- median(dataset$casual)

#Registered users
m_registered <- mean(dataset$registered) # Mean of the registered inputs
q_registered <- quantile(dataset$registered, probs = c(0.25, 0.50, 0.75), na.rm = TRUE) # Gets all quantiles 
med_registered <- median(dataset$registered)

#Create the table
table1 <- data.frame(
  Mean = c(mean(dataset$temp, na.rm = TRUE), 
           mean(dataset$casual, na.rm = TRUE), 
           mean(dataset$registered, na.rm = TRUE)),
  
  Median = c(median(dataset$temp, na.rm = TRUE), 
             median(dataset$casual, na.rm = TRUE), 
             median(dataset$registered, na.rm = TRUE)),
  
  Q1 = c(quantile(dataset$temp, 0.25, na.rm = TRUE), 
         quantile(dataset$casual, 0.25, na.rm = TRUE), 
         quantile(dataset$registered, 0.25, na.rm = TRUE)),
  
  Q = c(quantile(dataset$temp, 0.50, na.rm = TRUE), 
        quantile(dataset$casual, 0.50, na.rm = TRUE), 
        quantile(dataset$registered, 0.50, na.rm = TRUE)),
  
  Q3 = c(quantile(dataset$temp, 0.75, na.rm = TRUE), 
         quantile(dataset$casual, 0.75, na.rm = TRUE), 
         quantile(dataset$registered, 0.75, na.rm = TRUE))
)

rownames(table1) <- c("Temperature", "Casual users", "Registered users")
cat("\n===== Measures of central tendency and quartiles =====\n\n")
print(table1)

#===== Item n. 3 =====

# Convert to factor and assign labels
dataset$season <- factor(dataset$season,
                      levels = c(1, 2, 3, 4),
                      labels = c("Winter", "Spring", "Summer", "Autumn"))

dataset$weathersit <- ordered(dataset$weathersit,
                           levels = c(1, 2, 3, 4),
                           labels = c("Clear", "Cloudy", "Light Rain", "Heavy Rain"))
# Bar plot for both variables
plot_season <- ggplot(dataset, aes(x = season)) + geom_bar() + labs(title = "Seasons")
plot_weather <- ggplot(dataset, aes(x = weathersit)) + geom_bar() + labs(title = "Weather")

# Arrange plots side by side
plot_season + plot_weather

# Create a total users column first
dataset$total_users <- dataset$casual + dataset$registered

#Season summary
season_summary <- dataset %>%
  group_by(season) %>%
  summarise(
    total_casual = sum(casual, na.rm = TRUE),
    total_registered = sum(registered, na.rm = TRUE),
    total_users = sum(total_users, na.rm = TRUE),
    average_users = mean(total_users, na.rm = TRUE),
    n_observations = n()
  ) %>%
  arrange(desc(total_users))

# View the results
print(season_summary)

#Find the winner
season_summary[which.max(season_summary$total_users), ]

#Weather summary
weather_summary <- dataset %>%
  group_by(weathersit) %>%
  summarise(
    total_casual = sum(casual, na.rm = TRUE),
    total_registered = sum(registered, na.rm = TRUE),
    total_users = sum(casual + registered, na.rm = TRUE),
    average_daily_users = mean(casual + registered, na.rm = TRUE),
    number_of_days = n(),
    # Calculate usage per day to account for unequal sample sizes
    avg_usage_per_day = total_users / number_of_days
  ) %>%
  arrange(desc(total_users))

# View the results
print(weather_summary)

#Find the winner
weather_summary[which.max(weather_summary$total_users), ]

#===== Item n.4 ===== 

head(bike_data[c("casual_users", "registered_users", "total_users")])

# Convert date if it's not already in Date format
if (!inherits(dataset$dteday, "Date")) {
  dataset$dteday <- as.Date(dataset$dteday)
}

# Plot 1: Total Users Over Time
p1 <- ggplot(dataset, aes(x = dteday, y = total_users)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") +
  labs(title = "Time Series of Total Bike Users",
       x = "Date",
       y = "Total Users per Day") +
  theme_minimal()

# Plot 2: Temperature Over Time
p2 <- ggplot(dataset, aes(x = dteday, y = temp)) +
  geom_line(color = "darkorange", linewidth = 0.8) +
  geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") +
  labs(title = "Time Series of Temperature",
       x = "Date",
       y = "Temperature") +
  theme_minimal()

# Arrange plots vertically
p1 / p2

