#Method to calculate the mode of a series
getMode <- function(x) {
  uniques <- unique(x)
  uniques[which.max(tabulate(match(x, uniques)))]
}


data_set <- c(15.8,22.7,26.8,19.1,18.5,14.4,8.3,25.9,26.4,9.8,21.9,10.5,17.3,6.2,18.0,22.9,24.6,19.4,12.3,15.9,20.1,17.0,22.3,27.5,23.9,17.5,11.0,20.4,16.2,20.8,20.9,21.4,18.0,24.3,11.8,17.9,18.7,12.8,15.5,19.2,13.9,28.6,19.4,21.6,13.5,24.6,20.0,24.1,9.0,17.6,25.7,20.1,13.2,23.7,10.7,19.0,14.5,18.1,31.8,28.5,22.7,15.2,23.0,29.6,11.2,14.7,20.5,26.6,13.3,18.1,24.8,26.1,7.7,22.5,19.3,19.4,16.7,16.9,23.5,18.4)

# ===== Item n. 1 ===== #
m_data <- mean(data_set) # Mean of the data set
med_data <- median(data_set) # Median of the data set (also second quantile)
mod_data <- getMode(data_set) # Mode of the data set
var_data <- var(data_set) # Variance of the data set
std_data <- sd(data_set) # Standard Deviation
cv_data <- std_data/m_data # Coefficient of variation

# ===== Item n. 2 ===== #

# Create a histogram
hist(data_set, 
     main = "Histogram",
     xlab = "Pollutant",
     ylab = "Density",
     col = "lightblue",
     border = "black",
     freq = FALSE,  # Plot densities instead of frequencies
     ylim = c(0, 0.08))
lines(density(data), col = "red", lwd = 2)  # Add density curve

# Create a boxplot
boxplot(data_set,
        main = "Boxplot",
        ylab = "Pollutant",
        col = "lightgreen",
        outline = TRUE)
stripchart(data, method = "jitter", pch = 16, col = "blue", 
           vertical = TRUE, add = TRUE, cex = 0.7)  # Add data points


# ===== Item n. 3 ===== #
quanta <- quantile(data_set, probs = c(0.25, 0.50, 0.75), na.rm = TRUE) # Gets all quantiles 
Q1 <- quanta[1]
Q2 <- quanta[2]
Q3 <- quanta[3]
iqr <- Q3 - Q1

# ===== Item n. 4 ===== #
limit <- 25 # Threshold of pollutant emission
excess <- sum(data_set > limit) # Sum of days on which the limit was exceeded
total_days <- length(data_set) # Total number of days
prop <- excess/total_days # Ratio of days over the limit vs days below the limit
percent <- prop*100 




print("========== DATA ANALYSIS ==========")
sprintf("Mean: %s", m_data)
sprintf("Median: %s", med_data)
sprintf("Mode: %s", mod_data)
sprintf("Variance: %s", var_data)
sprintf("Standard Deviation: %s", std_data)
sprintf("Coefficient of variation: %s",cv_data)
sprintf("Q1: %s", Q1)
sprintf("Q2: %s", Q2)
sprintf("Q3: %s", Q3)
sprintf("IQR: %s", iqr)
sprintf("Percentage of days above the limit X days below the limit: %s ", percent)
print("==========//==========")


