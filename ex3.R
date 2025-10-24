dataset <- read.csv("HW1_bike_sharing.csv", header = TRUE, sep = ',')


# === ITEM 1 === 
# Record index: Numerical, Discrete 
# Date of observation: Categorical
# Season: Categorical, ordinal
# Weather condition: Categorical, nominal
# Temperature in C: Numerical, continuous
# Number of casual users: Numerical, discrete
# Number of registered users: Numerical, discrete

#=== ITEM N.2 ===
#Temperature
m_temp <- mean(dataset$temp) # Mean of the temperature inputs
q_temp <- quantile(dataset, probs = c(0.25, 0.50, 0.75), na.rm = TRUE) # Gets all quantiles 
med
med_temp <- median(dataset$temp)

#Casual users
#Registered users

