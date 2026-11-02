
# ==========================================
# WEATHER TRENDS AND RAINFALL ANALYSIS
# Main R Analysis Script
# ==========================================

# 1. Import the weather dataset
weather <- read.csv("weather_trends_rainfall.csv")

# 2. Check the dataset
head(weather)
str(weather)
summary(weather)

# 3. Convert Date to date format
weather$Date <- as.Date(weather$Date)

# 4. Calculate average temperature
weather$AverageTemp_C <- (
  weather$MaxTemp_C + weather$MinTemp_C
) / 2

# 5. Calculate overall weather statistics
total_days <- nrow(weather)

average_max_temp <- mean(weather$MaxTemp_C)
average_min_temp <- mean(weather$MinTemp_C)
average_temp <- mean(weather$AverageTemp_C)

total_rainfall <- sum(weather$Rainfall_mm)
average_rainfall <- mean(weather$Rainfall_mm)

rainy_days <- sum(weather$Rainfall_mm >= 10)
light_rain_days <- sum(
  weather$Rainfall_mm > 0 &
    weather$Rainfall_mm < 10
)
no_rain_days <- sum(weather$Rainfall_mm == 0)

# 6. Display summary results
cat("Total Days:", total_days, "\n")

cat(
  "Average Maximum Temperature:",
  round(average_max_temp, 2), "°C\n"
)

cat(
  "Average Minimum Temperature:",
  round(average_min_temp, 2), "°C\n"
)

cat(
  "Average Temperature:",
  round(average_temp, 2), "°C\n"
)

cat(
  "Total Rainfall:",
  round(total_rainfall, 2), "mm\n"
)

cat(
  "Average Daily Rainfall:",
  round(average_rainfall, 2), "mm\n"
)

cat("Rainy Days:", rainy_days, "\n")
cat("Light Rain Days:", light_rain_days, "\n")
cat("No Rain Days:", no_rain_days, "\n")

# 7. Find the day with the highest rainfall
highest_rainfall <- weather[
  which.max(weather$Rainfall_mm),
]

print(highest_rainfall)

# 8. Find the hottest day
hottest_day <- weather[
  which.max(weather$MaxTemp_C),
]

print(hottest_day)

# 9. Find the coolest day
coolest_day <- weather[
  which.min(weather$MinTemp_C),
]

print(coolest_day)

# 10. Create weekly rainfall summary
weather$Week <- format(
  weather$Date,
  "%Y-%U"
)

weekly_rainfall <- aggregate(
  Rainfall_mm ~ Week,
  data = weather,
  FUN = sum
)

print(weekly_rainfall)

# 11. Create daily temperature summary
daily_temperature <- weather[
  c("Date", "MaxTemp_C", "MinTemp_C", "AverageTemp_C")
]

print(daily_temperature)

# 12. Display important analysis objects
print(weather)
print(weekly_rainfall)
print(daily_temperature)

