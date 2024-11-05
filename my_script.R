# Set Up Your R Environment

install.packages(c("tidyverse", "lubridate", "caret", "randomForest", "xgboost", "forecast", "readxl"))


# Load the Required Libraries
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(xgboost)
library(forecast)
library(readxl)


# Load Your Data

# Load the data from the Excel file
data <- read_excel("Kinshasa_rainfall.xlsx")

# Display the first few rows of the dataset
head(data)


# Convert Date column to Date type
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

#Verify Data Structure
str(data)

# Check for Missing Values
sum(is.na(data))

# Exploratory Data Analysis (Optional)

# Plotting rainfall over time
ggplot(data, aes(x = Date, y = Rainfall)) +
  geom_line() +
  labs(title = "Daily Rainfall in Kinshasa (1991-2023)",
       x = "Date", y = "Rainfall (mm)")

# Split the Data into Training and Testing Sets

# Creating the training and test sets
train_data <- data %>% filter(Year < 2021)
test_data <- data %>% filter(Year >= 2021)


# Model Building
#Random Forest Model
# Train the Random Forest model
set.seed(123)  # For reproducibility
rf_model <- randomForest(Rainfall ~ Day + Month + Year, data = train_data, importance = TRUE)
# Print the model summary
print(rf_model)

# Evaluate the Random Forest Model

# Predictions
rf_predictions <- predict(rf_model, test_data)

# Calculate RMSE for Random Forest
rf_rmse <- sqrt(mean((test_data$Rainfall - rf_predictions)^2, na.rm = TRUE))
cat("Random Forest RMSE:", rf_rmse, "\n")

# XGBoost Model

# Convert to matrix format
dtrain <- xgb.DMatrix(data.matrix(train_data[, c("Day", "Month", "Year")]), label = train_data$Rainfall)
dtest <- xgb.DMatrix(data.matrix(test_data[, c("Day", "Month", "Year")]), label = test_data$Rainfall)

# Train the XGBoost model
xgb_model <- xgboost(data = dtrain, max.depth = 6, eta = 0.1, nrounds = 100, objective = "reg:squarederror", verbose = 0)

# Predictions
xgb_predictions <- predict(xgb_model, dtest)

# Calculate RMSE for XGBoost
xgb_rmse <- sqrt(mean((test_data$Rainfall - xgb_predictions)^2, na.rm = TRUE))
cat("XGBoost RMSE:", xgb_rmse, "\n")


# Future Projections (2030-2099)

# Create Future Dates
# Create future dates for 2030-2099
future_dates <- expand.grid(
  Year = 2030:2099,
  Month = 1:12,
  Day = 1:31
)
# Remove invalid dates
future_dates <- future_dates %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>%
  filter(Day <= days_in_month(Date)) %>%
  select(Date, Year, Month, Day)

# Predict Rainfall for Future Dates
# Prepare future data for Random Forest
future_rf_predictions <- predict(rf_model, future_dates)

# Prepare future data for XGBoost
dfuture <- xgb.DMatrix(data.matrix(future_dates[, c("Day", "Month", "Year")]))
future_xgb_predictions <- predict(xgb_model, dfuture)


# Historical Anomalies

historical_data <- data %>%
  group_by(Month) %>%
  summarize(Average_Rainfall = mean(Rainfall, na.rm = TRUE))

data <- data %>%
  left_join(historical_data, by = "Month", suffix = c("", "_Avg")) %>%
  mutate(Anomaly = Rainfall - Average_Rainfall)


# Future Anomalies
# Create a data frame for future predictions
future_data_rf <- data.frame(Date = future_dates$Date, Predicted_Rainfall = future_rf_predictions)
future_data_xgb <- data.frame(Date = future_dates$Date, Predicted_Rainfall = future_xgb_predictions)

# Calculate future anomalies
future_data_rf <- future_data_rf %>%
  left_join(historical_data, by = "Month") %>%
  mutate(Anomaly = Predicted_Rainfall - Average_Rainfall)

future_data_xgb <- future_data_xgb %>%
  left_join(historical_data, by = "Month") %>%
  mutate(Anomaly = Predicted_Rainfall - Average_Rainfall)

# Save Results
write.csv(historical_data, "historical_rainfall_anomalies.csv", row.names = FALSE)
write.csv(future_data_rf, "future_rf_predictions_anomalies.csv", row.names = FALSE)
write.csv(future_data_xgb, "future_xgb_predictions_anomalies.csv", row.names = FALSE)











# Required Packages for Index Calculations
install.packages(c("tidyverse", "lubridate", "extRemes", "zoo"))
install.packages("SPEI")
# Install the SPEI package if you haven't done so
install.packages("SPEI")
update.packages()
update.packages(ask = FALSE)
install.packages("readxl")
install.packages("writexl")
install.packages("SCI")

# Load the SCI package
library(SCI)
library(writexl)

# Load the SPEI package
library(SPEI)
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)        # For rolling sums and other time-based operations
library(extRemes)   # For SPI
library(SPEI)
library(dplyr)
library(lubridate)

ls("package:SPEI")  # List all functions in the SPEI package
# Load Future Data Files
# Load Historical Data
# Reload the data, letting R guess the column types
historical_data <- read_excel("D:/MSc_PAUWES_Tlemcen University/6_Others/REFORUM_Training/RTraining/Kinshasa_rainfall.xlsx")

# Inspect the first few rows to check the data
head(historical_data)
str(historical_data)
# Convert 'Date' from datetime to date
historical_data <- historical_data %>%
  mutate(Date = as.Date(Date))
# Verify the conversion
head(historical_data$Date)

# Load Future Prediction Data (Random Forest and XGBoost)

future_rf <- read_csv("D:/MSc_PAUWES_Tlemcen University/6_Others/REFORUM_Training/RTraining/future_rf_predictions_anomalies.csv")
future_xgb <- read_csv("D:/MSc_PAUWES_Tlemcen University/6_Others/REFORUM_Training/RTraining/future_xgb_predictions_anomalies.csv")

# Convert date columns and extract Year, Month, Day

historical_data <- historical_data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date))
head(historical_data$Date)
str(historical_data)
head(historical_data$Date)

future_rf <- future_rf %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date))

future_xgb <- future_xgb %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Year = year(Date))

# Replace negative values in future datasets with zeros
future_rf <- future_rf %>%
  mutate(Predicted_Rainfall = ifelse(Predicted_Rainfall < 0, 0, Predicted_Rainfall))

future_xgb <- future_xgb %>%
  mutate(Predicted_Rainfall = ifelse(Predicted_Rainfall < 0, 0, Predicted_Rainfall))

# Save the future datasets after replacing negative values
write_csv(future_rf, "D:/MSc_PAUWES_Tlemcen University/6_Others/REFORUM_Training/RTraining/future_rf_predictions_anomalies_zeroed.csv")
write_csv(future_xgb, "D:/MSc_PAUWES_Tlemcen University/6_Others/REFORUM_Training/RTraining/future_xgb_predictions_anomalies_zeroed.csv")

# Functions to Calculate Rainfall Indices
# 1. Total Annual or Seasonal Rainfall (PRCPTOT)

calculate_PRCPOT <- function(data, rainfall_column) {
  data %>%
    group_by(Year) %>%
    summarize(PRCPTOT = sum(.data[[rainfall_column]], na.rm = TRUE))
}

# 2. Rainy Days (RD)

calculate_RD <- function(data, rainfall_column, threshold = 1) {
  data %>%
    group_by(Year) %>%
    summarize(RD = sum(.data[[rainfall_column]] > threshold, na.rm = TRUE))
}

# 3. Maximum 1-Day Precipitation (RX1day)

calculate_RX1day <- function(data, rainfall_column) {
  data %>%
    group_by(Year) %>%
    summarize(RX1day = max(.data[[rainfall_column]], na.rm = TRUE))
}

# 4. Maximum 5-Day Precipitation (RX5day)

calculate_RX5day <- function(data, rainfall_column) {
  data %>%
    group_by(Year) %>%
    summarize(RX5day = max(rollsum(.data[[rainfall_column]], 5, fill = NA, align = "right"), na.rm = TRUE))
}

# 5. Consecutive Dry Days (CDD)

calculate_CDD <- function(data, rainfall_column) {
  data %>%
    group_by(Year) %>%
    summarize(CDD = max(rle(.data[[rainfall_column]] < 1)$lengths[rle(.data[[rainfall_column]] < 1)$values], na.rm = TRUE))
}

# 6. Consecutive Wet Days (CWD)

calculate_CWD <- function(data, rainfall_column) {
  data %>%
    group_by(Year) %>%
    summarize(CWD = max(rle(.data[[rainfall_column]] >= 1)$lengths[rle(.data[[rainfall_column]] >= 1)$values], na.rm = TRUE))
}

# 7. Heavy Rainfall Days (R10mm, R20mm)

calculate_heavy_rainfall_days <- function(data, rainfall_column, threshold) {
  data %>%
    group_by(Year) %>%
    summarize(Heavy_Rainfall_Days = sum(.data[[rainfall_column]] > threshold, na.rm = TRUE)) %>%
    rename(Heavy_Rainfall_Days = Heavy_Rainfall_Days)
}

# 8. Very Wet Days (R95p)

calculate_Very_Wet_Days <- function(data, rainfall_column) {
  threshold <- quantile(data[[rainfall_column]], 0.95, na.rm = TRUE)
  data %>%
    group_by(Year) %>%
    summarize(Very_Wet_Days = sum(.data[[rainfall_column]] > threshold, na.rm = TRUE))
}

# 9. Extremely Wet Days (R99p)

calculate_Extremely_Wet_Days <- function(data, rainfall_column) {
  threshold <- quantile(data[[rainfall_column]], 0.99, na.rm = TRUE)
  data %>%
    group_by(Year) %>%
    summarize(Extremely_Wet_Days = sum(.data[[rainfall_column]] > threshold, na.rm = TRUE))
}

# 10. Simple Daily Intensity Index (SDII)
calculate_SDII <- function(data, rainfall_column) {
  data %>%
    group_by(Year) %>%
    summarize(SDII = mean(.data[[rainfall_column]][.data[[rainfall_column]] > 0], na.rm = TRUE))
}

# 11. Percentage of Annual Rainfall from Heavy Rainfall Days (PERC > X mm)

calculate_PERC <- function(data, rainfall_column, threshold) {
  data %>%
    group_by(Year) %>%
    summarize(PERC = (sum(.data[[rainfall_column]] > threshold, na.rm = TRUE) / sum(.data[[rainfall_column]], na.rm = TRUE)) * 100)
}

# 12. Wet Spell Duration (WSD)

calculate_WSD <- function(data, rainfall_column) {
  wet_spell <- rle(.data[[rainfall_column]] >= 1)
  data %>%
    group_by(Year) %>%
    summarize(WSD = max(wet_spell$lengths[wet_spell$values], na.rm = TRUE))
}

calculate_WSD <- function(data, rainfall_column) {
  # Use tidy evaluation to reference the column
  data %>%
    summarise(WSD = mean(.data[[rainfall_column]], na.rm = TRUE)) # Example logic
}

# 13. Dry Spell Duration (DSD)
calculate_DSD <- function(data, rainfall_column) {
  dry_spell <- rle(.data[[rainfall_column]] < 1)
  data %>%
    group_by(Year) %>%
    summarize(DSD = max(dry_spell$lengths[dry_spell$values], na.rm = TRUE))
}
calculate_DSD <- function(data, rainfall_column) {
  # Ensure tidy evaluation is used
  data %>%
    summarise(DSD = mean(.data[[rainfall_column]], na.rm = TRUE))  # Example logic
}

# 14. Rainfall Anomaly Index (RAI)

calculate_RAI <- function(data, rainfall_column) {
  avg_annual_rainfall <- mean(data[[rainfall_column]], na.rm = TRUE)
  data %>%
    group_by(Year) %>%
    summarize(RAI = sum(.data[[rainfall_column]] - avg_annual_rainfall, na.rm = TRUE))
}

# 15. Standardized Precipitation Index (SPI)

calculate_SPI <- function(data, rainfall_column, scale = 12) {
  spi <- SPI(data[[rainfall_column]], scale = scale)
  data.frame(Year = unique(data$Year), SPI = spi)
}

calculate_SPI <- function(data, rainfall_column, scale) {
  SPI(data[[rainfall_column]], scale = scale)  # Ensure SPI is called correctly
}

# 16. Precipitation Concentration Index (PCI)

calculate_PCI <- function(data, rainfall_column) {
  data %>%
    group_by(Year) %>%
    summarize(PCI = (sum((.data[[rainfall_column]] / sum(.data[[rainfall_column]], na.rm = TRUE))^2, na.rm = TRUE)) / (length(.data[[rainfall_column]]) / sum(.data[[rainfall_column]], na.rm = TRUE)^2))
}

# 17. Percentile-Based Thresholds (Q90, Q95, etc.)

calculate_percentile_thresholds <- function(data, rainfall_column, percentile) {
  threshold <- quantile(data[[rainfall_column]], percentile / 100, na.rm = TRUE)
  data %>%
    group_by(Year) %>%
    summarize(Percentile_Thresholds = sum(.data[[rainfall_column]] > threshold, na.rm = TRUE))
}

colnames(historical_data)
# Apply Functions to Calculate Indices for Historical and Future Data
# Historical Data

historical_PRCPOT <- calculate_PRCPOT(historical_data, "Rainfall")
head(historical_PRCPOT)
write_xlsx(historical_PRCPOT, "historical_rainfall_PRCPOT.xlsx")
saved_data <- read_excel("historical_rainfall_PRCPOT.xlsx")
head(saved_data)

historical_RD <- calculate_RD(historical_data, "Rainfall")
head(historical_RD)
write_xlsx(historical_RD, "historical_rainfall_RD.xlsx")
saved_data <- read_excel("historical_rainfall_RD.xlsx")
head(saved_data)

historical_RX1day <- calculate_RX1day(historical_data, "Rainfall")
head(historical_RX1day)
write_xlsx(historical_RX1day, "historical_rainfall_RX1day.xlsx")
saved_data <- read_excel("historical_rainfall_RX1day.xlsx")
head(saved_data)

historical_RX5day <- calculate_RX5day(historical_data, "Rainfall")
head(historical_RX5day)
write_xlsx(historical_RX5day, "historical_rainfall_RX5day.xlsx")
saved_data <- read_excel("historical_rainfall_RX5day.xlsx")
head(saved_data)

historical_CDD <- calculate_CDD(historical_data, "Rainfall")
head(historical_CDD)
write_xlsx(historical_CDD, "historical_rainfall_CDD.xlsx")
saved_data <- read_excel("historical_rainfall_CDD.xlsx")
head(saved_data)

historical_CWD <- calculate_CWD(historical_data, "Rainfall")
head(historical_CDD)
write_xlsx(historical_CDD, "historical_rainfall_CDD.xlsx")
saved_data <- read_excel("historical_rainfall_CDD.xlsx")
head(saved_data)

historical_R10mm <- calculate_heavy_rainfall_days(historical_data, "Rainfall", 10)
head(historical_R10mm)
write_xlsx(historical_R10mm, "historical_rainfall_R10mm.xlsx")
saved_data <- read_excel("historical_rainfall_R10mm.xlsx")
head(saved_data)

historical_R20mm <- calculate_heavy_rainfall_days(historical_data, "Rainfall", 20)
head(historical_R20mm)
write_xlsx(historical_R20mm, "historical_rainfall_R20mm.xlsx")
saved_data <- read_excel("historical_rainfall_R20mm.xlsx")
head(saved_data)

historical_Very_Wet_Days <- calculate_Very_Wet_Days(historical_data, "Rainfall")
head(historical_Very_Wet_Days)
write_xlsx(historical_Very_Wet_Days, "historical_rainfall_Very_Wet_Days.xlsx")
saved_data <- read_excel("historical_rainfall_Very_Wet_Days.xlsx")
head(saved_data)

historical_Extremely_Wet_Days <- calculate_Extremely_Wet_Days(historical_data, "Rainfall")
head(historical_Extremely_Wet_Days)
write_xlsx(historical_Extremely_Wet_Days, "historical_rainfall_Extremely_Wet_Days.xlsx")
saved_data <- read_excel("historical_rainfall_Extremely_Wet_Days.xlsx")
head(saved_data)

historical_SDII <- calculate_SDII(historical_data, "Rainfall")
head(historical_SDII)
write_xlsx(historical_SDII, "historical_rainfall_SDII.xlsx")
saved_data <- read_excel("historical_rainfall_SDII.xlsx")
head(saved_data)

historical_PERC_10mm <- calculate_PERC(historical_data, "Rainfall", 10)
head(historical_PERC_10mm)
write_xlsx(historical_PERC_10mm, "historical_rainfall_PERC_10mm.xlsx")
saved_data <- read_excel("historical_rainfall_PERC_10mm.xlsx")
head(saved_data)

historical_PERC_20mm <- calculate_PERC(historical_data, "Rainfall", 20)
head(historical_PERC_20mm)
write_xlsx(historical_PERC_20mm, "historical_rainfall_PERC_20mm.xlsx")
saved_data <- read_excel("historical_rainfall_PERC_20mm.xlsx")
head(saved_data)

historical_WSD <- calculate_WSD(historical_data, "Rainfall")
head(historical_WSD)
write_xlsx(historical_WSD, "historical_rainfall_WSD.xlsx")
saved_data <- read_excel("historical_rainfall_WSD.xlsx")
head(saved_data)

historical_DSD <- calculate_DSD(historical_data, "Rainfall")
head(historical_DSD)
write_xlsx(historical_DSD, "historical_rainfall_DSD.xlsx")
saved_data <- read_excel("historical_rainfall_DSD.xlsx")
head(saved_data)

historical_RAI <- calculate_RAI(historical_data, "Rainfall")
head(historical_RAI)
write_xlsx(historical_RAI, "historical_rainfall_RAI.xlsx")
saved_data <- read_excel("historical_rainfall_RAI.xlsx")
head(saved_data)

historical_SPI <- calculate_SPI(historical_data, "Rainfall", scale = 12)
head(historical_SPI)
historical_SPI_df <- data.frame(
  Date = historical_data$Date,
  SPI = historical_SPI$fitted
)
str(historical_SPI_df)
write_xlsx(historical_SPI_df, "historical_rainfall_SPI.xlsx")
saved_data <- read_excel("historical_rainfall_SPI.xlsx")
head(saved_data)

historical_SPI <- spi(historical_data$Rainfall, scale = 12)
head(historical_SPI)

historical_PCI <- calculate_PCI(historical_data, "Rainfall")
head(historical_PCI)
write_xlsx(historical_PCI, "historical_rainfall_PCI.xlsx")
saved_data <- read_excel("historical_rainfall_PCI.xlsx")
head(saved_data)

historical_Q90 <- calculate_percentile_thresholds(historical_data, "Rainfall", 90)
head(historical_Q90)
write_xlsx(historical_Q90, "historical_rainfall_Q90.xlsx")
saved_data <- read_excel("historical_rainfall_Q90.xlsx")
head(saved_data)

historical_Q95 <- calculate_percentile_thresholds(historical_data, "Rainfall", 95)
head(historical_Q95)
write_xlsx(historical_Q95, "historical_rainfall_Q95.xlsx")
saved_data <- read_excel("historical_rainfall_Q95.xlsx")
head(saved_data)

# Combine historical indices into a single data frame
historical_indices <- historical_PRCPOT %>%
  left_join(historical_RD, by = "Year") %>%
  left_join(historical_RX1day, by = "Year") %>%
  left_join(historical_RX5day, by = "Year") %>%
  left_join(historical_CDD, by = "Year") %>%
  left_join(historical_CWD, by = "Year") %>%
  left_join(historical_R10mm, by = "Year") %>%
  left_join(historical_R20mm, by = "Year") %>%
  left_join(historical_Very_Wet_Days, by = "Year") %>%
  left_join(historical_Extremely_Wet_Days, by = "Year") %>%
  left_join(historical_SDII, by = "Year") %>%
  left_join(historical_PERC_10mm, by = "Year") %>%
  left_join(historical_PERC_20mm, by = "Year") %>%
  left_join(historical_WSD, by = "Year") %>%
  left_join(historical_DSD, by = "Year") %>%
  left_join(historical_RAI, by = "Year") %>%
  left_join(historical_SPI, by = "Year") %>%
  left_join(historical_PCI, by = "Year") %>%
  left_join(historical_Q90, by = "Year") %>%
  left_join(historical_Q95, by = "Year")

# Save historical indices to Excel
write_xlsx(historical_indices, "D:/MSc_PAUWES_Tlemcen University/6_Others/REFORUM_Training/RTraining/historical_rainfall_indices.xlsx")


# Future Data
# Random Forest Predictions
library(dplyr)
future_rf_PRCPOT <- calculate_PRCPOT(future_rf, "Predicted_Rainfall")
head(future_rf_PRCPOT)
write_xlsx(future_rf_PRCPOT, "future_rf_rainfall_PRCPOT.xlsx")
saved_data <- read_excel("future_rf_rainfall_PRCPOT.xlsx")
head(saved_data)

future_rf_RD <- calculate_RD(future_rf, "Predicted_Rainfall")
head(future_rf_RD)
write_xlsx(future_rf_RD, "future_rf_rainfall_RD.xlsx")
saved_data <- read_excel("future_rf_rainfall_RD.xlsx")
head(saved_data)

future_rf_RX1day <- calculate_RX1day(future_rf, "Predicted_Rainfall")
head(future_rf_RX1day)
write_xlsx(future_rf_RX1day, "future_rf_rainfall_RX1day.xlsx")
saved_data <- read_excel("future_rf_rainfall_RX1day.xlsx")
head(saved_data)

future_rf_RX5day <- calculate_RX5day(future_rf, "Predicted_Rainfall")
head(future_rf_RX5day)
write_xlsx(future_rf_RX5day, "future_rf_rainfall_RX5day.xlsx")
saved_data <- read_excel("future_rf_rainfall_RX5day.xlsx")
head(saved_data)

future_rf_CDD <- future_rf %>%
  group_by(Year) %>%
  summarize(CDD = max(Predicted_Rainfall, na.rm = TRUE))
future_rf <- future_rf %>%
  mutate(Predicted_Rainfall = ifelse(Predicted_Rainfall == 0, NA, Predicted_Rainfall))
future_rf_CDD <- calculate_CDD(future_rf, "Predicted_Rainfall")
head(future_rf_CDD)
write_xlsx(future_rf_CDD, "future_rf_rainfall_CDD.xlsx")
saved_data <- read_excel("future_rf_rainfall_CDD.xlsx")
head(saved_data)

future_rf_CWD <- calculate_CWD(future_rf, "Predicted_Rainfall")
head(future_rf_CWD)
write_xlsx(future_rf_CWD, "future_rf_rainfall_CWD.xlsx")
saved_data <- read_excel("future_rf_rainfall_CWD.xlsx")
head(saved_data)

future_rf_R10mm <- calculate_heavy_rainfall_days(future_rf, "Predicted_Rainfall", 10)
head(future_rf_R10mm)
write_xlsx(future_rf_R10mm, "future_rf_rainfall_R10mm.xlsx")
saved_data <- read_excel("future_rf_rainfall_R10mm.xlsx")
head(saved_data)

future_rf_R20mm <- calculate_heavy_rainfall_days(future_rf, "Predicted_Rainfall", 20)
head(future_rf_R20mm)
write_xlsx(future_rf_R20mm, "future_rf_rainfall_R20mm.xlsx")
saved_data <- read_excel("future_rf_rainfall_R20mm.xlsx")
head(saved_data)

future_rf_Very_Wet_Days <- calculate_Very_Wet_Days(future_rf, "Predicted_Rainfall")
head(future_rf_Very_Wet_Days)
write_xlsx(future_rf_Very_Wet_Days, "future_rf_rainfall_Very_Wet_Days.xlsx")
saved_data <- read_excel("future_rf_rainfall_Very_Wet_Days.xlsx")
head(saved_data)

future_rf_Extremely_Wet_Days <- calculate_Extremely_Wet_Days(future_rf, "Predicted_Rainfall")
head(future_rf_Extremely_Wet_Days)
write_xlsx(future_rf_Extremely_Wet_Days, "future_rf_rainfall_Extremely_Wet_Days.xlsx")
saved_data <- read_excel("future_rf_rainfall_Extremely_Wet_Days.xlsx")
head(saved_data)

future_rf_SDII <- calculate_SDII(future_rf, "Predicted_Rainfall")
head(future_rf_SDII)
write_xlsx(future_rf_SDII, "future_rf_rainfall_SDII.xlsx")
saved_data <- read_excel("future_rf_rainfall_SDII.xlsx")
head(saved_data)

future_rf_PERC_10mm <- calculate_PERC(future_rf, "Predicted_Rainfall", 10)
head(future_rf_PERC_10mm)
write_xlsx(future_rf_PERC_10mm, "future_rf_rainfall_PERC_10mm.xlsx")
saved_data <- read_excel("future_rf_rainfall_PERC_10mm.xlsx")
head(saved_data)

future_rf_PERC_20mm <- calculate_PERC(future_rf, "Predicted_Rainfall", 20)
head(future_rf_PERC_20mm)
write_xlsx(future_rf_PERC_20mm, "future_rf_rainfall_PERC_20mm.xlsx")
saved_data <- read_excel("future_rf_rainfall_PERC_20mm.xlsx")
head(saved_data)

future_rf_WSD <- calculate_WSD(future_rf, "Predicted_Rainfall")
head(future_rf_WSD)
write_xlsx(future_rf_WSD, "future_rf_rainfall_WSD.xlsx")
saved_data <- read_excel("future_rf_rainfall_WSD.xlsx")
head(saved_data)

future_rf_DSD <- calculate_DSD(future_rf, "Predicted_Rainfall")
head(future_rf_DSD)
write_xlsx(future_rf_DSD, "future_rf_rainfall_DSD.xlsx")
saved_data <- read_excel("future_rf_rainfall_DSD.xlsx")
head(saved_data)

future_rf_RAI <- calculate_RAI(future_rf, "Predicted_Rainfall")
head(future_rf_RAI)
write_xlsx(future_rf_RAI, "future_rf_rainfall_RAI.xlsx")
saved_data <- read_excel("future_rf_rainfall_RAI.xlsx")
head(saved_data)

future_rf_SPI <- calculate_SPI(future_rf, "Predicted_Rainfall", scale = 12)
head(future_rf_SPI)
write_xlsx(future_rf_SPI, "future_rf_rainfall_SPI.xlsx")
saved_data <- read_excel("future_rf_rainfall_SPI.xlsx")
head(saved_data)

future_rf_PCI <- calculate_PCI(future_rf, "Predicted_Rainfall")
head(future_rf_PCI)
write_xlsx(future_rf_PCI, "future_rf_rainfall_PCI.xlsx")
saved_data <- read_excel("future_rf_rainfall_PCI.xlsx")
head(saved_data)

future_rf_Q90 <- calculate_percentile_thresholds(future_rf, "Predicted_Rainfall", 90)
head(future_rf_Q90)
write_xlsx(future_rf_Q90, "future_rf_rainfall_Q90.xlsx")
saved_data <- read_excel("future_rf_rainfall_Q90.xlsx")
head(saved_data)

future_rf_Q95 <- calculate_percentile_thresholds(future_rf, "Predicted_Rainfall", 95)
head(future_rf_Q90)
write_xlsx(future_rf_Q95, "future_rf_rainfall_Q95.xlsx")
saved_data <- read_excel("future_rf_rainfall_Q95.xlsx")
head(saved_data)

# Combine future RF indices into a single data frame
future_rf_indices <- future_rf_PRCPOT %>%
  left_join(future_rf_RD, by = "Year") %>%
  left_join(future_rf_RX1day, by = "Year") %>%
  left_join(future_rf_RX5day, by = "Year") %>%
  left_join(future_rf_CDD, by = "Year") %>%
  left_join(future_rf_CWD, by = "Year") %>%
  left_join(future_rf_R10mm, by = "Year") %>%
  left_join(future_rf_R20mm, by = "Year") %>%
  left_join(future_rf_Very_Wet_Days, by = "Year") %>%
  left_join(future_rf_Extremely_Wet_Days, by = "Year") %>%
  left_join(future_rf_SDII, by = "Year") %>%
  left_join(future_rf_PERC_10mm, by = "Year") %>%
  left_join(future_rf_PERC_20mm, by = "Year") %>%
  left_join(future_rf_WSD, by = "Year") %>%
  left_join(future_rf_DSD, by = "Year") %>%
  left_join(future_rf_RAI, by = "Year") %>%
  left_join(future_rf_SPI, by = "Year") %>%
  left_join(future_rf_PCI, by = "Year") %>%
  left_join(future_rf_Q90, by = "Year") %>%
  left_join(future_rf_Q95, by = "Year")

# Save future RF indices to Excel
write_csv(future_rf_indices, "D:/MSc_PAUWES_Tlemcen University/6_Others/REFORUM_Training/RTraining/future_rf_rainfall_indices.csv")

# Future Data (XGBoost Predictions)
# Calculate indices for Future XGBoost Data
future_xgb_PRCPOT <- calculate_PRCPOT(future_xgb, "Predicted_Rainfall")
head(future_xgb_PRCPOT)
write_xlsx(future_xgb_PRCPOT, "future_xgb_rainfall_PRCPOT.xlsx")
saved_data <- read_excel("future_xgb_rainfall_PRCPOT.xlsx")
head(saved_data)

future_xgb_RD <- calculate_RD(future_xgb, "Predicted_Rainfall")
head(future_xgb_RD)
write_xlsx(future_xgb_RD, "future_xgb_rainfall_RD.xlsx")
saved_data <- read_excel("future_xgb_rainfall_RD.xlsx")
head(saved_data)

future_xgb_RX1day <- calculate_RX1day(future_xgb, "Predicted_Rainfall")
head(future_xgb_RX1day)
write_xlsx(future_xgb_RX1day, "future_xgb_rainfall_RX1day.xlsx")
saved_data <- read_excel("future_xgb_rainfall_RX1day.xlsx")
head(saved_data)

future_xgb_RX5day <- calculate_RX5day(future_xgb, "Predicted_Rainfall")
head(future_xgb_RX5day)
write_xlsx(future_xgb_RX5day, "future_xgb_rainfall_RX5day.xlsx")
saved_data <- read_excel("future_xgb_rainfall_RX5day.xlsx")
head(saved_data)


future_xgb_CDD <- calculate_CDD(future_xgb, "Predicted_Rainfall")
head(future_xgb_CDD)
write_xlsx(future_xgb_CDD, "future_xgb_rainfall_CDD.xlsx")
saved_data <- read_excel("future_xgb_rainfall_CDD.xlsx")
head(saved_data)

future_xgb_CWD <- calculate_CWD(future_xgb, "Predicted_Rainfall")
head(future_xgb_CWD)
write_xlsx(future_xgb_CWD, "future_xgb_rainfall_CWD.xlsx")
saved_data <- read_excel("future_xgb_rainfall_CWD.xlsx")
head(saved_data)

future_xgb_R10mm <- calculate_heavy_rainfall_days(future_xgb, "Predicted_Rainfall", threshold = 10)
head(future_xgb_R10mm)
write_xlsx(future_xgb_R10mm, "future_xgb_rainfall_R10mm.xlsx")
saved_data <- read_excel("future_xgb_rainfall_R10mm.xlsx")
head(saved_data)

future_xgb_R20mm <- calculate_heavy_rainfall_days(future_xgb, "Predicted_Rainfall", threshold = 20)
head(future_xgb_R20mm)
write_xlsx(future_xgb_R20mm, "future_xgb_rainfall_R20mm.xlsx")
saved_data <- read_excel("future_xgb_rainfall_R20mm.xlsx")
head(saved_data)


future_xgb_Very_Wet_Days <- calculate_Very_Wet_Days(future_xgb, "Predicted_Rainfall")
head(future_xgb_Very_Wet_Days)
write_xlsx(future_xgb_Very_Wet_Days, "future_xgb_rainfall_Very_Wet_Days.xlsx")
saved_data <- read_excel("future_xgb_rainfall_Very_Wet_Days.xlsx")
head(saved_data)

future_xgb_Extremely_Wet_Days <- calculate_Extremely_Wet_Days(future_xgb, "Predicted_Rainfall")
head(future_xgb_Extremely_Wet_Days)
write_xlsx(future_xgb_Extremely_Wet_Days, "future_xgb_rainfall_Extremely_Wet_Days.xlsx")
saved_data <- read_excel("future_xgb_rainfall_Extremely_Wet_Days.xlsx")
head(saved_data)

future_xgb_SDII <- calculate_SDII(future_xgb, "Predicted_Rainfall")
head(future_xgb_SDII)
write_xlsx(future_xgb_SDII, "future_xgb_rainfall_SDII.xlsx")
saved_data <- read_excel("future_xgb_rainfall_SDII.xlsx")
head(saved_data)


future_xgb_PERC_10 <- calculate_PERC(future_xgb, "Predicted_Rainfall", threshold = 10)
head(future_xgb_PERC_10)
write_xlsx(future_xgb_PERC_10, "future_xgb_rainfall_PERC_10.xlsx")
saved_data <- read_excel("future_xgb_rainfall_PERC_10.xlsx")
head(saved_data)

future_xgb_WSD <- calculate_WSD(future_xgb, "Predicted_Rainfall")
head(future_xgb_WSD)
write_xlsx(future_xgb_WSD, "future_xgb_rainfall_WSD.xlsx")
saved_data <- read_excel("future_xgb_rainfall_WSD.xlsx")
head(saved_data)

future_xgb_DSD <- calculate_DSD(future_xgb, "Predicted_Rainfall")
head(future_xgb_DSD)
write_xlsx(future_xgb_DSD, "future_xgb_rainfall_DSD.xlsx")
saved_data <- read_excel("future_xgb_rainfall_DSD.xlsx")
head(saved_data)

future_xgb_RAI <- calculate_RAI(future_xgb, "Predicted_Rainfall")
head(future_xgb_RAI)
write_xlsx(future_xgb_RAI, "future_xgb_rainfall_RAI.xlsx")
saved_data <- read_excel("future_xgb_rainfall_RAI.xlsx")
head(saved_data)

future_xgb_SPI <- calculate_SPI(future_xgb, "Predicted_Rainfall", scale = 12)
head(future_xgb_SPI)
write_xlsx(future_xgb_SPI, "future_xgb_rainfall_SPI.xlsx")
saved_data <- read_excel("future_xgb_rainfall_SPI.xlsx")
head(saved_data)

future_xgb_PCI <- calculate_PCI(future_xgb, "Predicted_Rainfall")
head(future_xgb_PCI)
write_xlsx(future_xgb_PCI, "future_xgb_rainfall_PCI.xlsx")
saved_data <- read_excel("future_xgb_rainfall_PCI.xlsx")
head(saved_data)

future_xgb_Q90 <- calculate_percentile_thresholds(future_xgb, "Predicted_Rainfall", percentile = 90)
head(future_xgb_Q90)
write_xlsx(future_xgb_Q90, "future_xgb_rainfall_Q90.xlsx")
saved_data <- read_excel("future_xgb_rainfall_Q90.xlsx")
head(saved_data)

future_xgb_Q95 <- calculate_percentile_thresholds(future_xgb, "Predicted_Rainfall", percentile = 95)
head(future_xgb_Q95)
write_xlsx(future_xgb_Q95, "future_xgb_rainfall_Q95.xlsx")
saved_data <- read_excel("future_xgb_rainfall_Q95.xlsx")
head(saved_data)

# Combine all indices into a single data frame
future_xgb_indices <- future_xgb_PRCPOT %>%
  left_join(future_xgb_RD, by = "Year") %>%
  left_join(future_xgb_RX1day, by = "Year") %>%
  left_join(future_xgb_RX5day, by = "Year") %>%
  left_join(future_xgb_CDD, by = "Year") %>%
  left_join(future_xgb_CWD, by = "Year") %>%
  left_join(future_xgb_R10mm, by = "Year") %>%
  left_join(future_xgb_R20mm, by = "Year") %>%
  left_join(future_xgb_Very_Wet_Days, by = "Year") %>%
  left_join(future_xgb_Extremely_Wet_Days, by = "Year") %>%
  left_join(future_xgb_SDII, by = "Year") %>%
  left_join(future_xgb_PERC_10, by = "Year") %>%
  left_join(future_xgb_WSD, by = "Year") %>%
  left_join(future_xgb_DSD, by = "Year") %>%
  left_join(future_xgb_RAI, by = "Year") %>%
  left_join(future_xgb_SPI, by = "Year") %>%
  left_join(future_xgb_PCI, by = "Year") %>%
  left_join(future_xgb_Q90, by = "Year") %>%
  left_join(future_xgb_Q95, by = "Year")

# Save the future XGBoost indices to an Excel file
write_csv(future_xgb_indices, "D:/MSc_PAUWES_Tlemcen University/6_Others/REFORUM_Training/RTraining/future_xgb_indices.csv")










# Define the index function for heavy rainfall days
calculate_heavy_rainfall_days <- function(data, rainfall_column, threshold) {
  heavy_rainfall_days <- sum(data[[rainfall_column]] > threshold)
  return(data.frame(Total_Heavy_Rainfall_Days = heavy_rainfall_days))
}

# Define the index function for percentile thresholds
calculate_percentile_thresholds <- function(data, rainfall_column, percentile) {
  threshold_value <- quantile(data[[rainfall_column]], probs = percentile / 100, na.rm = TRUE)
  very_wet_days <- sum(data[[rainfall_column]] > threshold_value)
  return(data.frame(Total_Very_Wet_Days = very_wet_days))
}

# Sensitivity Analysis Function for Historical and Future Data (Thresholds)
sensitivity_analysis_thresholds <- function(data, rainfall_column, thresholds, index_function, dataset_label) {
  results <- list()
  
  for (threshold in thresholds) {
    result <- index_function(data, rainfall_column, threshold)
    results[[as.character(threshold)]] <- result
  }
  
  # Combine all results into a single data frame
  results_df <- do.call(rbind, lapply(names(results), function(th) {
    cbind(Threshold = th, results[[th]])
  }))
  
  results_df$Dataset <- dataset_label
  return(results_df)
}

# Sensitivity Analysis Function for Historical and Future Data (Percentiles)
sensitivity_analysis_percentiles <- function(data, rainfall_column, percentiles, index_function, dataset_label) {
  results <- list()
  
  for (percentile in percentiles) {
    result <- index_function(data, rainfall_column, percentile)
    results[[as.character(percentile)]] <- result
  }
  
# Combine all results into a single data frame
  results_df <- do.call(rbind, lapply(names(results), function(p) {
    cbind(Percentile = p, results[[p]])
  }))
  
  results_df$Dataset <- dataset_label
  return(results_df)
}

# Define thresholds and percentiles to test
thresholds_to_test <- seq(10, 100, by = 10)  # Rainfall thresholds from 10 mm to 100 mm
percentiles_to_test <- c(90, 95, 99)  # 90th, 95th, and 99th percentiles

# Run sensitivity analysis for historical data
sensitivity_heavy_rainfall_hist <- sensitivity_analysis_thresholds(historical_data, "Rainfall", thresholds_to_test, calculate_heavy_rainfall_days, "Historical")
sensitivity_very_wet_days_hist <- sensitivity_analysis_percentiles(historical_data, "Rainfall", percentiles_to_test, calculate_percentile_thresholds, "Historical")

# View the results for historical data (thresholds and percentiles)
print(sensitivity_heavy_rainfall_hist)
print(sensitivity_very_wet_days_hist)

# Future Data Sensitivity Analysis for RF Model (Thresholds and Percentiles)
sensitivity_heavy_rainfall_rf <- sensitivity_analysis_thresholds(future_rf, "Predicted_Rainfall", thresholds_to_test, calculate_heavy_rainfall_days, "Future_RF")
sensitivity_very_wet_days_rf <- sensitivity_analysis_percentiles(future_rf, "Predicted_Rainfall", percentiles_to_test, calculate_percentile_thresholds, "Future_RF")

# View the results for future RF model projections
print(sensitivity_heavy_rainfall_rf)
print(sensitivity_very_wet_days_rf)

# Future Data Sensitivity Analysis for XGB Model (Thresholds and Percentiles)
sensitivity_heavy_rainfall_xgb <- sensitivity_analysis_thresholds(future_xgb, "Predicted_Rainfall", thresholds_to_test, calculate_heavy_rainfall_days, "Future_XGB")
sensitivity_very_wet_days_xgb <- sensitivity_analysis_percentiles(future_xgb, "Predicted_Rainfall", percentiles_to_test, calculate_percentile_thresholds, "Future_XGB")

# View the results for future XGB model projections
print(sensitivity_heavy_rainfall_xgb)
print(sensitivity_very_wet_days_xgb)






# Install devtools if you haven't already
install.packages("devtools")

# Load devtools
library(devtools)

# Use devtools to install packages
devtools::install_github("ggplot2")
devtools::install_github("dplyr")

update.packages(ask = FALSE)  # Update all packages without asking

install.packages("ggplot2")
install.packages("dplyr")

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Function to plot sensitivity analysis results for thresholds
plot_sensitivity_thresholds <- function(results_df, title) {
  ggplot(results_df, aes(x = as.numeric(Threshold), y = Total_Heavy_Rainfall_Days, color = Dataset)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = title, x = "Rainfall Threshold (mm)", y = "Total Heavy Rainfall Days") +
    theme_minimal() +
    scale_color_manual(values = c("Historical" = "blue", "Future_RF" = "red")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_blank())
}

# Function to plot sensitivity analysis results for percentiles
plot_sensitivity_percentiles <- function(results_df, title) {
  ggplot(results_df, aes(x = as.numeric(Percentile), y = Total_Very_Wet_Days, color = Dataset)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = title, x = "Rainfall Percentile (%)", y = "Total Very Wet Days") +
    theme_minimal() +
    scale_color_manual(values = c("Historical" = "blue", "Future_RF" = "red")) +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_blank())
}

# Sample data for thresholds
combined_thresholds <- data.frame(
  Threshold = c(10, 20, 30, 40, 50),
  Total_Heavy_Rainfall_Days = c(641, 441, 290, 199, 155),
  Dataset = rep("Historical", 5)  # Update with actual data labels
)

# Sample data for percentiles
combined_percentiles <- data.frame(
  Percentile = c(90, 95, 99, 100),
  Total_Very_Wet_Days = c(50, 30, 10, 5),
  Dataset = rep("Historical", 4)  # Update with actual data labels
)

# Create and display plots
plot1 <- plot_sensitivity_thresholds(combined_thresholds, "Sensitivity Analysis: Total Heavy Rainfall Days by Threshold")
plot2 <- plot_sensitivity_percentiles(combined_percentiles, "Sensitivity Analysis: Total Very Wet Days by Percentile")

# Print plots to the console
print(plot1)
print(plot2)
