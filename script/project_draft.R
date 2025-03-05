# Install and load necessary libraries
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Data Importing and Preprocessing ----

# Import the dataset
rawdata <- read_excel("data/rawdata.xlsx")

# Select relevant columns (exclude columns 2 to 12 and 29)
rawdata_cleaned <- rawdata[, -c(2:12, 29)]

# Filter rows where the second column is not "Neither" and the 15th column equals 4
processed_data <- rawdata_cleaned[rawdata_cleaned[[2]] != "Neither" & rawdata_cleaned[[15]] == 4, -15]

# Save the raw data and processed data to new CSV file
write.csv(rawdata, "data/rawdata.csv", row.names = FALSE)
write.csv(processed_data, "data/processed_data.csv", row.names = FALSE)