# Install and load necessary libraries
required_packages <- c("readxl", "stats")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)


# Data Importing and Preprocessing ----

# Import the dataset
rawdata <- read_excel("data/rawdata.xlsx")

# Select relevant columns (exclude columns 2 to 12 and 29)
rawdata_cleaned <- rawdata[, -c(2:12, 29)]

# Filter rows where the second column is not "Neither" and the 15th column equals 4
processed_data <- rawdata_cleaned[rawdata_cleaned[[2]] != "Neither" & rawdata_cleaned[[15]] == 4, -15]

# Save the raw data and processed data to new CSV file
write.csv(rawdata, "data/rawdata.xlsx", row.names = FALSE)
write.csv(processed_data, "data/processed_data.xlsx", row.names = FALSE)


# Statistical Analysis: Normality Tests ----

# Normality Test for Each Attitudes for Before and After Taking the ITP course
# Combine the columns as specified
processed_data$pre_anxiety <- processed_data[, 4] + processed_data[, 5]
processed_data$pre_appreciation <- processed_data[, 6] + processed_data[, 7]
processed_data$pre_self_efficacy <- processed_data[, 8] + processed_data[, 9]
processed_data$post_anxiety <- processed_data[, 10] + processed_data[, 11]
processed_data$post_appreciation <- processed_data[, 12] + processed_data[, 13]
processed_data$post_self_efficacy <- processed_data[, 14] + processed_data[, 15]

# Select the newly created combined columns
combined_columns <- processed_data[, c("pre_anxiety", "pre_appreciation", "pre_self_efficacy", 
                                       "post_anxiety", "post_appreciation", "post_self_efficacy")]

shapiro_results <- apply(combined_columns, 2, function(x) shapiro.test(x))

# Print w statistics and p-values
for (i in 1:length(shapiro_results)) {
  cat(names(combined_columns)[i], ":\n")
  cat("  w statistic:", shapiro_results[[i]]$statistic, "\n")
  cat("  p-value:", shapiro_results[[i]]$p.value, "\n\n")
}


# Normality Test for Each Attituds across Pre-University Education
# Create a new column 'education_type' based on the original column
processed_data$education_type <- as.character(processed_data$`What is the highest level of maths you did before University?`)

# Combine all categories except "A level Further Maths" and "Advanced Higher Maths" into "Other"
processed_data$education_type[!(processed_data$education_type %in% c("A level Further Maths", "Advanced Higher Maths"))] <- "Other"

# List the education types
education_types <- unique(processed_data$education_type)

# Loop through each education type
for (education in education_types) {
  cat("Shapiro-Wilk Test for", education, "education type:\n")
  education_data <- processed_data[processed_data$education_type == education, ]
  
  for (col in c("pre_anxiety", "pre_appreciation", "pre_self_efficacy")) {
    
    column_data <- education_data[[col]]
    if (is.list(column_data)) {
      column_data <- as.numeric(unlist(column_data))
    } else {
      column_data <- as.numeric(column_data)
    }
    
    shapiro_result <- shapiro.test(column_data)
    
    cat(col, "\n")
    cat("    w statistic:", shapiro_result$statistic, "\n")
    cat("    p-value:", shapiro_result$p.value, "\n\n")
  }
}


# Normality Test for Each Attitudes for Before and After taking both PPS and APPS course
# List the course types
course_types <- unique(processed_data$`Did you take Proofs and Problem Solving (PPS), Accelerated Proofs and Problem Solving (APPS) or neither?`)

# Loop through course type
for (course in course_types) {
  cat("Shapiro-Wilk Test for", course, "course type:\n")
  course_data <- processed_data[processed_data$`Did you take Proofs and Problem Solving (PPS), Accelerated Proofs and Problem Solving (APPS) or neither?` == course, ]
  
  for (col in c("pre_anxiety", "pre_appreciation", "pre_self_efficacy", 
                "post_anxiety", "post_appreciation", "post_self_efficacy")) {
    
    # Check if the column is a list, if so unlist and convert to numeric
    column_data <- course_data[[col]]
    
    # If it's a list, unlist it and convert to numeric
    if (is.list(column_data)) {
      column_data <- as.numeric(unlist(column_data))
    } else {
      column_data <- as.numeric(column_data)
    }
    
    # Apply the Shapiro-Wilk test
    shapiro_result <- shapiro.test(column_data)
    
    # Print W statistic and p-value
    cat(col, "\n")
    cat("    w statistic:", shapiro_result$statistic, "\n")
    cat("    p-value:", shapiro_result$p.value, "\n\n")
  }
}


# Kruskal-Wallis Test for Research Question 1 ----

# Select the columns pre_anxiety, pre_appreciation, pre_self_efficacy
kruskal_columns <- processed_data[, c("pre_anxiety", "pre_appreciation", "pre_self_efficacy")]

# Perform Kruskal-Wallis test for each of these columns based on 'education_type'
kruskal_results <- apply(kruskal_columns, 2, function(x) kruskal.test(x ~ processed_data$education_type))

# Print the Kruskal-Wallis test results (chi-squared statistic and p-value)
for (i in 1:length(kruskal_results)) {
  cat("For column:", names(kruskal_columns)[i], "\n")
  cat("  Education Types:", paste(unique(processed_data$education_type), collapse = ", "), "\n")
  cat("  chi-squared:", kruskal_results[[i]]$statistic, "\n")
  cat("  p-value:", kruskal_results[[i]]$p.value, "\n\n")
}


# Wilcoxon Signed-Rank Test for Research Question 2 ----

# List of comparisons
comparisons <- list(
  c("pre_anxiety", "post_anxiety"),
  c("pre_appreciation", "post_appreciation"),
  c("pre_self_efficacy", "post_self_efficacy")
)

# Loop through each comparison
for (comparison in comparisons) {
  pre_col <- comparison[1]
  post_col <- comparison[2]
  
  valid_data <- processed_data[!is.na(processed_data[[pre_col]]) & !is.na(processed_data[[post_col]]), ]
  
  pre_data <- valid_data[[pre_col]]
  post_data <- valid_data[[post_col]]
  
  # If the data is still in a list format, unlist it and convert to numeric
  if (is.list(pre_data)) {
    pre_data <- as.numeric(unlist(pre_data))
  } else {
    pre_data <- as.numeric(pre_data)
  }
  
  if (is.list(post_data)) {
    post_data <- as.numeric(unlist(post_data))
  } else {
    post_data <- as.numeric(post_data)
  }
  
  # Apply the Wilcoxon Signed-Rank test for the pair
  wilcox_result <- wilcox.test(pre_data, post_data, paired = TRUE)
  
  # Print results
  cat("Wilcoxon Signed-Rank Test for", pre_col, "vs", post_col, ":\n")
  cat("  v:", wilcox_result$statistic, "\n")
  cat("  p-value:", wilcox_result$p.value, "\n\n")

  alpha <- 0.05  # Significance level
  if (wilcox_result$p.value < alpha) {
    cat("  The null hypothesis is rejected: A statistically significant difference exists between the two samples.\n\n")
  } else {
    cat("  The null hypothesis is not rejected: No statistically significant difference was observed between the two samples.\n\n")
  }
}


# Supplemental Question in Research Question 2 ----
processed_data <- processed_data[, -3]

# Split the data into two subsets based on whether the entry is "PPS" or "APPS"
pps_data <- processed_data[processed_data$`Did you take Proofs and Problem Solving (PPS), Accelerated Proofs and Problem Solving (APPS) or neither?` == "PPS", ]
apps_data <- processed_data[processed_data$`Did you take Proofs and Problem Solving (PPS), Accelerated Proofs and Problem Solving (APPS) or neither?` == "APPS", ]

# Save the PPS and APPS data to new CSV files
write.csv(pps_data, "data/pps_data.xlsx", row.names = FALSE)
write.csv(apps_data, "data/apps_data.xlsx", row.names = FALSE)


# Comparing Each Attitudes Before and After taking both PPS and APPS course Using Mann Whitney U Test
# List of variables to test
variables <- c("pre_anxiety", "pre_appreciation", "pre_self_efficacy", 
               "post_anxiety", "post_appreciation", "post_self_efficacy")

# Loop through each variable
for (var in variables) {
  # Convert to numeric
  pps_data[[var]] <- as.numeric(unlist(pps_data[[var]]))
  apps_data[[var]] <- as.numeric(unlist(apps_data[[var]]))
  
  # Perform the Mann-Whitney U test (Wilcoxon rank-sum test)
  mann_whitney_result <- wilcox.test(pps_data[[var]], apps_data[[var]], alternative = "two.sided")
  
  # Display the result of the test
  print(paste("Results for", var))
  print(mann_whitney_result)}


# Comparing Each Attitudes Before Taking PPS course using Wilcoxon Signed Rank Test
# Define column groups for pre and post data
column_groups <- list(
  list(pre = c(3, 4), post = c(9, 10), name = "anxiety"),
  list(pre = c(5, 6), post = c(11, 12), name = "appreciation"),
  list(pre = c(7, 8), post = c(13, 14), name = "self_efficacy")
)

# Loop through each column group
for (group in column_groups) {
  # Access the data inside the columns, convert them to numeric, and sum them
  pre_data <- as.numeric(as.character(pps_data[[group$pre[1]]])) + as.numeric(as.character(pps_data[[group$pre[2]]]))
  post_data <- as.numeric(as.character(pps_data[[group$post[1]]])) + as.numeric(as.character(pps_data[[group$post[2]]]))
  
  # Assign the pre and post data to the appropriate column names
  pps_data[[paste0("pre_", group$name)]] <- pre_data
  pps_data[[paste0("post_", group$name)]] <- post_data
  
  # Perform the Wilcoxon Signed Rank test for paired data (pre vs post)
  wilcoxon_result <- wilcox.test(pre_data, post_data, paired = TRUE, alternative = "two.sided")
  
  # Display the result of the test
  print(paste("Wilcoxon Signed Rank test results for", group$name))
  print(wilcoxon_result)
}


# Comparing Appreciation Change over taking APPS Course Using Wilcoxon Signed Rank Test
# Access the data inside these columns correctly and convert them to numeric
apps_data$pre_appreciation <- as.numeric(as.character(apps_data[[5]])) + as.numeric(as.character(apps_data[[6]]))
apps_data$post_appreciation <- as.numeric(as.character(apps_data[[11]])) + as.numeric(as.character(apps_data[[12]]))

# Now perform the Wilcoxon Signed Rank test for paired data (pre vs post)
wilcoxon_result <- wilcox.test(apps_data$pre_appreciation, apps_data$post_appreciation, paired = TRUE, alternative = "two.sided")

# Display the result of the test
print(wilcoxon_result)

# Comparing `Anxiety` and `Self_Efficacy` Change over taking APPS Course Using Paired T Test
# Ensure the relevant columns are numeric
apps_data$pre_anxiety <- as.numeric(as.character(apps_data[[3]])) + as.numeric(as.character(apps_data[[4]]))
apps_data$post_anxiety <- as.numeric(as.character(apps_data[[9]])) + as.numeric(as.character(apps_data[[10]]))

apps_data$pre_self_efficacy <- as.numeric(as.character(apps_data[[7]])) + as.numeric(as.character(apps_data[[8]]))
apps_data$post_self_efficacy <- as.numeric(as.character(apps_data[[13]])) + as.numeric(as.character(apps_data[[14]]))

# Perform the paired t-test for pre_anxiety vs post_anxiety
t_test_anxiety <- t.test(apps_data$pre_anxiety, apps_data$post_anxiety, paired = TRUE)

# Perform the paired t-test for pre_self_efficacy vs post_self_efficacy
t_test_self_efficacy <- t.test(apps_data$pre_self_efficacy, apps_data$post_self_efficacy, paired = TRUE)

# Print the results of both tests
cat("Paired t-test for pre_anxiety vs post_anxiety:\n")
print(t_test_anxiety)

cat("\nPaired t-test for pre_self_efficacy vs post_self_efficacy:\n")
print(t_test_self_efficacy)