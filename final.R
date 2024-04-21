library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("/Users/thinuladamsith/Final Project/enrolment.csv")
df

df$Enrollment_Status <- factor(df$Enrollment_Status, levels = c(
  "New",
  "Resolved",
  "Closed",
  "Active",
  "Awaiting User Info",
  "Awaiting Problem"
))
df$Enrollment_Status <- as.numeric(df$Enrollment_Status)

df$Student_ID <- as.numeric(gsub("Caller ", "", df$Student_ID))

df$Opened_By <- as.numeric(gsub("Opened by ", "", df$Opened_By))

df$Application_Created_By <- as.numeric(gsub("^Created by (\\d+)$", "\\1", df$Application_Created_By))
df$Last_Updated_By <- as.numeric(gsub("^Updated by (\\d+)$", "\\1", df$Last_Updated_By))
df$Department <- as.numeric(gsub("^Location (\\d+)$", "\\1", df$Department))
df$Enrollment_Category <- as.numeric(gsub("^Category (\\d+)$", "\\1", df$Enrollment_Category))
df$Enrollment_Subcategory <- as.numeric(gsub("^Subcategory (\\d+)$", "\\1", df$Enrollment_Subcategory))
df$Issue_Description <- as.numeric(gsub("^Symptom (\\d+)$", "\\1", df$Issue_Description))

impact_mapping <- list(
  "1 - High" = 1,
  "2 - Medium" = 2,
  "3 - Low" = 3
)
df$Impact <- sapply(df$Impact, function(x) impact_mapping[[x]])

urgency_mapping <- list(
  "1 - High" = 1,
  "2 - Medium" = 2,
  "3 - Low" = 3
)
df$Urgency <- sapply(df$Urgency, function(x) urgency_mapping[[x]])

priority_mapping <- list(
  "1 - Critical" = 1,
  "2 - High" = 2,
  "3 - Moderate" = 3,
  "4 - Low" = 4
)
df$Priority <- sapply(df$Priority, function(x) priority_mapping[[x]])

df$Assignment_Group <- as.numeric(gsub("^Group (\\d+)$", "\\1", df$Assignment_Group))
df$Assigned_To <- as.numeric(gsub("^Resolver (\\d+)$", "\\1", df$Assigned_To))
df$Closed_Code <- as.numeric(gsub("^code (\\d+)$", "\\1", df$Closed_Code))
df$Resolved_By <- as.numeric(gsub("^Resolved by (\\d+)$", "\\1", df$Resolved_By))

unique_values <- unique(df$Notification_Status)

notification_mapping <- list(
  "Do Not Notify" = 1,
  "Send Email" = 2
)
df$Notification_Status <- sapply(df$Notification_Status, function(x) notification_mapping[[x]])

install.packages("lubridate")

library(lubridate)
library(stringr)

df$Opened_At <- dmy_hm(df$Opened_At)
df$Application_Created_At <- dmy_hm(df$Application_Created_At)
df$Last_Updated_At <- dmy_hm(df$Last_Updated_At)
df$Resolved_At <- dmy_hm(df$Resolved_At)
df$Closed_At <- dmy_hm(df$Closed_At)

unique <- unique(df$Contact_Type)

contact_mapping <- list(
  "Phone" = 1,
  "Email" = 2,
  "Self service" = 3,
  "Direct opening" = 4,
  "IVR" = 5
)
df$Contact_Type <- sapply(df$Contact_Type, function(x) contact_mapping[[x]])

install.packages("data.table")

library(data.table)

write.csv(df, file = './modified_enrol.csv', row.names = FALSE)

df_new <- read.csv("/Users/thinuladamsith/Final Project/modified_enrol.csv")

missing_val <- colSums(is.na(df_new))


df_new$Application_Created_at <- as.POSIXct(df_new$Application_Created_at, format = "%Y-%m-%d %H:%M:%S")
df_new$Opened_At <- as.POSIXct(df_new$Opened_At, format = "%Y-%m-%d %H:%M:%S")
df_new$Last_Updated_At <- as.POSIXct(df_new$Last_Updated_At, format = "%Y-%m-%d %H:%M:%S")
df_new$Resolved_At <- as.POSIXct(df_new$Resolved_At, format = "%Y-%m-%d %H:%M:%S")
df_new$Closed_At <- as.POSIXct(df_new$Closed_At, format = "%Y-%m-%d %H:%M:%S")


int_columns <- c("Enrollment_Status", "Student_ID", "Opened_By", "Application_Created_By", "Department", 
                 "Enrollment_Category", "Enrollment_Subcategory", "Issue_Description", "Assignment_Group",
                 "Assigned_To", "Closed_Code", "Resolved_By")

for(col in int_columns){
  mean_val <- mean(df_new[[col]], na.rm = TRUE)
  df_new[[col]][is.na(df_new[[col]])]<-mean_val
}


replace_na_with_mean_posixct <- function(data, column) {
  # Convert column to POSIXct if not already
  if (!is.POSIXct(data[[column]])) {
    data[[column]] <- as.POSIXct(data[[column]])
  }
  # Calculate the mean date and time
  mean_datetime <- mean(data[[column]], na.rm = TRUE)
  # Replace NA values with mean date and time
  data[[column]] <- data[[column]] %>% replace_na(mean_datetime)
  return(data)
}

for (col in posixct_columns) {
  df_new <- replace_na_with_mean_posixct(df_new, col)
}

na_find <- colSums(is.na(df_new))

posixct_columns <- c("Opened_At", "Closed_At")

# Function to replace NA values in POSIXct columns with mean date and time
replace_na_with_mean_posixct <- function(data, column) {
  # Convert column to POSIXct if not already
  if (!is.POSIXct(data[[column]])) {
    data[[column]] <- as.POSIXct(data[[column]])
  }
  # Calculate the mean date and time
  mean_datetime <- mean(data[[column]], na.rm = TRUE)
  # Replace NA values with mean date and time
  data[[column]] <- data[[column]] %>% replace_na(mean_datetime)
  return(data)
}

for (col in posixct_columns) {
  df_new <- replace_na_with_mean_posixct(df_new, col)
}

null_find <- colSums(is.na(df_new))

duplicated_rows <- df_new[duplicated(df_new), ]
duplicated_rows

write.csv(df, file = './modified_enrol_final.csv', row.names = FALSE)


df_final <- read.csv("/Users/thinuladamsith/Final Project/modified_enrol_final.csv")

o <- colSums(is.na(df_final))


int_columns <- c("Enrollment_Status", "Student_ID", "Opened_By", "Application_Created_By", "Department", 
                 "Enrollment_Category", "Enrollment_Subcategory", "Issue_Description", "Assignment_Group",
                 "Assigned_To", "Closed_Code", "Resolved_By")

for(col in int_columns){
  mean_val <- mean(df_final[[col]], na.rm = TRUE)
  df_final[[col]][is.na(df_final[[col]])]<-mean_val
}
na <- colSums(is.na(df_final))

write.csv(df_final, file = './modified_enrol_final_final.csv', row.names = FALSE)

enrol <- read.csv("/Users/thinuladamsith/Final Project/modified_enrol_final_final.csv")

enrol$Opened_At <- as.POSIXct(enrol$Opened_At, format = "%Y-%m-%d %H:%M:%S")
enrol$Application_Created_At <- as.POSIXct(enrol$Application_Created_At, format = "%Y-%m-%d %H:%M:%S")
enrol$Last_Updated_At <- as.POSIXct(enrol$Last_Updated_At, format = "%Y-%m-%d %H:%M:%S")
enrol$Resolved_At <- as.POSIXct(enrol$Resolved_At, format = "%Y-%m-%d %H:%M:%S")
enrol$Closed_At <- as.POSIXct(enrol$Closed_At, format = "%Y-%m-%d %H:%M:%S")

posixct_columns <- c("Opened_At", "Application_Created_At", "Last_Updated_At", "Resolved_At", "Closed_At")

replace_na_with_mean_posixct <- function(data, column) {
  if (!is.POSIXct(data[[column]])) {
    data[[column]] <- as.POSIXct(data[[column]])
  }
  mean_datetime <- mean(data[[column]], na.rm = TRUE)
  data[[column]] <- dplyr::coalesce(data[[column]], mean_datetime)
  return(data)
}

for (col in posixct_columns) {
  if (any(is.na(enrol[[col]]))) {
    enrol <- replace_na_with_mean_posixct(enrol, col)
  }
}

nullval <- colSums(is.na(enrol))

write.csv(enrol, file = './modified_enrol.csv', row.names = FALSE)

final_enrol <- read.csv("/Users/thinuladamsith/Final Project/modified_enrol.csv")
nann <- colSums(is.na(final_enrol))


final_enrol$Opened_At <- as.POSIXct(final_enrol$Opened_At)
final_enrol$Application_Created_At <- as.POSIXct(final_enrol$Application_Created_At)
final_enrol$Last_Updated_At <- as.POSIXct(final_enrol$Last_Updated_At)
final_enrol$Resolved_At <- as.POSIXct(final_enrol$Resolved_At)
final_enrol$Closed_At <- as.POSIXct(final_enrol$Closed_At)

numerical_variables <- c("Enrolment_Status", "Reassignment_Count", "Reopen_Count", 
                         "Modification_Count", "Student_ID", "Opened_By", 
                         "Application_Created_By", "Last_Updated_By", 
                         "Contact_Type", "Department", "Enrolment_Category", 
                         "Enrolment_Subcategory", "Issue_Description", 
                         "Impact", "Urgency", "Priority", "Assignment_Group", 
                         "Assigned_To", "Notification_Status", 
                         "Closed_Code", "Resolved_By")

for (variable in numerical_variables) {
  # Check for missing or non-numeric values
  if (any(is.na(final_enrol[[variable]])) || !all(is.numeric(final_enrol[[variable]]))) {
    cat("Variable", variable, "contains missing or non-numeric values. Skipping.\n")
    next
  }
  
  # Create box plot
  boxplot(final_enrol[[variable]], main = variable)
}

library(ggplot2)
library(dplyr)

# Define the variables for bar plots
bar_vars <- c("Reassignment_Count", "Reopen_Count", "Modification_Count", 
              "Application_Created_By", "Issue_Description", "Closed_Code")

# Create bar plots for each variable
par(mfrow=c(2, 3))  # Set up a 2x3 grid for the plots
for (var in bar_vars) {
  # Create a summary dataframe
  summary_df <- table(final_enrol[[var]])
  
  # Plot the bar plot
  barplot(summary_df, main = var, xlab = var, ylab = "Count")
}

for (var in bar_vars) {
  # Create a summary dataframe
  summary_df <- table(final_enrol[[var]])
  
  # Plot the bar plot
  barplot(summary_df, main = var, xlab = var, ylab = "Count")
}

remove_outliers <- function(data, var) {
  # Calculate the first and third quartiles
  q1 <- quantile(data[[var]], 0.25)
  q3 <- quantile(data[[var]], 0.75)
  
  # Calculate the interquartile range (IQR)
  iqr <- q3 - q1
  
  # Calculate the lower and upper bounds
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Filter the dataframe to exclude outliers
  filtered_data <- data[data[[var]] >= lower_bound & data[[var]] <= upper_bound, ]
  
  return(filtered_data)
}

vars_to_remove_outliers <- c("Reassignment_Count", "Reopen_Count", "Modification_Count", "Closed_Code")
for (var in vars_to_remove_outliers) {
  final_enrol <- remove_outliers(final_enrol, var)
}


bar_vars <- c("Reassignment_Count", "Reopen_Count", "Modification_Count", 
              "Application_Created_By", "Issue_Description", "Closed_Code")

# Create bar plots for each variable
par(mfrow=c(2, 3))  # Set up a 2x3 grid for the plots
for (var in bar_vars) {
  # Remove outliers using IQR method
  Q1 <- quantile(final_enrol[[var]], 0.25)
  Q3 <- quantile(final_enrol[[var]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  final_enrol_no_outliers <- final_enrol[final_enrol[[var]] >= lower_bound & final_enrol[[var]] <= upper_bound, ]
  
  # Create a summary dataframe
  summary_df <- table(final_enrol_no_outliers[[var]])
  
  # Plot the bar plot
  barplot(summary_df, main = var, xlab = var, ylab = "Count")
}

pie_vars <- c("Active_Status", "Enrolment_Status", "Opened_By", "Contact_Type",
              "Department", "Enrolment_Category", "Enrolment_Subcategory",
              "Impact", "Urgency", "Priority", "Assignment_Group",
              "Assigned_To", "Knowledge", "Priority_Confirmation",
              "Notification_Status", "Resolved_By")

par(mfrow=c(1, 1))
for (var in pie_vars) {
  # Calculate frequencies for the variable
  freq <- table(final_enrol[[var]])
  
  # Create the pie chart
  pie(freq, main=var)
}

freq_active <- table(final_enrol$Active_Status)
pie(freq_active, main = "Active_Status")

freq_contact <- table(final_enrol$Contact_Type)
pie(freq_contact, main = "Contact Type")

freq_enrol <- table(final_enrol$Enrollment_Category)
pie(freq_enrol, main = "Enrolment_Status")

freq_impact <- table(final_enrol$Impact)
pie(freq_impact, main = "Impact")

freq_urgency <- table(final_enrol$Urgency)
pie(freq_urgency, main = "Urgency")

freq_priority <- table(final_enrol$Priority)
pie(freq_priority, main = "Priority")

freq_notify <- table(final_enrol$Notification_Status)
pie(freq_notify, main = "Notification Status")

freq_resolved <- table(final_enrol$Resolved_By)
pie(freq_resolved, main = "Resolved By")

new_final_enrol <- final_enrol

check <- colSums(is.na(new_final_enrol))

write.csv(new_final_enrol, file = "new_final_enrol.csv", row.names = FALSE)
