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
  df_final <- replace_na_with_mean_posixct(df_final, col)
}

posixct_columns <- c("Opened_At", "Closed_At", "Application_Created_At", "Resolved_At")

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
  df_final <- replace_na_with_mean_posixct(df_final, col)
}

na <- colSums(is.na(df_final))

write.csv(df_final, file = './modified_enrol_final_final.csv', row.names = FALSE)

enrol <- read.csv("/Users/thinuladamsith/Final Project/modified_enrol_final_final.csv")

numerical_data <- enrol %>% select_if(is.numeric)
for (column_name in names(numerical_data)) {
  # Create box plot for each numerical column
  boxplot(numerical_data[[column_name]], main = paste("Box Plot for", column_name), ylab = column_name)
}

