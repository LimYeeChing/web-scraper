MASTER_CSV <- "all_products.csv"

differentiate_if_name_taken <- function(filename) {
  # If a file path of csv is taken, returns the path with (1) appended
    if(file.exists(filename)){
        filename <- gsub(".csv", "", filename)
        filename <- paste0(filename, "(1).csv")
        return(differentiate_if_name_taken(filename))
    }

    return(filename)
}

# Compare this week (today) with last week (7 days ago)
date_value1 <- Sys.Date() 
date_value2 <- Sys.Date() - 7

formatted_date <- format(date_value1, format = "%d-%b-%Y")
month <- format(date_value1, "%m")
year <- as.numeric(format(date_value1, "%Y"))
week <- ceiling(as.numeric(format(date_value1, "%d")) / 7)

last_weeks_month <- format(date_value2, "%m")
last_weeks_year <- as.numeric(format(date_value2, "%Y"))
last_weeks_week <- ceiling(as.numeric(format(date_value2, "%d")) / 7)

this_week_path <- paste("Results", month, year, "Week", week, sep = "_")
last_week_path <- paste("Results", last_weeks_month, last_weeks_year, "Week", last_weeks_week, sep = "_")

this_week_csv <- read.csv(paste0(this_week_path, "/COMPILATION.csv"))
last_week_csv <- read.csv(paste0(last_week_path, "/COMPILATION.csv"))

# Manual override of csv files to be read
# this_week_csv <- read.csv("Results_May_2024_Week_3/COMPILATION.csv")
# last_week_csv <- read.csv("Results_May_2024_Week_4/dummyforcomparison.csv")

# Compare the 2 weeks' results and identify new and retired products ----

# Backup all_products.csv before doing anything else

backup_file_name <- paste0("Backup/Backup_all_products_", this_week_path, ".csv")
file.copy(MASTER_CSV, differentiate_if_name_taken(backup_file_name))

this_week_names <- tolower(this_week_csv$product_name)
last_week_names <- tolower(last_week_csv$product_name)

new_product_index <- !(this_week_names %in% last_week_names)
retired_product_index <- !(last_week_names %in% this_week_names)

new_products <- this_week_csv[new_product_index, ]
retired_products <- last_week_csv[retired_product_index, ]

# Produce report after performing comparison

new_products$product_description <- NULL
retired_products$product_description <- NULL

report_file_path <- differentiate_if_name_taken(paste0("Reports/", paste("Report", month, year, "Week", week, ".csv", sep = "_")))

summary <- data.frame(c("Date Produced:", "Number of New Products:", "Number of Retired Products:"),
                      c(formatted_date, nrow(new_products), nrow(retired_products)))

colnames(summary) <- c(paste("Comparison between", last_week_path, "and", this_week_path), "")

write.csv(summary, file = report_file_path, row.names = FALSE)

if (nrow(new_products) > 0){ 
  cat("\nNew Products:\n", file = report_file_path, append = TRUE)
  write.table(new_products, file = report_file_path, append = TRUE, row.names = FALSE, col.names = TRUE, sep = ",")
}

if (nrow(retired_products) > 0){
  cat("\nRetired Products:\n", file = report_file_path, append = TRUE)
  write.table(retired_products, file = report_file_path, append = TRUE, row.names = FALSE, col.names = TRUE, sep = ",")
}

cat("Produced weekly comparison report at", report_file_path)

# Update master csv file
# Has columns : company, product_type, product_name, status, date_discovered, date_retired
# Identify and record retired products this week in the master_df

master_df <- read.csv(MASTER_CSV)

retired_products_index <- (tolower(master_df$product_name) %in% tolower(retired_products$product_name)) & (master_df$status != "Retired")

if (length(retired_products_index) > 0){
  master_df[retired_products_index, "status"] <- "Retired"
  master_df[retired_products_index, "date_retired"] <- formatted_date
}

# Change previously "New" products to "Existing"

existing_products_index <- master_df$status == "New"

if (length(existing_products_index) > 0){
  master_df[existing_products_index, "status"] <- "Existing"
}

# Add newly discovered products this week 

if (nrow(new_products) > 0){
  master_df <- rbind(master_df, 
                   cbind(new_products, "status" = "New", "date_discovered" = formatted_date, "date_retired" = "-"))
}

# Update discovered_place of master_df to match latest compilation
# Possible to enhance maybe

for (i in 1:nrow(master_df)){

  product_name <- master_df[i, "product_name"]

  if (product_name %in% this_week_csv$product_name){
    master_df[i, "discovered_place"] <- first(this_week_csv[this_week_csv$product_name == product_name, "discovered_place"])
  }

}

# Check for duplicates

if(any(duplicated(master_df$product_name))){

  print("Warning: duplicated products")
  print(master_df$product_name[duplicated(master_df$product_name)])

}

write.csv(master_df, file = MASTER_CSV, row.names = FALSE)

cat("Updated", MASTER_CSV, "\n")


