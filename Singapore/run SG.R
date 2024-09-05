library(rvest)
library(dplyr)
library(chromote)
# Set "Web Scraping" as working directory 
# Before running the code, run the line of code below and clear the captcha for Zurich's website first
# read_html_live("https://www.zurich.com.my/en/insurance-products/protection")$view()

start_time <- Sys.time()

# Separating for companies and banks, because slightly different code 

COMPANY_NAMES <- c("AIA" = "aia sg", 
                   "Etiqa" = "etiqa sg",
                   "Great Eastern" = "ge sg",
                   "HSBC" = "hsbc sg",
                   "Income" = "income sg",
                   "Manulife" = "manulife sg")

BANK_NAMES <- c()

code_file_names <- paste0("Code/", COMPANY_NAMES, ".R")
bank_code_file_names <- paste0("Code/", BANK_NAMES, ".R")

# Create "Results" folder if it is yet to exist

date_value <- Sys.Date()
month <- format(date_value, "%m")
year <- format(date_value, "%Y")
week <- ceiling(as.numeric(format(date_value, "%d")) / 7)
results_folder_path <- paste("Results", month, year, "Week", week, sep = "_")

if (!file.exists(results_folder_path)){dir.create(results_folder_path)}

company_results_path <- paste0(results_folder_path, "/", COMPANY_NAMES, ".csv")
bank_results_path    <- paste0(results_folder_path, "/", BANK_NAMES, ".csv")

# Load all results in separate CSVs in the "Results_Month_Year_Week" folder
# so they can be checked/retrieved individually 

for (k in 1:length(COMPANY_NAMES)){
  print(paste("Running", code_file_names[k]))
  source(code_file_names[k])
  write.csv(results, file = company_results_path[k], row.names = FALSE) 
  # 'results' is the name of the data frame containing the scraped results from each code file
}

# Note: fwd abit slow sometimes and times out, might need to run manually.. (can enhance)

for (k in 1:length(BANK_NAMES)){
  print(paste("Running", bank_code_file_names[k]))
  source(bank_code_file_names[k])
  write.csv(results, file = bank_results_path[k], row.names = FALSE) 
}

# Compile all results into one file ----

compilation <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(compilation) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")
bank_compilation <- compilation

for (i in 1:length(COMPANY_NAMES)){

  if(file.exists(company_results_path[i])){
    company_results <- read.csv(company_results_path[i])
    company_results <- company_results[-nrow(company_results), ] # Remove timestamps
    compilation <- rbind(compilation, company_results)
  }
  else{
    print(paste("Warning:", company_results_path[i], "not found."))
    
    } # Make sure all companies' files exist

}

for (i in 1:length(BANK_NAMES)){

  if(file.exists(bank_results_path[i])){
    bank_results <- read.csv(bank_results_path[i])
    bank_results <- bank_results[-nrow(bank_results), ] 
    bank_compilation <- rbind(bank_compilation, bank_results)
  }
  else{stop(paste("Warning:", bank_results_path[i], "not found."))} 

}

# Add discovered_place ----

compilation <- cbind(compilation, discovered_place = "Insurer")
bank_compilation <- cbind(bank_compilation, discovered_place = "Bank")

# Combine products with same company and product name 

compilation <- rbind(compilation, bank_compilation)

# compilation <- compilation %>%
#   group_by(company, product_name) %>%
#   summarise(
#     product_type = paste(unique(product_type), collapse = ", "),
#     discovered_place = paste(unique(discovered_place), collapse = ", "),
#     product_description = first(product_description),
#     insurance_type = first(insurance_type)
#   ) %>%
#   ungroup()

compilation <- compilation[, c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type", "discovered_place")]

### Remove duplicated products from insurer and bank
# Create a new data frame to store the cleaned-up results
cleaned_compilation <- compilation[0, ]
dummy_compilation <- compilation

# Create a list to store information about duplicated rows
duplicate_info <- list()

# Loop through each unique combination of product_name, insurer, and bank_name
for (combo in unique(paste(tolower(dummy_compilation$product_name), dummy_compilation$insurer, dummy_compilation$bank_name))) {
  
  # Identify rows that match the current combination
  matching_rows <- which(paste(tolower(dummy_compilation$product_name), dummy_compilation$insurer, dummy_compilation$bank_name) == combo)
  
  # Check if there are multiple rows and if discovered_place values are different
  if (length(matching_rows) > 1) {
    discovered_places <- dummy_compilation$discovered_place[matching_rows]
    if (length(unique(discovered_places)) > 1) { # Different discovered_place values
      # Combine the discovered_place values for duplicates
      combined_discovered_place <- paste(unique(discovered_places), collapse = ", ")
      
      # Update the first occurrence with the combined discovered_place
      compilation$discovered_place[matching_rows[1]] <- combined_discovered_place
      
      # Store the duplicate information (excluding the first row)
      duplicate_info[[length(duplicate_info) + 1]] <- list(combo = combo, duplicate_rows = matching_rows[-1])
      
      # Add the first occurrence to the cleaned compilation
      cleaned_compilation <- rbind(cleaned_compilation, compilation[matching_rows[1], ])
      
    } else {
      # If discovered_place values are the same, consider it not a duplicate
      cleaned_compilation <- rbind(cleaned_compilation, compilation[matching_rows[1], ])
    }
  } else {
    # If there's only one match, add it to the cleaned compilation
    cleaned_compilation <- rbind(cleaned_compilation, compilation[matching_rows, ])
  }
}

cleaned_compilation$discovered_place[!cleaned_compilation$discovered_place %in% c("Insurer", "Bank", "Insurer, Bank")] <- "Insurer, Bank"

# Display the duplicated rows information
if (length(duplicate_info) > 0) {
  print("Duplicated rows identified:")
  for (dup in duplicate_info) {
    cat("Combination:", dup$combo, "\n")
    cat("Duplicated Rows:", dup$duplicate_rows, "\n")
  }
} else {
  print("No duplicates found.")
}

compilation <- cleaned_compilation

# Write to COMPILATION.csv

compilation_path <- paste0(results_folder_path, "/new_COMPILATION.csv")

write.csv(compilation, file = compilation_path, row.names = FALSE)

# Calculate runtime ---- 

end_time <- Sys.time()
runtime <- round(end_time - start_time, 2)
print(paste("Scraping completed. Total Runtime:", runtime, "minutes"))
