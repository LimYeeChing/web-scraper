library(rvest)
library(dplyr)
library(chromote)

# Set "Web Scraping" as working directory 
# Before running the code, run the line of code below and clear the captcha for Zurich's website first
# read_html_live("https://www.zurich.com.my/en/insurance-products/protection")$view()

start_time <- Sys.time()

# Separating for companies and banks, because slightly different code 

COMPANY_NAMES <- c("AIA" = "aia hk", 
                   "AXA" = "axa hk",
                   "Blue" = "blue hk",
                   "BOC Life" = "boc life",
                   "Bowtie" = "bowtie",
                   "China Life" = "china life",
                   "Chubb" = "chubb hk",
                   "CTF Life" = "ctf life",
                   "Fubon Life" = "fubon life",
                   "FWD" = "fwd hk",
                   "Generali" = "generali hk",
                   "Hong Kong Life" = "hong kong life",
                   "Manulife" = "manulife hk",
                   "Pacific Life" = "pacific life",
                   "Prudential" = "prudential hk",
                   "Sun Life" = "sunlife hk",
                   "Tahoe Life" = "tahoe life",
                   "Transamerica" = "transamerica",
                   "Well Link" = "well link",
                   "YFLife" = "yflife",
                   "ZA Insure" = "za insure",
                   "Zurich" = "zurich hk")

BANK_NAMES <- c("Bank of China" = "bank of china",
                "Bank of Communication" = "bank of communication",
                "BEA" = "bea",
                "China Construction Bank" = "china construction",
                "Chong Hing Bank" = "chong hing",
                "Citibank" = "citibank",
                "Dah Sing Bank" = "dah sing",
                "DBS" = "dbs",
                "Hang Seng Bank" = "hang seng",
                "HSBC Bank" = "hsbc",
                "ICBC (Asia)" = "icbc",
                "Nanyang Commercial Bank" = "nanyang",
                "Public Bank" = "public bank",
                "Standard Chartered" = "sc")

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

# Loop through company files
for (k in 1:length(COMPANY_NAMES)) {
  if (file.exists(code_file_names[k])) {  # Check if the file exists
    print(paste("Running", code_file_names[k]))
    source(code_file_names[k])  # Execute the scraping code
    write.csv(results, file = company_results_path[k], row.names = FALSE)
  } else {
    print(paste("Skipping", code_file_names[k], "- file not found."))
  }
}

# Loop through bank files
for (k in 1:length(BANK_NAMES)) {
  if (file.exists(bank_code_file_names[k])) {  # Check if the file exists
    print(paste("Running", bank_code_file_names[k]))
    source(bank_code_file_names[k])  # Execute the scraping code
    write.csv(results, file = bank_results_path[k], row.names = FALSE)
  } else {
    print(paste("Skipping", bank_code_file_names[k], "- file not found."))
  }
}

# Compile all results into one file ----

compilation <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(compilation) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")
bank_compilation <- compilation

for (i in 1:length(COMPANY_NAMES)){
  
  if(file.exists(company_results_path[i])){
    company_results <- cbind(insurer = names(COMPANY_NAMES[i]), read.csv(company_results_path[i]))
    company_results <- company_results[-nrow(company_results), ] # Remove timestamps
    compilation <- rbind(compilation, company_results)
  }
  else{
    print(paste("Warning:", company_results_path[i], "not found."))
    
  } # Make sure all companies' files exist
  
}

for (i in 1:length(BANK_NAMES)){
  
  if(file.exists(bank_results_path[i])){
    bank_results <- cbind(bank_name = names(BANK_NAMES[i]), read.csv(bank_results_path[i])) # Bank csvs already have 'company'
    colnames(bank_results)[which(names(bank_results) == "company")] <- "insurer"
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

# Write to COMPILATION.csv

compilation_path <- paste0(results_folder_path, "/new_COMPILATION.csv")

write.csv(compilation, file = compilation_path, row.names = FALSE)

# Calculate runtime ---- 

end_time <- Sys.time()
runtime <- round(end_time - start_time, 2)
print(paste("Scraping completed. Total Runtime:", runtime, "minutes"))
