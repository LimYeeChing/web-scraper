library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/mbsb.csv"

insurance_tab <- read_html("https://www.mbsbbank.com/consumer-banking/wealth-management/protection")

product_name <- insurance_tab %>%
  html_elements(".card-title") %>%
  html_text2()

product_description <- insurance_tab %>%
  html_elements(".product-detials") %>%
  html_nodes("p") %>%
  html_text2()

product_description <- Filter(function(x) x != "", product_description)

results <- data.frame(company = "", product_type = "", product_name = product_name, product_description = product_description, insurance_type = "")
  
results <- results %>%
  mutate(product_type = case_when(
    grepl("motor", tolower(product_description)) | grepl("motor", tolower(product_name)) ~ "Motor",
    grepl("fire", tolower(product_description)) | grepl("fire", tolower(product_name)) ~ "Fire",
    grepl("personal accident", tolower(product_description)) | grepl("personal accident", tolower(product_name)) ~ "H&P",    
    grepl("medical", tolower(product_description)) | grepl("medical", tolower(product_name)) ~ "Medical",
    grepl("savings", tolower(product_description)) | grepl("savings", tolower(product_name)) ~ "Savings",
    grepl("mortgage", tolower(product_description)) | grepl("mortgage", tolower(product_name)) ~ "Home", 
    grepl("credit", tolower(product_description)) | grepl("credit", tolower(product_name)) ~ "Liability",
    grepl("life", tolower(product_description)) | grepl("life", tolower(product_name)) ~ "Life",
    TRUE ~ NA_character_
  )) %>%
  mutate(product_type = ifelse(is.na(product_type),
                               ifelse(grepl("death", tolower(product_description)) | grepl("death", tolower(product_name)), "Life", "Misc."),
                               product_type))

results <- results %>%
  mutate(company = case_when(
    product_name == "An-Nur" ~ "Great Eastern",
    TRUE ~ "Takaful Ikhlas"
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    company == "Great Eastern" ~ "Family Takaful",
    product_type == "Life" ~ "Family Takaful",
    TRUE ~ "General Takaful"
  ))

# Rearrange columns, remove unnecessary ones 

results <- results[, c("company", "product_type", "product_name", "product_description", "insurance_type")]

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

