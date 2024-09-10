library(rvest)
library(dplyr)
library(tools)

OUTPUT_FILE_PATH <- "Results/singlife sg.csv"

HOMEPAGE <- "https://singlife.com/en"

insurance_tab <- read_html(HOMEPAGE)

URL_list <- insurance_tab %>%
  html_elements(".sl-main-sub-menu-link") %>%
  html_attr("href")

PRODUCT_TYPE <- insurance_tab %>%
  html_elements(".sl-main-sub-menu-link") %>%
  html_text2()

end_index <- which(grepl("fund-performance-reports", URL_list)) - 1
URL_list <- URL_list[1:end_index]
PRODUCT_TYPE <- PRODUCT_TYPE[1:end_index]

product_types <- data.frame(product_type = PRODUCT_TYPE, url = URL_list)
product_types <- product_types[!(product_types$product_type %in% c("DollarDex", "GROW with Singlife")),]

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

for (i in 1:nrow(product_types)){
  
  link <- paste0("https://singlife.com", product_types$url[i])
  html <- read_html(link)
  
  if (product_types$product_type[i] %in% c("Maternity Insurance", "Public Officers", "Retirement", "Singlife Account")){
    
    product_name <- gsub("-", " ", sub(".*/", "", product_types$url[i]))
    product_name <- toTitleCase(product_name)
    
    product_desc <- html %>%
      html_elements(".header-text") %>%
      html_text2() %>%
      .[1]
    
  } else{
    
    product_name <- html %>%
      html_elements(".product-card-header") %>%
      html_text2()
    
    product_desc <- html %>%
      html_elements(".product-card-description") %>%
      html_text2()
    
  }
  
  product_type <- product_types$product_type[i]
  
  bank_name <- "-"
  
  results <- rbind(results, data.frame(insurer = "Singlife", bank_name = bank_name, product_type = product_type, product_name = product_name, product_description = product_desc, insurance_type = ""))
}

results$product_description <- gsub("\\n", " ", results$product_description)  # Replace newlines with spaces
results$product_description <- gsub("\\s+", " ", results$product_description)  # Remove extra spaces
results$product_description <- trimws(results$product_description) # Remove unnecessary spaces 

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Life Insurance" ~ "Life",
    product_type == "Medical Insurance" ~ "Medical",
    product_type == "Critical Illness Insurance" ~ "Critical Illness",
    product_type == "Disability Insurance" ~ "Disability",
    product_type == "Maternity Insurance" ~ "Life",
    product_type == "Accident Insurance" ~ "H&P",
    product_type == "Car Insurance" ~ "Motor",
    product_type == "Travel Insurance" ~ "Travel",
    product_type == "Home Insurance" ~ "Home",
    product_type == "MINDEF" & grepl("life", tolower(product_name)) ~ "Life",
    product_type == "MINDEF" & grepl("injury", tolower(product_name)) ~ "H&P",
    product_type == "MHA" & grepl("life", tolower(product_name)) ~ "Life",
    product_type == "MHA" & grepl("injury", tolower(product_name)) ~ "H&P",
    product_type == "Public Officers" ~ "Life",
    product_type == "Savings" ~ "Savings",
    product_type == "Investment-Linked Plans" ~ "Investment",
    product_type == "Retirement" ~ "Savings",
    product_type == "Singlife Account" ~ "Savings",
    TRUE ~ NA_character_
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    product_type %in% c("Life", "Investment", "Savings", "Medical", "H&P", "Disability", "Critical Illness", "Legacy") ~ "Life Insurance",
    product_type %in% c("Home", "Travel", "Motor", "Fire", "Property", "Liability", "Misc.") ~ "General Insurance",
    TRUE ~ NA_character_
  ))

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
