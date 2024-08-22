library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/manulife hk.csv"

PRODUCT_TYPE <- c("Life", "Save Savings", "Save Retirement", "Health VHIS", "Health CI", "Health Medical", 
                  "Health Accident", "Health Disability", "General")

URL          <- c("https://www.manulife.com.hk/en/individual/products/life/life-protection.html",
                  "https://www.manulife.com.hk/en/individual/products/save/savings.html",
                  "https://www.manulife.com.hk/en/individual/products/save/retirement.html.html",
                  "https://www.manulife.com.hk/en/individual/products/health/vhis.html",
                  "https://www.manulife.com.hk/en/individual/products/health/critical-illness-protection.html",
                  "https://www.manulife.com.hk/en/individual/products/health/quality-medical-care.html",
                  "https://www.manulife.com.hk/en/individual/products/health/accident-protection.html",
                  "https://www.manulife.com.hk/en/individual/products/health/disability-protection.html",
                  "https://www.manulife.com.hk/en/individual/products/others/general-insurance.html")

product_types <- data.frame(product_type = PRODUCT_TYPE, url = URL)

results <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(results) <- c("bank_name", "product_type", "product_name", "product_description", "insurance_type")

for (i in 1:nrow(product_types)){
  
  html <- read_html(product_types$url[i])
  
  product_type <- product_types$product_type[i]
  
  bank_name <- "-"
  
  product_info <- html %>%
    html_elements("a.cmp-productteaser__link") %>%
    html_attr("data-elbl")
  
  if (length(product_info) != 0){
    
    # Split each string in the vector using the "|" delimiter
    split_strings <- lapply(product_info, function(x) strsplit(x, "|", fixed = TRUE)[[1]])
    
    product_name <- sapply(split_strings, function(x) x[1])
    product_desc <- sapply(split_strings, function(x) x[2])
    
    results <- rbind(results, data.frame(bank_name = bank_name, product_type = product_type, product_name = product_name, product_description = product_desc, insurance_type = ""))
  }
}

# Remove all HTML tags
results$product_name <- gsub("<p>", "", results$product_name)
results$product_name <- gsub("</p>", "", results$product_name)

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Life" ~ "Life",
    product_type == "Save Savings" ~ "Savings",
    product_type == "Save Retirement" ~ "Savings",
    product_type == "Health VHIS" ~ "Medical",
    product_type == "Health CI" ~ "Critical Illness",
    product_type == "Health Medical" ~ "Medical",
    product_type == "Health Accident" ~ "H&P",
    product_type == "Health Disability" ~ "Disability",
    product_type == "General" & grepl("home", tolower(product_name)) ~ "Home",
    product_type == "General" & grepl("helper", tolower(product_name)) ~ "Misc.",
    product_type == "General" & grepl("motor", tolower(product_name)) ~ "Motor",
    product_type == "General" & grepl("travel", tolower(product_name)) ~ "Travel",
    product_type == "General" & grepl("accident", tolower(product_name)) ~ "H&P",
    TRUE ~ NA_character_
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    product_type %in% c("Life", "Investment", "Savings", "Critical Illness", "Medical", "H&P", "Legacy", "Disability") ~ "Life Insurance",
    product_type %in% c("Home", "Travel", "Motor", "Fire", "Property", "Liability", "Misc.") ~ "General Insurance",
    TRUE ~ NA_character_
  ))

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(bank_name = "Scraped at", product_type = ":", product_name = formatted_timestamp, product_description = "", insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

