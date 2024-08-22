library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/prudential hk.csv"

PRODUCT_TYPE <- c("Health Critical Illness", "Health Medical", "Health Accident and Disability", "Life", 
                  "Wealth Appreciation", "Wealth Protection", "Wealth Succession", "Home and Pet", 
                  "Travel", "Leisure", "Investment-linked")

URL          <- c("https://www.prudential.com.hk/en/health-insurance/critical-illness/",
                  "https://www.prudential.com.hk/en/health-insurance/medical/",
                  "https://www.prudential.com.hk/en/health-insurance/accident-disability/",
                  "https://www.prudential.com.hk/en/life-insurance/",
                  "https://www.prudential.com.hk/en/wealth-series/wealth-appreciation/",
                  "https://www.prudential.com.hk/en/wealth-series/wealth-protection/",
                  "https://www.prudential.com.hk/en/wealth-series/wealth-succession/",
                  "https://www.prudential.com.hk/en/home-insurance/",
                  "https://www.prudential.com.hk/en/travel-insurance/",
                  "https://www.prudential.com.hk/en/leisure-insurance/",
                  "https://www.prudential.com.hk/en/investment-linked-insurance/")

product_types <- data.frame(product_type = PRODUCT_TYPE, url = URL)

results <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(results) <- c("bank_name", "product_type", "product_name", "product_description", "insurance_type")

for (i in 1:nrow(product_types)){
  
  html <- read_html_live(product_types$url[i])
  
  product_name <- html %>%
    html_elements(".title-content-bold") %>%
    html_text2()
  
  product_desc <- html %>%
    html_elements("p.content") %>%
    html_text2()
  
  product_type <- product_types$product_type[i]
  
  bank_name <- "-"
  
  results <- rbind(results, data.frame(bank_name = bank_name, product_type = product_type, product_name = product_name, product_description = product_desc, insurance_type = ""))
}

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Health Critical Illness" ~ "Critical Illness",
    product_type == "Health Medical" ~ "Medical",
    product_type == "Health Accident and Disability" ~ "H&P",
    product_type == "Life" ~ "Life",
    product_type == "Wealth Appreciation" ~ "Savings",
    product_type == "Wealth Protection" ~ "Legacy",
    product_type == "Wealth Succession" ~ "Legacy",
    product_type == "Home and Pet" & grepl("home", tolower(product_name)) ~ "Home",
    product_type == "Home and Pet" ~ "Misc.",
    product_type == "Travel" ~ "Travel",
    product_type == "Leisure" & grepl("motor", tolower(product_name)) ~ "Motor",
    product_type == "Leisure" ~ "Misc.",
    product_type == "Investment-linked" ~ "Investment",
    TRUE ~ NA_character_
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    product_type %in% c("Life", "Investment", "Savings", "Critical Illness", "Medical", "H&P", "Legacy", "Disability") ~ "Life Insurance",
    product_type %in% c("Home", "Travel", "Motor", "Fire", "Property", "Liability", "Misc.") ~ "General Insurance",
    TRUE ~ NA_character_
  ))

# Combine rows with the same product_name
combined_results <- results %>%
  group_by(product_name) %>%
  summarise(
    product_type = paste(unique(product_type), collapse = ", "), 
    product_description = first(product_description), # Assuming product_description is the same or should be kept from the first row
    insurance_type = paste(unique(insurance_type), collapse = ", "),
    bank_name = first(bank_name)
  )

# Rearrange the columns in the desired order
combined_results <- combined_results %>%
  select(bank_name, product_type, product_name, product_description, insurance_type)

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
combined_results <- rbind(combined_results, c(bank_name = "Scraped at", product_type = ":", product_name = formatted_timestamp, product_description = "", insurance_type = ""))

# write.csv(combined_results, file = OUTPUT_FILE_PATH, row.names = FALSE)
