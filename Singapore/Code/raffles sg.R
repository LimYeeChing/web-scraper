library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/raffles sg.csv"

HOMEPAGE <- "https://www.raffleshealthinsurance.com/products/personal/singapore-medical-cover/"

insurance_tab <- read_html(HOMEPAGE)

product_name <- insurance_tab %>%
  html_elements(".title") %>%
  html_text2()

product_desc <- insurance_tab %>%
  html_elements(".infobox-content") %>%
  html_nodes("p") %>%
  html_text2()

results <- data.frame(insurer = "Raffles", bank_name = "-", product_type = "", product_name = product_name, product_description = product_desc, insurance_type = "Life Insurance")

results <- results %>%
  mutate(product_type = case_when(
    grepl("medical", tolower(product_description)) ~ "Medical",
    grepl("critical illness", tolower(product_description)) ~ "Critical Illness",
    TRUE ~ NA_character_
  ))

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
