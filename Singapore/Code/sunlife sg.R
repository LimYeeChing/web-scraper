library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/sunlife sg.csv"

HOMEPAGE <- "https://www.sunlife.com/sl/sg/en/product-solutions/"

insurance_tab <- read_html_live(HOMEPAGE)

product_name <- insurance_tab %>%
  html_elements(".right-item.text-section") %>%
  html_nodes("h4") %>%
  html_text2()

product_desc <- insurance_tab %>%
  html_elements(".right-item.text-section") %>%
  html_nodes("p") %>%
  html_text2()

indices <- seq(4, length(product_desc), by = 3)
product_desc <- product_desc[indices]

results <- data.frame(insurer = "Sun Life", bank_name = "-", product_type = "", product_name = product_name, product_description = product_desc, insurance_type = "Life Insurance")

results <- results %>%
  mutate(product_type = case_when(
    grepl("life", tolower(product_name)) ~ "Life",
    TRUE ~ NA_character_
  ))

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
