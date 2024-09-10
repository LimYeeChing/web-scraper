library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/transamerica sg.csv"

HOMEPAGE <- "https://www.transamericalifebermuda.com/en/our-products"

insurance_tab <- read_html(HOMEPAGE)

product_name <- insurance_tab %>%
  html_elements(".teaser-block__inner") %>%
  html_nodes("h3") %>%
  html_text2()

product_desc <- insurance_tab %>%
  html_elements(".teaser-block__inner") %>%
  html_nodes("p") %>%
  html_text2()

product_desc <- product_desc[product_desc != ""]

results <- data.frame(insurer = "Transamerica", bank_name = "-", product_type = "Life", product_name = product_name, product_description = product_desc, insurance_type = "Life Insurance")

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
