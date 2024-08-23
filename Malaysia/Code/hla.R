library(rvest)
library(dplyr)
OUTPUT_FILE_PATH <- "results/hla.csv"

# HLA's website currently shows all of its personal products on one page 

INSURANCE_PAGE <- "https://www.hla.com.my/en/personal.html"

html <- read_html_live(INSURANCE_PAGE)

# Scrape information from the insurance site ---- 

product_type <-
  html %>%
  html_elements(".body3-bold.mb-1.text-secondary-blue-base") %>%
  html_text2()

product_name <- 
  html %>%
  html_elements(".heading-h5-bold.mb-4.text-neutral-black-base") %>%
  html_text2()

product_description <-
  html %>%
  html_elements(".flex-grow.px-6.pb-6.pt-5") %>%
  html_text2() 

results <- data.frame(product_type, product_name, product_description, "insurance_type" = "Life Insurance") 

# Scrape from hlmtakaful site

TAKAFUL_PAGE <- "https://www.hlmtakaful.com.my/en/personal.html"

takaful_html <- read_html_live(TAKAFUL_PAGE)

product_type <-
  takaful_html %>%
  html_elements(".body3-bold.mb-1.text-secondary-blue-base") %>%
  html_text2()

product_name <- 
  takaful_html %>%
  html_elements(".heading-h5-bold.mb-4.text-neutral-black-base") %>%
  html_text2()

product_name <- gsub("‑", "-", product_name) # Non-breaking hyphen causes issues with char encoding
product_name <- gsub("‑", "-", product_name)

product_description <-
  takaful_html %>%
  html_elements(".flex-grow.px-6.pb-6.pt-5") %>%
  html_text2() 

results <- rbind(results, data.frame(product_type, product_name, product_description, "insurance_type" = "Family Takaful")) 

results$product_description <- trimws(results$product_description)

# Adjust product_type to align with other companies, TBD used if unclear

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Life Protection" ~ "Life",
    product_type == "Savings & Wealth Planning" ~ "Savings/Investment",
    product_type == "Savings & Investment" ~ "Savings/Investment",
    product_type == "Health & Medical Protection" ~ "TBD (CI/Medical/H&P)",
    product_type == "Accident Protection" ~ "H&P",
    product_type == "Disability Income" ~ "H&P",
    product_type == "Health & Medical" ~ "CI/H&P/Medical",
    TRUE ~ product_type
  ))

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp, ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
