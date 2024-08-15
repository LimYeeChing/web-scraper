library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/alrajhi.csv"

insurance_tab <- read_html("https://www.alrajhibank.com.my/page/personal/wealth-management/bancatakaful-64/")

product_name <- insurance_tab %>%
  html_elements("ul.sidebar-list") %>%
  html_nodes("a") %>%
  html_text2() %>%
  unique()

product_urls <- insurance_tab %>%
  html_elements("ul.sidebar-list") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  unique()

results <- data.frame(company = "", product_type = "", product_name = product_name, product_description = "", insurance_type = "", product_urls = product_urls)

company_mentioned <- function(html, company){
  any(grepl(company, html))
}

for (i in 1:nrow(results)){
  
  html <- read_html(results$product_urls[i])
  
  product_desc <- html %>%
    html_elements(".saving-info-content") %>%
    html_nodes("p") %>%
    html_text2()
  
  html_in_text <- html %>%
    html_text2() %>%
    tolower()
  
  if (company_mentioned(html_in_text, "sun life")){
    company <- "Sun Life"
  } else {
    company <- "To be filled"
  }
  
  results$product_description[i] <- product_desc
  results$company[i] <- company
  
}

results <- results %>%
  mutate(product_type = case_when(
    grepl("critical", tolower(product_description)) | grepl("critical", tolower(product_name)) ~ "Critical Illness",
    grepl("investment", tolower(product_description)) | grepl("investment", tolower(product_name)) ~ "Investment",
    grepl("savings", tolower(product_description)) | grepl("savings", tolower(product_name)) ~ "Savings",
    TRUE ~ "Life"
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    company == "Sun Life" ~ "Family Takaful",
    TRUE ~ NA_character_
  ))

# Rearrange columns, remove unnecessary ones 

results <- results[, c("company", "product_type", "product_name", "product_description", "insurance_type")]

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

