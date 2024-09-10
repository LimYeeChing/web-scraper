library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/prudential sg.csv"

HOMEPAGE <- "https://www.prudential.com.sg/products"

insurance_tab <- read_html(HOMEPAGE)

URL_list <- insurance_tab %>%
  html_elements(".child-link") %>%
  html_attr("href")

PRODUCT_TYPE <- insurance_tab %>%
  html_elements(".child-link") %>%
  html_text2()

URL_list <- URL_list[grepl("/products", URL_list)]

URL_list <- URL_list[!grepl("/funds", URL_list)]

product_types <- data.frame(product_type = "", url = URL_list)

product_types <- product_types %>%
  mutate(product_type = case_when(
    grepl("medical", url) ~ "Medical",
    grepl("accident", url) ~ "H&P",
    grepl("critical-illness", url) ~ "Critical Illness",
    grepl("dengue", url) ~ "H&P",
    grepl("covid", url) ~ "H&P",
    grepl("life-insurance", url) & grepl("mortgage", url) ~ "Home",
    grepl("life-insurance", url) & !grepl("mortgage", url) ~ "Life",
    grepl("savings", url) ~ "Savings",
    grepl("income", url) ~ "Savings",
    grepl("retirement", url) ~ "Savings",
    grepl("investments", url) ~ "Investment",
    grepl("legacy", url) ~ "Legacy",
    TRUE ~ NA_character_
  ))

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

for (i in 1:nrow(product_types)){
  
  link <- paste0("https://www.prudential.com.sg", product_types$url[i])
  html <- read_html(link)
  
  if (link %in% c("https://www.prudential.com.sg/products/legacy-planning/pruvantage-legacy-index",
                  "https://www.prudential.com.sg/products/legacy-planning/prulife-vantage-achiever-prime-series",
                  "https://www.prudential.com.sg/products/wealth-accumulation/savings/prulifetime-income-plus")){
    
    product_name <- html %>%
      html_elements(".product-summary__content--name") %>%
      html_text2()
    
    product_desc <- html %>%
      html_elements(".product-summary__content--desc") %>%
      html_text2()
    
  } else{
    
    product_name <- html %>%
      html_elements(".title-content-bold") %>%
      html_text2()
    
    product_desc <- html %>%
      html_elements(".content") %>%
      html_text2()
  }
  
  product_type <- product_types$product_type[i]
  
  bank_name <- "-"
  
  results <- rbind(results, data.frame(insurer = "Prudential", bank_name = bank_name, product_type = product_type, product_name = product_name, product_description = product_desc, insurance_type = "Life Insurance"))
}

results$product_description <- gsub("\\r", "", results$product_description) # Remove \r
results$product_description <- trimws(results$product_description) # Remove unnecessary spaces

results <- results %>%
  group_by(product_name) %>%
  summarise(
    insurer = first(insurer),
    bank_name = first(bank_name),
    product_type = paste(unique(product_type), collapse = ", "),
    product_description = paste(unique(product_description), collapse = ", "),
    insurance_type = first(insurance_type)
  )

results <- results[, c(2, 3, 4, 1, 5, 6)] # Rearrange columns, summarise() changes their order

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
