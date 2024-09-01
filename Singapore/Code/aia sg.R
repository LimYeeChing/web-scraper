library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/aia sg.csv"

URL_LIST <- data.frame("product_category" = c("Life Insurance", "Participant Savings", "Investment-Linked", "Accident Protection", "Wealth Accumulation", "Legacy Planning", "Accident & Health", "Travel", "Home", "Critical Illness Insurance", "Medical Insurance", "Disability Income Insurance"),
                       "urls" = c("https://www.aia.com.sg/en/our-products/life-insurance",
                                  "https://www.aia.com.sg/en/our-products/save-and-invest/participating-savings",
                                  "https://www.aia.com.sg/en/our-products/save-and-invest/investment-linked",
                                  "https://www.aia.com.sg/en/our-products/accident-protection",
                                  "https://www.aia.com.sg/en/our-products/platinum/wealth-accumulation",
                                  "https://www.aia.com.sg/en/our-products/platinum/legacy-planning",
                                  "https://www.aia.com.sg/en/our-products/platinum/accident-health",
                                  "https://www.aia.com.sg/en/our-products/travel-and-lifestyle/travel",
                                  "https://www.aia.com.sg/en/our-products/travel-and-lifestyle/home",
                                  "https://www.aia.com.sg/en/our-products/health/critical-illness",
                                  "https://www.aia.com.sg/en/our-products/health/medical-insurance",
                                  "https://www.aia.com.sg/en/our-products/health/disability-income-insurance"))

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

for (i in 1:nrow(URL_LIST)){

  url  <- URL_LIST$urls[i]
  html <- read_html_live(url)
  
  for(n in 1:2){
    Sys.sleep(2)
  }
  
  product_name <-
    html %>% 
    html_elements("h2.cmp-productfilterlist__productcard__title") %>%
    html_text2()

  product_description <-
    html %>% 
    html_elements("div.cmp-productfilterlist__productdescription__item") %>%
    html_text2()
  
  product_type <- URL_LIST$product_category[i]

  results <- rbind(results, data.frame(insurer = "AIA", bank_name = "-", product_type = product_type, product_name = product_name, product_description = product_description, insurance_type = ""))

}

# Adjust product_type to align with other companies

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Life Insurance" ~ "Life",
    product_type == "Participant Savings" ~ "Savings",
    product_type == "Investment-Linked" ~ "Investment",
    product_type == "Accident Protection" ~ "H&P",
    product_type == "Wealth Accumulation" ~ "Savings",
    product_type == "Legacy Planning" ~ "Legacy",
    product_type == "Accident & Health" ~ "H&P",
    product_type == "Travel" ~ "Travel", 
    product_type == "Home" ~ "Home",
    product_type == "Critical Illness Insurance" ~ "Critical Illness",
    product_type == "Medical Insurance" ~ "Medical",
    product_type == "Disability Income Insurance" ~ "Disability",
    TRUE ~ NA_character_
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    product_type %in% c("Life", "Savings", "Investment", "H&P", "Critical Illness", "Medical", "Legacy", "Disability") ~ "Life Insurance",
    product_type %in% c("Travel", "Home") ~ "General Insurance",
    TRUE ~ NA_character_
  ))

results <- results %>%
  group_by(product_name) %>%
  summarise(
    insurer = first(insurer),
    bank_name = first(bank_name),
    product_type = paste(unique(product_type), collapse = ", "),
    product_description = paste(unique(product_description), collapse = ", "),
    insurance_type = paste(unique(insurance_type), collapse = ", ")
  )

results <- results[, c(2, 3, 4, 1, 5, 6)] # Rearrange columns, summarise() changes their order

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
