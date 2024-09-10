library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/tokiomarine sg.csv"

HOMEPAGE <- "https://www.tokiomarine.com/sg/en.html"

insurance_tab <- read_html_live(HOMEPAGE)

URL_list <- insurance_tab %>%
  html_elements(".quick-help-custom__item") %>%
  html_nodes("a") %>%
  html_attr("href")

PRODUCT_TYPE <- insurance_tab %>%
  html_elements(".quick-help-custom__title.quickLinkGtm") %>%
  html_text2()

product_types <- data.frame(product_type = PRODUCT_TYPE, url = URL_list)

#Business Insurance requires special handling
product_types <- product_types[product_types$product_type != "Business Insurance",]

#ILP requires special handling
product_types <- product_types[product_types$product_type != "Investments",]
product_types <- rbind(product_types, data.frame(product_type = "Investments", url = "/sg/en/life/products/personal/investments/plans.html"))
row.names(product_types) <- NULL

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

for (i in 1:nrow(product_types)){
  
  link <- paste0("https://www.tokiomarine.com", product_types$url[i])
  html <- read_html_live(link)
  
  insurance_type <- ifelse(grepl("non-life", link), "General Insurance", "Life Insurance")
    
  product_name <- html %>%
    html_elements(".signpost-custom-card__item__title") %>%
    html_text2()
    
  product_desc <- html %>%
    html_elements(".signpost-custom-card__item__text") %>%
    html_text2()
    
  if (length(product_name) == 0 & length(product_desc) == 0){
    
    product_name1 <- html %>%
      html_elements(".masthead-colors__title") %>%
      html_text2()
    
    product_name2 <- html %>%
      html_elements(".headerTitle") %>%
      html_text2() %>%
      unique()
    
    product_name <- c(product_name1, product_name2)
    
    product_desc1 <- html %>%
      html_elements(".masthead-colors__text") %>%
      html_text2()
    
    product_desc2 <- html %>%
      html_elements(".mb-2") %>%
      html_text2()
    
    product_desc <- c(product_desc1, product_desc2)
  }
  
  product_type <- product_types$product_type[i]
  
  bank_name <- "-"
  
  results <- rbind(results, data.frame(insurer = "Tokio Marine", bank_name = bank_name, product_type = product_type, product_name = product_name, product_description = product_desc, insurance_type = insurance_type))
}

results$product_name <- gsub("\\n", "", results$product_name) # Remove \n

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Motor" ~ "Motor",
    product_type == "Home" ~ "Home", 
    product_type == "Personal Accident" ~ "H&P",
    product_type == "Travel" ~ "Travel",
    product_type == "Critical Illness" ~ "Critical Illness",
    product_type == "Disability" ~ "Disability",
    product_type == "Life Insurance" ~ "Life",
    product_type == "Savings" ~ "Savings",
    product_type == "Education" ~ "Savings",
    product_type == "Retirement" ~ "Savings",
    product_type == "Investments" ~ "Investment",
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

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
