library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/etiqa sg.csv"

HOMEPAGE_URL <- "https://www.etiqa.com.sg/personal/" 

results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(results) <- c("product_type", "product_name", "product_description", "insurance_type")

homepage <- read_html(HOMEPAGE_URL)

urls <- homepage %>%
  html_elements(".categories-listing") %>%
  html_nodes("a") %>%
  html_attr("href")

for (i in 1:length(urls)){
  
  html <- read_html_live(urls[i])
  
  for(n in 1:2){
    Sys.sleep(2)
  }
  
  product_name1 <- html %>%
    html_elements(".hover-card.has-promo.animated") %>%
    html_nodes("h4") %>%
    html_text2()
    
  product_name2 <- html %>%
    html_elements(".hover-card.no-hover.animated") %>%
    html_nodes("h4") %>%
    html_text2()
  
  product_name3 <- html %>%
    html_elements(".hover-card.no-hover.tiq-product.animated") %>%
    html_nodes("h4") %>%
    html_text2()
  
  product_name <- unique(c(product_name1, product_name2, product_name3))
  
  product_description <- html %>%
    html_elements(".hover-card-front") %>%
    html_text2()
  
  if (length(product_name) > 0 && length(product_name) == length(product_description)) {
    results <- rbind(results, data.frame(product_type = "", product_name = product_name, product_description = product_description, insurance_type = ""))
  }
}

results <- results %>%
  mutate(product_type = case_when(
    grepl("cancer", tolower(product_name)) | grepl("critical", tolower(product_name)) ~ "Critical Illness",
    grepl("life", tolower(product_name)) ~ "Life",
    grepl("esteem", tolower(product_name)) | grepl("income", tolower(product_name)) ~ "Savings",
    grepl("saver", tolower(product_name)) | grepl("enrich", tolower(product_name)) ~ "Savings",
    grepl("invest", tolower(product_name)) ~ "Investment",
    grepl("solitaire", tolower(product_name)) | grepl("accident", tolower(product_name)) ~ "H&P",
    grepl("travel", tolower(product_name)) ~ "Travel",
    grepl("motor", tolower(product_name)) | grepl("car", tolower(product_name)) ~ "Motor",
    grepl("home", tolower(product_name)) | grepl("mortgage", tolower(product_name)) ~ "Home",
    grepl("cyber", tolower(product_name)) | grepl("pet", tolower(product_name)) | grepl("maid", tolower(product_name)) ~ "Misc.",
    TRUE ~ NA_character_
  ))

results <- results %>%
  group_by(product_name) %>%
  summarise(
    product_type = first(product_type),
    product_description = paste(product_description, collapse = " "),
    insurance_type = first(insurance_type)
  )

results <- results %>%
  mutate(insurance_type = case_when(
    product_type %in% c("Critical Illness", "Life", "H&P", "Savings", "Investment") ~ "Life Insurance",
    product_type %in% c("Travel", "Motor", "Home", "Misc.") ~ "General Insurance",
    TRUE ~ NA_character_
  ))

results <- results[, c(2, 1, 3, 4)] # Rearrange columns, summarise() changes their order

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp, ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
