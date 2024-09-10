library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/fwd sg.csv"

HOMEPAGE <- "https://www.fwd.com.sg/"

insurance_tab <- read_html_live(HOMEPAGE)

URL_list1 <- insurance_tab %>%
  html_elements(".dropdownItem-module--dropdownItemCollapse--wZDGm") %>%
  html_attr("href")

URL_list2 <- insurance_tab %>%
  html_elements(".navitem-module--collapseLink--9yXU8") %>%
  html_attr("href")

PRODUCT_TYPE1 <- insurance_tab %>%
  html_elements(".dropdownItem-module--dropdownItemCollapse--wZDGm") %>%
  html_text2()

PRODUCT_TYPE2 <- insurance_tab %>%
  html_elements(".navitem-module--collapseLink--9yXU8") %>%
  html_text2()

end_index <- which(grepl("claim-online", URL_list1)) - 1
URL_list1 <- URL_list1[1:end_index]
PRODUCT_TYPE1 <- PRODUCT_TYPE1[1:end_index]

PRODUCT_TYPE1 <- gsub("Get Advice", "", PRODUCT_TYPE1)
PRODUCT_TYPE1 <- gsub("get advice", "", PRODUCT_TYPE1)

URL_list <- c(URL_list1, URL_list2)
PRODUCT_TYPE <- c(PRODUCT_TYPE1, PRODUCT_TYPE2)

product_types <- data.frame(product_type = PRODUCT_TYPE, url = URL_list)

product_types <- product_types[!(is.na(product_types$url)),]
product_types <- product_types[!(product_types$product_type == "Promotions"),]

product_types <- product_types %>%
  mutate(product_type = case_when(
    grepl("life", url) & !(grepl("income", url)) ~ "Life",
    grepl("life", url) & grepl("income", url) ~ "Savings",
    grepl("accident", url) ~ "H&P",
    grepl("critical-illness", url) ~ "Critical Illness",
    grepl("health", url) ~ "H&P",
    grepl("home", url) ~ "Home",
    grepl("fire", url) ~ "Fire",
    grepl("maid", url) ~ "Misc.",
    grepl("car", url) ~ "Motor",
    grepl("motor", url) ~ "Motor",
    grepl("travel", url) ~ "Travel",
    grepl("save", url) ~ "Savings",
    grepl("invest", url) ~ "Investment",
    TRUE ~ NA_character_
  ))

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

for (i in 1:nrow(product_types)){
  
  html <- read_html_live(product_types$url[i])
  
  for(n in 1:2){
    Sys.sleep(4)
  }
  
  product_name <- html %>%
    html_elements(".heading") %>%
    html_text2()
    
  product_desc <- html %>%
    html_elements(".heading1") %>%
    html_text2()
  
  product_type <- product_types$product_type[i]
  
  bank_name <- "-"
  
  results <- rbind(results, data.frame(insurer = "FWD", bank_name = bank_name, product_type = product_type, product_name = product_name, product_description = product_desc, insurance_type = ""))
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
