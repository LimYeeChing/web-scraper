library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/hsbc sg.csv"

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

URL_list <- data.frame(product_category = c("Life and critical illness", "Savings", "Investment", "Personal accident", "Legacy", "Health", "Employee health and benefits"),
                       urls = c("https://www.insurance.hsbc.com.sg/life-and-critical-illness/products/",
                                "https://www.insurance.hsbc.com.sg/savings/",
                                "https://www.insurance.hsbc.com.sg/investment/",
                                "https://www.insurance.hsbc.com.sg/personal-accident/",
                                "https://www.insurance.hsbc.com.sg/legacy/",
                                "https://www.insurance.hsbc.com.sg/health/",
                                "https://www.insurance.hsbc.com.sg/employee-health-benefits/"))

for (i in 1:nrow(URL_list)){
  
  html <- read_html(URL_list$urls[i])
  
  product_name <- html %>%
    html_elements(".link.text") %>%
    html_text2()
  
  product_desc <- html %>%
    html_elements(".A-TYPS5R-RW-DEV.text-container.text") %>%
    html_text2()
  
  # There is extra description for some categories with the line "we've got you covered"
  product_desc <- product_desc[!grepl("we've got you covered", product_desc, ignore.case = TRUE)]
  
  product_type <- URL_list$product_category[i]
  
  # The last four product_name and product_desc for each category are not related to products (except for life and critical illness)
  if (product_type != "Life and critical illness"){
    product_name <- product_name[1:(length(product_name) - 4)]
    product_desc <- product_desc[1:(length(product_desc) - 4)]
  }
  
  results <- rbind(results, data.frame(insurer = "HSBC", bank_name = "-", product_name = product_name, product_type = product_type, product_description = product_desc, insurance_type = ""))
}

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Life and critical illness" & (grepl("critic", tolower(product_name)) | grepl("cancer", tolower(product_name))) ~ "Critical Illness",
    product_type == "Life and critical illness" & !(grepl("critic", tolower(product_name)) | grepl("cancer", tolower(product_name))) ~ "Life",
    product_type == "Savings" ~ "Savings",
    product_type == "Investment" ~ "Investment",
    product_type == "Personal accident" ~ "H&P",
    product_type == "Legacy" ~ "Legacy",
    product_type == "Health" ~ "H&P",
    product_type == "Employee health and benefits" & grepl("life", tolower(product_name)) ~ "Life",
    TRUE ~ NA_character_
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    product_type %in% c("Critical Illness", "Life", "H&P", "Savings", "Investment", "Legacy", "Medical", "Disability") ~ "Life Insurance",
    product_type %in% c("Travel", "Motor", "Home", "Fire", "Property", "Liability", "Misc.") ~ "General Insurance",
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
