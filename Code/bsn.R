library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/bsn.csv"

life_insurance_tab <- read_html("https://www.bsn.com.my/PersonalBanking/WealthManagement/wealth-management")
gen_insurance_tab <- read_html("https://www.bsn.com.my/PersonalBanking/WealthManagement/general-takaful")

life_product_name <- life_insurance_tab %>%
  html_elements(".action-card-title") %>%
  html_text2()

life_product_url <- life_insurance_tab %>%
  html_elements(".fom-action-card-link") %>%
  html_attr("href")

# Find the index of the word "General Takaful"
index <- which(life_product_name == "General Takaful")

# Remove all elements from the "General Takaful" word onward
if (length(index) > 0) {
  life_product_name <- life_product_name[1:(index - 1)]
  life_product_url <- life_product_url[1:(index - 1)]
}

life_results <- data.frame(company = "Prudential", product_type = "Life", product_name = life_product_name, product_description = "", insurance_type = "", product_url = life_product_url)

for (i in 1:nrow(life_results)){
  
  link <- paste0("https://bsn.com.my", life_results$product_url[i])
  html <- read_html(link)
  
  life_product_description <- html %>%
    html_elements(".overlap_caption") %>%
    html_text2()
  
  if (length(life_product_description) == 0) {
    life_product_description <- html %>%
      html_elements(".textslide-texts") %>%
      html_nodes("h3") %>%
      html_text2()
  }
  
  life_results$product_description[i] <- life_product_description
  
  html_content <- html %>%
    html_elements(".container-fluid") %>%
    html_text2() %>%
    tolower()
  
  company <- ifelse(grepl("takaful", html_content), "Prudential", "FWD")
  life_results$company[i] <- company
  
}

life_results <- life_results %>%
  mutate(insurance_type = case_when(
    company == "Prudential" ~ "Family Takaful",
    company == "FWD" ~ "Life Insurance",
    TRUE ~ NA_character_
  ))

gen_product_name <- gen_insurance_tab %>%
  html_elements(".list-text") %>%
  html_nodes("h2") %>%
  html_text2()

gen_product_description <- gen_insurance_tab %>%
  html_elements(".list-text") %>%
  html_nodes("p") %>%
  html_text2()

gen_product_description <- gen_product_description[sapply(strsplit(gen_product_description, "\\s+"), length) >= 5]

gen_results <- data.frame(company = "Takaful Ikhlas", product_type = "", product_name = gen_product_name, product_description = gen_product_description, insurance_type = "General Takaful", product_url = "")

gen_results <- gen_results %>%
  mutate(product_type = case_when(
    grepl("car", tolower(product_description)) | grepl("car", tolower(product_name)) ~ "Motor",
    grepl("house", tolower(product_description)) | grepl("house", tolower(product_name)) ~ "Home",
    grepl("personal accident", tolower(product_description)) | grepl("personal accident", tolower(product_name)) ~ "H&P",
    TRUE ~ NA_character_
  ))

results <- rbind(life_results, gen_results)

# Rearrange columns, remove unnecessary ones 

results <- results[, c("company", "product_type", "product_name", "product_description", "insurance_type")]

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

