library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/uob.csv"

insurance_tab <- read_html_live("https://www.uob.com.my/personal/insure/index.page")

urls <- insurance_tab %>%
  html_elements(".link") %>%
  html_attr("href")
  
urls <- head(urls, -4)
urls <- unique(urls)

product_names <- insurance_tab %>%
  html_nodes(".descs h3") %>%
  html_text2()

product_names <- head(product_names, -6)
product_names <- unique(product_names)

insurance_type <- ifelse(grepl("general-insurance", urls), "General Insurance", "Life Insurance")

results <- data.frame(company = "", product_type = "", product_name = product_names, product_description = "", insurance_type = insurance_type, product_url = urls)
results$product_url <- as.character(results$product_url)

company_list <- data.frame(company = character(nrow(results)), stringsAsFactors = FALSE)
desc_list <- data.frame(product_description = character(nrow(results)), stringsAsFactors = FALSE)

for (i in 1:nrow(results)){
  
  incomplete_link <- results$product_url[i]
  
  link <- ifelse(grepl("https://", incomplete_link), 
                 incomplete_link, 
                 paste("https://www.uob.com.my", incomplete_link, sep = ""))
  
  html <- read_html(link)
  html$session$default_timeout <- 10*100000000000
  
  descript <- html %>%
    html_elements(".description") %>%
    html_nodes("p") %>%
    html_text2()
  
  if (length(descript) == 0) {
    descript <- html %>%
      html_elements(".description") %>%
      html_nodes("h2") %>%
      html_text2()
  } else if (length(descript) > 0 && any(grepl("This is an insurance product", descript, ignore.case = TRUE))) {
    descript <- descript[2]
  } else {
    descript <- descript
  }
  
  if (results$insurance_type[i] == "Life Insurance") {
    node <- "div.free-text"
  } else if (results$insurance_type[i] == "General Insurance") {
    node <- "div.panel-body"
  }
  
  company_text <-  html %>%
    html_nodes(node) %>%
    html_text() %>%
    tolower() %>%
    .[which(grepl("underwritten by", .))]
  
  if (length(company_text) == 0) {
    company_text <-  html %>%
      html_nodes("div.free-text") %>%
      html_text() %>%
      tolower()
  } else {
    company_text <- company_text
  }
  
  company_text <- company_text[grep("underwritten by[:]?\\s*(.*?)(,|\\.|\\().*", company_text, ignore.case = TRUE)]
  
  product_company <- gsub(".*underwritten by[:]?\\s*(.*?)(,|\\.|\\().*", "\\1", company_text, ignore.case = TRUE) %>%
    trimws()
  
  if (length(product_company) > 0) {
    company_list$company[i] <- product_company
  } else {
    company_list$company[i] <- NA
  }
  
  if (length(descript) > 0) {
    desc_list$product_description[i] <- descript
  } else {
    desc_list$product_description[i] <- NA
  }
}

company_list <- company_list %>%
  mutate(company = case_when(
    grepl("prudential", company, ignore.case = TRUE) ~ "Prudential",
    grepl("liberty", company, ignore.case = TRUE) ~ "Liberty",
    TRUE ~ company  # Keep the original if no match
  ))

results$company <- company_list
results$product_description <- desc_list

results <- results %>%
  mutate(product_type = case_when(
    grepl("medical", tolower(product_description)) | grepl("medical", tolower(product_name)) ~ "Medical",
    grepl("care", tolower(product_description)) | grepl("care", tolower(product_name)) ~ "H&P",
    grepl("motor", tolower(product_description)) | grepl("motor", tolower(product_name)) ~ "Motor",
    grepl("road", tolower(product_description)) | grepl("road", tolower(product_name)) ~ "Motor",
    grepl("travel", tolower(product_description)) | grepl("travel", tolower(product_name)) ~ "Travel",
    grepl("investment", tolower(product_description)) | grepl("investment", tolower(product_name)) ~ "Investment"
  )) %>%
  mutate(product_type = case_when(
    insurance_type == "Life Insurance" & product_type == "" ~ "Life",
    insurance_type == "General Insurance" & product_type == "" ~ "Misc.",
    TRUE ~ product_type
  ))

# Rearrange columns, remove unnecessary ones 

results <- results[, c("company", "product_type", "product_name", "product_description", "insurance_type")]

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

