# Probably can enhance determining insurance_type to be more accurate

library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/maybank.csv"

HOMEPAGE <- "https://www.maybank2u.com.my/home/m2u/common/login.do" 

homepage <- read_html_live(HOMEPAGE)

# Scrape product types and URLs from bottom of homepage

insurance_tab <- html_elements(homepage, ".group-link")[4] # Insurance is the 4th group

product_type <-
  insurance_tab %>% 
  html_elements("li") %>%
  html_text2()

url <-
  insurance_tab %>% 
  html_elements("a") %>%
  html_attr("href")


product_types <- data.frame(product_type, url)

# Change product_type to align with other companies, TBD used if unclear

product_types <- product_types %>%
  mutate(product_type = case_when(
    product_type == "Personal Accident" ~ "H&P",
    product_type == "Car" ~ "Motor",
    product_type == "Retirement" ~ "Savings/Investment",
    product_type == "Education" ~ "Savings/Investment",
    product_type == "Home Insurance" ~ "Home",
    product_type == "Business" ~ "TBD",
    product_type == "Savings & Protection" ~ "Savings",
    TRUE ~ product_type
  ))

product_types$url <- gsub("\\?", "", product_types$url)

# Loop through all product types, visiting each type's page and scraping info

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

for (i in 1:nrow(product_types)){

  html <- read_html(product_types$url[i])

  product_name <-
    html %>%
    html_elements(".photo-note") %>%
    html_text2()

  product_description <-
    html %>%
    html_elements(".info") %>%
    html_text2()
  
  if(length(product_name) > 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description))
  }

}

# Figure out insurance_type 

results <- cbind(results, insurance_type = "Insurance")
takaful_index <- grepl("takaful", results$product_name, ignore.case = TRUE) | grepl("takaful", results$product_description, ignore.case = TRUE)
results$insurance_type[takaful_index] <- "Takaful"

results <- results %>%
  mutate(insurance_type = case_when(
    insurance_type == "Insurance" & product_type == "H&P" ~ "General Insurance",
    insurance_type == "Insurance" & product_type == "Motor" ~ "General Insurance",
    insurance_type == "Insurance" & product_type == "Travel" ~ "General Insurance",
    insurance_type == "Insurance" & product_type == "Savings/Investment" ~ "Life Insurance",
    insurance_type == "Insurance" & product_type == "Savings" ~ "Life Insurance",
    insurance_type == "Insurance" & product_type == "Home" ~ "General Insurance",
    insurance_type == "Insurance" & product_type == "TBD" ~ "General Insurance",
    insurance_type == "Takaful" & product_type == "H&P" ~ "General Takaful",
    insurance_type == "Takaful" & product_type == "Motor" ~ "General Takaful",
    insurance_type == "Takaful" & product_type == "Travel" ~ "General Takaful",
    insurance_type == "Takaful" & product_type == "Savings/Investment" ~ "Family Takaful",
    insurance_type == "Takaful" & product_type == "Savings" ~ "Family Takaful",
    insurance_type == "Takaful" & product_type == "Home" ~ "General Takaful",
    insurance_type == "Takaful" & product_type == "TBD" ~ "General Takaful",
    product_type == "Medical" ~ insurance_type,
    TRUE ~ insurance_type
  ))

# Figure out company 

results <- cbind(company = "Etiqa", results)

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

