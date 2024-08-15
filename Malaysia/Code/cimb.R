library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/cimb.csv"

PRODUCT_TYPE <- c("Life", "Motor", "Travel", "H&P", "Life (credit-related)", "Property")

URL <- c("https://www.cimb.com.my/en/personal/day-to-day-banking/insurance-takaful/life-insurance-family-takaful.html",
         "https://www.cimb.com.my/en/personal/day-to-day-banking/insurance-takaful/car-insurance.html",
         "https://www.cimb.com.my/en/personal/day-to-day-banking/insurance-takaful/travel-insurance.html",
         "https://www.cimb.com.my/en/personal/day-to-day-banking/insurance-takaful/personal-accident-insurance.html",
         "https://www.cimb.com.my/en/personal/day-to-day-banking/insurance-takaful/credit-related.html",
         "https://www.cimb.com.my/en/personal/day-to-day-banking/insurance-takaful/property-insurance-takaful.html")

product_types <- data.frame(product_type = PRODUCT_TYPE, url = URL)

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

load_more <- function(html){
    tryCatch(
        {
        html$click("button.btn")
        return(TRUE)
        },
        error = function(e) {
            return(FALSE)
        }
    )
}

# Loop through all product types, visiting each type's page and scraping product name and url

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_url")

for (i in 1:nrow(product_types)){

  html <- read_html_live(product_types$url[i])

  # To retrieve data for this paginated site, we need to repeatedly push
  # the "Load More" button

  while(TRUE){
    if(!load_more(html))
      break
  }

  product_name <-
    html %>%
    html_elements(".product-card h5") %>%
    html_text2()

  product_url <-
    html %>%
    html_elements(".product-card a") %>%
    html_attr("href")
  
  if(length(product_name) > 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_url))
  }

}

# Visit each product_url to scrape product_description and product_note, used to determine insurance_type and company

results$product_description <- "To be filled"
results$product_note <- "To be filled"

for (i in 1:nrow(results)){

  html <- read_html(results[i, "product_url"])

  results[i, "product_description"] <-
    html %>%
    html_elements(".banner-description") %>%
    html_text2()

  results[i, "product_note"] <-
    html %>%
    html_elements(".text-author-content") %>%
    html_text2() %>%
    paste(collapse = "")

}

product_note <- results$product_note

# Figure out insurance_type 

takaful_index <- grepl("The Bank merely acts as a distributor for this takaful product.", product_note, ignore.case = TRUE)
# This note tells us that the product is a takaful product 

results$insurance_type <- "Insurance"
results$insurance_type[takaful_index] <- "Takaful"
 
results <- results %>%
  mutate(insurance_type = case_when(
    insurance_type == "Insurance" & product_type == "Life" ~ "Life Insurance",
    insurance_type == "Insurance" & product_type == "Motor" ~ "General Insurance",
    insurance_type == "Insurance" & product_type == "Travel" ~ "General Insurance",
    insurance_type == "Insurance" & product_type == "H&P" ~ "General Insurance",
    insurance_type == "Insurance" & product_type == "Life (credit-related)" ~ "Life Insurance",
    insurance_type == "Insurance" & product_type == "Property" ~ "General Insurance",
    insurance_type == "Takaful" & product_type == "Life" ~ "Family Takaful",
    insurance_type == "Takaful" & product_type == "Motor" ~ "General Takaful",
    insurance_type == "Takaful" & product_type == "Travel" ~ "General Takaful",
    insurance_type == "Takaful" & product_type == "H&P" ~ "General Takaful",
    insurance_type == "Takaful" & product_type == "Life (credit-related)" ~ "Family Takaful",
    insurance_type == "Takaful" & product_type == "Property" ~ "General Takaful",
    TRUE ~ insurance_type
  ))

# Figure out company 

results$company <- "TBD"

results$company[grepl("Sun Life", product_note)] <- "Sun Life"
results$company[grepl("Berjaya Sompo", product_note)] <- "Berjaya Sompo"
results$company[grepl("Takaful Ikhlas", product_note, ignore.case = TRUE)] <- "Takaful Ikhlas"

# Rearrange columns, remove unnecessary ones 

results <- results[, c("company", "product_type", "product_name", "product_description", "insurance_type")]

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

