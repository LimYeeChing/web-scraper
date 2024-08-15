library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/bankislam.csv"

homepage <- read_html("https://www.bankislam.com/personal-banking/wealth-management/takaful-protection/")

product_name <-
  homepage %>% 
  html_elements(".nohoverchange h5") %>%
  html_text2()

product_description <- 
  homepage %>% 
  html_elements(".nohoverchange") %>%
  html_text2() %>% 
  trimws()

url <-
  homepage %>% 
  html_elements("a.nohoverchange") %>%
  html_attr("href")

url <- paste0("https://www.bankislam.com", url)

product_pages <- data.frame(product_name, product_description, url)

# Remove empty rows
product_pages <- product_pages[product_pages$product_name != "", ]

results <- product_pages
results$url <- NULL
results <- cbind(company = "To be filled", product_type = "To be filled", results, insurance_type = "To be filled")

# No way to autofill product_type now, can enhance
# Visit pages to find company names to get company and insurance_type

company_mentioned <- function(html, company_name) {
  
  # Return TRUE if any mentions of company name found in the website, FALSE otherwise
  any(grepl(company_name, html))

}

for (i in 1:nrow(product_pages)){

  html <- read_html(product_pages$url[i])

  if (company_mentioned(html, "Syarikat Takaful Malaysia Am")){

    results[i, "company"] <- "Syarikat Takaful"
    results[i, "insurance_type"] <- "General Takaful"

  } else if (company_mentioned(html, "Syarikat Takaful Malaysia Keluarga")){
     
    results[i, "company"] <- "Syarikat Takaful"
    results[i, "insurance_type"] <- "Family Takaful"

  }

}

# Enhance this part to compare the results with last week's result. New product's product_type and insurance_type have to be filled in manually (if any).
date_value2 <- Sys.Date() - 7

last_weeks_month <- format(date_value2, "%m")
last_weeks_year <- as.numeric(format(date_value2, "%Y"))
last_weeks_week <- ceiling(as.numeric(format(date_value2, "%d")) / 7)
last_week_path <- paste("Results", last_weeks_month, last_weeks_year, "Week", last_weeks_week, sep = "_")
last_week_csv <- read.csv(paste0(last_week_path, "/bankislam.csv"))

this_week_names <- tolower(results$product_name)
last_week_names <- tolower(last_week_csv$product_name)
last_week_names <- head(last_week_names, -1)

for (i in 1:nrow(results)) {
  # Check if the current product name exists in last week's data
  if (this_week_names[i] %in% last_week_names) {
    # Find the corresponding row in last week's data
    last_week_row <- last_week_csv[last_week_names == this_week_names[i], ]
    
    # Fill in the product_type if they are "To be filled"
    if (results$product_type[i] == "To be filled") {
      results$product_type[i] <- last_week_row$product_type
    }
  } else {
    # Handle case where product is not found in last week's data
    results$product_type[i] <- ifelse(results$product_type[i] == "To be filled", NA, results$product_type[i])
  }
}
# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
