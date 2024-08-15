library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/hsbc.csv"

homepage <- read_html("https://www.hsbc.com.my/insurance/")

product_name <- 
  homepage %>%
  html_elements(".A-LNKND38L-RW-ALL") %>%
  html_text2()

product_name <- gsub("’", "'", product_name)

product_description <-
  homepage %>% 
  html_elements(".main-list") %>%
  html_text2()

product_url <-
  homepage %>%
  html_elements(".md-12 div:nth-child(1) a") %>%
  html_attr("href")

product_url <- paste0("https://www.hsbc.com.my", product_url)

TBF <- "To be filled"
results <- cbind(company = TBF, product_type = TBF, product_name, product_description, insurance_type = TBF)

company_mentioned <- function(html, company_name) {
  
  # Return TRUE if any mentions of company name found in the website, FALSE otherwise

  any(grepl(company_name, html))

}

for (i in 1:length(product_url)){

  html <- read_html(product_url[i])

  if (company_mentioned(html, "Allianz General")){

    results[i, "company"] <- "Allianz"
    results[i, "insurance_type"] <- "General Insurance"

  } else if (company_mentioned(html, "Allianz Life")){
     
    results[i, "company"] <- "Allianz"
    results[i, "insurance_type"] <- "Life Insurance"

  }
}

results <- as.data.frame(results)

# Enhance this part to compare the results with last week's result. New product's product_type and insurance_type have to be filled in manually (if any).
date_value2 <- Sys.Date() - 7

last_weeks_month <- format(date_value2, "%m")
last_weeks_year <- as.numeric(format(date_value2, "%Y"))
last_weeks_week <- ceiling(as.numeric(format(date_value2, "%d")) / 7)
last_week_path <- paste("Results", last_weeks_month, last_weeks_year, "Week", last_weeks_week, sep = "_")
last_week_csv <- read.csv(paste0(last_week_path, "/hsbc.csv"))

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
