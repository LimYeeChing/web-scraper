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

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
