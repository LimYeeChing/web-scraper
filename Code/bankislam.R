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

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
