library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/publicbank.csv"

PRODUCT_TYPE <- c("Investment", "Savings", "Critical Illness", "TBD (Telemarketing)", "Life (credit-related)", "TBD (online)")
URL          <- c("https://www.pbebank.com/en/insure/wealth-management-and-planning/",
                  "https://www.pbebank.com/en/insure/savings-for-your-future/",
                  "https://www.pbebank.com/en/insure/protect-you-from-any-critical-illness-events/",
                  "https://www.pbebank.com/en/insure/hassle-free-protection-via-telemarketing/",
                  "https://www.pbebank.com/en/insure/protect-your-outstanding-loan/",
                  "https://www.pbebank.com/en/insure/get-protected-at-your-fingertips/")

product_types <- data.frame(product_type = PRODUCT_TYPE, url = URL)

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Loop through all product types, visiting each type's page and scraping info

for (i in 1:nrow(product_types)){

  html <- read_html(product_types$url[i])

  product_name <-
    html %>%
    html_elements(".vgap18 .font-bold a") %>%
    html_text2()

  product_description <-
    html %>%
    html_elements(".vgap18 .desc") %>%
    html_text2()
  
  if(length(product_name) > 0){
    results <- rbind(results, data.frame(product_type = product_types$product_type[i], product_name, product_description))
  }

}

# Clean empty results

results <- results[!results$product_name == "", ] 
results <- results[!results$product_name == " ", ] 

# As of right now 12/7/2024, PB's products are all from AIA Life (this might change in the future)

results <- cbind(company = "AIA", results, insurance_type = "Life Insurance")

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

