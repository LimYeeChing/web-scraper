library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/rhb.csv"

HOMEPAGE <- "https://www.rhbgroup.com/overview/insurance/index.html"

homepage <- read_html(HOMEPAGE)

# Scrape category urls and names

product_type <- 
  homepage %>% 
  html_elements(".grid.grid-cols-2.container-fluid-tw a .text-primary") %>% 
  html_text2()

url <- 
  homepage %>% 
  html_elements(".grid.grid-cols-2.container-fluid-tw a") %>% 
  html_attr("href")

url <- paste0("https://www.rhbgroup.com", url)

product_types <- data.frame(product_type, url)

# Change product_type to match other companies

product_types <- product_types %>%
  mutate(product_type = case_when(
    product_type == "Personal Accident" ~ "H&P", 
    product_type == "Motor Add-on" ~ "Motor", 
    product_type == "Miscellaneous" ~ "Misc.",
    TRUE ~ product_type
  ))


# Loop through all product types to scrape product_name, product_description

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

for (i in 1:nrow(product_types)){

  html <- read_html(product_types$url[i])

  product_name <- 
    html %>%
    html_elements(".text-primary.p-5.font-bold, .text-black.font-bold.text-3xl, .text-xl.pb-4.font-bold") %>%
    html_text2()

  product_description <- 
    html %>%
    html_elements(, "//*[contains(@class, '-mt-1') and contains(@class, 'text-black')]//*[contains(@class, 'rte') and .//span] | //*[contains(@class, 'p-5') and contains(@class, 'h-full') and contains(@class, 'flex') and contains(@class, 'flex-col') and contains(@class, 'justify-between')]") %>%
    html_text2()

  results <- rbind(results, 
             data.frame(product_type = product_types$product_type[i], product_name, product_description))

}

# Remove 'Benefits' from results, not a product
results <- results[results$product_name != "Benefits", ]

# Prevent problems with character encoding
results$product_name <- gsub("’", "'", results$product_name)

results$product_description <- trimws(results$product_description)

# RHB Insurance is a general insurance company 

results <- cbind(company = "RHB Insurance", results, insurance_type = "General Insurance")

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)