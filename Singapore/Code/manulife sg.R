library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/manulife sg.csv"

HOMEPAGE <- "https://www.manulife.com.sg/" 

homepage <- read_html(HOMEPAGE)

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

# Scrape product types and URLs from category index page

product_type <-
  homepage %>% 
  html_elements(".cmp-navigation__item-link") %>%
  html_attr("title")

urls <-
  homepage %>% 
  html_elements(".cmp-navigation__item-link") %>%
  html_attr("href")

start_index <- which(product_type == "Solutions") + 1
end_index <- which(product_type == "Promotions") - 1

product_type <- product_type[start_index:end_index]
urls <- urls[start_index:end_index]

product_types <- data.frame(product_type, urls)

for (i in 1:nrow(product_types)){
  
  url <- paste0("https://www.manulife.com.sg", product_types$urls[i])
  
  html <- read_html(url)
  
  product_name <- html %>%
    html_elements(".cmp-productteaser__title-span.cmp-teaser__titleOrDescription--replaceFromPage") %>%
    html_text2()
  
  product_desc <- html %>%
    html_elements(".cmp-productteaser__description.cmp-teaser__titleOrDescription--replaceFromPage") %>%
    html_text2()
  
  product_desc <- sub("\\n.*", "", product_desc)
  
  bank_name <- "-"
  
  results <- rbind(results, data.frame(insurer = "Manulife", bank_name = bank_name, product_type = product_types$product_type[i], product_name = product_name, product_description = product_desc, insurance_type = "Life Insurance"))
}

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Life" ~ "Life",
    product_type == "Health" ~ "H&P",
    product_type == "Save" ~ "Savings",
    product_type == "Invest" ~ "Investment",
    product_type == "Signature" ~ "Legacy",
    TRUE ~ NA_character_
  ))

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

