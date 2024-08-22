library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/aia hk.csv"

URL_LIST     <- c("Health" = "https://www.aia.com.hk/en/products/health",
                  "Life" = "https://www.aia.com.hk/en/products/life",
                  "Save" = "https://www.aia.com.hk/en/products/save",
                  "General" = "https://www.aia.com.hk/en/products/general-insurance")

results <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(results) <- c("bank_name", "product_type", "product_name", "product_description", "insurance_type")

# `load_more()` attempts to click "Show More" at the bottom of the page
# Returns TRUE on success, FALSE on error
# Not all products would load if we don't click "Load More"

load_more <- function(html){
  tryCatch(
    {
      html$click("div.cmp-hkproductfilterlist__more")
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

for (i in 1:length(URL_LIST)){
  
  url  <- URL_LIST[i]
  html <- read_html_live(url)
  
  # Fully load all products on the page so they can be scraped
  
  while(TRUE){
    if(!load_more(html))
      break
  }
  
  product_name <-
    html %>% 
    html_elements("h2.cmp-hkproductfilterlist__productcard__title") %>%
    html_text2()
  
  product_type<-
    html %>% 
    html_elements("div.cmp-hkproductfilterlist__productcard__categorycrumbs") %>%
    html_text2()
  
  product_desc <-
    html %>% 
    html_elements("div.cmp-hkproductfilterlist__productdescription") %>%
    html_text2()
  
  # Replace "\n" with ", " in the product_type variable
  product_type <- gsub("\n", ", ", product_type)
  
  #No bancassurance product in this company
  bank_name <- "-"
  
  results <- rbind(results, data.frame(bank_name = bank_name, product_type = product_type, product_name = product_name, product_description = product_desc, insurance_type = ""))
}

# Extract the first product type before mutation
results <- results %>%
  mutate(first_product_type = sapply(strsplit(product_type, ","), function(x) trimws(x[1])))  # Take the first category and trim whitespace

# Apply the mutation to the first product type only
results <- results %>%
  mutate(first_product_type = case_when(
    first_product_type == "Critical Illness Protection" ~ "Critical Illness",
    first_product_type == "Medical Protection" ~ "Medical",
    first_product_type == "Voluntary Health Insurance Scheme" ~ "Medical",
    first_product_type == "Severity-based Health Protection" ~ "Medical",
    first_product_type == "Accident & Other Protections" ~ "H&P",
    first_product_type == "Life Protection" ~ "Life",
    first_product_type == "Savings Insurance" ~ "Savings",
    first_product_type == "Insurance with Investment Focus" ~ "Investment",
    first_product_type == "Retirement Income" ~ "Savings",
    first_product_type == "Home Insurance" ~ "Home",
    first_product_type == "Helper Insurance" ~ "Misc.",
    first_product_type == "Leisure Insurance" ~ "Misc.",
    TRUE ~ NA_character_
  ))

# Rename the first_product_type column back to product_type if needed
results <- results %>%
  select(-product_type) %>%
  rename(product_type = first_product_type)

results <- results %>%
  mutate(insurance_type = case_when(
    product_type %in% c("Life", "Investment", "Savings", "Critical Illness", "Medical", "H&P", "Legacy", "Disability") ~ "Life Insurance",
    product_type %in% c("Home", "Travel", "Motor", "Fire", "Property", "Liability", "Misc.") ~ "General Insurance",
    TRUE ~ NA_character_
  ))

# Combine rows with the same product_name
combined_results <- results %>%
  group_by(product_name) %>%
  summarise(
    product_type = paste(unique(product_type), collapse = ", "), 
    product_description = first(product_description), # Assuming product_description is the same or should be kept from the first row
    insurance_type = paste(unique(insurance_type), collapse = ", "),
    bank_name = first(bank_name)
  )

# Rearrange the columns in the desired order
combined_results <- combined_results %>%
  select(bank_name, product_type, product_name, product_description, insurance_type)

# Timestamp last row
formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
combined_results <- rbind(combined_results, c(bank_name = "Scraped at", product_type = ":", product_name = formatted_timestamp, product_description = "", insurance_type = ""))

# write.csv(combined_results, file = OUTPUT_FILE_PATH, row.names = FALSE)


  
  
