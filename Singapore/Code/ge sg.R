library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/ge sg.csv"

results <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) <- c("insurer", "bank_name", "product_type", "product_name", "product_description", "insurance_type")

URL_list <- data.frame(product_category = c("Life Insurance", "Health Insurance", "Personal Accident Insurance", "Retirement Income", "Wealth Accumulation", "Travel Insurance", "Car Insurance", "Home Insurance", "Maid Insurance", "Government Schemes", "Prestige Series"),
                       urls = c("https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/life-and-health/life-insurance",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/life-and-health/health-insurance",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/life-and-health/personal-accident-insurance",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/wealth/retirement-income",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/wealth/wealth-accumulation",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/lifestyle/travel",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/lifestyle/car",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/lifestyle/home",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/lifestyle/maid",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/government-schemes",
                                "https://www.greateasternlife.com/sg/en/personal-insurance/our-products.html?category=corp-site:product-category/prestige-series"))

load_more <- function(html){
  tryCatch(
    {
      html$click(".leo-button.leo-button--secondary")
      return(TRUE)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

for (i in 1:nrow(URL_list)){
  
  html <- read_html_live(URL_list$urls[i])
  
  for (n in 1:2){
    load_more(html)
    Sys.sleep(4)
  }
  
  product_name <- html %>%
    html_elements("h5.leo-card-title") %>%
    html_text2()
  
  product_desc <- html %>%
    html_elements(".banifits-content") %>%
    html_text2()
  
  product_type <- URL_list$product_category[i]
  
  results <- rbind(results, data.frame(insurer = "Great Eastern", bank_name = "-", product_type = product_type, product_name = product_name, product_description = product_desc, insurance_type = ""))
}

results <- results %>%
  mutate(product_name = sub("\\|.*", "", product_name))

results <- results %>%
  mutate(product_type = case_when(
    product_type == "Life Insurance" ~ "Life",
    product_type == "Health Insurance" ~ "H&P",
    product_type == "Personal Accident Insurance" ~ "H&P",
    product_type == "Retirement Income" ~ "Savings",
    product_type == "Wealth Accumulation" ~ "Investment",
    product_type == "Travel Insurance" ~ "Travel",
    product_type == "Car Insurance" ~ "Motor",
    product_type == "Home Insurance" ~ "Home",
    product_type == "Maid Insurance" ~ "Misc.",
    TRUE ~ NA_character_
  ))

results <- results %>%
  mutate(insurance_type = case_when(
    product_type %in% c("Life", "H&P", "Savings", "Investment") ~ "Life Insurance",
    product_type %in% c("Travel", "Motor", "Home") ~ "General Insurance",
    product_type == "Misc." & grepl("maid", tolower(product_name)) ~ "General Insurance",
    TRUE ~ NA_character_
  ))

results <- results %>%
  group_by(product_name) %>%
  summarise(
    insurer = first(insurer),
    bank_name = first(bank_name),
    product_type = first(product_type),
    product_description = paste(unique(product_description), collapse = ", "),
    insurance_type = first(insurance_type)
  )

results <- results[, c(2, 3, 4, 1, 5, 6)] # Rearrange columns, summarise() changes their order

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(insurer = "", bank_name = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
