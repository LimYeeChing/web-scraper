library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/ammetlife.csv"

# Ammetlife's website doesn't have a product menu/index page, so these links have to be manually copied 
# Check https://www.ammetlife.com/ from time-to-time in case some links change, or if there's a new category
# Give product_type to each url as names

INSURANCE_URLS <- c("Life" = "https://www.ammetlife.com/family/protection/",
                   "Life (credit-related)" = "https://www.ammetlife.com/family/credit-related/",
                   "H&P" = "https://www.ammetlife.com/health/hospitalisation/hospitalisation-agency/",
                   "H&P" = "https://www.ammetlife.com/health/hospitalisation/hospitalisation-banca/",
                   "Critical Illness" = "https://www.ammetlife.com/health/critical-illness/",
                   "Savings/Investment" = "https://www.ammetlife.com/future/wealth/",
                   "TBD" = "https://www.ammetlife.com/future/others/")

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Loop through all product types, visiting each type's page and scraping info

for (i in 1:length(INSURANCE_URLS)) {
  html <- read_html(INSURANCE_URLS[i])
  
  # Scrape product names
  
  product_name <-
    html %>% 
    html_elements("div.product-module__top") %>%
    html_elements("h3") %>%
    html_text2()

  # Scrape product descriptions
 
  product_description <-
    html %>%
    html_elements("div.product-module__list") %>%
    html_text2()
  
  # DailyCash in Healthcare (Banca) doesn't have a product description
  # Band-aid solution for now

  if (length(product_name) > length(product_description)){
    print(paste("(ammetlife) Warning: one or more of the products in", INSURANCE_URLS[i], "is missing a product desciption, filling in with 'No description found'"))
    print("Please check if this is correct, and if the descriptions match the products")

    product_description[(length(product_description) + 1):length(product_name)] <- "*No description found"
  }

  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = names(INSURANCE_URLS[i]), product_name, product_description)) 
  }
}

results <- cbind(results, "insurance_type" = "Life Insurance")

# Scrape takaful results ----

takaful_results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(takaful_results) <- c("product_type", "product_name", "product_description")

TAKAFUL_HOMEPAGE <- "https://www.ammetlifetakaful.com"

homepage <- read_html(TAKAFUL_HOMEPAGE)

takaful_urls <- 
  html_elements(homepage, "ul.subCategories")[1]  %>%
  html_elements("li a") %>%
  html_attr("href")

takaful_urls <- paste0(TAKAFUL_HOMEPAGE, takaful_urls)

for (i in 1:length(takaful_urls)){
  
  html <- read_html(takaful_urls[i])

  product_type <- 
    html %>% 
    html_elements("h1") %>% 
    html_text2()    

  product_type <- trimws(gsub("\r", "", product_type[1])) # To prevent product_type looking like "Protection\r \r \r \r"

  product_name <- 
    html %>% 
    html_elements("h4") %>% 
    html_text2()              

  product_description <- 
    html %>% 
    html_elements("div.productTextHolder ul") %>% 
    html_text2() 

  takaful_results <- rbind(takaful_results, data.frame(product_type, product_name, product_description))

}

takaful_results <- takaful_results %>%
  mutate(product_type = case_when(
    product_type == "Protection" ~ "Life",
    product_type == "Health" ~ "H&P/Medical",
    product_type == "Savings" ~ "Savings/Investment",
    product_type == "Education" ~ "Savings/Investment",
    product_type == "Reducing Term Takaful" ~ "TBD",
    product_type == "Group Products" ~ "TBD", 
    TRUE ~ product_type
  ))

takaful_results <- cbind(takaful_results, "insurance_type" = "Family Takaful")

results <- rbind(results, takaful_results)

# Combine duplicate results
# some products fall into more than one category, thus appearing in multiple pages 

results <- results %>%
  group_by(product_name, product_description) %>%
  summarise(
    product_type = paste(unique(product_type), collapse = ", "),
    insurance_type = paste(unique(insurance_type), collapse = ", ")
  )

# Write results 

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

results <- results[, c(3, 1, 2, 4)] # Rearrange columns, summarise() changes their order

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)



