library(rvest)

OUTPUT_FILE_PATH <- "results/ammetlife.csv"

# Ammetlife's website doesn't have a product menu/index page, so these links have to be manually copied 
# Check https://www.ammetlife.com/ from time-to-time in case some links change, or if there's a new category

CATEGORY_URLS <- c("Family, Protection" = "https://www.ammetlife.com/family/protection/",
                   "Family, Credit-related" = "https://www.ammetlife.com/family/credit-related/",
                   "Health, Hospitalisation (Agency)" = "https://www.ammetlife.com/health/hospitalisation/hospitalisation-agency/",
                   "Health, Hospitalisation (Banca)" = "https://www.ammetlife.com/health/hospitalisation/hospitalisation-banca/",
                   "Health, Critical Illness" = "https://www.ammetlife.com/health/critical-illness/",
                   "Future, Wealth" = "https://www.ammetlife.com/future/wealth/",
                   "Future, Others" = "https://www.ammetlife.com/future/others/")

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# Loop through all product types, visiting each type's page and scraping info

for (i in 1:length(CATEGORY_URLS)) {
  html <- read_html(CATEGORY_URLS[i])
  
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
    print(paste("(ammetlife) Warning: one or more of the products in", CATEGORY_URLS[i], "is missing a product desciption, filling in with 'No description found'"))
    print("Please check if this is correct, and if the descriptions match the products")

    product_description[(length(product_description) + 1):length(product_name)] <- "*No description found"
  }

  if (length(product_name) != 0 && length(product_description) != 0){
    results <- rbind(results, data.frame(product_type = names(CATEGORY_URLS[i]), product_name, product_description)) 
  }
}

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)



