library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/sc.csv"

HOMEPAGE <- "https://www.sc.com/my/insurance/"

homepage <- read_html(HOMEPAGE)

# Scrape category urls and names

product_type <- 
  homepage %>% 
  html_elements(".item.card.has-desc h3") %>% 
  html_text2()

url <- 
  homepage %>% 
  html_elements(".item.card.has-desc a") %>% 
  html_attr("href")

product_types <- data.frame(product_type, url)

# These pages don't contain product info, remove
product_types <- product_types[!grepl("Service Guide", product_type), ] 

# Change product_type to match other companies

product_types$product_type <- gsub(" Insurance", "", product_types$product_type)

product_types <- product_types %>%
  mutate(product_type = case_when(
    product_type == "Health" ~ "H&P", 
    product_type == "Children’s Education" ~ "Savings/Investment", 
    product_type == "Retirement" ~ "Savings/Investment", 
    product_type == "Insurance Savings Plan" ~ "Savings",
    product_type == "Protection" ~ "Life",
    TRUE ~ product_type
  ))


# Some of the urls lead direct to product page, some lead to an index page for the product type
# We will gather all product pages first 
# Create a data frame `product_pages` to store product pages 

product_pages <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(product_pages) <- c("product_name", "product_type", "url")

product_page_index <- vector(mode = "numeric", length = 0)
# product_page_index will contain indices of the rows of product_types that lead to product pages

for (i in 1:nrow(product_types)){

  html <- read_html(product_types$url[i])

  product_name <-
    html %>%
    html_elements(".item.card.has-desc h3.title") %>%
    html_text2()

  url <- 
   html %>% 
   html_elements(".item.card.has-desc a") %>% 
   html_attr("href")
    
  if (length(product_name) > 0){
    product_pages <- rbind(product_pages, data.frame(product_name, product_type = product_types$product_type[i],url))
  } else {
    product_page_index <- c(product_page_index, i)
    # Add i to the index
  }

}

TBF <- "To be filled"
additional_product_pages <- cbind("product_name" = TBF, product_types[product_page_index, ])

product_pages <- rbind(additional_product_pages, product_pages)

# Initialise results data frame
results <- product_pages[, -3]
results <- cbind("company" = TBF, results, "product_description" = TBF, "insurance_type" = TBF)

# Loop through each url to fill in missing data points in 'results'

urls <- product_pages$url

company_mentioned <- function(html, company_name) {
  
  # Return TRUE if any mentions of company name found in the website, FALSE otherwise

  any(grepl(company_name, html))

}

for (i in 1:nrow(results)){

  html <- read_html(urls[i])

  # Search for underwriting insurance company 
  # As of 16/7/24 SC only partners with Prudential and Allianz 
  # Will skip products that can't identify company
  # Using this to filter non-product pages

  if(company_mentioned(html, company_name = "Prudential Assurance")){

    results[i, "company"] <- "Prudential"
    results[i, "insurance_type"] <- "Life Insurance"  

  } else if(company_mentioned(html, company_name = "Allianz General Insurance")){

    results[i, "company"] <- "Allianz"
    results[i, "insurance_type"] <- "General Insurance"  

  } else if(company_mentioned(html, company_name = "Prudential BSN Takaful")){

    results[i, "company"] <- "Prudential"
    results[i, "insurance_type"] <- "Family Takaful"  

  } else if(company_mentioned(html, company_name = "Allianz Life")){

    results[i, "company"] <- "Allianz"
    results[i, "insurance_type"] <- "Life Insurance"  

  } else {

    print(paste0("Warning: Couldn't determine underwriting company for ", urls[i], ", will remove this site from results"))
    next
    # Can expect https://www.sc.com/my/insurance/health to show up
    # As of 16/7/24 it's an empty index page with no products

  }

  description <- 
    html %>% 
    html_elements("p.message") %>% 
    html_text2()

  if (length(description) > 0){

    results[i, "product_description"] <- paste(description, collapse = "\n")

  }

  if (results$product_name[i] == TBF){

    results$product_name[i] <-
      html %>% 
      html_elements("span.post.post-page.current") %>% 
      html_text2()

  }

}

# Clean out products that we couldn't determine underwriting company for 

results <- results[results$company != TBF, ]

# Combine duplicate results
# some products may fall into more than one category, thus appearing in multiple pages 

results <- results %>%
  group_by(company, product_name) %>%
  summarise(product_type = paste(unique(product_type), collapse = ", "),
            product_description = first(product_description),
            insurance_type = paste(unique(insurance_type), collapse = ", "))

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)