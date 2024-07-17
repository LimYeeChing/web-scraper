library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/ocbc.csv"

HOME <- "https://www.ocbc.com.my/personal-banking/wealth-management/insurance/overview" 

home_html <- read_html(HOME)

extract_ptype <- function(x){

  # Turn 'Find a ____ plan' to '____'

  ptype <- gsub("Find a ", "", x)
  ptype <- gsub("Find an ", "", ptype)
  tools::toTitleCase(gsub(" plan", "", ptype))

}

index_pages <- 
  home_html %>% 
  html_elements(".mob-mb2 div div div div a")

product_type <- 
  index_pages %>% 
  html_text2() %>%
  extract_ptype()

urls <- 
 index_pages %>% 
 html_attr("href")

urls <- paste0("https://www.ocbc.com.my", urls)

product_types <- data.frame(product_type, url = urls)

product_pages <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(product_pages) <- c("product_type", "product_name", "product_descripton", "url")

# Adjust product_type to match other companies' results

product_types <- product_types %>% 
  mutate(product_type = case_when(
    product_type == "Endowment" ~ "Savings", 
    product_type == "Protection" ~ "Life/Medical/CI/H&P", 
    product_type == "Legacy" ~ "Investment", 
    TRUE ~ product_type
  ))

# Each index page has conventional and islamic section
# Scrape product name, description, url from index page, rest from product page itself

for (i in 1:nrow(product_types)){

  html <- read_html_live(product_types$url[i])

  product_name <- 
    html %>% 
    html_elements(".com_insurance-text-listing .com_insurance-text-title") %>% 
    html_text2() 

  product_description <-
    html %>% 
    html_elements(".com_insurance-text-listing .com_insurance-text-item") %>% 
    html_text2()

  url <- 
    html %>% 
    html_elements(".com_insurance-text-listing .com_insurance-text-item a") %>% 
    html_attr("href")

  # Make sure all urls start with https://www.ocbc.com.my

  url[startsWith(url, "/")] <- paste0("https://www.ocbc.com.my", url[startsWith(url, "/")])

  product_pages <- rbind(product_pages, data.frame(product_type = product_types$product_type[i], product_name, product_description, url))

}

# Initialise results data frame

results <- cbind(company = "To be filled", product_pages, insurance_type = "To be filled")
results$url <- NULL

# Visit each site to fill in company and insurance type 
# As of 17/7/24 OCBC only partners with GE so only checking for that
# There's an idea to write a global function for all websites to check for a database of companies 
# No time to try though :(

company_mentioned <- function(html, company_name) {
  
  # Return TRUE if any mentions of company name found in the website, FALSE otherwise

  any(grepl(company_name, html))

}

for (i in 1:nrow(product_pages)){

  html <- read_html(product_pages$url[i])

  if (company_mentioned(html, "Great Eastern General Insurance")){

    results[i, "company"] <- "Great Eastern"
    results[i, "insurance_type"] <- "General Insurance"

  } else if (company_mentioned(html, "Great Eastern Life Assurance")){
     
    results[i, "company"] <- "Great Eastern"
    results[i, "insurance_type"] <- "Life Insurance"

  } else if (company_mentioned(html, "Great Eastern Takaful Berhad")){
     
    results[i, "company"] <- "Great Eastern"
    results[i, "insurance_type"] <- "Family Takaful"

  }

}

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)