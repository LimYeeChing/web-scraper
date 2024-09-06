library(rvest)
library(dplyr)
library(chromote)

OUTPUT_FILE_PATH <- "Results/bankrakyat.csv"

b <- ChromoteSession$new()
b$Page$navigate("https://www.bankrakyat.com.my/")
b$Page$loadEventFired(timeout = 60000)
html <- b$Runtime$evaluate("document.documentElement.outerHTML")
homepage <- read_html(html$result$value)

# homepage <- read_html_live("https://www.bankrakyat.com.my/")

# They don't have takaful index page, so forced to scrape from menu 
# Takaful products are between 'Will Writing' and 'Unit Trust'

urls <- 
  homepage %>% 
  html_elements("a.dropdown-item") %>%
  html_attr("href")

# Find the indices of the start and end URLs

end_index <- which(urls == "https://www.bankrakyat.com.my/portal-main/article/unit-trust")
start_index <- which(urls == "https://www.bankrakyat.com.my/portal-main/article/will-writing")

product_urls <- urls[(start_index + 1):(end_index - 1)]

# Clean filler urls

product_urls <- product_urls[product_urls != "#"]

# Visit product pages and scrape data points

results <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(results) <- c("company", "product_type", "product_name", "product_description", "insurance_type")

company_mentioned <- function(html, company_name) {
  
  # Return TRUE if any mentions of company name found in the website, FALSE otherwise

  any(grepl(company_name, html))

}

companies <- data.frame(full_name = c("Takaful Ikhlas Family", "Takaful Ikhlas General", "Etiqa Family Takaful", "Etiqa General Takaful", "Syarikat Takaful Malaysia Keluarga", "Syarikat Takaful Am Malaysia"),
                        name = c("Takaful Ikhlas", "Takaful Ikhlas", "Etiqa", "Etiqa", "Syarikat Takaful", "Syarikat Takaful"),
                        type = c("Family Takaful", "General Takaful", "Family Takaful", "General Takaful", "Family Takaful", "General Takaful"))

for (i in 1:length(product_urls)){

  # c <- ChromoteSession$new()
  # c$Page$navigate(product_urls[i])
  # c$Page$loadEventFired(timeout = 480000)
  # dummy_html <- c$Runtime$evaluate("document.documentElement.outerHTML")
  # html <- read_html(dummy_html$result$value)
  html <- read_html_live(product_urls[i])
  
  for(n in 1:2){
    Sys.sleep(2)
  }

  product_name <- 
    html %>% 
    html_elements("h1") %>% 
    html_text2() %>%
    unique() %>% 
    first()

  product_description <-
   html %>% 
   html_elements(".page-section-ptb .container") %>% 
   html_text2() %>%
   first() %>%
   trimws()
  
  # Don't have a way to determine product_type, maybe can enhance
  
  product_type <- "To be filled"

  # Use company names to fill in company and insurance_type
  # Some products have multiple companies' options
  # One entry in results for each

  any_company_mentioned <- FALSE

  html_text <- html_elements(html, "*")

  for (j in 1:nrow(companies)){
    if(company_mentioned(html_text, companies$full_name[j])){
      
      company <- companies$name[j] 
      insurance_type <- companies$type[j]
      
      results <- rbind(results, data.frame(company, product_type, product_name, product_description, insurance_type))
      any_company_mentioned <- TRUE
      
    }
  }
  
  if(!any_company_mentioned){
    
    company <- "To be filled"
    insurance_type <- "To be filled"
    results <- rbind(results, data.frame(company, product_type, product_name, product_description, insurance_type))
    
  }
}

# Combine rows with the same product_name
combined_results <- results %>%
  group_by(product_name) %>%
  summarise(
    company = paste(unique(company), collapse = ", "),
    product_type = first(product_type), # Assuming product_type is the same for all rows with the same product_name
    product_description = first(product_description), # Assuming product_description is the same or should be kept from the first row
    insurance_type = paste(unique(insurance_type), collapse = ", ")
  )

combined_results <- combined_results %>%
  select(company, product_type, product_name, product_description, insurance_type)

# Enhance this part to compare the results with last week's result. New product's product_type and insurance_type have to be filled in manually (if any).
date_value2 <- Sys.Date() - 7

last_weeks_month <- format(date_value2, "%m")
last_weeks_year <- as.numeric(format(date_value2, "%Y"))
last_weeks_week <- ceiling(as.numeric(format(date_value2, "%d")) / 7)
last_week_path <- paste("Results", last_weeks_month, last_weeks_year, "Week", last_weeks_week, sep = "_")
last_week_csv <- read.csv(paste0(last_week_path, "/bankrakyat.csv"))

this_week_names <- tolower(combined_results$product_name)
last_week_names <- tolower(last_week_csv$product_name)
last_week_names <- head(last_week_names, -1)

for (i in 1:nrow(combined_results)) {
  # Check if the current product name exists in last week's data
  if (this_week_names[i] %in% last_week_names) {
    # Find the corresponding row in last week's data
    last_week_row <- last_week_csv[last_week_names == this_week_names[i], ]
    
    # Fill in the company, product_type, and insurance_type if they are "To be filled"
    if (combined_results$company[i] == "To be filled") {
      combined_results$company[i] <- last_week_row$company
    }
    
    if (combined_results$product_type[i] == "To be filled") {
      combined_results$product_type[i] <- last_week_row$product_type
    }
    
    if (combined_results$insurance_type[i] == "To be filled") {
      combined_results$insurance_type[i] <- last_week_row$insurance_type
    }
  } else {
    # Handle case where product is not found in last week's data
    combined_results$company[i] <- ifelse(combined_results$company[i] == "To be filled", NA, combined_results$company[i])
    combined_results$product_type[i] <- ifelse(combined_results$product_type[i] == "To be filled", NA, combined_results$product_type[i])
    combined_results$insurance_type[i] <- ifelse(combined_results$insurance_type[i] == "To be filled", NA, combined_results$insurance_type[i])
  }
}

  
# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
combined_results <- rbind(combined_results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(combined_results, file = OUTPUT_FILE_PATH, row.names = FALSE)