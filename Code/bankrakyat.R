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

companies <- data.frame(full_name = c("Takaful Ikhlas Family", "Takaful Ikhlas General", "Etiqa Family Takaful", "Etiqa General Takaful", "Syarikat Takaful Malaysia Keluarga", "Syarikat Takaful Malaysia Am"),
                        name = c("Takaful Ikhlas", "Takaful Ikhlas", "Etiqa", "Etiqa", "Syarikat Takaful", "Syarikat Takaful"),
                        type = c("Family Takaful", "General Takaful", "Family Takaful", "General Takaful", "Family Takaful", "General Takaful"))

for (i in 1:length(product_urls)){

  c <- ChromoteSession$new()
  c$Page$navigate(product_urls[i])
  c$Page$loadEventFired(timeout = 300000)
  dummy_html <- c$Runtime$evaluate("document.documentElement.outerHTML")
  html <- read_html(dummy_html$result$value)
  # html <- read_html_live(product_urls[i])

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
  
  # Don't have a way to determine product_type, maybe can enahnce 
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

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)