library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "Results/hlb.csv"

insurance_tab <- read_html_live("https://www.hlb.com.my/en/personal-banking/insurance.html")

product_type <- 
  insurance_tab %>%
  html_elements(".category") %>%
  html_attr("data-key") %>%
  as.character() %>%
  .[-1]

product_type <- data.frame(product_type)

url_results <- data.frame(product_type = character(), product_name = character(), product_description = character(), product_url = character(), stringsAsFactors = FALSE)

# Loop through each product type
for (i in 1:nrow(product_type)) {
  
  dummy1 <- product_type[i,]
  
  tryCatch({
    # Extract product names based on current data-key
    names <- insurance_tab %>%
      html_elements(paste("div.col-xs-12.col-lg-4[data-key='", dummy1, "']", sep = "")) %>%
      html_nodes("h6.dax-medium.title") %>%
      html_text()
    
    product_name <- unique(names)
    
    # Extract product URLs based on current data-key
    urls <- insurance_tab %>%
      html_elements(paste("div.col-xs-12.col-lg-4[data-key='", dummy1, "']", sep = "")) %>%
      html_nodes("a.w-100.dax-medium.btn.btn-light") %>%
      html_attr("href")
    
    product_urls <- unique(urls)
    
    descript <- insurance_tab %>%
      html_elements(paste("div.col-xs-12.col-lg-4[data-key='", dummy1, "']", sep = "")) %>%
      html_nodes(".additional-details.margin-top-l span") %>%
      html_text2()
    
    product_description <- unique(descript)
									
    # Create a data frame for current data-key results
    key_results <- data.frame(product_type = rep(dummy1, length(product_name)),
                              product_name = product_name,
                              product_description = product_description,
                              product_url = product_urls,
                              stringsAsFactors = FALSE)
    
    # Append to results data frame
    if (length(product_name) > 0) {
      url_results <- bind_rows(url_results, key_results)
    }
  }, error = function(e) {
    cat("Error occurred:", conditionMessage(e), "\n")
  })
}

company_list <- data.frame(company = character(nrow(url_results)), stringsAsFactors = FALSE)

for (j in 1:nrow(url_results)){
  
    link <- paste("https://hlb.com.my", url_results[j,4], sep = "")
      
    html <- read_html(link)
    html$session$default_timeout <- 10*100000000000
    
    company_text <-  html %>%
      html_nodes("div.text.parbase.section") %>%
      html_nodes("p") %>%
      html_text() %>%
      tolower() %>%
      .[which(grepl("underwritten by", .))]
    
    product_company <- gsub(".*underwritten by[:]?\\s*(.*?)(,|\\.|\\().*", "\\1", company_text, ignore.case = TRUE) %>%
      trimws()
    
    if (length(product_company) > 0) {
      company_list$company[j] <- product_company
    } else {
      company_list$company[j] <- NA
    }
}

company_list <- company_list %>%
  mutate(company = ifelse(company == "msig insurance", "MSIG", 
                                  ifelse(company == "hong leong assurance berhad", "Hong Leong Assurance", 
                                         ifelse(!company %in% c("MSIG", "Hong Leong Assurance"), 
                                                paste0(company, " (New Underwriter!)"), 
                                                company))))

url_results <- url_results %>%
  mutate(product_type = case_when(
    product_type == "motor" ~ "Motor",
    product_type == "travel" ~ "Travel",
    product_type == "home" ~ "Home",
    product_type == "life" ~ "Life",
    product_type == "personal-accident" ~ "H&P",
    product_type == "medical" ~ "Medical",
    product_type == "card" ~ "Liability",
    product_type == "investment-linked" ~ "Investment",
    product_type == "safe-deposit-locker" ~ "Misc."
  ))

url_results <- url_results %>%
  mutate(insurance_type = case_when(
    product_type == "Motor" ~ "General Insurance",
    product_type == "Travel" ~ "General Insurance",
    product_type == "Home" ~ "General Insurance",
    product_type == "Life" ~ "Life Insurance",
    product_type == "H&P" ~ "General Insurance",
    product_type == "Medical" ~ "Life Insurance",
    product_type == "Liability" ~ "General Insurance",
    product_type == "Investment" ~ "Life Insurance",
    product_type == "Misc." ~ "General Insurance"
  ))

results <- cbind(company_list, url_results)

# Rearrange columns, remove unnecessary ones 

results <- results[, c("company", "product_type", "product_name", "product_description", "insurance_type")]

# Timestamp last row

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c(company = "", product_type = "Scraped at", product_name = ":", product_description = formatted_timestamp, insurance_type = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)

