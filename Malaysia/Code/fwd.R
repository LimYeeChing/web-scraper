library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/fwd.csv"

# FWD's website doesn't have a product menu/index page, so these links have to be manually copied 
# Check https://www.fwd.com.my from time-to-time in case some links change, or if there's a new category

CATEGORY_URLS <- c("Life" = "https://www.fwd.com.my/life-insurance-takaful/",
                   "Medical" = "https://www.fwd.com.my/medical-insurance-takaful/",
                   "Critical Illness" = "https://www.fwd.com.my/critical-illness-insurance-takaful/",
                   "Savings/Investment" = "https://www.fwd.com.my/savings-investment-insurance-takaful/",
                   "TBD" = "https://www.fwd.com.my/insurance-by-partners/")

# Create data frame for urls, distinguishing insurance type

conventional_urls <- paste0(CATEGORY_URLS, "#insurance")
takaful_urls <- paste0(CATEGORY_URLS, "#takaful")

urls <- data.frame(product_type = names(CATEGORY_URLS), insurance_type = "Life Insurance", url = conventional_urls)
urls <- rbind(urls, data.frame(product_type = names(CATEGORY_URLS), insurance_type = "Family Takaful", url = takaful_urls))

results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(results) <- c("product_type", "product_name", "product_description", "insurance_type")

# `load_more()` attempts to click "Show More" at the bottom of the page
# Returns TRUE on success, FALSE on error

load_more <- function(html){
    tryCatch(
        {
        html$click(".hQIhXh")
        return(TRUE)
        },
        error = function(e) {
            return(FALSE)
        }
    )
}

remove_even_occurrences <- function(char_vector) {
  appearance_count <- vector("list", length(char_vector))
  names(appearance_count) <- unique(char_vector)
  
  to_keep <- logical(length(char_vector))
  
  for (k in seq_along(char_vector)) {
    desc <- char_vector[k]
    
    # Increment the count for this description
    appearance_count[[desc]] <- (appearance_count[[desc]] %||% 0) + 1
    
    # Keep the element if it's the 1st, 3rd, 5th, etc., occurrence
    to_keep[k] <- (appearance_count[[desc]] %% 2) == 1
  }
  
  filtered_vector <- char_vector[to_keep]
  
  return(filtered_vector)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

html$session$default_timeout <- 10*100000000

for (i in 1:nrow(urls)){
  
  # b <- ChromoteSession$new()
  # b$Page$navigate(urls$url[i])
  # b$Page$loadEventFired(timeout = 60000)
  # dummy_html <- b$Runtime$evaluate("document.documentElement.outerHTML")
  # html <- read_html(dummy_html$result$value)
  html <- read_html_live(urls$url[i])

  for(n in 1:2){
    load_more(html)
    Sys.sleep(2)
  }

  product_name <- 
    html %>%
    html_elements(".ProductCardDetail__Title-sc-15om8hj-3") %>%
    html_text2()

  product_description <-
    html %>%
    html_elements("#product-list p, .kglZYx, .cBkefT, .cBkEMD, .cBkRZl, .cauigS, .hsviFU, .cavtqs") %>%
    html_text2() %>%
    remove_even_occurrences() 
    # Clean out duplicates, not using unique() because there are products with same desc

  if(length(product_description) > 0){
    results <- rbind(results, data.frame(product_type = urls$product_type[i], product_name, product_description, insurance_type = urls$insurance_type[i])) 
  }
}

  # Combine duplicate results with different product_type

results <- results %>%
  group_by(product_name, product_description) %>%
  summarise(
    product_type = paste(unique(product_type), collapse = ", "),
    insurance_type = paste(unique(insurance_type), collapse = ", ")
  )

results <- results[, c(3, 1, 2, 4)] # Rearrange columns, summarise() changes their order

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("product_type" = "Scraped at", "product_name" = ":", "product_description" = formatted_timestamp, "insurance_type" = ""))

# write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
