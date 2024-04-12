library(rvest)
library(dplyr)

OUTPUT_FILE_PATH <- "results/FWD.csv"

# FWD's website doesn't have a product menu/index page, so these links have to be manually copied 
# Check https://www.fwd.com.my from time-to-time in case some links change, or if there's a new category

CATEGORY_URLS <- c("Life Insurance" = "https://www.fwd.com.my/life-insurance-takaful/#insurance",
                   "Medical Insurance" = "https://www.fwd.com.my/medical-insurance-takaful/#insurance/",
                   "Critical Illness" = "https://www.fwd.com.my/critical-illness-insurance-takaful/#insurance/",
                   "Savings & Investment" = "https://www.fwd.com.my/savings-investment-insurance-takaful/#insurance/",
                   "Distribution Partners" = "https://www.fwd.com.my/insurance-by-partners/#insurance/")

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

# `load_more()` attempts to click "Show More" at the bottom of the page
# Returns TRUE on success, FALSE on error

load_more <- function(html){
    tryCatch(
        {
        html$click(".otQLA")
        return(TRUE)
        },
        error = function(e) {
            return(FALSE)
        }
    )
}

for (i in 1:length(CATEGORY_URLS)){

  html <- read_html_live(CATEGORY_URLS[i])
  html$view()
  # Attempt to press 'show more' 10 times to load the whole page

  load_more(html)

  # 1 second delay to let the page load

  Sys.sleep(1)

  product_name <- 
    html %>%
    html_elements(".ProductCardDetail__Title-sc-15om8hj-3") %>%
    html_text2()

  product_description <-
    html %>%
    html_elements(".kglZYx, .cBkefT, #product-list p") %>%
    html_text2() %>%
    unique() # Clean out duplicates

  results <- rbind(results, data.frame(product_type = names(CATEGORY_URLS[i]), product_name, product_description)) 

}


# Combine duplicate results with different product_type

results <- results %>%
  group_by(product_name, product_description) %>%
  summarise(product_type = paste(unique(product_type), collapse = ", "))

# Write results to .csv file

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
