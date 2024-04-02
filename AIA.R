library(rvest)

OUTPUT_FILE_PATH <- "results/AIA.csv"

URL_LIST <- c("Health Protection" = "https://www.aia.com.my/en/our-products/health-protection.html",
            "Wealth Protection" = "https://www.aia.com.my/en/our-products/wealth-protection.html",
            "Accident Protection" = "https://www.aia.com.my/en/our-products/accident-protection.html",
            "Home Protection" = "https://www.aia.com.my/en/our-products/home-protection.html",
            "Motor Insurance" = "https://www.aia.com.my/en/our-products/motor-insurance.html")

results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(results) <- c("product_type", "product_category", "product_name", "product_description")

# `load_more()` attempts to click "Load More" at the bottom of the page
# Returns TRUE on success, FALSE on error

load_more <- function(html){
    tryCatch(
        {
        html$click("div.cmp-productfilterlist__more")
        return(TRUE)
        },
        error = function(e) {
            return(FALSE)
        }
    )
}

# Loop through every url in `URL_LIST`

for (i in 1:length(URL_LIST)){

  url  <- URL_LIST[i]
  html <- read_html_live(url)

  # Fully load all products on the page

  while(TRUE){
    if(!load_more(html))
      break
    }
  
  # Scrape product_name, product_category, product_description

  product_name <-
    html %>% 
    html_elements("h2.cmp-productfilterlist__productcard__title") %>%
    html_text2()

  product_category <-
    html %>% 
    html_elements("div.cmp-productfilterlist__productcard__category") %>%
    html_text2()

  product_description <-
    html %>% 
    html_elements("div.cmp-productfilterlist__productdescription") %>%
    html_text2()

  # Clean data 

  product_name <- product_name[product_name != "huhu"]
  product_description <- product_description[product_description != "huhu"]

  # Compile data from this site with results previously scraped

  if(!any(length(product_category) == 0, length(product_name) == 0, length(product_description) == 0)){
    results <- rbind(results, data.frame(product_type = names(url), product_category, product_name, product_description)) 
  }

}

# Write results to .csv file

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
