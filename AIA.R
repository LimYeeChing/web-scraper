library(rvest)

OUTPUT_FILE_PATH <- "results/aia.csv"

URL_LIST <- c("Health Protection" = "https://www.aia.com.my/en/our-products/health-protection.html",
            "Wealth Protection" = "https://www.aia.com.my/en/our-products/wealth-protection.html",
            "Accident Protection" = "https://www.aia.com.my/en/our-products/accident-protection.html",
            "Home Protection" = "https://www.aia.com.my/en/our-products/home-protection.html",
            "Motor Insurance" = "https://www.aia.com.my/en/our-products/motor-insurance.html")

results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(results) <- c("product_type", "product_name", "product_description")

# `load_more()` attempts to click "Load More" at the bottom of the page
# Returns TRUE on success, FALSE on error
# Not all products would load if we don't click "Load More"

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

for (i in 1:length(URL_LIST)){

  url  <- URL_LIST[i]
  html <- read_html_live(url)

  # Fully load all products on the page so they can be scraped

  while(TRUE){
    if(!load_more(html))
      break
    }
  
  product_name <-
    html %>% 
    html_elements("h2.cmp-productfilterlist__productcard__title") %>%
    html_text2()

  product_type<-
    html %>% 
    html_elements("div.cmp-productfilterlist__productcard__category") %>%
    html_text2()

  product_description <-
    html %>% 
    html_elements("div.cmp-productfilterlist__productdescription") %>%
    html_text2()

  # Clean data, there are some placeholder products in the site

  product_name <- product_name[product_name != "huhu"]
  product_description <- product_description[product_description != "huhu"]

  # Compile data from this site with results previously scraped

  if(!any(length(product_type) == 0, length(product_name) == 0, length(product_description) == 0)){
    results <- rbind(results, data.frame(product_type, product_name, product_description)) 
  }

}

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)
