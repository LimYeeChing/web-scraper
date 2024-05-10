library(rvest)

OUTPUT_FILE_PATH <- "results/hla.csv"

# HLA's website currently shows all of its personal products on one page 

PRODUCTS_PAGE <- "https://www.hla.com.my/en/personal.html"

results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(results) <- c("product_type", "product_name", "product_description")

html <- read_html_live(PRODUCTS_PAGE)

# Scrape information from the site ---- 

product_type <-
  html %>%
  html_elements(".mb-1.text-secondary-blue-base") %>%
  html_text2()

product_name <- 
  html %>%
  html_elements(".heading-h5-bold.mb-4.text-neutral-black-base") %>%
  html_text2()

product_description <-
  html %>%
  html_elements(".flex-grow.px-6.pb-6.pt-5") %>%
  html_text2() 

results <- data.frame(product_type, product_name, product_description) 

formatted_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
results <- rbind(results, c("Scraped at", ":", formatted_timestamp))

write.csv(results, file = OUTPUT_FILE_PATH, row.names = FALSE)