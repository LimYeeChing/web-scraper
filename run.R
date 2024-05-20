library(rvest)
# Before running the code, run the line of code below and clear the captcha for Zurich's website first
# read_html_live("https://www.zurich.com.my/en/insurance-products/protection")$view()

start_time <- Sys.time()

FILE_PATH_OUTPUT <- "results/COMPILATION.csv"

COMPANY_NAMES <- c("AIA" = "aia", 
                   "Allianz" = "allianz",
                   "AmMetLife" = "ammetlife",
                   "Etiqa" = "etiqa",
                   "FWD" = "fwd",
                   "Great Eastern" = "ge",
                   "Generali" = "generali",
                   "Hong Leong Assurance" = "hla",
                   "Manulife" = "manulife",
                   "MCIS" = "mcis",
                   "Prudential" = "prudential",
                   "Sun Life" = "sunlife",
                   "Tokio Marine" = "tokiomarine",
                   "Zurich" = "zurich")

code_file_names <- paste0(COMPANY_NAMES, ".R")

# Load all results in separate CSVs in the "Results" folder
# so they can be checked/retrieved individually 

for (file_name in code_file_names) {source(file_name)}

# Compile all results into one file ----

results_file_names <- paste0("results/", COMPANY_NAMES, ".csv")

results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(results) <- c("company", "product_type", "product_name", "product_description")

for (i in 1:length(COMPANY_NAMES)){

  company_results <- cbind(company = names(COMPANY_NAMES[i]), read.csv(results_file_names[i]))
  company_results <- company_results[-nrow(company_results), ] # Remove timestamps
  results <- rbind(results, company_results)

}

write.csv(results, file = FILE_PATH_OUTPUT, row.names = FALSE)

end_time <- Sys.time()

# Calculate runtime
runtime <- end_time - start_time
print(paste("Scraping completed. Total Runtime:", runtime, "minutes"))