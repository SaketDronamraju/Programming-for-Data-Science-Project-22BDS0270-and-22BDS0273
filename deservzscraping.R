# --- Install required packages (only first time) ---
install.packages("rvest")
install.packages("httr")

# --- Load libraries ---
library(rvest)
library(httr)

# --- Set working directory ---
setwd("C:/Users/Saket/OneDrive/Desktop/pds project")

# --- URL of the article ---
url <- "https://indianstartupnews.com/funding/wealth-management-platform-dezerv-raises-rs-350-crore-led-by-premji-invest-and-accels-global-growth-fund-10557796"

# --- Read and parse the webpage ---
page <- read_html(url)

# --- Extract article text ---
content <- page %>%
  html_elements("p") %>%
  html_text(trim = TRUE) %>%
  paste(collapse = "\n\n")

# --- Save content to text file ---
writeLines(content, "dezerv_funding_article.txt")

# --- Confirmation message ---
cat("✅ Article content saved to 'C:/Users/Saket/OneDrive/Desktop/pds project/dezerv_funding_article.txt'\n")
