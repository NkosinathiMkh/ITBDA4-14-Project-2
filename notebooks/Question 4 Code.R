# Question 4.1: Web Scraping and Regex Analysis

# Loading the required library
library(rvest)

# Select three webpage addresses
page_urls <- c(
  "https://quotes.toscrape.com/",
  "https://quotes.toscrape.com/page/2/",
  "https://quotes.toscrape.com/page/3/"
)

# Creating an empty data frame
quote_data <- data.frame()

# Extract quotations from each webpage
for (i in 1:length(page_urls)) {
  
  # Read the webpage
  webpage <- read_html(page_urls[i])
  
  # Identify the quotation records
  quote_items <- html_elements(webpage, "div.quote")
  
  # Extract quotation text
  quote_text <- html_text2(
    html_element(quote_items, "span.text")
  )
  
  # Extract authors
  authors <- html_text2(
    html_element(quote_items, "small.author")
  )
  
  # Extract descriptive tags
  tags <- sapply(quote_items, function(x) {
    paste(
      html_text2(html_elements(x, "a.tag")),
      collapse = ", "
    )
  })
  
  # Create a data frame for the current page
  page_data <- data.frame(
    quote_text = quote_text,
    author = authors,
    tags = tags,
    source_page = page_urls[i],
    stringsAsFactors = FALSE
  )
  
  
  quote_data <- rbind(quote_data, page_data)
  
  
  Sys.sleep(1)
}

# identification numbers
quote_data$record_id <- 1:nrow(quote_data)

# Select three webpage addresses
page_urls <- c(
  "https://quotes.toscrape.com/",
  "https://quotes.toscrape.com/page/2/",
  "https://quotes.toscrape.com/page/3/"
)

# Create an empty data frame
quote_data <- data.frame()

# Extract quotations from each webpage
for (i in 1:length(page_urls)) {
  
  # Read the webpage
  webpage <- read_html(page_urls[i])
  
  # Identify the quotation records
  quote_items <- html_elements(webpage, "div.quote")
  
  # Extract quotation text
  quote_text <- html_text2(
    html_element(quote_items, "span.text")
  )
  
  # Extract authors
  authors <- html_text2(
    html_element(quote_items, "small.author")
  )
  
  # Extract descriptive tags
  tags <- sapply(quote_items, function(x) {
    paste(
      html_text2(html_elements(x, "a.tag")),
      collapse = ", "
    )
  })
  
  # Create a data frame for the current page
  page_data <- data.frame(
    quote_text = quote_text,
    author = authors,
    tags = tags,
    source_page = page_urls[i],
    stringsAsFactors = FALSE
  )
  
  
  quote_data <- rbind(quote_data, page_data)
  
  
  Sys.sleep(1)
}

# Add record identification numbers
quote_data$record_id <- 1:nrow(quote_data)

# Apply regular expressions to the quotation text

# Remove punctuation and numbers for word counting
quote_words <- gsub(
  "[^[:alpha:][:space:]]",
  " ",
  quote_data$quote_text
)

# Remove unnecessary white spaces
quote_words <- gsub(
  "[[:space:]]+",
  " ",
  quote_words
)

quote_words <- trimws(quote_words)

# the number of words in each quotation
quote_data$word_count <- sapply(
  strsplit(quote_words, " "),
  length
)

# Arranging the columns
quote_data <- quote_data[, c(
  "record_id",
  "quote_text",
  "author",
  "tags",
  "word_count",
  "source_page"
)]

# View the extracted dataset
View(quote_data)

# Check the structure
str(quote_data)

# Check the number of observations and variables
dim(quote_data)

# Preview the first six records
head(quote_data)

# Checking the numerical attribute
summary(quote_data$word_count)

# Identifying records without tags
sum(quote_data$tags == "")


#Question 4.2: Data Cleaning and Transformation

# Create acopy of the original dataset
quote_data_clean <- quote_data

# Remove unnecessary whitespace from quotations
quote_data_clean$quote_text <- trimws(
  gsub("[[:space:]]+", " ", quote_data_clean$quote_text)
)

# Clean author names
quote_data_clean$author <- trimws(
  gsub("[[:space:]]+", " ", quote_data_clean$author)
)

# Clean tags
quote_data_clean$tags <- trimws(
  gsub("[[:space:]]+", " ", quote_data_clean$tags)
)

# Standardise spaces around commas in tags
quote_data_clean$tags <- gsub(
  "[[:space:]]*,[[:space:]]*",
  ", ",
  quote_data_clean$tags
)

# Replace missing or empty tags
quote_data_clean$tags[
  is.na(quote_data_clean$tags) |
    quote_data_clean$tags == ""
] <- "Not specified"

# Replace missing author names where necessary
quote_data_clean$author[
  is.na(quote_data_clean$author) |
    quote_data_clean$author == ""
] <- "Unknown"

# Remove records without quotation text
quote_data_clean <- quote_data_clean[
  !is.na(quote_data_clean$quote_text) &
    quote_data_clean$quote_text != "", 
]

# Remove duplicate quotations from the same author
quote_data_clean <- quote_data_clean[
  !duplicated(
    quote_data_clean[, c("quote_text", "author")]
  ),
]

# Reset row numbers
rownames(quote_data_clean) <- NULL

# View the cleaned dataset
View(quote_data_clean)

# Check the dimensions
dim(quote_data_clean)

# Check for missing values
colSums(is.na(quote_data_clean))

# Check for empty tags
sum(quote_data_clean$tags == "")

# Check the imputed tags
sum(quote_data_clean$tags == "Not specified")

# Check for duplicate records
sum(duplicated(
  quote_data_clean[, c("quote_text", "author")]
))

# View numerical summary
summary(quote_data_clean$word_count)

# Export the cleaned dataset as a CSV file

write.csv(
  quote_data_clean,
  "quotes_cleaned.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Confirm that the CSV file was created
file.exists("quotes_cleaned.csv")

# View the location of the saved file
getwd()
