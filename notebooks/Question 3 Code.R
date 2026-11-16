# Installing package for web scraping
install.packages("rvest")

# Question 3.1: Web Text Extraction

# Load required library
library(rvest)

# Selecting the website we are using
website_url <- "https://en.wikipedia.org/wiki/Natural_language_processing"
# Reading the webpage
webpage <- read_html(website_url)

# Extracting the text paragraphs from the article
paragraphs <- webpage %>%
  html_elements("div.mw-parser-output p") %>%
  html_text2()

# Remove citation numbers such as [1] and [2]
paragraphs <- gsub("\\[[0-9]+\\]", "", paragraphs)
# Remove unnecessary white spaces
paragraphs <- gsub("[[:space:]]+", " ", paragraphs)

# Remove leading and trailing spaces
paragraphs <- trimws(paragraphs)

# Remove empty or very short paragraphs
paragraphs <- paragraphs[nchar(paragraphs) > 40]
# Check the number of extracted paragraphs
length(paragraphs)
# Preview the first paragraph
substr(paragraphs[1], 1, 300)


# Creating a data frame from the extracted web content
web_data <- data.frame(
  paragraph_number = 1:length(paragraphs),
  content = paragraphs,
  stringsAsFactors = FALSE
)

web_data$source_url <- website_url
# View the dataset
View(web_data)
# Check the structure of the dataset
str(web_data)
# Check the number of records and columns
dim(web_data)
# Preview the first three records
head(web_data, 3)
# Check for empty records
sum(nchar(web_data$content) == 0)

# Question 3.2: Text Preprocessing and Denoising
# Load the required library
library(stopwords)
# Load the required library
library(stopwords)

# Create a function for cleaning the web text
clean_web_text <- function(text) {
  
  # Convert text to lowercase
  text <- tolower(text)
  
  # Remove website links
  text <- gsub("https?://[^[:space:]]+", " ", text)
  
  # Remove numbers and punctuation
  text <- gsub("[^a-z[:space:]]", " ", text)
  
  # Remove extra white spaces
  text <- gsub("[[:space:]]+", " ", text)
  
  # Remove leading and trailing spaces
  text <- trimws(text)
  
  # Split text into individual words
  words <- strsplit(text, " ")[[1]]
  
  # Remove English stopwords
  words <- words[!words %in% stopwords("en")]
  
  # Combine the remaining words
  cleaned_text <- paste(words, collapse = " ")
  
  return(cleaned_text)
}

# Apply text preprocessing

web_data$clean_content <- sapply(
  web_data$content,
  clean_web_text,
  USE.NAMES = FALSE
)

# View the updated data frame
View(web_data)

# Check dataset structure
str(web_data)

# Check the number of records and columns
dim(web_data)

# Verify the cleaned web dataset

sum(nchar(web_data$clean_content) == 0)

colSums(is.na(web_data))


# Question 3.3: Text Summarisation

# 3.3a Frequency-Based Text Summarisation

# Combine the original paragraphs into one text
web_text <- paste(web_data$content, collapse = " ")

# Split the text into sentences
sentences <- unlist(
  strsplit(web_text, "(?<=[.!?])\\s+", perl = TRUE)
)

# Remove unnecessary spaces
sentences <- trimws(sentences)

# Remove very short sentences
sentences <- sentences[nchar(sentences) >= 40]

# Check the number of sentences
length(sentences)

# Preview the first five sentences
head(sentences, 5)

# Clean the sentences for word frequency analysis
clean_sentences <- sapply(
  sentences,
  clean_web_text,
  USE.NAMES = FALSE
)

# Combine the cleaned sentences
all_text <- paste(clean_sentences, collapse = " ")

# Split the cleaned text into individual words
all_words <- unlist(strsplit(all_text, "\\s+"))

# Remove empty words
all_words <- all_words[nchar(all_words) > 0]

# Calculate word frequencies
word_freq <- table(all_words)

# Arrange words from highest to lowest frequency
word_freq <- sort(word_freq, decreasing = TRUE)

# View the top 10 most frequent words
head(word_freq, 10)



# Calculate scores for each sentence

# Create an empty vector for sentence scores
sentence_scores <- numeric(length(sentences))

# Calculate sentence scores
for (i in 1:length(sentences)) {
  
  # Extract words from the cleaned sentence
  words <- unlist(strsplit(clean_sentences[i], "\\s+"))
  
  # Remove empty words
  words <- words[nchar(words) > 0]
  
  # Calculate the average word frequency
  if (length(words) > 0) {
    
    sentence_scores[i] <- sum(
      word_freq[words],
      na.rm = TRUE
    ) / length(words)
    
  }
}

# Create a data frame containing sentence scores
sentence_results <- data.frame(
  Sentence_ID = 1:length(sentences),
  Score = round(sentence_scores, 4),
  Sentence = sentences,
  stringsAsFactors = FALSE
)

# View the results
head(sentence_results)

# View the highest-scoring sentences
head(
  sentence_results[order(-sentence_results$Score), ],
  10
)




# Select the five highest-scoring sentences

top_n <- 5

# Identify the highest-scoring sentences
top_sentences <- head(
  order(sentence_scores, decreasing = TRUE),
  top_n
)

# Arrange the selected sentences in their original order
top_sentences <- sort(top_sentences)

# Generate the summary
frequency_summary <- paste(
  sentences[top_sentences],
  collapse = " "
)

# Display the frequency-based summary
cat("FREQUENCY-BASED TEXT SUMMARY\n\n")

cat(frequency_summary)

# Check the number of selected sentences
length(top_sentences)




# Install the package once
install.packages("textrank")

# Load the required library
library(textrank)

# 3.3b TextRank Summarisation

# Create a data frame of original sentences
sentence_data <- data.frame(
  textrank_id = 1:length(sentences),
  sentence = sentences,
  stringsAsFactors = FALSE
)

# Extract meaningful words from each cleaned sentence
term_list <- lapply(clean_sentences, function(text) {
  
  words <- unlist(strsplit(text, "\\s+"))
  
  # Remove single letters and repeated terms
  words <- unique(words[nchar(words) > 1])
  
  return(words)
})

# Create the terminology data frame
terminology <- data.frame(
  textrank_id = rep(
    sentence_data$textrank_id,
    times = lengths(term_list)
  ),
  word = unlist(term_list),
  stringsAsFactors = FALSE
)

# View the datasets
head(sentence_data)
head(terminology)


# Apply TextRank to rank the sentences
textrank_model <- textrank_sentences(
  data = sentence_data,
  terminology = terminology
)

# View the model results
textrank_model

# View the ten highest sentence ranking scores
head(
  sort(textrank_model$pagerank$vector, decreasing = TRUE),
  10
)

# Extract the five highest-ranked sentences
textrank_summary <- summary(
  textrank_model,
  n = 5,
  keep.sentence.order = TRUE
)

# Display the TextRank summary
cat("TEXTRANK TEXT SUMMARY\n\n")

cat(textrank_summary, sep = "\n")

# Check the number of selected sentences
length(textrank_summary)


# Question 3.3: Frequency-Based Summarisation Visualisation

library(ggplot2)

# Arrange sentence scores from highest to lowest
freq_scores <- sort(sentence_scores, decreasing = TRUE)

# Select the top 10 scores
freq_plot_data <- data.frame(
  Sentence_Rank = paste("Rank", 1:10),
  Score = head(freq_scores, 10)
)

# Plot the frequency-based scores
freq_plot <- ggplot(
  freq_plot_data,
  aes(x = reorder(Sentence_Rank, Score), y = Score)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Frequency-Based Sentence Scores",
    x = "Sentence Ranking",
    y = "Average Word Frequency Score"
  ) +
  theme_minimal()

# Display the graph
print(freq_plot)


# Question 3.3: TextRank Summarisation Visualisation

# Extract TextRank scores
textrank_scores <- textrank_model$pagerank$vector

# Arrange scores from highest to lowest
textrank_scores <- sort(
  textrank_scores,
  decreasing = TRUE
)

# Select the top 10 scores
textrank_plot_data <- data.frame(
  Sentence_Rank = paste("Rank", 1:10),
  Score = as.numeric(head(textrank_scores, 10))
)

# Plot the TextRank scores
textrank_plot <- ggplot(
  textrank_plot_data,
  aes(x = reorder(Sentence_Rank, Score), y = Score)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 TextRank Sentence Scores",
    x = "Sentence Ranking",
    y = "TextRank Score"
  ) +
  theme_minimal()

# Display the graph
print(textrank_plot)