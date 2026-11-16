# Question 2
# Installing packages

install.packages("topicmodels")
install.packages("tm")
install.packages("stopwords")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("wordcloud")
install.packages("RColorBrewer")

# LDA TOPIC MODELLING
# libraries

library(topicmodels)
library(tm)
library(stopwords)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

# 2.1 Loading the news files and create a data frame

# Check the working directory
getwd()
setwd("C:/Users/Nkosinathi Mkhwanazi/Downloads/Project 2 Big Data")
getwd()

# Extract the news files
unzip("news.zip", exdir = "news_files")

# List all text documents
news_files <- list.files(
  "news_files",
  pattern = "\\.txt$",
  full.names = TRUE,
  recursive = TRUE
)
# Check the number of documents
length(news_files)

# Reading the content of each news document

file_list <- c()
text_list <- c()

for (i in 1:length(news_files)) {
  
  file_name <- news_files[i]
  
  text <- readLines(file_name, warn = FALSE, encoding = "UTF-8")
  
  text <- paste(text, collapse = " ")
  
  file_list <- append(file_list, basename(file_name))
  text_list <- append(text_list, text)
}

# Create a data frame with file names and content
news_data <- data.frame(
  file_name = file_list,
  content = text_list,
  stringsAsFactors = FALSE
)

# Create the text preprocessing function
clean_text <- function(text) {
  
  # Convert text to lowercase
  text <- tolower(text)
  
  # Remove numbers and punctuation
  text <- gsub("[^a-z[:space:]]", " ", text)
  
  # Remove extra white spaces
  text <- gsub("[[:space:]]+", " ", text)
  
  # Remove leading and trailing spaces
  text <- trimws(text)
  
  # Split the text into words
  words <- strsplit(text, " ")[[1]]
  
  # Remove English stopwords
  words <- words[!words %in% stopwords("en")]
  
  # Combine the remaining words
  cleaned_text <- paste(words, collapse = " ")
  
  return(cleaned_text)
}

# Apply text preprocessing
news_data$content <- sapply(
  news_data$content,
  clean_text,
  USE.NAMES = FALSE
)

# View the dataset structure
str(news_data)

# Check the number of records and columns
dim(news_data)

# View the file names
news_data$file_name

# Preview the first document
substr(news_data$content[1], 1, 300)
# Check for empty documents
sum(nchar(news_data$content) == 0)

# Question 2.2: LDA Topic Modelling

# Load required libraries
library(tm)
library(topicmodels)

# Create a corpus from the cleaned news content
corpus <- Corpus(VectorSource(news_data$content))

# Create the Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# View the Document-Term Matrix
dtm

# Check the number of documents and terms
dim(dtm)

# Check the number of words in each document
rowSums(as.matrix(dtm))

# Set the number of topics
num_topics <- 5

# Set the number of top words per topic
top_k <- 10

# Apply LDA
lda_model <- LDA(
  dtm,
  k = num_topics,
  control = list(seed = 2026)
)

# View the top words in each topic
top_words <- terms(lda_model, top_k)

top_words



# Extract the topic-word probability distributions
topic_probabilities <- posterior(lda_model)$terms

# Create a data frame for the top words in each topic
topic_results <- data.frame()

for (i in 1:num_topics) {
  
  topic_df <- data.frame(
    Topic = paste("Topic", i),
    Word = top_words[, i],
    Probability = as.numeric(
      topic_probabilities[i, top_words[, i]]
    )
  )
  
# Arrange words by probability
  topic_df <- topic_df[
    order(topic_df$Probability, decreasing = TRUE),
  ]
  
# Combine the results
  topic_results <- rbind(topic_results, topic_df)
}

# View the topic-word probability table
print(topic_results, row.names = FALSE)

# Create the combined bar graph
bar_plot <- ggplot(
  topic_results,
  aes(
    x = reorder_within(Word, Probability, Topic),
    y = Probability
  )
) +
  geom_col(fill = "steelblue", width = 0.7) +
  coord_flip() +
  facet_wrap(~Topic, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(
    title = "Top 10 Words Across Five LDA Topics",
    x = "Words",
    y = "Word Probability"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    strip.text = element_text(size = 13, face = "bold"),
    panel.spacing = grid::unit(2, "lines"),
    plot.title = element_text(size = 16, hjust = 0.5)
  )

# Save the graph at a larger size
ggsave(
  "Figure_2.5_LDA_Bar_Graphs.png",
  plot = bar_plot,
  width = 14,
  height = 15,
  dpi = 300
)

# Open the saved graph in Windows
shell.exec(normalizePath("Figure_2.5_LDA_Bar_Graphs.png"))


# Question 2.3: Combined word clouds for five LDA topics

# Load the required libraries
library(wordcloud)
library(RColorBrewer)

# Create a high-resolution image
png(
  "Figure_2.6_LDA_Word_Clouds.png",
  width = 2400,
  height = 1800,
  res = 180
)

# Arrange five topics in two columns
# Topic 5 occupies the full bottom row
layout(
  matrix(c(1, 2,
           3, 4,
           5, 5),
         nrow = 3, byrow = TRUE)
)

# Reduce margins around the word clouds
par(mar = c(0.5, 0.5, 2, 0.5))

# Create word clouds for the five topics
for (i in 1:5) {
  
  # Select the words for each topic
  topic_df <- subset(
    topic_results,
    Topic == paste("Topic", i)
  )
  
  # Set seed for reproducibility
  set.seed(2026 + i)
  
  # Generate word cloud
  wordcloud(
    words = topic_df$Word,
    freq = topic_df$Probability * 1000,
    min.freq = 0,
    max.words = 10,
    scale = c(5, 1.8),
    random.order = FALSE,
    rot.per = 0,
    colors = brewer.pal(8, "Dark2")
  )
  
  # Add topic heading
  title(
    main = paste("Topic", i),
    cex.main = 1.5,
    font.main = 2
  )
}

# Save the figure
dev.off()

# Open the saved image
shell.exec(normalizePath("Figure_2.6_LDA_Word_Clouds.png"))