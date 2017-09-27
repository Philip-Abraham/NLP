
library(readr)
text <- read_tsv("BeyondEntitiesAndRelationships.txt")
text2 <- read_tsv("SixHeresiesForBI.txt")

library(tm)

# Make a vector source: text_source
text_source <- VectorSource(text) 

# Make a volatile corpus: text_corpus
text_corpus <- VCorpus(text_source)

clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "goal", "via"))
        
        return(corpus)
}

# Apply your customized function to the text_corpus: clean_corp
clean_corp <- clean_corpus(text_corpus)


## Make a term-document matrix -  transpose of the document-term matrix
## The TDM is often the matrix used for language analysis. 

# Create a TDM from clean_corp: text_tdm
text_tdm <- TermDocumentMatrix(clean_corp)

# Print text_tdm data
text_tdm

# Convert text_tdm to a matrix: text_m
text_m <- as.matrix(text_tdm)

# Calculate the rowSums: term_frequency
term_frequency <- rowSums(text_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = TRUE)

# View the top 10 most common words
term_frequency[1:10]

# Plot a barchart of the 10 most common words
barplot(term_frequency[1:10], col = "tan", las = 2)


## Frequent terms with qdap
library(qdap)

# Create frequency
frequency <- freq_terms(text, top=4, at.least=3, stopwords="Top200Words")

# Make a frequency barchart
plot(frequency)

# Create frequency2
frequency2 <- freq_terms(text, top=4, at.least=3, tm::stopwords("english"))

# Make a frequency2 barchart
plot(frequency2)


## A simple word cloud
library(wordcloud)

# Print the first 5 entries in term_frequency
term_frequency[1:5]

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
colors()
wordcloud(word_freqs$term, word_freqs$num, max.words = 100, colors = c("grey80", "darkgoldenrod1", "cornflowerblue"))

# Call thedisplay.brewer.all() function to list all available palettes
display.brewer.all()

# Create purple_orange selecting the first 10 colors from the PuOr palette.
purple_orange <- brewer.pal(10, "PuOr")

# Drop 2 faintest colors
purple_orange <- purple_orange[-(6:7)]

# Create a wordcloud with purple_orange palette
wordcloud(word_freqs$term, word_freqs$num, max.words = 100, colors = purple_orange)


## Word networks

text3<-readLines("BeyondEntitiesAndRelationships.txt")
v1 <-as.vector(text3)

# Word association
word_associate(v1, match.string = c("model"), 
               stopwords = c(Top200Words), 
               network.plot = TRUE, cloud.colors = c("gray85", "darkred"))

# Add title
title(main = "Model Associations")


## Plot a dendogram
hc <- hclust(d= dist(text_tdm, method = "euclidean"), method = "complete")
plot(hc)

## Visualize common words in two documents

# Combine all text documents
all_texts <- c(text, text2)

# Make a vector source: text_source
text_source <- VectorSource(all_texts) 

# Make a volatile corpus: text_corpus
text_corpus <- VCorpus(text_source)

clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "goal", "via"))
        
        return(corpus)
}

# Apply your customized function to the text_corpus: clean_corp
clean_corp <- clean_corpus(text_corpus)


# Make a term-document matrix -  transpose of the document-term matrix
# The TDM is often the matrix used for language analysis. 

# Create a TDM from clean_corp: text_tdm
text_tdm <- TermDocumentMatrix(clean_corp)

# Print text_tdm data
text_tdm

# Convert text_tdm to a matrix: text_m
text_m <- as.matrix(text_tdm)

# Print a commonality cloud
commonality.cloud(text_m, max.words = 100, colors = purple_orange)


## Pyramid plots

# Create common_words
common_words <- subset(text_m, text_m[, 1] > 0 & text_m[, 2] > 0)

# Create difference
difference <- abs(common_words[, 1] - common_words[, 2])

# Combine common_words and difference
common_words <- cbind(common_words, difference)

# Order the data frame from most differences to least
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

# Create top25_df
top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))

# Create the pyramid plot
library(plotrix)
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels,
             gap = 8,
             top.labels = c("BeyondEntitiesAndRelationships", "Words", "SixHeresiesForBI"),
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)


## Visualize dissimilar words in two documents

# Give the columns distinct names
colnames(text_tdm) <- c("BeyondEntitiesAndRelationships", "SixHeresiesForBI")

# matrix form
text_m1 <- as.matrix(text_tdm)

# Create comparison cloud
comparison.cloud(text_m1, colors = c("orange", "blue"), max.words = 50)











