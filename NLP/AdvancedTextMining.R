city <- c("Cleveland", "Portland", "Boston", "New Orleans")
rainfall <- c(39.14, 39.14, 43.77, 62.45)

rain <- data.frame(city, rainfall)

## Distance matrix and dendrogram

# Create dist_rain
dist_rain <- dist(rain[, 2])

# View the distance matrix
dist_rain

# Reclassify distances as hierarchical cluster object
hc <- hclust(dist_rain)

# Plot dendrogram with city labels
plot(hc, labels = rain$city)


## Make a distance matrix and dendrogram from a TDM

library(readr)
text <- read_tsv("bigdata.txt")

library(tm)
# Make a vector source: text_source
text_source <- VectorSource(text) 

# Make a volatile corpus: text_corpus
text_corpus <- VCorpus(text_source)

clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
        
        return(corpus)
}

# Apply your customized function to the text_corpus: clean_corp
clean_corp <- clean_corpus(text_corpus)


## Make a term-document matrix -  transpose of the document-term matrix
## The TDM is often the matrix used for language analysis. 

# Create a TDM from clean_corp: text_tdm
text_tdm <- TermDocumentMatrix(clean_corp)

# Print the dimensions of tweets_tdm
dim(text_tdm)

library(tm)

# Create tdm1
tdm1 <- removeSparseTerms(text_tdm, sparse = 0.95)

# Create tdm2
tdm2 <- removeSparseTerms(text_tdm, sparse = 0.975)

# Print tdm1 to the console to see how many terms are left
tdm1

# Print tdm2 to the console to see how many terms are left
tdm2

# Create tdm2 _m which is a matrix form.
tdm2_m <- as.matrix(tdm2)

# Create tdm2_df by converting tdm2_m to a data frame
tdm2_df <- as.data.frame(tdm2_m)

# Create tdm2_dist containing the distances of tdm2_df
tdm2_dist <- dist(tdm2_df)

# Create a hierarchical cluster object called hc
hc <- hclust(tdm2_dist)

# Plot the dendrogram
plot(hc)


## Dendrogram aesthetics

# Load dendextend
library(dendextend)

# Create hc
hc <- hclust(tdm2_dist)

# Create hcd
hcd <- as.dendrogram(hc)

# Print the labels in hcd
labels(hcd)

# Change the branch color to red for "data" and "big"
hcd <- branches_attr_by_labels(hcd, c("data", "big"), "red")

# Plot hcd
plot(hcd, main = "Better Dendrogram")

# Add cluster rectangles 
rect.dendrogram(hcd, k = 2, border = "grey50")


## Using word association

# Create associations
# Another way to think about word relationships is with the findAssocs() function 
# in the tm package. For any given word, findAssocs() calculates its correlation 
# with every other word in a TDM or DTM. Scores range from 0 to 1. A score of 1 means 
# that two words always appear together, while a score of 0 means that they never 
# appear together.

# So given a DocumentTermMatrix dtm containing terms "word1" and "word2" such that 
# findAssocs(dtm, "word1", 0.2) returns "word2" with a value of x, the correlation 
# of the term vectors for "word1" and "word2" is x.

library(tm)
data <-  c("", "word1", "word1 word2","word1 word2 word3","word1 word2 word3 word4","word1 word2 word3 word4 word5") 
corp <- VCorpus(VectorSource(data))
dtm <- DocumentTermMatrix(corp)
as.matrix(dtm)

associations <- findAssocs(dtm, "word1", 0.2) 
associations

# Create associations_df
library(qdap)
associations_df <- as.data.frame(list_vect2df(associations)[, 2:3])

# Plot the associations_df values
library(ggplot2)
ggplot(associations_df, aes(y = associations_df[, 1])) + 
        geom_point(aes(x = associations_df[, 2]), 
                   data = associations_df, size = 3)


## TfIdf weighting -term frequency-inverse document frequency
# The TfIdf score increases by term occurrence but is penalized by the frequency 
# of appearance among all documents.
dtm_TfIdf <- DocumentTermMatrix(corp, control = list(weighting = weightTfIdf))
as.matrix(dtm_TfIdf)

# Compare above with below no weighting
dtm <- DocumentTermMatrix(corp)
as.matrix(dtm)


## Changing n-grams
# So far, we have only made TDMs and DTMs using single words. The default is to 
# make them with unigrams, but you can also focus on tokens containing two or more 
# words. This can help extract useful phrases which lead to some additional insights 
# or provide improved predictive attributes for a machine learning algorithm.

library(RWeka)
# The function below uses the RWeka package to create bigram (two word) tokens: 
# min and max are both set to 2
tokenizer <- function(x) 
        NGramTokenizer(x, Weka_control(min = 2, max = 2))

library(readr)
text <- read_tsv("BeyondEntitiesAndRelationships.txt")

#inspect a particular document
writeLines(as.character(text))

library(tm)
# Make a vector source: text_source
text_source <- VectorSource(text) 

# Make a volatile corpus: text_corpus
text_corpus <- VCorpus(text_source)

clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
        
        return(corpus)
}

# Apply your customized function to the text_corpus: clean_corp
clean_corp <- clean_corpus(text_corpus)

#inspect a particular document
writeLines(as.character(clean_corp))

bigram_dtm <- DocumentTermMatrix(
        clean_corp, 
        control = list(tokenize = tokenizer)
)

# Create bigram_dtm_m by converting bigram_dtm to a matrix.
bigram_dtm_m  <- as.matrix(bigram_dtm)

# Create an object freq consisting of the word frequencies by applying colSums() on bigram_dtm_m.
freq <- colSums(bigram_dtm_m)

# Extract the character vector of word combinations with names(freq) and assign the result to bi_words.
bi_words <- names(freq)

# Plot a simple wordcloud() passing bi_words, freq and max.words = 15 into the function.
library(wordcloud)
par(bg="black")
wordcloud(bi_words, freq, max.words = 15, col=terrain.colors(length(bi_words) , alpha=0.9) , rot.per=0.3 )
