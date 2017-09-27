# At its heart, bag of words text mining represents a way to count terms, or 
# n-grams, across a collection of documents. 

# Manually counting words in the sentences above is a pain! Fortunately, the 
# qdap package offers a better alternative. You can easily find the top 4 
# most frequent terms (including ties) in text

library(qdap)
text <- "Text mining usually involves the process of structuring the input text. The overarching goal is, essentially, to turn text into data for analysis, via application of natural language processing (NLP) and analytical methods."
frequent_terms <- freq_terms(text, 4)

# The frequent_terms object stores all unique words and their count. 
# You can then make a bar chart simply by calling the plot function on the 
# frequent_terms object.

plot(frequent_terms)


## Common cleaning functions from tm

# The tm package offers a number of transformations that ease the tedium of cleaning data.
getTransformations()

# Create the object: text
text <- "<b>She</b> woke up at       6 A.M. It\'s so early!  She was only 10% awake and began drinking coffee in front of her computer."

# All lowercase
tolower(text)

library(tm)
# Remove punctuation
removePunctuation(text)

# Remove numbers
removeNumbers(text)

# Remove whitespace
stripWhitespace(text)

library(qdap)
bracketX(text) #Remove all text within brackets 
replace_number(text) #Replace numbers with their word equivalents
replace_contraction(text) #Convert contractions back to their base words
replace_symbol(text) # Replace common symbols with their word equivalents 
replace_abbreviation(text) #Replace abbreviations with their full text 


## stop words
library(tm)

# List standard English stop words
stopwords("en")

# Print text without standard stop words
removeWords(text, stopwords("en"))

# Add "coffee" and "bean" to the list: new_stops
new_stops <- c("coffee", "bean", stopwords("en"))

# Print out text with your customized stop words removed
removeWords(text, new_stops)


## word stemming and stem completion
library(tm)

# Create complicate
complicate <- c("complicated", "complication", "complicatedly")

# Perform word stemming: stem_doc
stem_doc <- stemDocument(complicate)

# Create the completion dictionary: comp_dict
comp_dict <- c("complicate")

# Perform stem completion: complete_text 
complete_text <- stemCompletion(stem_doc,  comp_dict)

# Print complete_text
complete_text


## Word stemming and stem completion on a sentence
library(tm)

text_data <- c("In a complicated haste, Tom rushed to fix a new complication, too complicatedly.")


# Remove punctuation: rm_punc
rm_punc <- removePunctuation(text_data)
        
# Create character vector: n_char_vec
n_char_vec <- unlist(strsplit(rm_punc, split = ' '))

# Perform word stemming: stem_doc
stem_doc <- stemDocument(n_char_vec)
        
# Print stem_doc
stem_doc        
        
# Create the completion dictionary: comp_dict
comp_dict <- c("complicate")

# Re-complete stemmed document: complete_doc
complete_doc <- stemCompletion(stem_doc, comp_dict)
        
# Print complete_doc
complete_doc 


## Apply preprocessing steps to a corpus
library(tm)

text <- "Text mining usually    involves the process of structuring    the input text. The overarching goal is, essentially, to turn text into data for analysis, via application of natural language processing (NLP) and analytical methods."


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
clean_corp[[1]][1]


## Make a document-term matrix

# Create the dtm from the corpus: text_dtm
text_dtm <- DocumentTermMatrix(clean_corp)

# Print out text_dtm data
text_dtm

# Convert text_dtm to a matrix: text_m
text_m <- as.matrix(text_dtm)

# Print the dimensions of text_my
dim(text_m)

# Review  the matrix
text_m


## Make a term-document matrix -  transpose of the document-term matrix
## The TDM is often the matrix used for language analysis. 

# Create a TDM from clean_corp: text_tdm
text_tdm <- TermDocumentMatrix(clean_corp)

# Print text_tdm data
text_tdm

# Convert text_tdm to a matrix: text_m
text_m <- as.matrix(text_tdm)

# Print the dimensions of the matrix
dim(text_m)

# Review the matrix
text_m


## Sparsity Example
myText <- c("the quick brown furry fox jumped over a second furry brown fox",
            "the sparse brown furry matrix",
            "the quick matrix")

require(tm)
myVCorpus <- VCorpus(VectorSource(myText))
myTdm <- DocumentTermMatrix(myVCorpus)

# Sparsity
as.matrix(myTdm)
as.matrix(removeSparseTerms(myTdm, .01))
as.matrix(removeSparseTerms(myTdm, .99))
as.matrix(removeSparseTerms(myTdm, .5))
# In the last example with sparse = 0.34, only terms occurring in two-thirds of 
# the documents were retained.
as.matrix(removeSparseTerms(myTdm, .34))
