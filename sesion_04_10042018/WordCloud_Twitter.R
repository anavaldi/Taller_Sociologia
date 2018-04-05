####################################################
#
#        # WORD CLOUD TEXT
#
# Author: Ana Valdivia
# Date: 5th April 2018
#
################

# Libraries
install.packages("readr")
install.packages("rvest")
install.packages("beepr")
install.packages("stringr")
install.packages("RXKCD")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("data.table")
install.packages("wordcloud2") 

library(readr)
library(rvest)
library(beepr)
library(stringr)
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(data.table)
library(wordcloud2) 

# Set directory 
setwd("") # escribir la direcci�n d�nde teng�is la carpeta Taller_Sociologia

# Read data
data <- read.csv("./Taller_Sociologia/Sesion_01_06032018/lamanada.csv")

text <- data$text

# Clean text function
clean.text = function(x)
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # set Spanish symbols
  x = gsub("ã¡", "a", x)
  x = gsub("ã©", "e", x)
  x = gsub("ã³", "o", x)
  x = gsub("ãº", "u", x)
  x = gsub("ã±", "ñ", x)
  x = gsub("ã¨", "e", x)
  x = gsub("ã²", "o", x)
  x = gsub("ã", "i", x)
  return(x)
}

# Text mining
cleaned_text <- clean.text(text)


# Build Corpus
corpus <- Corpus(VectorSource(cleaned_text))
d  <- tm_map(corpus, tolower)
d  <- tm_map(d, stripWhitespace)
d <- tm_map(d, removePunctuation)
# d <- tm_map(d, removeWords,  stopwords("spanish"))
tdm <- TermDocumentMatrix(d)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
df <- data.frame(word = names(v),freq=v)
rownames(df) <- c(1:nrow(df))
# Delete some words
df <- df[-c(1, 3:5, 8:9),] # eliminamos algunas palabras


# Word Clouds
wordcloud2(df, color="purple")

letterCloud(df, word = "LA MANADA", wordSize = 1)
