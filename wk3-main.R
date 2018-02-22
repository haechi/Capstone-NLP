# Packages
library(dplyr)
library(parallel)
library(stringi) 
library(ggplot2)
library(tm)

# Clear workspace
rm(list = ls())

# Options
dir.separator <- "/"
swift.dir <- paste("..","Data", sep = dir.separator)
swift.lang <- "en_US"
swift.path <- paste(swift.dir, dir.separator, swift.lang, dir.separator, sep = "")
sample.dir <- "samples"
sample.size <- 0.01
sample.name <- paste("samples", sample.size, swift.lang, "txt", sep = ".")
sample.path <- paste(sample.dir, dir.separator, sample.name, sep = "")
corpus.name <- paste("corpus", sample.size, swift.lang, "RData", sep = ".")
corpus.path <- paste(sample.dir, dir.separator, corpus.name, sep = "")
set.seed(2501)
options(java.parameters = "-Xmx8192m" ) 
options(mc.cores = detectCores())
grams.n <- 20

# Create new sample data if file not found
if (!file.exists(sample.path)){
  cat("  -> Creating new sample","\n")
  
  if (!dir.exists(sample.dir)) dir.create(sample.dir)
  blogs <- readLines(paste(swift.path, swift.lang,".blogs.txt", sep = ""), 
                     encoding = "UTF-8", skipNul = TRUE)
  news <- readLines(paste(swift.path, swift.lang,".news.txt", sep = ""), 
                    encoding = "UTF-8", skipNul = TRUE)
  twitter <- readLines(paste(swift.path, swift.lang,".twitter.txt", sep = ""), 
                       encoding = "UTF-8", skipNul = TRUE)
  words.sample <- c(sample(blogs, length(blogs) * sample.size, replace=FALSE),
                    sample(news, length(news) * sample.size, replace=FALSE),
                    sample(twitter, length(twitter) * sample.size, replace=FALSE))
  write(words.sample, file = sample.path) 
}

# Corpus 
if (!file.exists(corpus.path)){
  cat("  -> Corpus generation","\n")
  
  # Abusive word list
  if (!file.exists("abusive-words.txt")){
    download.file("https://www.freewebheaders.com/wordpress/wp-content/uploads/full-list-of-bad-words-banned-by-google-txt-file.zip", "abusive-words.zip")
    unzip("abusive-words.zip")
    file.rename("full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt","abusive-words.txt", showWarnings = FALSE)
    file.remove("abusive-words.zip")
  }
  words.abusive <- readLines("abusive-words.txt", encoding="latin1", warn=FALSE, skipNul=TRUE)
  
  # Corpus creation
  if (!exists("words.sample")) words.sample <- readLines(sample.path)
  words.clean <- VCorpus(VectorSource(words.sample)) %>% 
    tm_map(iconv, "latin1", "ASCII", sub="") %>% 
    tm_map(function(x) gsub("[[:punct:]]", " ", x)) %>% 
    tm_map(function(x) gsub("^\\s+", "", x)) %>% # Tailing whitespace 
    tm_map(function(x) gsub("\\s+$", "", x)) %>% # Leading whitespace
    tm_map(removeNumbers) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, words.abusive) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(tolower) %>%
    tm_map(PlainTextDocument)
  save(words.clean, file=corpus.path)
}

# nGrams
if (!exists("words.clean")) words.clean <- load(corpus.path)

