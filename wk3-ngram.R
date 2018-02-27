# Packages
library(dplyr)
library(parallel)
library(stringi) 
library(ggplot2)
library(tidytext)
library(slam)
library(tm)

# Clear workspace
rm(list = ls())

# Options
dir.separator <- "/"
swift.dir <- paste("..","Data", sep = dir.separator)
swift.lang <- "en_US"
swift.path <- paste(swift.dir, dir.separator, swift.lang, dir.separator, sep = "")
sample.dir <- "samples"
sample.size <- 0.1
sample.name <- paste("samples", sample.size, swift.lang, "txt", sep = ".")
sample.path <- paste(sample.dir, dir.separator, sample.name, sep = "")
corpus.name <- paste("corpus", sample.size, swift.lang, "RData", sep = ".")
corpus.path <- paste(sample.dir, dir.separator, corpus.name, sep = "")
set.seed(2501)
options(java.parameters = "-Xmx8192m") 
options(mc.cores = detectCores())

# Create new sample data if file not found
if (!file.exists(sample.path)){
  cat("  -> Sample: Creation using ",sample.size*100,"% of the data","\n")
  
  if (!dir.exists(sample.dir)) dir.create(sample.dir)
  blogs <- iconv(readLines(paste(swift.path, swift.lang,".blogs.txt", sep = ""), 
                     encoding = "UTF-8", skipNul = TRUE), "latin1", "ASCII", sub = "")
  news <- iconv(readLines(paste(swift.path, swift.lang,".news.txt", sep = ""), 
                    encoding = "UTF-8", skipNul = TRUE),  "latin1", "ASCII", sub = "")
  twitter <- iconv(readLines(paste(swift.path, swift.lang,".twitter.txt", sep = ""), 
                       encoding = "UTF-8", skipNul = TRUE), "latin1", "ASCII", sub = "")
  words.sample <- c(sample(blogs, length(blogs) * sample.size, replace = FALSE),
                    sample(news, length(news) * sample.size, replace = FALSE),
                    sample(twitter, length(twitter) * sample.size, replace = FALSE))
  write(words.sample, file = sample.path) 
} else {
  cat("  -> Sample: Skipping, since file exists","\n")
}
  

# Corpus 
if (!file.exists(corpus.path)){
  cat("  -> Corpus: Initial generation","\n")
  
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
  
  # Start cluster
  cl <- makeCluster(detectCores())  
  tm_parLapply_engine(cl)
  
  words.corpus <- VCorpus(VectorSource(words.sample)) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, words.abusive) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(tolower) %>%
    tm_map(PlainTextDocument)
  
  # Stop cluster
  tm_parLapply_engine(NULL)
  stopCluster(cl)
  
  save(words.corpus, file=corpus.path)
}

# nGrams
if (!exists("words.corpus")) {
  cat(paste("  -> Corpus: Loading existing data", "\n"))
  load(corpus.path)
}

# Start cluster
cl <- makeCluster(detectCores())  
tm_parLapply_engine(cl)

cat(paste("  -> nGarm:  Start time:", Sys.time(), "\n"))

# Uni-gram
gram.token <- function (x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))
gram.1 <- TermDocumentMatrix(words.corpus, control = list(tokenize = gram.token)) %>%
  rollup(. , 2, na.rm = TRUE, FUN = sum) %>% tidy() %>% select(term, count) %>% 
  as.data.frame() %>% arrange(desc(count)) %>% subset(count > 1)
gram.name <- paste("gram","1",sample.size, swift.lang, "RData", sep = ".")
save(gram.1, file = paste(sample.dir, dir.separator, gram.name, sep = "")) 
cat("  -> nGarm:  ...25%"); gc()

# Bi-Gram
gram.token <- function (x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
gram.2 <- TermDocumentMatrix(words.corpus, control = list(tokenize = gram.token)) %>%
  rollup(. , 2, na.rm = TRUE, FUN = sum) %>% tidy() %>% select(term, count) %>% 
  as.data.frame() %>% arrange(desc(count)) %>% subset(count > 1)
gram.name <- paste("gram","2",sample.size, swift.lang, "RData", sep = ".")
save(gram.2, file = paste(sample.dir, dir.separator, gram.name, sep = "")) 
cat("...50%"); gc()

# Tri-Gram
gram.token <- function (x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
gram.3 <- TermDocumentMatrix(words.corpus, control = list(tokenize = gram.token)) %>%
  rollup(. , 2, na.rm = TRUE, FUN = sum) %>% tidy() %>% select(term, count) %>% 
  as.data.frame() %>% arrange(desc(count)) %>% subset(count > 1)
gram.name <- paste("gram","3",sample.size, swift.lang, "RData", sep = ".")
save(gram.3, file = paste(sample.dir, dir.separator, gram.name, sep = "")) 
cat("...75%"); gc()

# Quad-Gram
gram.token <- function (x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))
gram.4 <- TermDocumentMatrix(words.corpus, control = list(tokenize = gram.token)) %>%
  rollup(. , 2, na.rm = TRUE, FUN = sum) %>% tidy() %>% select(term, count) %>% 
  as.data.frame() %>% arrange(desc(count)) %>% subset(count > 1)
gram.name <- paste("gram","4",sample.size, swift.lang, "RData", sep = ".")
save(gram.4, file = paste(sample.dir, dir.separator, gram.name, sep = "")) 
cat("...100%","\n"); gc()

cat(paste("  -> nGarm:  End time:", Sys.time(), "\n\n"))

# Stop cluster
tm_parLapply_engine(NULL)
stopCluster(cl)
  
# n-Gram plotting helper
gram.plot <- function(gram, n = "n", show = 20) {
  gram <- head(gram, show)
  ggplot(gram, aes(x=reorder(term, -count), y = count)) + 
    geom_bar(stat="identity", aes(fill = count)) + 
    ggtitle(paste(n, "-gram", sep = "")) + ylab("Frequency") + 
    theme(axis.title.x=element_blank()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) + 
    guides(fill = FALSE)
}

gram.plot(gram.3, "3")



