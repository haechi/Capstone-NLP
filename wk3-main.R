# Packages
library(dplyr)
library(parallel)
library(stringi) 
library(ggplot2)
library(tm)

# Options
dir.separator <- "/"
swift.dir <- paste("..","Data", sep = dir.separator)
swift.lang <- "en_US"
swift.path <- paste(swift.dir, dir.separator, swift.lang, dir.separator, sep = "")
set.seed(2501)
options(java.parameters = "-Xmx8192m" ) 
options(mc.cores = detectCores())
grams.n <- 20

# Data Samples