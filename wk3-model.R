# Suggest a list of words baed on input sequence
model.suggestion <- function(seq, n = 3) {
  suggestion <- c()
  gram.names <- c("gram.1","gram.2","gram.3","gram.4","gram.5")
  gram.n <- length(gram.names)
  word.count <- length(strsplit(seq, " ")[[1]])
  for (i in gram.n) {
    if (word.count < (gram.n - i)) next
    if (gram.names[i] == gram.names[gram.n]) {
      
    }
  }    
}


    

