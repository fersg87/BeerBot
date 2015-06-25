if(!require(tm)) install.packages("tm")
if(!require(XML)) install.packages("XML")

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

websiteToString <- function(website, xPathMatch = "//p"){
  # Read and parse HTML file
  doc.html = htmlTreeParse(website, useInternal = TRUE)
  
  # Extract all the paragraphs (HTML tag is p, starting at
  # the root of the document). Unlist flattens the list to
  # create a character vector.
  doc.text = unlist(xpathApply(doc.html, xPathMatch, xmlValue))
  
  # Replace all \n by spaces
  doc.text = gsub('\\n', ' ', doc.text)
  
  # Join all the elements of the character vector into a single
  # character string, separated by spaces
  doc.text = paste(doc.text, collapse = ' ')
  
  return(doc.text)
}

unigramsTokenizer <- function(string, minLength =4){
  # Converting text into a corpus
  trainingCorpus = VCorpus(VectorSource(string))
  
  # Corpus cleansing 
  trainingCorpus = tm_map(trainingCorpus, content_transformer(tolower))
  trainingCorpus = tm_map(trainingCorpus, removePunctuation)
  trainingCorpus = tm_map(trainingCorpus, removeNumbers)
  trainingCorpus = tm_map(trainingCorpus, removeWords, stopwords("english"))
  
  # Word extraction
  dtm <- DocumentTermMatrix(trainingCorpus)
  words <- Terms(dtm)[sapply(Terms(dtm), nchar) >= minLength]
  words <- unlist(lapply(words, simpleCap))
  
  return(words)
}

beerWords <- function(words){
  # Converting beer words into data frame
  unigramBMatches <- gregexpr("^B.+", words)
  unigramB <- unlist(regmatches(words, unigramBMatches))
  
  unigramEMatches <- gregexpr("^E.+", words)
  unigramE <- unlist(regmatches(words, unigramEMatches))
  
  unigramRMatches <- gregexpr("^R.+", words)
  unigramR <- unlist(regmatches(words, unigramRMatches))
  
  beer <- list(b=unigramB, e=unigramE, r=unigramR)
  
  return(beer)
}

generateMeeting <- function(website){
  x <- websiteToString(website)
  x <- unigramsTokenizer(x)
  beerWords <- beerWords(x)
  
  b <-  sample(beerWords$b, 1)
  e <-  sample(beerWords$e, 2)
  r <-  sample(beerWords$r, 1)
  
  meeting <- paste(b[1], e[1], e[2], r[1], sep = " ")
  
  return(meeting)
}


cat(generateMeeting('http://arstechnica.com/'))

