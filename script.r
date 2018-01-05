setwd("~/FORMATION/Coursera/Capstone/final")

path <- "~/FORMATION/Coursera/Capstone/final/en_US/"
files <- list.files(path, full.names = TRUE)


all_txt <- NULL
for (file in files){
  txt <- readLines(file, n = -1, skipNul = T)#, encoding = "UTF-8")
  all_txt <- c(all_txt, txt)
}


# # Quiz
# twitter <- scan("en_US/en_US.twitter.txt", nlines = -1, sep = "\n")
# 
# containsWord <- function(text, pattern){
#   return(pattern %in% unlist(strsplit(tolower(text), ' ')))
# }
# 
# love <- lapply(twitter, containsWord, pattern = "love")
# hate <- lapply(twitter, containsWord, pattern = "hate")
# 
# biostats <- lapply(twitter, containsWord, pattern = "biostats")
# 
# rm(list = c("love", "hate", "biostats", "twitter"))

# Exploratory
library(SnowballC)
library(tm)
library(stringr)
library(dplyr)

removeWeirdChar <- function(text){
  str_replace_all(text, "[^[:alnum:]]", " ")
}

profanities <- readLines("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt")

sample_text <- unlist(sample(all_txt, 7000), use.names = F)

sample_text <- iconv(sample_text, "latin1", "ASCII", sub="")

corpora <- VCorpus(VectorSource(sample_text))
corpora <- tm_map(corpora, content_transformer(tolower))
corpora <- tm_map(corpora, content_transformer(removeWeirdChar))

#corpora <- tm_map(corpora, removeWords, stopwords("english"))
corpora <- tm_map(corpora, removeWords, profanities)
corpora <- tm_map(corpora, removeNumbers)
corpora <- tm_map(corpora, removePunctuation)
corpora <- tm_map(corpora, stripWhitespace)

dtm <- DocumentTermMatrix(corpora)

# Word freq
freq <- colSums(as.matrix(dtm))
head(freq[order(freq, decreasing = T)],50)
findFreqTerms(dtm, lowfreq=50) 
library(ggplot2)
wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p 

# Associations
findAssocs(dtm, "abaya", corlimit = 0.1)


# N-grams
BigramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
TrigramTokenizer <-function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)


dtm_bigrams <- DocumentTermMatrix(corpora, control = list(tokenize = BigramTokenizer))
findFreqTerms(dtm_bigrams, lowfreq=10)
freqbigrams <- colSums(as.matrix(dtm_bigrams))
bf <- data.frame(word=names(freqbigrams), freq=freqbigrams)

# Exemple
bf %>%
  filter(grepl("^like ", .$word))%>%
  arrange(desc(freq))


## Trigrams
dtm_trigrams <- DocumentTermMatrix(corpora, control = list(tokenize = TrigramTokenizer))
freqtrigrams <- colSums(as.matrix(dtm_trigrams))
tf <- data.frame(word=names(freqtrigrams), freq=freqtrigrams)


# Exemple
tf %>%
  filter(grepl("^how are ", .$word))%>%
  arrange(desc(freq))


findNextWordFromBigrams <- function(text)
{
  #text <- preProcessText(text)
  
  # Use only the last word
  words <- unlist(str_split(text, " "))
  lastWord <- words[length(words)]
  print(lastWord)
  pattern_regexp <- paste0("^", lastWord, " ")
  print(pattern_regexp)
  selected <- bf %>%
    filter(grepl(pattern_regexp, .$word))%>%
    arrange(desc(freq))%>%
    top_n(5)
}
