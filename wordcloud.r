# On commence par se placer dans le répertoire du script
workdir <- getSrcDirectory(function(x) {x})
setwd(workdir)
#install.packages('RSQLite')
#install.packages("wordcloud")

library(DBI)
library(tm)
library(wordcloud)
mydb <- dbConnect(RSQLite::SQLite(), "slackingzone.db")


removeHtml <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

removeApostrophe <- function(htmlString) {
  return(gsub("'", " ", htmlString))
}


messages <- dbGetQuery(mydb, 'SELECT * from forum')
bloc1 <- paste(messages$Message[0:100], collapse=" ")
bloc2 <- paste(messages$Message[100:200], collapse=" ")
bloc3 <- paste(messages$Message[200:300], collapse=" ")
docs <- Corpus(VectorSource(c(bloc1, bloc2, bloc3)))

docs <- tm_map(docs, removeHtml)
docs <- tm_map(docs, removeApostrophe)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, stopwords("fr"))
plain_docs <- tm_map(docs, PlainTextDocument) 
#all_text <- paste(docs$content, collapse=" ")
#corpus_clean <- Corpus(VectorSource(docs))

tdm = TermDocumentMatrix(plain_docs)
tdm = as.matrix(tdm)
colnames(tdm) <- rep("Forum", length(docs$content))
comparison.cloud(tdm,
                 scale = c(3,.5), random.order = FALSE,
				 max.words=250,
                 title.size = 1.5)

