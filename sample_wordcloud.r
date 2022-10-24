################ 
# https://georeferenced.wordpress.com/2013/01/15/rwordcloud/ 
# Don Fernandes 
# CBC Technologies 
# Created: 20180505 
# Modified: 20221023
# Create wordcloud from csv file 
# Run from same directory as csv file
##############

## Install Packages ##

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tm", "SnowballC", "wordcloud", "RColorBrewer")

## Load Require Library ##
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

## Read the Data ##
RT_Tickets <- read.csv(file.choose()) # Outlook csv export from RT mailbox
RT_Tickets <- data.frame(RT_Tickets)


## Calculate Corpus -- Options are $Subject, $Body, $From ##
RT_Tickets.Corpus <- Corpus(VectorSource(RT_Tickets$Subject))

## Data Cleaning and Wrangling ##
RT_Tickets.Clean <- tm_map(RT_Tickets.Corpus, PlainTextDocument)
RT_Tickets.Clean <- tm_map(RT_Tickets.Corpus, content_transformer(tolower))
RT_Tickets.Clean <- tm_map(RT_Tickets.Clean,removeNumbers)
RT_Tickets.Clean <- tm_map(RT_Tickets.Clean,removePunctuation)
RT_Tickets.Clean <- tm_map(RT_Tickets.Clean,removeWords,stopwords("english"))
RT_Tickets.Clean <- tm_map(RT_Tickets.Clean,removeWords, c("helpdesk", "completed", "completion", "success", "add", "need", "question","com","comment","service", "queue", "desk", "please", "question"))
RT_Tickets.Clean <- tm_map(RT_Tickets.Clean,stripWhitespace)
RT_Tickets.Clean <- tm_map(RT_Tickets.Clean,stemDocument)
 
## Build the wordcloud ##               
wordcloud(words = RT_Tickets.Clean, min.freq = 5, max.words=30, random.order=FALSE, rot.per=0.1, colors=brewer.pal(8, "Dark2"))

