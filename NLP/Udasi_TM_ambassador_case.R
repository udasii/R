#' Title: A4_Assignment
#' Purpose: NLP Analysis of Bios and Interests of Hult Global Ambassadors 
#' Author: Suraj Udasi
#' Date: Feb 01, 2024
#'

# Set the working directory
setwd("~/Visualizing_Analyzing_Data_with_R/personalFiles/A3_NLP")

# Libs
library(tm)  # For text mining
library(wordcloud)  # For creating word clouds
library(qdapRegex)  # For regular expressions and cleaning
library(RColorBrewer)  # For color palettes
library(ggplot2)  # For data visualization
library(ggthemes)  # For additional ggplot themes
library(tidytext)  # For tidy text analysis
library(dplyr)  # For data manipulation
library(radarchart)  # For creating radar charts
library(textdata)  # For accessing text datasets
library(tidyr)  # For tidying data
library(topicmodels)  # For topic modeling


# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}


# Create custom stop words
customStopwords <- c(stopwords('english'), 'hult', 'university', 'business', 
                     'school', 'student', 'masters', 'international', 'hi', 'name',
                     'everyone', 'hello')


# Review all Pallettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(9, "GnBu")
pal <- pal[-(1:2)]

# Load Data
studentBios <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing_Analyzing_Data_with_R/main/Cases/A3_NLP/Student%20Ambassador%20Bios/final_student_data.csv')

# Rename the columns as required
names(studentBios)[names(studentBios) == "cid"] <- "doc_id"
names(studentBios)[names(studentBios) == "bio"] <- "text"
str(studentBios)

unique(studentBios$campus)
unique(studentBios$programTitle)
unique(studentBios$namSorGender.likelyGender)

campusSummary <- studentBios %>% 
  group_by(campus) %>% 
  summarise(count = n())

programSummary <- studentBios %>% 
  group_by(programTitle) %>% 
  summarise(count = n())

genderSummary <- studentBios %>% 
  group_by(namSorGender.likelyGender) %>% 
  summarise(count = n())

datasetSummary <- studentBios %>% 
  group_by(campus, programTitle, namSorGender.likelyGender) %>% 
  summarise(count = n())

ggplot(datasetSummary, aes(x = campus, y = count, fill = namSorGender.likelyGender)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Students by Campus, Program Title, and Gender",
       fill = "Gender") +
  ylab(NULL) +  
  xlab(NULL) +
  facet_wrap(~ programTitle, scales = "free_x", ncol = 5) +  # Facet by program title
  scale_fill_manual(values = c("male" = '#141F52', "female" = "#E2365B")) +  # Contrast colors
  theme_minimal() +
  theme(
    panel.background = element_blank(),  # Remove background
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add a box line
    plot.margin = margin(20, 30, 20, 20)) +
  theme(legend.position = "top")  # Place the legend at the top


# Subset Data by Campus
bostonCampus    <- studentBios %>% filter(campus == 'Boston') %>% select(interests, text)
sanfranCampus   <- studentBios %>% filter(campus == 'San Francisco') %>% select(interests, text)
dubaiCampus     <- studentBios %>% filter(campus == 'Dubai') %>% select(interests, text)
londonCampus    <- studentBios %>% filter(campus == 'London') %>% select(interests, text)

# Subset Data by Programs enrolled
mba      <- subset(studentBios, programTitle %in% c('Executive MBA', 
                                                    'Virtual Part-Time MBA', 
                                                    'Master of Business Administration')) %>% select(interests, text)
mban     <- studentBios %>% filter(programTitle == 'Master of Business Analytics') %>% select(interests, text)
mim      <- studentBios %>% filter(programTitle == 'Master of International Marketing') %>% select(interests, text)
mib      <- studentBios %>% filter(programTitle == 'Master of International Business') %>% select(interests, text)
mfin     <- studentBios %>% filter(programTitle == 'Master of Finance') %>% select(interests, text)
mei      <- studentBios %>% filter(programTitle == 'Master of Disruptive Innovation') %>% select(interests, text)
bba      <- studentBios %>% filter(programTitle == 'Bachelor of Business Administration') %>% select(interests, text)

# Subset Data by Gender
male      <- studentBios %>% filter(namSorGender.likelyGender == 'male') %>% select(interests, text)
female    <- studentBios %>% filter(namSorGender.likelyGender == 'female') %>% select(interests, text)


# Read in multiple files as individuals
campusFiles<-c(bostonCampus, sanfranCampus, dubaiCampus, londonCampus) 
campusTopics <- c('boston','sanfran', 'dubai', 'london')

programFiles<-c(mba, mban, mim, mib, mfin, mei, bba) 
programTopics <- c('MBA','Analytics', 'Marketing', 'Business', 'Finance', 'Innovation', 'BBA')

genderFiles<-c(male, female) 
genderTopics <- c('Male','Female')

# Read in as a list
campusData <- list(boston=bostonCampus, sanfran=sanfranCampus, dubai=dubaiCampus, london=londonCampus)
programData <- list(MBA=mba, Analytics = mban, Marketing = mim, Business = mib, Finance = mfin, Innovation = mei, BBA = bba)
genderData <- list(Male = male, Female = female )

# Now, create a volatile corpus from the adapted data frame
studentCorpus <- VCorpus(DataframeSource(studentBios))

# Preprocess the corpus
studentCorpus <- cleanCorpus(studentCorpus, customStopwords)

test_df <- data.frame(originalText = studentBios$text,
                   cleanText    = unlist(sapply(studentCorpus, `[`, "content")),
                   stringsAsFactors=F)
#write.csv(df,'plain_coffee.csv',row.names = F)

# Compare a single tweet
test_df[1,]

# document term matrix (DTM)
studentDTM <- DocumentTermMatrix(studentCorpus)
studentDTMm <- as.matrix(studentDTM)

# Examine the dimensions
dim(studentDTMm)

#word freq matrix
studentFreq <- colSums(studentDTMm)
studentFreq <- data.frame(word=names(studentFreq), #arrange to get word freq matrix
                          frequency=studentFreq, 
                          row.names = NULL)
head(studentFreq)

# Simple barplot; values greater than 30 
topWords      <- subset(studentFreq, studentFreq$frequency >= 30) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

topwords_plot <- ggplot(topWords, aes(x= reorder(word, frequency), y=frequency)) + 
  geom_bar(stat="identity", fill='black') + 
  coord_flip()+
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0) +
  theme_minimal() +  
  theme(
    panel.background = element_blank(),  # Remove background
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add a box line
    plot.margin = margin(20, 30, 20, 20)) +
  ylab("Frequency") +  
  xlab(NULL) +
  ggtitle("Top Words")
topwords_plot

# Wordcloud for studentBios
# Set up a custom theme for the word cloud
wordcloud(studentFreq$word,
          studentFreq$frequency,
          max.words = 80,
          random.order = FALSE,
          colors = pal)

# Inspect word associations
associations_experience <- findAssocs(studentDTM, 'experience', 0.35)
associations_experience

associations_club <- findAssocs(studentDTM, 'club', 0.35)
associations_club

associations_campus <- findAssocs(studentDTM, 'campus', 0.29)
associations_campus
# Assuming $campus is your named numeric vector
hultCampus <- c("boston", "dubai", "london")

# Filter the vector
associations_campus_filt <- associations_campus$campus[hultCampus]
associations_campus_filt

# Organize the word associations
assoex_df <- data.frame(terms = names(associations_experience[[1]]),
                        value = unlist(associations_experience),
                        row.names = NULL)
assoex_df$terms <- factor(assoex_df$terms, levels=assoex_df$terms)
assoex_df

assoclubs_df <- data.frame(terms = names(associations_club[[1]]),
                        value = unlist(associations_club),
                        row.names = NULL)
assoclubs_df$terms <- factor(studentclubs_df$terms, levels=studentclubs_df$terms)
assoclubs_df

assocampus_df <- data.frame(terms = names(associations_campus[[1]]),
                              value = unlist(associations_campus),
                              row.names = NULL)
assocampus_df$terms <- factor(studentcampus_df$terms, levels=studentcampus_df$terms)
assocampus_df

# Make a dot plot for Experience
ggplot(assoex_df, aes(y = reorder(terms, value))) +
  geom_point(aes(x = value), size = 4, color = '#1DC9A4', alpha = 0.7) +
  geom_text(aes(x = value, label = value), color = "black", hjust = "inward", vjust = "inward", size = 4) +
  theme_minimal() +  
  theme(
    panel.background = element_blank(),  # Remove background
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add a box line
    plot.margin = margin(20, 30, 20, 20)
  ) +
  ylab(NULL) +  # Remove y-axis label
  xlab("Value") +
  ggtitle("Words Associated with 'Experience'")

# Make a dot plot for clubs
ggplot(assoclubs_df, aes(y = reorder(terms, value))) +
  geom_point(aes(x = value), size = 4, color = 'steelblue', alpha = 0.7) +
  geom_text(aes(x = value, label = value), color = "black", hjust = "inward", vjust = "inward", size = 4) +
  theme_minimal() +  
  theme(
    panel.background = element_blank(),  # Remove background
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add a box line
    plot.margin = margin(20, 30, 20, 20)
  ) +
  ylab(NULL) +  # Remove y-axis label
  xlab("Value") +
  ggtitle("Words Associated with 'Club'")

# Make a dot plot for campus
ggplot(assocampus_df, aes(y = reorder(terms, value))) +
  geom_point(aes(x = value), size = 4, color = '#475ED1', alpha = 0.7) +
  geom_text(aes(x = value, label = value), color = "black", hjust = "inward", vjust = "inward", size = 4) +
  theme_minimal() +  
  theme(
    panel.background = element_blank(),  # Remove background
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add a box line
    plot.margin = margin(20, 30, 20, 20)
  ) +
  ylab(NULL) +  # Remove y-axis label
  xlab("Value") +
  ggtitle("Words associated with 'Campus'")

# Make bi-gram DTM according to the tokenize control & convert it to matrix
studentDTMb  <- DocumentTermMatrix(studentCorpus, 
                                   control=list(tokenize=bigramTokens))
studentDTMbm <- as.matrix(studentDTMb)

# See a bi-gram
idx <- grep('campus', colnames(studentDTMbm))
studentDTMbm[1:10,idx]

# Get Row Sums & organize
studentTDMmVec <- sort(colSums(studentDTMbm), decreasing = TRUE)
wordFreqDF   <- data.frame(word      = names(studentTDMmVec), 
                           freq      = studentTDMmVec, 
                           row.names = NULL)
head(wordFreqDF, 10)

# Word cloud for bigram
# Reminder to expand device pane
wordcloud(wordFreqDF$word,
          wordFreqDF$freq,
          max.words=100,
          random.order=FALSE,
          colors=pal)

# Logical T/F vector that a string appears at least ONCE
word_mba    <- grepl("mba", studentBios$text, ignore.case=TRUE)
word_marketing    <- grepl("marketing", studentBios$text, ignore.case=TRUE)
word_finance <- grepl("finance", studentBios$text, ignore.case=TRUE)
word_london <- grepl("london", studentBios$text, ignore.case=TRUE)
word_dubai <- grepl("dubai", studentBios$text, ignore.case=TRUE)
word_boston <- grepl("boston", studentBios$text, ignore.case=TRUE)
word_sanfran <- grepl("san francisco", studentBios$text, ignore.case=TRUE)

# Find the row positions of a specific word appearing at least ONCE
#this shows the difference between grep and grepl
grep("mba", studentBios$text, ignore.case=TRUE)

# Grep for indexing; better when you have more information like author and time
studentBios[grep('mba', studentBios$text),]

# Logical T/F for one word OR another appears at least ONCE
keywords    <-"mba|finance|boston"
mbaFinanceBoston <-grepl(keywords, studentBios$text,ignore.case=TRUE)

# Calculate the % of times among all tweets
sum(word_mba) / nrow(studentBios)
sum(word_finance) / nrow(studentBios)
sum(word_marketing) / nrow(studentBios)
sum(word_london) / nrow(studentBios)
sum(word_dubai) / nrow(studentBios)
sum(word_boston) / nrow(studentBios)
sum(word_sanfran) / nrow(studentBios)


#### Sentiment Analysis

# 1. By Campus
campusTibbles <- list()
for(i in 1:length(campusData)){
  x <- VCorpus(VectorSource(campusData[i])) #declare as a corpus
  x <- cleanCorpus(x, customStopwords) #clean each corpus
  x <- DocumentTermMatrix(x) #make a DTM
  x <- tidy(x) #change orientation
  x$document <- campusTopics[i]
  campusTibbles[[campusTopics[i]]] <- x #put it into the list
}

# 2. By Program
programTibbles <- list()
for(i in 1:length(programData)){
  x <- VCorpus(VectorSource(programData[i])) #declare as a corpus
  x <- cleanCorpus(x, customStopwords) #clean each corpus
  x <- DocumentTermMatrix(x) #make a DTM
  x <- tidy(x) #change orientation
  x$document <- programTopics[i]
  programTibbles[[programTopics[i]]] <- x #put it into the list
}

# 2. By Gender
genderTibbles <- list()
for(i in 1:length(genderData)){
  x <- VCorpus(VectorSource(genderData[i])) #declare as a corpus
  x <- cleanCorpus(x, customStopwords) #clean each corpus
  x <- DocumentTermMatrix(x) #make a DTM
  x <- tidy(x) #change orientation
  x$document <- genderTopics[i]
  genderTibbles[[genderTopics[i]]] <- x #put it into the list
}

# Examine
campusTibbles$london
dim(campusTibbles$london)

# Organize into a single tibble
campusTibble <- do.call(rbind, campusTibbles)
programTibble <- do.call(rbind, programTibbles)
genderTibble <- do.call(rbind, genderTibbles)

# Get "bing", "afinn", , "nrc", "loughran" lexicon
bing <- get_sentiments(lexicon = c("bing"))
bing

afinn <- get_sentiments(lexicon = c("afinn")) 
afinn

nrc <- lexicon_nrc()
nrc

# Perform Inner Join with Bing Sentiment
bingSent_campus <- inner_join(campusTibble,
                       bing, 
                       by=c('term'='word'))
bingSent_campus

bingSent_program <- inner_join(programTibble,
                              bing, 
                              by=c('term'='word'))
bingSent_program

bingSent_gender <- inner_join(genderTibble,
                               bing, 
                               by=c('term'='word'))
bingSent_gender

# Quick Analysis - count of words
bingResults_campus <- aggregate(count~document+sentiment, bingSent_campus, sum)
pivot_wider(bingResults_campus, names_from = document, values_from = count)

bingResults_program <- aggregate(count~document+sentiment, bingSent_program, sum)
pivot_wider(bingResults_program, names_from = document, values_from = count)

bingResults_gender <- aggregate(count~document+sentiment, bingSent_gender, sum)
pivot_wider(bingResults_gender, names_from = document, values_from = count)

ggplot(bingResults_campus, aes(x = reorder(document, -count), y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(pal[length(pal)], pal[1])) +  # Use the first and last colors in the palette
  theme_minimal() +
  labs(title = "(Bing) Sentiment Analysis by Campus",
       x = NULL,  # Remove x-axis label
       y = "Count of Sentiments",
       fill = "Sentiment") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid.major.x = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    legend.position = "top"  # Place the legend at the top
  )

ggplot(bingResults_program, aes(x = reorder(document, -count), y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(pal[length(pal)], pal[1])) +  # Use the first and last colors in the palette
  theme_minimal() +
  labs(title = "(Bing) Sentiment Analysis by Program",
       x = NULL,  # Remove x-axis label
       y = "Count of Sentiments",
       fill = "Sentiment") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid.major.x = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    legend.position = "top"  # Place the legend at the top
  )

ggplot(bingResults_gender, aes(x = reorder(document, -count), y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(pal[length(pal)], pal[1])) +  # Use the first and last colors in the palette
  theme_minimal() +
  labs(title = "(Bing) Sentiment Analysis by Gender",
       x = NULL,  # Remove x-axis label
       y = "Count of Sentiments",
       fill = "Sentiment") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    panel.grid.major.x = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
    legend.position = "top"  # Place the legend at the top
  )

# Word Sequence for afinn Sentiment
campusTibble$idx       <- as.numeric(ave(campusTibble$document, 
                                         campusTibble$document, FUN=seq_along))

programTibble$idx       <- as.numeric(ave(programTibble$document, 
                                         programTibble$document, FUN=seq_along))

genderTibble$idx       <- as.numeric(ave(genderTibble$document, 
                                         genderTibble$document, FUN=seq_along))

# Perform Inner Join of Afinn Sentiments
afinnSent_campus <- inner_join(campusTibble,
                        afinn, 
                        by=c('term'='word'))
afinnSent_campus

afinnSent_program <- inner_join(programTibble,
                               afinn, 
                               by=c('term'='word'))
afinnSent_program

afinnSent_gender <- inner_join(genderTibble,
                                afinn, 
                                by=c('term'='word'))
afinnSent_gender

# Calc
afinnSent_campus$ValueCount <- afinnSent_campus$value * afinnSent_campus$count 
afinnSent_campus

afinnSent_program$ValueCount <- afinnSent_program$value * afinnSent_program$count 
afinnSent_program

afinnSent_gender$ValueCount <- afinnSent_gender$value * afinnSent_gender$count 
afinnSent_gender

# Visualization, keep in mind these are words in alphabetical order, some analysis would use time
ggplot(afinnSent_campus, aes(idx, ValueCount, fill = document)) +
  geom_col(show.legend = FALSE) +  # Use default legend to distinguish documents
  facet_wrap(~document, ncol = 1, scales = "free_y") +
  labs(title = "Sentiment Analysis by Campus",
       x = "Indexed word",
       y = "Value Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

ggplot(afinnSent_program, aes(idx, ValueCount, fill = document)) +
  geom_col(show.legend = FALSE) +  # Use default legend to distinguish documents
  facet_wrap(~document, ncol = 1, scales = "free_y") +
  labs(title = "Sentiment Analysis by Program",
       x = "Indexed word",
       y = "Value Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

ggplot(afinnSent_gender, aes(idx, ValueCount, fill = document)) +
  geom_col(show.legend = FALSE) +  # Use default legend to distinguish documents
  facet_wrap(~document, ncol = 1, scales = "free_y") +
  labs(title = "Sentiment Analysis by Gender",
       x = "Indexed word",
       y = "Value Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

ggplot(afinnSent_campus, aes(x = document, y = ValueCount, fill = value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sentiment Analysis by Campus",
       x = "Campus",
       y = "Valuecount",
       fill = "Sentiment") +
  scale_fill_gradient(low = "red", high = "green") +  # Customize color scale
  theme_minimal() +
  guides(fill = FALSE) 


ggplot(afinnSent_program, aes(x = document, y = ValueCount, fill = value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sentiment Analysis by Program",
       x = "Program",
       y = "Valuecount",
       fill = "Sentiment") +
  scale_fill_gradient(low = "red", high = "green") +  # Customize color scale
  theme_minimal() + 
  guides(fill = FALSE) 

ggplot(afinnSent_gender, aes(x = document, y = ValueCount, fill = value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Sentiment Analysis by Gender",
       x = "Gender",
       y = "Valuecount",
       fill = "Sentiment") +
  scale_fill_gradient(low = "red", high = "green") +  # Customize color scale
  theme_minimal()

# Perform Inner Join of nrc Sentiment
nrcSent_campus <- inner_join(campusTibble,
                      nrc, 
                      by = c('term' = 'word'),
                      multiple = "all")
nrcSent_campus

nrcSent_program <- inner_join(programTibble,
                             nrc, 
                             by = c('term' = 'word'),
                             multiple = "all")
nrcSent_program

nrcSent_gender <- inner_join(genderTibble,
                              nrc, 
                              by = c('term' = 'word'),
                              multiple = "all")
nrcSent_gender

# Drop pos/neg leaving only emotion
nrcSent_campus <- nrcSent_campus[-grep('positive|negative',nrcSent_campus$sentiment),]
nrcSent_program <- nrcSent_program[-grep('positive|negative',nrcSent_program$sentiment),]
nrcSent_gender <- nrcSent_gender[-grep('positive|negative',nrcSent_gender$sentiment),]

# Quick chk
table(nrcSent_campus$sentiment,nrcSent_campus$document)
table(nrcSent_program$sentiment,nrcSent_program$document)
table(nrcSent_gender$sentiment,nrcSent_gender$document)

# Manipulate for radarchart
nrcSentRadar_campus <- as.matrix(table(nrcSent_campus$sentiment, nrcSent_campus$document))
nrcSentRadar_campus

nrcSentRadar_program <- as.matrix(table(nrcSent_program$sentiment, nrcSent_program$document))
nrcSentRadar_program

nrcSentRadar_gender <- as.matrix(table(nrcSent_gender$sentiment, nrcSent_gender$document))
nrcSentRadar_gender

# Normalize for length; prop.table by column is "2"
nrcSentRadar_campus <- prop.table(nrcSentRadar_campus,2)
nrcSentRadar_campus
colSums(nrcSentRadar_campus) #quick check to see what prop table did

pivot_longer(as.data.frame.matrix(nrcSentRadar_campus), col = everything())

nrcSentRadar_program <- prop.table(nrcSentRadar_program,2)
nrcSentRadar_gender <- prop.table(nrcSentRadar_gender,2)

# Organize
plotDF_campus <- data.frame(labels = rownames(nrcSentRadar_campus),
                     as.data.frame.matrix(nrcSentRadar_campus),
                     row.names = NULL)

plotDF_program <- data.frame(labels = rownames(nrcSentRadar_program),
                            as.data.frame.matrix(nrcSentRadar_program),
                            row.names = NULL)

plotDF_gender <- data.frame(labels = rownames(nrcSentRadar_gender),
                            as.data.frame.matrix(nrcSentRadar_gender),
                            row.names = NULL)
# Chart
chartJSRadar(scores = plotDF_campus, labelSize = 10, showLegend = T)
chartJSRadar(scores = plotDF_program, labelSize = 10, showLegend = T)
chartJSRadar(scores = plotDF_gender, labelSize = 10, showLegend = T)


#### Commonality and Comparison Analysis

# 1. By Campus
# This could be made more concise but we're going to do it within a loop
campusText <- list()
for(i in 1:length(campusData)){
  x <- VCorpus(VectorSource(campusData[i])) #declare as a corpus
  x <- cleanCorpus(x, customStopwords) #clean each corpus
  x <- unlist(sapply(x, `[`, "content")) #extract out each cleaned text using content()
  x <- paste(x, collapse = ' ') #collapse campusData text from 85 students to 1 blob of text by subject
  campusText[[campusTopics[i]]] <- x #put it into the list
}

# 2. By Program
programText <- list()
for(i in 1:length(programData)){
  x <- VCorpus(VectorSource(programData[i])) #declare as a corpus
  x <- cleanCorpus(x, customStopwords) #clean each corpus
  x <- unlist(sapply(x, `[`, "content")) #extract out each cleaned text using content()
  x <- paste(x, collapse = ' ') #collapse programData text from 85 students to 1 blob of text by subject
  programText[[programTopics[i]]] <- x #put it into the list
}

# 3. By Gender
genderText <- list()
for(i in 1:length(genderData)){
  x <- VCorpus(VectorSource(genderData[i])) #declare as a corpus
  x <- cleanCorpus(x, customStopwords) #clean each corpus
  x <- unlist(sapply(x, `[`, "content")) #extract out each cleaned text using content()
  x <- paste(x, collapse = ' ') #collapse genderData text from 85 students to 1 blob of text by subject
  genderText[[genderTopics[i]]] <- x #put it into the list
}

# Make a combined corpus of 3 subject matters
campusTopic_r <- unlist(campusText)
campusTopic_r <- VCorpus(VectorSource(campusTopic_r))
campusTopic_r

programTopic_r <- unlist(programText)
programTopic_r <- VCorpus(VectorSource(programTopic_r))
programTopic_r

genderTopic_r <- unlist(genderText)
genderTopic_r <- VCorpus(VectorSource(genderTopic_r))
genderTopic_r

# Make DTM and transpose it later
campustopicDTM  <- DocumentTermMatrix(campusTopic_r)
campustopicDTM <- as.matrix(campustopicDTM)

programtopicDTM  <- DocumentTermMatrix(programTopic_r)
programtopicDTM <- as.matrix(programtopicDTM)

gendertopicDTM  <- DocumentTermMatrix(genderTopic_r)
gendertopicDTM <- as.matrix(gendertopicDTM)

# Label the new TDM, remember the order of subjects from lines 80,81, and 82!
rownames(campustopicDTM) <- campusTopics
campustopicDTM[1:4,50:55]

rownames(programtopicDTM) <- programTopics
programtopicDTM[1:7,10:15]

rownames(gendertopicDTM) <- genderTopics
gendertopicDTM[1:2,50:55]

# Make commonality cloud requires rows as terms and columns as documents.
# Thus, it really needs a TermDocumentMatrix but for ease of learning we made DTM abov.
# So, its a simple transposition here but really should be adjusted above in DocumentTermMatrix to TermDocumentMatrix & 
# colnames() section too
commonality.cloud(t(campustopicDTM), 
                  max.words=150, 
                  random.order=FALSE,
                  colors=pal,
                  scale=c(3.5,0.25))

commonality.cloud(t(programtopicDTM), 
                  max.words=150, 
                  random.order=FALSE,
                  colors=pal,
                  scale=c(3.5,0.25))

commonality.cloud(t(gendertopicDTM), 
                  max.words=150, 
                  random.order=FALSE,
                  colors=pal,
                  scale=c(3.5,0.25))


# Make comparison cloud
comparison.cloud(t(campustopicDTM), 
                 max.words=100, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('#F97A1F', '#E2365B', '#475ED1', '#141F52'),
                 scale=c(2,0.5))

comparison.cloud(t(programtopicDTM), 
                 max.words=100, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('#F97A1F', '#E2365B', '#475ED1', '#141F52', '#F9C31F','#595959'
                          ,'#D0E1E1'),
                 scale=c(2,0.5))

comparison.cloud(t(gendertopicDTM), 
                 max.words=100, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=c('#E2365B', '#141F52'),
                 scale=c(2,0.5))

# End



