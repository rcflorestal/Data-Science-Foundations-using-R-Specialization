## Data Science Capstone ##
#
# Tasks
#
## Load packages
library(sentimentr)         # for emotional analysis
library(wordcloud)          # word-cloud generator 
library(wordcloud2)         # for word cloud chart
library(tm)                 # for text mining
library(SnowballC)          # for text stemming
library(quanteda)
library(quanteda.textstats)
library(RColorBrewer)       # color palettes
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(gt)
library(janeaustenr)
library(gridExtra)

## Set the work directory
setwd('C:/Data-Science-Foundations-using-R-Specialization/Data-Science-Capstone/tasks/')

##-------------------------------** Task 0 **---------------------------------##
#
## Get the data set
if(!dir.exists('./data')){   # Check out if the directory data exist
        dir.create('./data') # Create the data directory
}

if(!file.exists('./data/Coursera-SwiftKey.zip')){ # Check out if the file exist
        # Download the data set
        download.file(  
                url = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',
                destfile = './data/Coursera-SwiftKey.zip',
                mode = 'wb'
        )
        # Unzip the files
        unzip(zipfile = './data/Coursera-SwiftKey.zip', exdir = './data')
}

##-------------------------------** Task 1 **---------------------------------##
#
## Starts a connection with the file
con <- file('./data/final/en_US/en_US.twitter.txt', open = 'r')

## Read the first line of the data set
readLines(con, 1)

## Read in the next 5 lines of the data set
readLines(con, 5)

## Read the tweeter data set
twitter <- readLines(con)

## It's important to close the connection when you are done
close(con)

## 'Tokenization' - identifying appropriate tokens such as words,
## punctuation, and numbers. Writing a function that takes a file 
## as input and returns a 'tokenized' version of it.
#
tokenized <- function(txt){
        # Remove all punctuation signs without apostrophe
        txt <- gsub("[^[:alnum:][:space:]']", '', txt)
        
        # Remove all numbers
        txt <- gsub('[0-9]', '', txt)
        
        # Remove excessive space between words
        txt <- gsub('\\s+', ' ', txt)
        txt <- gsub('^[[:space:]]*', '', txt)
        
        # Converts all word to lowercase
        txt <- tolower(txt)
        
        # return clean data set
        return(txt)
}

## Apply the function to clear the tweeter texts 
twitter <- tokenized(twitter)
head(twitter)
tail(twitter)

## Save the cleaned text
write(
        twitter, 
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Science-Capstone/tasks/tweeterClear.txt'
)

twitterClear <- readLines(
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Science-Capstone/tasks/tweeterClear.txt',
        skipNul = TRUE
)

##-------------------------------** Task 2 **---------------------------------##
#
## Profanity filtering - removing profanity and other words you do 
## not want to predict.
#
## We use the profanity list from https://www.cs.cmu.edu/~biglou/resources/bad-words.txt
#
## Load cleaned twitter data set
tw <- readLines('C:/Data-Science-Foundations-using-R-Specialization/Data-Science-Capstone/tasks/tweeterClear.txt')

## Set the sample of the data set
samp <- sample(tw, length(tw) * 0.03, replace = FALSE)

write(
        samp, 
        'C:/Data-Science-Foundations-using-R-Specialization/Data-Science-Capstone/tasks/sample.txt'
)


## Summary statistic ##
# Get the size of each file
size = file.size(c('./data/final/en_US/en_US.twitter.txt', './tweeterClear.txt', './sample.txt'))
size = round(size/1e6, 2)  # Convert to MB

# get the numbers of lines in each file
lines = sapply(list(twitter, tw, samp), length)

# get the numbers of characters in each file
char <- sapply(list(twitter, tw, samp), function(x){sum(nchar(x))})

sumDF <- data.frame(
        File = c('./data/final/en_US/en_US.twitter.txt', 
                 './tweeterClear.txt', 
                 './sample.txt'),
        Size = size,
        Line = lines,
        Character = char
)

# Set summary table
sumDF %>%
        gt() %>%
        tab_header(
                title = md('Table 1. _Summary of file sizes, number of lines and characters in each file._')
        ) %>%
        cols_label(
                File = md('**File**'),
                Size = md('**Size (MB)**'),
                Line = md('**Line**'),
                Character = md('**Character**')
                
        ) %>%
        fmt_number(
                columns = c('Size'),
                decimals = 2
        ) %>%
        fmt_number(
                columns = c('Line', 'Character'),
                decimals = 0,
                sep_mark = ','
        ) %>%
        tab_style(
                style = cell_borders(
                        color = 'white',
                ),
                locations = cells_stub()
        ) %>%
        tab_options(
                heading.title.font.size = px(16),
                table.width = pct(100)
        ) %>%
        cols_align(align = 'center') %>%
        tab_style(
                style = list(
                        cell_borders(
                                sides = c('top', 'bottom'),
                                color = 'white',
                                weight = px(1)
                        ),
                        cell_text(
                                align = 'center'
                        ),
                        cell_fill(color = 'white', alpha = NULL)
                ),
                locations = 
                        cells_body(
                                columns = everything(),
                                rows = everything()
                        )
        ) %>%
        tab_style(
                style = cell_borders(
                        color = 'white',
                ),
                locations = cells_stub()
        ) %>%
        tab_options(
                ## hide the top-most border
                table.border.top.color = 'white',
                ## make the title size match the body text
                heading.title.font.size = px(16),
                ## change the column labels section
                column_labels.border.top.width = 3,
                column_labels.border.top.color = 'black', 
                column_labels.border.bottom.width = 3,
                column_labels.border.bottom.color = 'black',
                ## Change the vertical lines of summary body
                stub.border.color = 'white',
                stub.background.color = 'white',
                ## Change the foot source body
                footnotes.border.bottom.color = 'white',
                ## change the bottom of the body
                table_body.border.bottom.color = 'black',
                ## hide the bottom-most line or footnotes
                ## will have a border
                table.border.bottom.color = 'white',
                ## make the width 100%
                table.width = pct(60),
                table.background.color = 'white'
        )
        
## Extract sentences of the sample
tws <- get_sentences(samp)

## Set profanity
profTwitter <- profanity(tws)

## Extract all profanity phrases
prof_words <- extract_profanity_terms(tws)

tail(prof_words, 10)

head(prof_words$sentence)
tail(prof_words$sentence)
head(prof_words$neutral)
#head(prof_words$profanity)


## Count profanity terms
countProf <- attributes(extract_profanity_terms(tws))$counts
countProf <- countProf[countProf$profanity >= 1, c(1, 3)]

## Chart of the most profanity used in twitter
countProfplot <- countProf %>%
        # get the 10 most profanity used
        slice_max(n, n = 10) %>%
        # Capitalize words
        mutate(words = stringr::str_to_sentence(words))
        

## Reorder the words by values
countProfplot$words <- with(countProfplot, reorder(words, n))

# set the chart
p <- ggplot(countProfplot, aes(x = n, y = words)) +
        geom_bar(stat = 'identity', 
                 position = position_dodge(0.9),
                 fill = 'steelblue') +
        theme(
                plot.title = element_text(color = 'black', 
                                          size = 11, face = 'bold'),
                axis.title.x = element_text(size = 11),
                axis.title.y = element_text(size = 11),
                #strip.text = element_text(face = 'italic'),
                legend.position = 'top',
                legend.background = element_rect(fill = "gray90")
                
        ) +
        labs(
                title = 'Most Frequent Profanity Words',
                x = 'n', 
                y = 'Frequency'
        )


## How many of the words come from foreign languages? ##
# profElem <- attributes(extract_profanity_terms(tws))$elements
# p <- profElem[profElem$words!=c("'"), ]

## Set n-grams
sentences <- as.data.frame(prof_words$sentence)
varCol <- 'Sentence'
names(sentences) <- varCol

# 2 n-grams
toBigrams <- sentences %>% 
        unnest_tokens(bigram, 'Sentence', n = 2, token = 'ngrams') %>%
        filter(!is.na(bigram)) %>%
        count(bigram, sort = TRUE) %>%
        mutate(id = 1:n()) %>%
        filter(id <= 15)

bigrams <- ggplot(toBigrams, aes(reorder(bigram, n), n)) +
        geom_col(fill = 'steelblue') +
        coord_flip() +
        theme(plot.title = element_text(colour = 'black',
                                        size = 11, face = 'bold')) +
        labs(title = 'Most Frequent Bigrams', y = 'n', x = 'Bigram')

# 3 n-grams
threeGrams <- sentences %>%
        unnest_tokens(bigram, 'Sentence', n = 3, token = 'ngrams') %>%
        filter(!is.na(bigram)) %>%
        count(bigram, sort = TRUE) %>%
        mutate(id = 1:n()) %>%
        filter(id <= 15)

thrGrames <- ggplot(threeGrams, aes(reorder(bigram, n), n)) +
        geom_col(fill = 'steelblue') +
        coord_flip() +
        theme(plot.title = element_text(color = 'black',
                                        size = 11, face = 'bold')) +
        labs(title = 'Most Frequent Threegrams', x = 'Trigram', y = 'n')

# Filter the words that starts with "not" to make a setiment analyze


## Extract emotion words
nEmoTwitter <- attributes(extract_emotion_terms(tws))$count
head(nEmoTwitter)

wEmoTwitter <- attributes(extract_emotion_terms(tws))$elements
head(wEmoTwitter)

## Extract sentiment words
wSentTwitter <- attributes(extract_sentiment_terms(tws))$elements
head(wSentTwitter)

### Set word cloud ###
## Set the data as a corpus
voc <- Corpus(VectorSource(samp), 
              readerControl = list(language = "eng"))

## Build a term-document matrix 
dtm <- TermDocumentMatrix(voc)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = TRUE)

## Words Frequency
wdfreq <- data.frame(word = names(v),freq = v)
wdfreq <- wdfreq %>%
        anti_join(get_stopwords(language = 'en')) # Remove stop words
head(wdfreq, 10)

## Set frequency table
tab_2 <- wdfreq %>% 
        mutate(N = 1:n()) %>%
        # arrange(desc(freq)) %>%
        filter(N <= 10) %>% 
        select(N, word, freq)

table_2 <- tab_2 %>% 
        gt(rowname_col = 'N') %>%
        tab_stubhead(label = 'N') %>%
        tab_header(
                title = md('_Table 2. Most frequent Words on twitter._')
        ) %>%
        cols_width(
                N ~ px(20),
                word ~ px(100),
                freq ~ px(80)
        ) %>%
        fmt_number(
                columns = c('freq'),
                decimals = 0,
                sep_mark = '.'
        ) %>%
        cols_label(  ## Change the style names of the columns
                freq = md('**Frequency**'),
                word = md('**Word**')
        ) %>%
        cols_align(align = 'center') %>%
        tab_style(
                style = list(
                        cell_borders(
                                sides = c('top', 'bottom'),
                                color = 'white',
                                weight = px(1)
                        ),
                        cell_text(
                                align = 'center'
                        ),
                        cell_fill(color = 'white', alpha = NULL)
                ),
                locations = 
                        cells_body(
                                columns = everything(),
                                rows = everything()
                        )
        ) %>%
        tab_style(
                style = cell_borders(
                        color = 'white',
                ),
                locations = cells_stub()
        ) %>%
        tab_source_note(
                source_note = html('<div align="center">',
                                   'Source: Twitter.',
                                   '</div>')
        ) %>%
        tab_style(
                style = cell_borders(
                        color = 'white',
                ),
                locations = cells_stub()
        ) %>%
        # tab_options(
        #         heading.title.font.size = px(16),
        #         table.width = pct(100)
        # ) %>%
        tab_options(
                ## hide the top-most border
                table.border.top.color = 'white',
                ## make the title size match the body text
                heading.title.font.size = px(16),
                ## change the column labels section
                column_labels.border.top.width = 3,
                column_labels.border.top.color = 'black', 
                column_labels.border.bottom.width = 3,
                column_labels.border.bottom.color = 'black',
                ## Change the vertical lines of summary body
                stub.border.color = 'white',
                stub.background.color = 'white',
                ## Change the foot source body
                footnotes.border.bottom.color = 'white',
                ## change the bottom of the body
                table_body.border.bottom.color = 'black',
                ## hide the bottom-most line or footnotes
                ## will have a border
                table.border.bottom.color = 'white',
                ## make the width 100%
                table.width = pct(100),
                table.background.color = 'white'
        )

table_1

### Generate the Word cloud without profanity###
set.seed(1234)
wd = wordcloud(words = wdfreq$word, 
          freq = wdfreq$freq, 
          min.freq = 1,
          max.words = 200,
          random.order = FALSE,
          rot.per = 0.35, 
          colors = brewer.pal(6, 'Dark2'),
          fixed.asp = TRUE)

w <- wdfreq %>%
        mutate(N = 1:n()) %>%
        filter(N <= 10) %>%
        ggplot(aes(reorder(word, freq), freq)) +
        geom_col(fill = 'steelblue') +
        theme(plot.title = element_text(color = 'black',
                                        size = 11, face = 'bold')) +
        coord_flip() +
        labs(title = 'Most Frequent Word', x = 'Word', y = 'Frequency')

## Set merge charts
chartMerge <- grid.arrange(w, p, bigrams, thrGrames)

## Most common sentiment words
tw_counts <- nEmoTwitter %>%
        #select(word, freq) %>%
        rename(word = words) %>%
        inner_join(get_sentiments('bing')) %>%
        count(word, sentiment, sort = TRUE) %>%
        group_by(sentiment)

head(tw_counts)

nEmoTwitter %>%
        group_by(emotion_type) %>%
        filter(!is.na(emotion_type)) %>%
        slice_max(n, n = 10) %>% 
        ungroup() %>%
        mutate(words = reorder(words, n)) %>%
        ggplot(aes(x = n, y = words, fill = emotion_type)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~emotion_type, scales = 'free_y') +
        theme(
                strip.text = element_text(face = 'italic')
        ) +
        labs(x = 'Contribution to sentiment',
             y = NULL)
