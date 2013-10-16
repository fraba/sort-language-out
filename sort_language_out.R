### DESCRIPTION ###
# This simple script labels and removes messages made in a different language. 
# In this case the languages are English and Italian but it is possible to use any 
# combination of languages.
#
# The case scenario is a forum where comments in English are treated as spam while comments
# in Italian are considered ham. The script labels string based on their probable language 
# and then delete comments in one specific language (in this case English). 
# You can of course easily tweak the script so to manipulate the messages once labelled
# in the most appropriate way depending on your needs. 
#
# One table with comments
authorUrl <- c("http://www.one.com", "http://www.two.com","http://www.three.com","http://www.one.com")
message <- c("This is a message in English, clearly. It's easy to tell a language from the other searching from common words",
             "Questo è in italiano, chiaramento. E' facile distinguere una lingua dall'altra cercando parole frequenti",
             "Questo è anche in italiano.",
             "This is another message in English")
comment <- data.frame(authorUrl, message)
rm(authorUrl);rm(message)
#
# One table with author names
authorUrl <- c("http://www.one.com", "http://www.two.com","http://www.three.com")
authorName <- c("Peter","Pietro","Alice")
author <- data.frame(authorUrl, authorName)
rm(authorUrl);rm(authorName)
#
# Create dictionary vectors of most common words in each language. Each dictionary should contain at least 50 words. 
dictionary_english <- c("the","is","to","of","a","and","easy","from","this")
dictionary_italian <- c("cosa","anno","uomo","giorno","volta","casa","questo","una","uno","facile","da","anche")

# tm library required
library(tm)

### FUNCTIONS ###
replaceCodingError <- function(vector) {
  vector <- gsub("Ã\u0083Æ\u0092Ã\u0082Â¨","è", vector)
  vector <- gsub("Â¨","è", vector)
  vector <- gsub("Ã¨","è", vector)
  vector <- gsub("Ã\u0083Æ\u0092Ã\u0082Â©","é", vector)
  vector <- gsub("Â©","é", vector)
  vector <- gsub("Ã©","é", vector)
  vector <- gsub("Ã\u0083Æ\u0092Ã\u008bâ\u0080","É", vector)
  vector <- gsub("Ã\u0083Æ\u0092Ã\u0082Â¬","ì", vector)
  vector <- gsub("Â¬","ì", vector)
  vector <- gsub("Ã¬","ì", vector)
  vector <- gsub("Ã\u0083Æ\u0092Ã\u0082Â²","ò", vector)
  vector <- gsub("Â²","ò", vector)
  vector <- gsub("Ã²","ò", vector)
  vector <- gsub("Ã\u0083Æ\u0092Ã\u0082Â¹","ù", vector)
  vector <- gsub("Â¹","ù", vector)
  vector <- gsub("Ã¹","ù", vector)
  vector <- gsub("Ã\u0083Æ\u0092Ã\u0082Â±","ñ", vector)
  vector <- gsub("Â±","ñ", vector)
  vector <- gsub("Ã±","ñ", vector)
  vector <- gsub("Ã\u0083â\u0080\u009aÃ\u0082Â´","’", vector)
  vector <- gsub("Ã\u0083Â¢Ã¢â\u0080\u009aÂ¬Ã¢â\u0080\u009eÂ¢","’", vector)
  vector <- gsub("â€™","’", vector)
  vector <- gsub("\nÃ\u0083â\u0080\u009aÃ\u0082Â«"," «", vector)
  vector <- gsub("Ã\u0083â\u0080\u009aÃ\u0082Â»"," »", vector)
  vector <- gsub("Ã\u0083Â¢Ã¢â\u0080\u009aÂ¬Ã\u0085â\u0080\u009c","\"", vector)
  vector <- gsub("â€œ","“", vector)
  vector <- gsub("â€\u009d","”", vector)
  vector <- gsub("Ã\u0083Â¢Ã¢â\u0082¬Å¡Ã\u0082Â¬","€", vector)
  vector <- gsub("â‚¬","€", vector)
  vector <- gsub("â€¢","•", vector)
  vector <- gsub("â€¢","•", vector)
  vector <- gsub("à·","-", vector)
  vector <- gsub("&nbsp;"," ", vector)
  vector <- gsub("Ã\u0083Æ\u0092Ã\u0082Â","à", vector)
  vector <- gsub("Ã","à", vector)
  vector <- gsub("Â","à", vector)
}

getSpamLabel <- function (text_vector, dictionary, threshold, wordLengths) {
  text_vector <- as.character(text_vector)
  corpus <- Corpus(VectorSource(text_vector))
  dtm <- DocumentTermMatrix(corpus, list(dictionary = dictionary,
                                         wordLengths = c(wordLengths, Inf))
  )
  dtm.matrix <- as.matrix(dtm)
  dtm.sum <- rowSums(dtm.matrix)
  dtm.label <- as.numeric(dtm.sum > threshold)
}

filterAuthorOut <- function(string,pattern) {
  if (grepl(pattern,string)) {
    string <- "THIS_IS_HAM"
    return(string)
  } else {
    return(string)
  }
} 

#### - ####


#### BEGINNING OF SCRIPT ####

# Store original tables
raw_author <- author
raw_comment <- comment

# Clean tables from common coding errors. See function
author$name <- replaceCodingError(author$authorName)
comment$message <- replaceCodingError(comment$message)

# Create vectors of dictionaries
dictionary_english <- as.character(dictionary_english)
dictionary_italian <- as.character(dictionary_italian)

# Create dictionaries (tm package objects) 
dictionary_english <- Dictionary(dictionary_english)
dictionary_italian <- Dictionary(dictionary_italian)

# Create a Document-term Matrix (counting frequency of each word)
# This is the most important function. There are two values that must be indicated for calibrating
# the function.
# The first calibrating value passed to the function as argument after the dictionary (dictionary_english or 
# dictionary_italian) is the minimum number of word matches within a dictionary to define the string language;
# in the example is 1 for Engligh and 2 for Italian. That is, it will lable as probably English each string 
# with more that one word from the English dictionary and probably Italian each string with more that two 
# words from the Italian dictionary.
# The second calibrating value is the minimum lenght of a word to be use by the function. In both examples is 2.
# That is, the word "is" will be used to try to define the language but the word "I" will not. 
# WARNING: COMPUTATIONALLY INTENSIVE
dtm_english.label <- vapply(comment$message, getSpamLabel, FUN.VALUE = numeric(1), dictionary_english, 1, 2, USE.NAMES = FALSE)
dtm_italian.label <- vapply(comment$message, getSpamLabel, FUN.VALUE = numeric(1), dictionary_italian, 2, 2, USE.NAMES = FALSE)
# rm(dictionary_english); rm(dictionary_italian)

# Create binary vector for spam (1 for spam, 0 for ham)
spam <- ifelse(dtm_english.label==1 & dtm_italian.label==0, 1, 0)
# rm(dtm_english.label); rm(dtm_italian.label) 

# Check flagged comments
checkFlagged <- subset(comment, spam==1)
# rm(checkFlagged)

# Create vector with authorUrl for spammers and filter genuine authors out
# If there are author urls you want to protect from filtering enter them as pattern variables
spamAuthor <- comment$authorUrl[which(spam==1)]
pattern_1 = "ENTER FIRST PATTERN HERE"
pattern_2 = "ENTER SECOND PATTERN HERE"
spamAuthor <- sapply(spamAuthor,filterAuthorOut,pattern_1)
spamAuthor <- sapply(spamAuthor,filterAuthorOut,pattern_2)
#count(spamAuthor=="THIS_IS_HAM")
spamAuthor <- spamAuthor[which(spamAuthor!="THIS_IS_HAM")]
spamAuthor <- unique(spamAuthor)
# rm(pattern_1);rm(pattern_2)

# Create dataframe for spam
spam_author <- data.frame()
spam_comment <- data.frame()

# Remove spammers from authors and comments made by spammers 
# It also stores spam authors and comments
# WARNING: COMPUTATIONALLY INTENSIVE
i <- 1; N <- length(spamAuthor)
while (i <= N) {
  spam_author <- rbind(spam_author, author[author$authorUrl == spamAuthor[i], ])
  author <- author[author$authorUrl != spamAuthor[i], ]
  spam_comment <- rbind(spam_comment, comment[comment$authorUrl == spamAuthor[i], ])
  comment <- comment[comment$authorUrl != spamAuthor[i], ]
  i <- i + 1
}
rm(N);rm(i)
# rm(spamAuthor);rm(spam_comment); rm(spam); rm(spam_author)
