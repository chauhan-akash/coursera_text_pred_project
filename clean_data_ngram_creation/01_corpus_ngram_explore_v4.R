
###########################################################
# Part 1: importing data and sampling
###########################################################

library(readr)
library(stringr)
library(quanteda)
library(tm)
library(data.table)

# import the text files from raw folder
blogs <- read_lines("./dataset/en_US.blogs.txt", skip = 0)
twit <- read_lines("./dataset/en_US.twitter.txt", skip = 0)
news <- read_lines("./dataset/en_US.news.txt", skip = 0)

# create a combined files of all three text sources
text_corpus <- c(blogs, twit, news)
length(text_corpus) # 4.269 mill lines

# view the text sample
text_corpus[1:25]
sum(nchar(text_corpus)) # 572 mill. characters

# sampling the total text corpus into test and train data for later validation
set.seed(12345)
ind <- 1:length(text_corpus)
train_ind <- sample(ind, length(text_corpus)*0.75)
test_ind <- ind[-train_ind]
train_text <- text_corpus[train_ind]
test_text <- text_corpus[test_ind]

sum(nchar(train_text))
sum(nchar(test_text))

# write the train and test data so that we can use later on
write_lines(train_text, "./clean_data_quanteda/train_text.txt")
write_lines(test_text, "./clean_data_quanteda/test_text.txt")

rm(blogs, twit, news, ind, train_ind, test_ind, text_corpus, test_text)
gc()

###########################################################
# Part 2: cleaning and pre-processing text data
###########################################################

# do some clean up and get the time estimate for pre-processing

bw <- read_lines("./dataset/google_bad_words_v1.txt", skip=0)

st <- Sys.time()

# conver to lower case
train_text <- tolower(train_text)
# remove bad words / profanity - list taken from google standard profanity check
train_text <- removeWords(train_text, bw)
# remove digits
train_text <- str_remove_all(train_text, "[:digit:]")
# remove non ascii characters from the text
Encoding(train_text) <- "latin1"
train_text <- iconv(train_text, "latin1", "ASCII", sub="")
# replace more than twice repeated characters with single character
train_text <- str_replace_all(train_text, "([[:alpha:]])\\1{2,}", "\\1")
# remove hyperlink/URLs
train_text <- str_remove_all(train_text, "http[[:alnum:]]*")
# remove URLs without http
train_text <- str_remove_all(train_text, "www\\.[[:alnum:]]+\\.com")
# remove email ids
train_text <- str_remove_all(train_text, "[[:alnum:]]+\\@[[:alnum:]]+\\.[[:alpha:]]{1,3}")
# remove twitter screen names
train_text <- str_remove_all(train_text,"@[[:alnum:]]*")
# remove hashtags of twitter
train_text <- str_remove_all(train_text, "#[[:alnum:]]*")
# remove re tweets tags
train_text <- str_remove_all(train_text, "(RT|via)((?:\\b\\W*@\\w+)+)")
# remove punctuations
train_text <- str_remove_all(train_text, "[:punct:]")
# remove any non alphabetic and non space character
train_text <- str_remove_all(train_text, "[^[:alpha:]\\s]")
# remove multiple spaces with single space
train_text <- str_replace_all(train_text, " +", " ")

et <- Sys.time()
print(et - st)

# time taken to clean text samp of characters ~429 mill. : 26 min
# time taken to clean text samp of characters ~150 mill. : 9 min
# time taken to clean text samp of characters ~28 mill. : 2 min
# time taken to clean text samp of characters ~5.7 mill. : 20 secs

print("Total charaters in cleaned text:")
print(sum(nchar(train_text)))

# remaining characters after cleaning ~ 404 mill. (~6% reduction)

###########################################################
# Part 3: creating n grams
###########################################################

st <- Sys.time()

#create 1,2 & 3 gram from the cleaned corpus

uni_freq <- docfreq(dfm(train_text, n=1))
uni_freq <- sort(uni_freq, decreasing = TRUE)
uni_freq_dt <- data.frame(ngram=names(uni_freq), freq=uni_freq, row.names = NULL)
rm(uni_freq)
et1 <- Sys.time()

bi_freq <- docfreq(dfm(train_text,n=2))
bi_freq <- sort(bi_freq, decreasing = TRUE)
bi_freq_dt <- data.frame(ngram=names(bi_freq), freq=bi_freq, row.names = NULL)
rm(bi_freq)
et2 <- Sys.time()

tri_freq <- docfreq(dfm(train_text,n=3))
tri_freq <- sort(tri_freq, decreasing = TRUE)
tri_freq_dt <- data.frame(ngram=names(tri_freq), freq=tri_freq, row.names = NULL)
rm(tri_freq)
et3 <- Sys.time()

quad_freq <- docfreq(dfm(train_text,n=4))
quad_freq <- sort(quad_freq, decreasing = TRUE)
quad_freq_dt <- data.frame(ngram=names(quad_freq), freq=quad_freq, row.names = NULL)
rm(quad_freq)
et4 <- Sys.time()

print(et1 - st)
print(et2 - et1)
print(et3 - et2)
print (et4 - st)
gc()

# time to create unigram dfm and freq : 5 mins
# time to create bigram dfm and freq : 9 mins
# time to create trigram dfm and freq : 24 mins
# time to create quadgram dfm and freq : 42 mins

# memory used by 3 datatables of freq
object.size(uni_freq_dt) # 54 MB
object.size(bi_freq_dt) # 945 MB
object.size(tri_freq_dt) # 3 GB
object.size(quad_freq_dt) # 4.6 GB

# write these n grams to csv files to be used later directly
fwrite(uni_freq_dt, "./clean_data_quanteda/unigram_freq.csv")
fwrite(bi_freq_dt, "./clean_data_quanteda/bigram_freq.csv")
fwrite(tri_freq_dt, "./clean_data_quanteda/trigram_freq.csv")
fwrite(quad_freq_dt, "./clean_data_quanteda/quadgram_freq.csv")