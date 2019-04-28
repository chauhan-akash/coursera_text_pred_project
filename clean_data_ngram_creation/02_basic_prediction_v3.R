
library(readr)
library(stringr)
library(quanteda)
library(tm)
library(data.table)

###########################################################
# Part 1: importing the ngram freq tables and trimming the size
###########################################################

# time with fread

st <- Sys.time()
uni <- fread("./clean_data_quanteda/unigram_freq.csv", header = TRUE)
bi <- fread("./clean_data_quanteda/bigram_freq.csv", header = TRUE)
tri <- fread("./clean_data_quanteda/trigram_freq.csv", header = TRUE)
quad <- fread("./clean_data_quanteda/quadgram_freq.csv", header = TRUE)
et <- Sys.time()
print(et - st)
# time to read the full csv file : 3.2 mins

# create shorter ngram freq tables

uni_sh <- uni[uni$freq > 4,]
bi_sh <- bi[bi$freq > 4,]
tri_sh <- tri[tri$freq > 4,]
quad_sh <- quad[quad$freq > 4,]

rm(uni, bi, tri, quad, quad_freq_dt)
gc()

object.size(quad_sh) # size reduced from 3 GB to 88 MB
object.size(tri_sh) # size reduced from 3 GB to 88 MB
object.size(bi_sh) # size reduced from 950 MB to 74 MB
object.size(uni_sh) # size reduced from 54 MB to 8 MB

# size of shorter freq tables for k = 5
# tri : reduced from 3 gb to 88 MB
# bi  : reduced from 950 mb to 74 mb
# uni : reduced from 54 mb to 8 mb

# size of shorter freq tables for k = 4
# quad : reduced from 4.6 gb to 57 MB
# tri : reduced from 3 gb to 112 MB
# bi  : reduced from 950 mb to 88 mb
# uni : reduced from 54 mb to 9 mb

# size of shorter freq tables for k = 2
# tri : reduced from 3 gb to 231 MB
# bi  : reduced from 950 mb to 153 mb
# uni : reduced from 54 mb to 13 mb

tail(quad_sh)
tail(tri_sh)
tail(bi_sh)
tail(uni_sh)

# overall memory used for full ngram : ~4 GB
# overall memory used for shorten ngram with k = 5 : ~160 MB
# overall memory used for shorten ngram with k = 4 : ~260 MB
# overall memory used for shorten ngram with k = 2 : ~400 MB

# save the shorten ngrams to save load time

fwrite(quad_sh, "./clean_data_quanteda/quadgram_freq_k4.csv")
fwrite(tri_sh, "./clean_data_quanteda/trigram_freq_k4.csv")
fwrite(bi_sh, "./clean_data_quanteda/bigram_freq_k4.csv")
fwrite(uni_sh, "./clean_data_quanteda/unigram_freq_k4.csv")

# time to read shorten data

st <- Sys.time()

quad <- fread("./clean_data_quanteda/quadgram_freq_k4.csv", header = TRUE)
tri <- fread("./clean_data_quanteda/trigram_freq_k4.csv", header = TRUE)
bi <- fread("./clean_data_quanteda/bigram_freq_k4.csv", header = TRUE)
uni <- fread("./clean_data_quanteda/unigram_freq_k4.csv", header = TRUE)

et <- Sys.time()

print(et-st)
# 0.5 secs to read all the shorten tables k = 5
# 1.2 secs to read all the shorten tables k = 2
# 0.7 secs to read all the shorten tables k = 4

rm(uni, bi, tri, quad)
gc()

###########################################################
# Part 2: defining functions to be used in predicting
###########################################################

# clean text function

bw <- read_lines("./dataset/google_bad_words_v1.txt", skip=0)

clean_text <- function(s){
  # conver to lower case
  train_text <- tolower(s)
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
  # remove trailing space
  train_text <- trimws(train_text)
  
  return(train_text)
}

search_quad <- function(s){
  s <- tail(str_split(s, " ")[[1]],n=3)
  s <- paste("^",paste(paste(s[1], s[2], s[3], sep = "_"),"_", sep=""), sep = "")
  res <- quad_sh[grepl(s, quad_sh$ngram),]
  return(res)
}

search_tri <- function(s){
  s <- tail(str_split(s, " ")[[1]],n=2)
  s <- paste("^",paste(paste(s[1], s[2], sep = "_"),"_", sep=""), sep = "")
  res <- tri_sh[grepl(s, tri_sh$ngram),]
  return(res)
}

search_bi <- function(s){
  s <- tail(str_split(s, " ")[[1]],n=1)
  s <- paste("^",paste(s[1],"_", sep=""), sep = "")
  res <- bi_sh[grepl(s, bi_sh$ngram),]
  return(res)
}

search_uni <- function(){
  res <- uni_sh[1:3,]
  return(res)
}

# function to predict next word using stupid backoff logic

predict_next <- function(inp){
  inp <- clean_text(inp)
  wcount <- length(strsplit(inp, " ")[[1]])
  nw <- ""
  
  if(wcount >= 3){
    
    res <- search_quad(inp)
    
    if(nrow(res) > 0){
      if(nrow(res) >= 3){
        nw <- c(tail(str_split(res$ngram[[1]], "_")[[1]],n=1),
                tail(str_split(res$ngram[[2]], "_")[[1]],n=1),
                tail(str_split(res$ngram[[3]], "_")[[1]],n=1))
      }# end of nrow > 3
      else{
        for(i in (1:nrow(res))){
          nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
        }
      }# end of nrow < 3
    }# end of nrow > 0
    
    else{
      
      res <- search_tri(inp)
      
      if(nrow(res) > 0){
        if (nrow(res) >= 3){
          nw <- c(tail(str_split(res$ngram[[1]], "_")[[1]],n=1),
                  tail(str_split(res$ngram[[2]], "_")[[1]],n=1),
                  tail(str_split(res$ngram[[3]], "_")[[1]],n=1))
        }
        else{
          for(i in (1:nrow(res))){
            nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
          }
        }
      }
      
      else{
        
        res <- search_bi(inp)
        
        if(nrow(res) > 0){
          if (nrow(res) >= 3){
            nw <- c(tail(str_split(res$ngram[[1]], "_")[[1]],n=1),
                    tail(str_split(res$ngram[[2]], "_")[[1]],n=1),
                    tail(str_split(res$ngram[[3]], "_")[[1]],n=1))
          }
          else{
            for(i in (1:nrow(res))){
              nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
            }
          }
        }
        
        else{
          
          res <- search_uni()
          
          for(i in (1:nrow(res))){
            nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
          }
        }
      }
    }
  }# end of wcount = 3
  
  
  # more than 2 word input
  else if(wcount >= 2){
    
    res <- search_tri(inp)
    
    if(nrow(res) > 0){
      if (nrow(res) >= 3){
        nw <- c(tail(str_split(res$ngram[[1]], "_")[[1]],n=1),
                tail(str_split(res$ngram[[2]], "_")[[1]],n=1),
                tail(str_split(res$ngram[[3]], "_")[[1]],n=1))
      }
      else{
        for(i in (1:nrow(res))){
          nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
          }
      }
    }
    
    else{
      
      res <- search_bi(inp)
      
      if(nrow(res) > 0){
        if (nrow(res) >= 3){
          nw <- c(tail(str_split(res$ngram[[1]], "_")[[1]],n=1),
                  tail(str_split(res$ngram[[2]], "_")[[1]],n=1),
                  tail(str_split(res$ngram[[3]], "_")[[1]],n=1))
        }
        else{
          for(i in (1:nrow(res))){
            nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
            }
        }
      }
      
      else{
        
        res <- search_uni()
        
        for(i in (1:nrow(res))){
          nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
        }
      }
    }
  }# end of wcount = 2
  
  
  # more than 1 word input
  else if(wcount>=1){
    
    res <- search_bi(inp)
    
    if (nrow(res) >= 3){
      nw <- c(tail(str_split(res$ngram[[1]], "_")[[1]],n=1),
              tail(str_split(res$ngram[[2]], "_")[[1]],n=1),
              tail(str_split(res$ngram[[3]], "_")[[1]],n=1))
    }
    else{
      for(i in (1:nrow(res))){
        nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
      }
    }
  }# end of wcount = 1
  
  # no word input
  else{
    
    res <- search_uni()
    
    for(i in (1:nrow(res))){
      nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
    }
  
  }
  
  return(nw)
}

head(quad_sh)

###########################################################
# Part 3: predicting top 3 results
###########################################################

st <- Sys.time()

predict_next("what is ")

et <- Sys.time()
print(et - st)

# time to predict : 0.45 secs for n gram table with k = 5
# time to predict : 0.35 - 0.76 secs for n gram table with k = 4
# time to predict : 1.08 secs for n gram table with k = 2
