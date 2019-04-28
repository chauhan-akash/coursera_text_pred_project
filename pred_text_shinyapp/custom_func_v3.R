clean_text <- function(s, bw){
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

search_quad <- function(s, quad_sh){
  s <- tail(str_split(s, " ")[[1]],n=3)
  s <- paste("^",paste(paste(s[1], s[2], s[3], sep = "_"),"_", sep=""), sep = "")
  res <- quad_sh[grepl(s, quad_sh$ngram),]
  return(res)
}


search_tri <- function(s, tri_sh){
  s <- tail(str_split(s, " ")[[1]],n=2)
  s <- paste("^",paste(paste(s[1], s[2], sep = "_"),"_", sep=""), sep = "")
  res <- tri_sh[grepl(s, tri_sh$ngram),]
  return(res)
}

search_bi <- function(s, bi_sh){
  s <- tail(str_split(s, " ")[[1]],n=1)
  s <- paste("^",paste(s[1],"_", sep=""), sep = "")
  res <- bi_sh[grepl(s, bi_sh$ngram),]
  return(res)
}

search_uni <- function(uni_sh){
  res <- uni_sh[1:3,]
  return(res)
}

predict_next <- function(inp, uni, bi, tri, quad, b){
  inp <- clean_text(inp, b)
  wcount <- length(strsplit(inp, " ")[[1]])
  nw <- str(NULL)
  
  if(wcount >= 3){
    
    res <- search_quad(inp, quad)
    
    if(nrow(res) > 0){
      if(nrow(res) >= 3){
        nw <- c(tail(str_split(res$ngram[[1]], "_")[[1]],n=1),
                tail(str_split(res$ngram[[2]], "_")[[1]],n=1),
                tail(str_split(res$ngram[[3]], "_")[[1]],n=1))
      }# end of nrow > 3
      else{
        for(i in (1:nrow(res))){
          nw <- c(nw,tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
        }
      }# end of nrow < 3
    }# end of nrow > 0
    
    else{
      
      res <- search_tri(inp, tri)
      
      if(nrow(res) > 0){
        if (nrow(res) >= 3){
          nw <- c(tail(str_split(res$ngram[[1]], "_")[[1]],n=1),
                  tail(str_split(res$ngram[[2]], "_")[[1]],n=1),
                  tail(str_split(res$ngram[[3]], "_")[[1]],n=1))
        }
        else{
          for(i in (1:nrow(res))){
            nw <- c(nw,tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
          }
        }
      }
      
      else{
        
        res <- search_bi(inp, bi)
        
        if(nrow(res) > 0){
          if (nrow(res) >= 3){
            nw <- c(tail(str_split(res$ngram[[1]], "_")[[1]],n=1),
                    tail(str_split(res$ngram[[2]], "_")[[1]],n=1),
                    tail(str_split(res$ngram[[3]], "_")[[1]],n=1))
          }
          else{
            for(i in (1:nrow(res))){
              nw <- c(nw,tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
            }
          }
        }
        
        else{
          
          res <- search_uni(uni)
          
          for(i in (1:nrow(res))){
            nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
          }
        }
      }
    }
  }# end of wcount = 3
  
  
  # more than 2 word input
  else if(wcount >= 2){
    
    res <- search_tri(inp, tri)
    
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
      
      res <- search_bi(inp, bi)
      
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
        
        res <- search_uni(uni)
        
        for(i in (1:nrow(res))){
          nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
        }
      }
    }
  }# end of wcount = 2
  
  
  # more than 1 word input
  else if(wcount>=1){
    
    res <- search_bi(inp, bi)
    
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
      res <- search_uni(uni)
      
      for(i in (1:nrow(res))){
        nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
      }
    }
  }# end of wcount = 1
  
  # no word input
  else{
    
    res <- search_uni(uni)
    
    for(i in (1:nrow(res))){
      nw <- c(nw, tail(str_split(res$ngram[[i]], "_")[[1]],n=1))
    }
    
  }
  
  return(nw)
}