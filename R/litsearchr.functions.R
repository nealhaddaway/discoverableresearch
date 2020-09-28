#' Functions from litsearchr (not yet on CRAN)


#' Quick keyword extraction
#' @description Extracts potential keywords from text separated by stop words
#' @param text A string object to extract terms from
#' @param stopwords A character vector of stop words to remove
#' @param min_n Numeric: the minimum length ngram to consider
#' @param max_n Numeric: the maximum length ngram to consider
#' @return A character vector of potential keywords
#' @export
fakerake <- function(text,
                     stopwords,
                     min_n = 2,
                     max_n = 5) {
  if (missing(stopwords)) {
    stopwords <- get_stopwords()
  }
  
  stops <- unique(append(
    stopwords,
    c(
      ",",
      "\\.",
      ":",
      ";",
      "\\[",
      "\\]",
      "/",
      "\\(",
      "\\)",
      "\"",
      "&",
      "=",
      "<",
      ">",
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      0
    )
  ))
  
  # text <- remove_punctuation(text, preserve_punctuation = c("-", "_"))
  stop1 <- paste(" ", stops[1], " ", sep = "")
  text <- gsub("([-_])|[[:punct:]]", stop1, text)
  
  if (any(grepl("  ", text))) {
    while (any(grepl("  ", text))) {
      text <- gsub("  ", " ", text)
    }
  }
  
  text <- tolower(text)
  
  n_lengths <- seq(min_n, max_n, 1)
  for (i in min_n:max_n) {
    if (i == min_n) {
      ngrams <-
        lapply(text,
               get_ngrams,
               n = i,
               stop_words = stops)
    } else{
      ngrams <-
        Map(c,
            ngrams,
            lapply(text,
                   get_ngrams,
                   n = i,
                   stop_words = stops))
    }
  }
  
  terms <- unlist(ngrams)
  return(terms)
}

#' Retrieve stop words for a given language
#'
#' @description This function retrieves stop words to use for a specified language.
#' @param language A character vector containing the name of the language for which to retrieve stop words. Defaults to "English"
#' @return Returns a character vector of stop words.
#' @examples get_stopwords("English")
#' @export
get_stopwords <- function(language = "English"){
  if(!requireNamespace("stopwords")){
    stop("Package 'stopwords' needed for this function to work. Please install it.")
  }
  if(!any(possible_langs$Language==language)){stop("The language you specified is not supported.")} else {
    la_code <- language_code(language)
  }
  
  if(length(la_code) > 0){
    
    if(la_code=="en"){stopwords <- stopwords::stopwords("en", source="smart")
    } else if(any(stopwords::stopwords_getlanguages("snowball")==la_code)){
      stopwords <- stopwords::stopwords(la_code, source="snowball")
    } else if (any(stopwords::stopwords_getlanguages("stopwords-iso")==la_code)){
      stopwords <- stopwords::stopwords(la_code, source="stopwords-iso")
    } else {stop("The language you specified is not supported.")}
  }
  return(stopwords)
}

#' Remove stopwords from text
#'
#' @description Removes stopwords from text in whichever language is specified.
#' @param text A character vector containing text from which to remove stopwords.
#' @param language A string indicating the language of the text.
#' @return Returns the input text with stopwords removed.
#' @examples get_tokens("On the Origin of Species", language="English")
#' @export
get_tokens <- function(text, language = "English"){
  language <- language_code(language)
  text <- tm::scan_tokenizer(tm::removeWords(text, tm::stopwords(language)))
  return(text)
}


#' Extract n-grams from text
#'
#' @description This function extracts n-grams from text.
#' @param x A character vector from which to extract n-grams.
#' @param n Numeric: the minimum number of terms in an n-gram.
#' @param min_freq Numeric: the minimum number of times an n-gram must occur to be returned.
#' @param ngram_quantile Numeric: what quantile of ngrams should be retained. Defaults to 0.8; i.e. the 80th percentile of ngram frequencies.
#' @param stop_words A character vector of stopwords to ignore.
#' @param rm_punctuation Logical: should punctuation be removed before selecting ngrams?
#' @param preserve_chars A character vector of punctuation marks to be retained if rm_punctuation is TRUE.
#' @param language A string indicating the language to use for removing stopwords.
#' @return A character vector of n-grams.
#' @examples get_ngrams("On the Origin of Species By Means of Natural Selection")
#' @export
get_ngrams <- function(x, n=2, min_freq=1, ngram_quantile=NULL, stop_words, rm_punctuation=FALSE, preserve_chars=c("-", "_"), language="English"){
  
  if (missing(stop_words)) {
    if(missing(language)){
      language <- "English"
    }
    stop_words <- get_stopwords(language)
  }
  
  ngram_x <- x[!is.na(x)]
  ngram_x <- ngram_x[unlist(lapply(ngram_x, ngram::wordcount)) >= n]
  if (length(ngram_x) > 0) {
    ngrams <- ngram::get.phrasetable(ngram::ngram(ngram_x, n = n))
    
    if(!is.null(min_freq)){
      ngrams <- ngrams[ngrams$freq >= min_freq,]
    }else if(!is.null(ngram_quantile)){
      ngrams <- ngrams[ngrams$freq > stats::quantile(ngrams$freq,
                                                     ngram_quantile),]
    }
    
    if (nrow(ngrams) > 0) {
      ngram_list <- strsplit(ngrams$ngrams, " ")
      
      ngram_df <- as.data.frame(do.call(rbind, ngram_list),
                                stringsAsFactors = FALSE)
      
      keep_rows <- apply(ngram_df[, 1:n, drop=FALSE], 1, function(a,
                                                                  sw) {
        all(nchar(a) > 4) & !any(a %in% sw)
      }, sw = stop_words)
      if (any(keep_rows)) {
        ngram_df <- ngram_df[keep_rows, , drop=FALSE]
        ngrams <- apply(ngram_df, 1, function(a) {
          paste(a, collapse = " ")
        })
        if(rm_punctuation){
          ngrams <- remove_punctuation(ngrams, preserve_punctuation = preserve_chars)
        }
        return(ngrams)
        
      }
    }
  }
}


#' Remove punctuation from text
#'
#' @description Removes common punctuation marks from a text.
#' @param text A character vector from which to remove punctuation.
#' @param preserve_punctuation A string or vector of punctuation to retain
#' @return Returns the input text with punctuation removed.
#' @examples remove_punctuation("#s<<<//<y>!&^n$$t/>h%e&s$is#!++r!//")
#' @export
remove_punctuation <- function(text, preserve_punctuation = NULL){
  
  if (!is.null(preserve_punctuation)){
    retain <-
      paste("([",
            paste(preserve_punctuation, collapse = ""),
            "])|[[:punct:]]",
            collapse = "")
    retain <- gsub(" ", "", retain)
    output <- gsub(retain, "\\1 ", text, perl=TRUE)
    for(i in 1:length(preserve_punctuation)){
      output <- gsub(paste(preserve_punctuation[i], " ", sep=""), preserve_punctuation[i], output)
      if(any(grepl(paste(" ", preserve_punctuation[i], sep=""), output))){
        output <- gsub(paste(" ", preserve_punctuation[i], sep=""), preserve_punctuation[i], output)
      }
    }
    if(any(grepl(" -", output))){
      while(any(grepl(" -", output))){
        output <- gsub(" -", "-", output)
      }
    }
    
    if(any(grepl("  ", output))){
      while(any(grepl("  ", output))){
        output <- gsub("  ", " ", output)
      }
    }
  } else{
    output <- tm::removePunctuation(text)
  }
  
  return(output)
  
}


#' Get short language codes
#'
#' @description This is a lookup function that returns the two-letter language code for specified language.
#' @param language A character vector containing the name of a language.
#' @return Returns a character vector containing a two-letter language code.
#' @examples language_code("French")
#' @export
language_code <- function(language){
  if(nchar(language == 2)){la_code <- tolower(language)}
  if(nchar(language) > 2){
    la_code <- as.character(possible_langs$Short[which(tolower(possible_langs$Language)==tolower(language))])
  }
  return(la_code)
}