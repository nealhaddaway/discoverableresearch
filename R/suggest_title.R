#' Remove stop words from text
#'
#' Removes stop words from a text string (adapted from 'litsearchr' <https://github.com/elizagrames/litsearchr/>) 
#' and returns the remaining words as a vector of strings
#' @param text An input string
#' @param language The language used to look up stop words (default is "English")
#' @return A vector of strings consisting of the non-stop words from the 'text' input
#' @examples 
#' text <- "A methodology for systematic mapping in environmental sciences"
#' tokens <- get_tokens(text)
#' tokens;
#' @importFrom magrittr "%>%"
#' @export
get_tokens <- function(text, language = "English"){
  
  # convert input text to lower case
  text <- tolower(text)
  
  # look up list of stop words based on the specified language and remove them from the text
  language <- language_code(language)
  text <- tm::scan_tokenizer(tm::removeWords(text, tm::stopwords(language)))
  
  # return the text without the stop words
  return(text)
}

#' Suggest title words
#'
#' Suggests possible title words by extracting uni-, 'bi-, and tri-grams from a long text (e.g. 
#' article full text), having removed punctuation and stop words. Returns the remaining words as a 
#' vector of strings and assesses whether they are already present in the title or abstract
#' @param abstract An article abstract
#' @param fulltext An article full text
#' @param keywords An article keywords, supplied as a vector
#' @param suggest A logical argument of TRUE or FALSE. If TRUE, the output data frame returned is 
#' sub-setting to only include potential keywords (i.e. those not already in the abstract or keywords)
#' @return A data frame consisting of potential candidate title words and their suitability. If suggest 
#' = FALSE, only good candidates are returned.
#' @examples 
#' abstract <- "Systematic mapping was developed in social sciences in response to a lack of empirical 
#'   data when answering questions using systematic review methods, and a need for a method to describe 
#'   the literature across a broad subject of interest. Systematic mapping does not attempt to answer 
#'   a specific question as do systematic reviews, but instead collates, describes and catalogues 
#'   available evidence (e.g. primary, secondary, theoretical, economic) relating to a topic or 
#'   question of interest. The included studies can be used to identify evidence for policy-relevant 
#'   questions, knowledge gaps (to help direct future primary research) and knowledge clusters (sub-
#'   sets of evidence that may be suitable for secondary research, for example systematic review). 
#'   Evidence synthesis in environmental sciences faces similar challenges to those found in social 
#'   sciences. Here we describe the translation of systematic mapping methodology from social sciences 
#'   for use in environmental sciences. We provide the first process-based methodology for systematic 
#'   maps, describing the stages involved: establishing the review team and engaging stakeholders; 
#'   setting the scope and question; setting inclusion criteria for studies; scoping stage; protocol 
#'   development and publication; searching for evidence; screening evidence; coding; production of a 
#'   systematic map database; critical appraisal (optional); describing and visualising the findings; 
#'   report production and supporting information. We discuss the similarities and differences in 
#'   methodology between systematic review and systematic mapping and provide guidance for those 
#'   choosing which type of synthesis is most suitable for their requirements. Furthermore, we discuss 
#'   the merits and uses of systematic mapping and make recommendations for improving this evolving 
#'   methodology in environmental sciences."
#' keywords <- c("Systematic mapping", 
#'   "Evidence-based environmental management", 
#'   "Systematic evidence synthesis", 
#'   "Evidence review", 
#'   "Knowledge gaps", 
#'   "Knowledge clusters")
#' filepath <- system.file("extdata", "fulltext.rds", package="discoverableresearch")
#' fulltext <- readRDS(filepath)
#' fulltext <- gsub("\n", " ", fulltext)
#' fulltext <- gsub("\\s+"," ",fulltext)
#' poss_titlewords <- suggest_title(abstract, keywords, fulltext)
#' poss_titlewords;
#' @importFrom magrittr "%>%"
#' @export
suggest_title <- function(abstract, keywords, fulltext, suggest = FALSE){
  
  # extract tokens from full text (removes stop words)
  y <- get_tokens(fulltext)
  
  # extract bi- and tri-grams from full text
  z <- fakerake(fulltext, min_n = 2, max_n = 3)
  
  # bind the candidate terms together in a single vector
  w <- cbind(c(z, y))
  
  # remove punctuation and double spaces
  words <- remove_punctuation(w)
  words <- gsub("^[^a-zA-Z]+", "\\1", words)
  words <- gsub("[^a-zA-Z]+$", "\\1", words)
  words <- gsub('\\b\\w{1,3}\\b', '', words)
  
  # remove blank values
  words <- words[words != ""]
  
  # create new data frame containing the extracted words
  dat <- as.data.frame(words)
  
  # count the number of times each word appears in the full text
  dat$counts <- sapply(words, function(x) stringi::stri_detect_fixed(words, x)%>%
                         sum())
  # look for each word from the full text in the keywords and report if present
  kywrds <- NA
  dat$kywrds <- logical(length(dat$words))
  for(i in seq_along(dat$words)){ 
    dat$kywrds[i] <- grepl(dat$words[i], tolower(keywords), fixed = TRUE)
  }
  
  # look for each word from the full text in the abstract and report if present
  dat$abstract <- logical(length(dat$words))
  for(i in seq_along(dat$words)){ 
    dat$abstract[i] <- grepl(dat$words[i], tolower(abstract), fixed = TRUE)
  }
  
  # concatenate assessments for abstract and keywords and generate report text for each row
  posstw <- NA
  dat$posstw <- paste(dat$abstract, dat$kywrds)
  dat$posstw <- gsub("FALSE FALSE", "Yes, possible title candidate", dat$posstw)
  dat$posstw <- gsub("FALSE TRUE", "No, word exists in keywords", dat$posstw)
  dat$posstw <- gsub("TRUE FALSE", "No, word exists in abstract", dat$posstw)
  dat$posstw <- gsub("TRUE TRUE", "No, word exists in abstract and keywords", dat$posstw)
  
  # if suggest = TRUE, then subset the data to show only those terms not already present in the 
  # abstract or keywords
  if (suggest == TRUE){
    dat <- subset(dat, substr(posstw, 1, 3) == "Yes")
    dat <- subset(dat, select = -c(posstw))
  }
  
  # remove duplicates
  dat <- unique(dat)
  dat <- subset(dat, select = -c(abstract, kywrds))
  
  # return the data frame
  return(dat)
}