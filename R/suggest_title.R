#' Remove stopwords from text
#'
#' Removes stopwords from a text string (adapted from litsearchr) and returns the remaining words as 
#' a vector of strings
#' @param text An input string
#' @param language The language used to lookup stop words (default is "English")
#' @return A vector of strings consisting of the non-stop words from the 'text' input
#' @examples 
#' text <- "A methodology for systematic mapping in environmental sciences"
#' tokens <- get_tokens(text)
#' tokens;
#' @importFrom magrittr "%>%"
#' @export
get_tokens <- function(text, language = "English"){
  text <- tolower(text)
  text <- tm::scan_tokenizer(tm::removeWords(text, tm::stopwords(language)))
  return(text)
}

#' Suggest title words
#'
#' Suggests possible title words by extracting uni-, bi-, and tri-grams from a long text (e.g. article 
#' full text), having removed punctuation and stopwords. Returns the remaining words as a vector of 
#' strings and assesses whether they are already present in the title or abstract
#' @param abstract An article abstract
#' @param fulltext An article full text
#' @param keywords An article keywords, supplied as a vector
#' @param suggest A logical argument of TRUE or FALSE. If TRUE, the output dataframe returned is 
#' subsetting to only include potential keywords (i.e. those not already in the abstract or keywords)
#' @return A dataframe consisting of potential candidate title words and their suitability. If suggest 
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
  y <- get_tokens(fulltext)
  z <- litsearchr::fakerake(fulltext, min_n = 2, max_n = 3)
  w <- cbind(c(z, y))
  words <- litsearchr::remove_punctuation(w)
  words <- gsub("^[^a-zA-Z]+", "\\1", words)
  words <- gsub("[^a-zA-Z]+$", "\\1", words)
  words <- gsub('\\b\\w{1,3}\\b', '', words)
  words <- words[words != ""]
  dat <- as.data.frame(words)
  dat$counts <- sapply(words, function(x) stringi::stri_detect_fixed(words, x)%>%
                         sum())
  kywrds <- NA
  dat$kywrds <- logical(length(dat$words))
  for(i in seq_along(dat$words)){ 
    dat$kywrds[i] <- grepl(dat$words[i], tolower(keywords), fixed = TRUE)
  }
  dat$abstract <- logical(length(dat$words))
  for(i in seq_along(dat$words)){ 
    dat$abstract[i] <- grepl(dat$words[i], tolower(abstract), fixed = TRUE)
  }
  posstw <- NA
  dat$posstw <- paste(dat$abstract, dat$kywrds)
  dat$posstw <- gsub("FALSE FALSE", "Yes, possible title candidate", dat$posstw)
  dat$posstw <- gsub("FALSE TRUE", "No, word exists in keywords", dat$posstw)
  dat$posstw <- gsub("TRUE FALSE", "No, word exists in abstract", dat$posstw)
  dat$posstw <- gsub("TRUE TRUE", "No, word exists in abstract and keywords", dat$posstw)
  if (suggest == TRUE){
    dat <- subset(dat, substr(posstw, 1, 3) == "Yes")
    dat <- subset(dat, select = -c(posstw))
  }
  dat <- unique(dat)
  dat <- subset(dat, select = -c(abstract, kywrds))
  return(dat)
}