#' Check keyword suitability
#'
#' Check given keywords for an article to assess whether they are already represented in the title and 
#' abstract
#' @param title The article title: a short string
#' @param abstract The article abstract: a string
#' @param keywords The article keywords: a vector of strings
#' @return A dataframe displaying the presence of the keywords in the title and abstract
#' @examples 
#' title <- "A methodology for systematic mapping in environmental sciences"
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
#' check <- check_keywords(title, abstract, keywords)
#' check;
#' @export
check_keywords <- function(title, abstract, keywords){
  
  # create a blank data frame to store the results
  keywordoverlap <- data.frame(term = keywords)
  
  # loop through the keywords and state overlapping terms
  for(i in seq_along(keywords)){ 
    keywordoverlap$title[i] <- grepl(keywords[i], tolower(title), fixed = TRUE)
  }
  for(i in seq_along(keywords)){ 
    keywordoverlap$abstract[i] <- grepl(keywords[i], tolower(abstract), fixed = TRUE)
  }
  
  # concatenate title and abstract assessment, then output summary, replacing the working columns
  keywordoverlap$posskw <- paste(keywordoverlap$title, keywordoverlap$abstract)
  keywordoverlap$posskw <- gsub("FALSE FALSE", "Not present, possible keyword candidate", keywordoverlap$posskw)
  keywordoverlap$posskw <- gsub("FALSE TRUE", "No, word exists in abstract", keywordoverlap$posskw)
  keywordoverlap$posskw <- gsub("TRUE FALSE", "No, word exists in title", keywordoverlap$posskw)
  keywordoverlap$posskw <- gsub("TRUE TRUE", "No, word exists in title and abstract", keywordoverlap$posskw)
  keywordoverlap <- subset(keywordoverlap, select = -c(title, abstract))
  
  return(keywordoverlap)
  
}


#' Format input keywords
#'
#' Convert string of keywords with separator into a vector
#' @param keywords The article keywords: a vector of strings
#' @param sep Character that separates keywords in a single string
#' @return A vector of lowercase keywords
#' @examples 
#' keywords <- c("Systematic mapping; 
#'   Evidence-based environmental management; 
#'   Systematic evidence synthesis; 
#'   Evidence review; 
#'   Knowledge gaps; 
#'   Knowledge clusters")
#' newkeywords <- format_keywords(keywords, sep = ";")
#' newkeywords;
#' @export
format_keywords <- function(keywords, sep = ";"){
  
  # if a ';' separator is present, then separate terms by this
  if (grepl(";", keywords) == TRUE){
    keywords <- as.vector(strsplit(keywords, ";"))
    keywords <- unlist(keywords)
    keywords <- trimws(tolower(keywords))
    
    # output a lower case vector of keywords
    return(keywords)
    
  } else {
    
    #otherwise, separate terms based on the user-specified separator character
    keywords <- as.vector(strsplit(keywords, sep))
    keywords <- unlist(keywords)
    keywords <- trimws(tolower(keywords))
    
    # output a lower case vector of keywords
    return(keywords)
  } 
}