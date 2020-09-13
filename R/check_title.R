#' Wrapper function for check_titles
#'
#' Wrapper function that runs check_title_length() and compare_title() together
#' @param title The article title: a short string
#' @return An output describing the suitability of the title for research discovery based on its length and the number of non-stop words
#' @examples 
#' title <- "A methodology for systematic mapping in environmental sciences"
#' check <- check_title_length(title)
#' check;
#' @export
check_title_length <- function(title){
  title <- tolower(title)
  titlelength <- sapply(strsplit(title, " "), length)
  tokens <- get_tokens(title)
  tokenlength <- length(tokens)
  output <- paste("Your title contains ", titlelength, " words in total; ", tokenlength, " of these words are useful for research discovery (i.e. they are not 'stop words'). This compares to a median of 7 total words (range: 4 to 53) and 5 useful search words (range: 1 to 40) in a sample of bibliographic records in PubMed (see documentation for details).", 
                  if(titlelength <= 7){
                    " Your title could therefore be made longer to improve the likelihood of it being discovered in a search."
                  } else {
                    "Your title is therefore somewhat longer than average in total length.\n\n"
                  }, "Your title has a proportional word utility of ", round(tokenlength/titlelength, 2), ", which compares to a mean word utility of 0.73 (SD: 0.002) in the PubMed sample.",
                  if(tokenlength/titlelength <= 0.73){
                    " You could therefore improve your title by increasing the number of useful terms (i.e. non-stop words)."
                  } else {
                    " Your title has a relatively good utility."
                  }
                  , sep = "")
  return(cat(output))
}


#' Check title suitability
#'
#' Check given tile for an article to assess how discoverable it is
#' @param title The article title: a short string
#' @return An output describing the suitability of the title for research discovery based on its length and the number of non-stop words
#' @examples 
#' title <- "A methodology for systematic mapping in environmental sciences"
#' check <- check_title_length(title)
#' check;
#' @export
check_title_length <- function(title){
  title <- tolower(title)
  titlelength <- sapply(strsplit(title, " "), length)
  tokens <- get_tokens(title)
  tokenlength <- length(tokens)
  output <- paste("Your title contains ", titlelength, " words in total; ", tokenlength, " of these words are useful for research discovery (i.e. they are not 'stop words'). This compares to a median of 7 total words (range: 4 to 53) and 5 useful search words (range: 1 to 40) in a sample of bibliographic records in PubMed (see documentation for details).", 
                  if(titlelength <= 7){
                    " Your title could therefore be made longer to improve the likelihood of it being discovered in a search."
                  } else {
                    "Your title is therefore somewhat longer than average in total length.\n\n"
                  }, "Your title has a proportional word utility of ", round(tokenlength/titlelength, 2), ", which compares to a mean word utility of 0.73 (SD: 0.002) in the PubMed sample.",
                  if(tokenlength/titlelength <= 0.73){
                    " You could therefore improve your title by increasing the number of useful terms (i.e. non-stop words)."
                  } else {
                    " Your title has a relatively good utility."
                  }
                  , sep = "")
  return(cat(output))
}

#' Compare title with those from a testset
#'
#' Check given tile for an article to assess how discoverable it is
#' @param title The article title: a short string
#' @param testset A provided sample set of representative titles to compare with, entered as a .bib or .ris file
#' @param threshold A threshold between 0 and 1 for the similarity score of titles in the sample set relative to the title provided,
#' above which matching titles will be printed out in 'matches'. Default threshold set to 0.6 (arbitrarily)
#' @param matches Logical argument TRUE or FALSE. If TRUE, the matches with a similarity score above the threshold are printed to a dataframe 
#' ('matches'). If FALSE, no output is provided.
#' @return An output describing the suitability of the title for research discovery based on a comparison with the sample set
#' @examples 
#' title <- "A methodology for systematic mapping in environmental sciences"
#' testset <- "sample titles.txt"
#' check <- compare_title(title, testset = testset, threshold = 0.7, matches = TRUE)
#' check;
#' @export
compare_title <- function(title, testset = "data/sample_titles.txt", threshold = 0.6, matches = FALSE){
  imported_files <- synthesisr::read_refs(filename = testset)
  titles <- imported_files$title
  sim <- stringdist::stringsim(title, titles)
  dat <- data.frame(sim, imported_files)
  hist(sim, main = "Histogram of similarity scores against test set", xlab = "Similarity score",xlim = c(0, 1), col = col1)
  output <- paste(sum(sim > threshold), 
                  if(sum(sim > threshold) == 1){
                    paste(" record from the test set have a similarity score greater than ", threshold, " in comparison to your title. See the histogram for more details. Any records above the similarity threshold of ", threshold, " are provided in the exported dataframe object.", sep = "")
                  } else {
                    paste(" records from the test set have a similarity score greater than ", threshold, " in comparison to your title. See the histogram for more details. Any records above the similarity threshold of ", threshold, " are provided in the exported dataframe object.", sep = "")
                  },
                  if(sum(sim > threshold) > 1){
                    paste("\nThis indicates that you may need to alter your title wording to improve discoverability. Check out the histogram for more details. Any records above the similarity threshold of ", threshold, " are provided in the exported dataframe object.", sep = "")
                  }, sep = ""
                  )
  if(matches == TRUE){
    matchdat <- subset(dat, sim > threshold)
    print(output)
    return(matchdat)
  }
  return(output)
}