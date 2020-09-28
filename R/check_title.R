#' Check title suitability
#'
#' Check given title for an article to assess how discoverable it is based on its length and proportion of words that are non-stop words
#' @param title The article title: a short string
#' @return An output describing the suitability of the title for research discovery based on its length and the number of non-stop words
#' @examples 
#' title <- "A methodology for systematic mapping in environmental sciences"
#' check <- check_title_length(title)
#' check;
#' @export
check_title_length <- function(title){
  
  # calculate the total word length of the title
  title <- tolower(title)
  titlelength <- sapply(strsplit(title, " "), length)
  
  # remove stop words and calculate the length of the title without stop words
  tokens <- get_tokens(title)
  tokenlength <- length(tokens)
  
  # generate a report comparing total title length and token length compared to a random sample of records from 'PubMed'
  output <- paste("Your title contains ", titlelength, " words in total; ", 
                  tokenlength, 
                  " of these words are useful for research discovery (i.e. they are not 'stop words'). ", "
                  This compares to a median of 7 total words (range: 4 to 53) ", 
                  "and 5 useful search words (range: 1 to 40) in a sample of bibliographic records in PubMed (see documentation for details). ", 
                  if(titlelength <= 7){
                    " Your title could therefore be made longer to improve the likelihood of it being discovered in a search. "
                  } else {
                    "Your title is therefore somewhat longer than average in total length. "
                  }, "Your title has a proportional word utility of ", 
                  round(tokenlength/titlelength, 2), 
                  ", which compares to a mean word utility of 0.73 (SD: 0.002) in the PubMed sample.",
                  if(tokenlength/titlelength <= 0.73){
                    " You could therefore improve your title by increasing the number of useful terms (i.e. non-stop words)."
                  } else {
                    " Your title has a relatively good discoverability"
                  }
                  , sep = "")
  
  # return the report
  return(output)
}

#' Check title with those from a test set
#'
#' Check given title for an article to assess how discoverable it is
#' @param title The article title: a short string
#' @param testset A provided sample set of representative titles to compare with, entered as a .bib 
#' or .ris file (or using the RIS .txt file in data as specified in the example below)
#' @param threshold A threshold between 0 and 1 for the similarity score of titles in the sample 
#' set relative to the title provided, above which matching titles will be printed out in 'matches'. 
#' Default threshold set to 0.6 (arbitrarily)
#' @param matches Logical argument TRUE or FALSE. If TRUE, the matches with a similarity score above 
#' the threshold are printed to a data frame ('matches'). If FALSE, no output is provided.
#' @param plot Logical argument TRUE or FALSE. If TRUE, a histogram of the similarity scores of test
#' set titles compared to the title is plotted.
#' @return A report describing the suitability of the title for research discovery based on a 
#' comparison with the test set. If 'matches = TRUE', a list containing a report describing the 
#' suitability of the title for research discovery based on a comparison with the test set and a 
#' database containing matches with a similarity score above the threshold value.
#' @examples 
#' title <- "A methodology for systematic mapping in environmental sciences"
#' testset <- system.file("extdata", "sample_titles.txt", package="discoverableresearch")
#' check <- check_title(title, testset = testset, threshold = 0.7, matches = TRUE, plot = TRUE)
#' check$output
#' check$dat;
#' @export
check_title <- function(title, testset, threshold = 0.6, matches = FALSE, plot = TRUE){
  
  # load a test set of bibliographic records and extract the titles
  imported_files <- synthesisr::read_refs(filename = testset)
  titles <- imported_files$title
  
  # calculate a similarity score between the title and the uploaded test set titles
  sim <- stringdist::stringsim(title, titles)
  
  # create a data frame containing the similarity scores for each test set article
  dat <- data.frame(sim, imported_files)
  
  # generate a report based on the similarity scores for the test set compared to the title
  output <- paste(sum(sim > threshold), 
                  if(sum(sim > threshold) == 1){
                    paste(" record from the test set have a similarity score greater than ", 
                          threshold, 
                          " in comparison to your title. See the histogram for more details. ",
                          "Any records above the similarity threshold of ", 
                          threshold, 
                          " are provided in the exported dataframe object.", sep = "")
                  } else {
                    paste(" records from the test set have a similarity score greater than ", 
                          threshold, 
                          " in comparison to your title. See the histogram for more details. ",
                          "Any records above the similarity threshold of ", 
                          threshold, 
                          " are provided in the exported dataframe object.", sep = "")
                  },
                  if(sum(sim > threshold) > 1){
                    paste("\nThis indicates that you may need to alter your title wording to improve discoverability.", 
                          "Check out the histogram for more details. ",
                          "Any records above the similarity threshold of ", 
                          threshold, 
                          " are provided in the exported dataframe object (use 'matches = TRUE' to return the dataframe).", 
                          sep = "")
                  }, sep = ""
                  )
  
  # if plot = TRUE, plot a histogram of similarity scores
  if (plot == TRUE){
  hist1 <- graphics::hist(sim, 
                          main = "Histogram of similarity scores against test set", 
                          xlab = "Similarity score", 
                          xlim = c(0, 1))
  }
  
  # if matches = TRUE, return a list containing the output and the subset data frame of matches
  if(matches == TRUE){
    dat <- subset(dat, sim > threshold)
    
    # if no titles exceed the similarity threshold, a note is given instead of the blank data frame
    if (nrow(dat) <1){
      dat <- "No records to return, similarity threshold not exceeded"
    }
    
    # return a list containing the output, the subset of data with similarity scores above the threshold, 
    # and the histogram
    return(list(output = output, dat = dat))
  } else {
    
  # return the output report
  return(output)
  }
}