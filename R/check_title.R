sampletitles <- read.csv("sample_titles_R.csv")
attach(sampletitles)

#' The following code was used to calculate word number and word number following stopword removal (tokenlength)
#' The titles are the result of a PubMed topic word search for 'researc' on 11/09/2020 with an export of the first 
#' 10,000 records sorted by publication date.
#sampletitles <- subset(sampletitles, word.length >= "4") #retain only titles of 4 words or more
#sampletitles$tokenlength <- mapply(get_tokens, sampletitles$title)
#sampletitles$tokenlength <- mapply(length, sampletitles$tokenlength)
#write.csv(sampletitles,"sample_titles_R.csv", row.names = FALSE)
#mean(sampletitles$word.length)
#median(sampletitles$word.length)
#min(sampletitles$word.length)
#max(sampletitles$word.length)
#mean(sampletitles$tokenlength)
#median(sampletitles$tokenlength)
#min(sampletitles$tokenlength)
#max(sampletitles$tokenlength)
#utility <- mean(sampletitles$tokenlength/sampletitles$word.length)
#utilse <- sd(sampletitles$tokenlength/sampletitles$word.length) / sqrt(length(sampletitles$tokenlength/sampletitles$word.length))


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

#devtools::install_github("rmetaverse/metaverse", dependencies = TRUE) #Needed to access synthesisr functions (not yet on CRAN)

compare_title <- function(title, testset, threshold = 0.6, matches = FALSE){
  imported_files <- synthesisr::read_refs(filename = testset)
  titles <- imported_files$title
  sim <- stringdist::stringsim(title, titles)
  dat <- data.frame(sim, imported_files)
  hist(sim, main = "Histogram of similarity scores against test set", xlab = "Similarity score")
  output <- paste(sum(sim > threshold), 
                  if(sum(sim > threshold) == 1){
                    paste(" record from the test set have a similarity score greater than ", threshold, " in comparison to your title. See the histogram for more details. Any records above the similarity threshold of", threshold, "are provided in 'matches'.", sep = "")
                  } else {
                    paste(" records from the test set have a similarity score greater than ", threshold, " in comparison to your title. See the histogram for more details. Any records above the similarity threshold of", threshold, "are provided in 'matches'.", sep = "")
                  },
                  if(sum(sim > threshold) > 1){
                    paste("\nThis indicates that you may need to alter your title wording to improve discoverability. Check out the histogram for more details. Any records above the similarity threshold of ", threshold, " are provided in 'matches'.", sep = "")
                  }, sep = ""
                  )
  if(matches == TRUE){
    matches <<- subset(dat, sim > threshold)
  }
  return(output)
}

v <- compare_title(title, "sample titles.txt", matches = TRUE)
print(v)


