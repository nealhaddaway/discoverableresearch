remotes::install_github("elizagrames/litsearchr", ref="main")

title <- "A methodology for systematic mapping in environmental sciences"
abstract <- "Systematic mapping was developed in social sciences in response to a lack of empirical data when answering questions using systematic review methods, and a need for a method to describe the literature across a broad subject of interest. Systematic mapping does not attempt to answer a specific question as do systematic reviews, but instead collates, describes and catalogues available evidence (e.g. primary, secondary, theoretical, economic) relating to a topic or question of interest. The included studies can be used to identify evidence for policy-relevant questions, knowledge gaps (to help direct future primary research) and knowledge clusters (sub-sets of evidence that may be suitable for secondary research, for example systematic review). Evidence synthesis in environmental sciences faces similar challenges to those found in social sciences. Here we describe the translation of systematic mapping methodology from social sciences for use in environmental sciences. We provide the first process-based methodology for systematic maps, describing the stages involved: establishing the review team and engaging stakeholders; setting the scope and question; setting inclusion criteria for studies; scoping stage; protocol development and publication; searching for evidence; screening evidence; coding; production of a systematic map database; critical appraisal (optional); describing and visualising the findings; report production and supporting information. We discuss the similarities and differences in methodology between systematic review and systematic mapping and provide guidance for those choosing which type of synthesis is most suitable for their requirements. Furthermore, we discuss the merits and uses of systematic mapping and make recommendations for improving this evolving methodology in environmental sciences."
keywords <- c("Systematic mapping", "Evidence-based environmental management", "Systematic evidence synthesis", "Evidence review", "Knowledge gaps", "Knowledge clusters")
fulltext <- readr::read_file("fulltext.txt")
fulltext <- gsub("\n", " ", fulltext)
fulltext <- gsub("\\s+"," ",fulltext)

#' Remove stopwords from text (adapted from litsearchr)
get_tokens <- function(text, language = "English"){
  text <- tolower(text)
  text <- tm::scan_tokenizer(tm::removeWords(text, tm::stopwords(language)))
  return(text)
}

#' Extract uni-, bi-, and tri-grams from text, having removed punctuation and stopwords
suggest_keywords <- function(text, title, abstract, suggest = FALSE){
  y <- get_tokens(fulltext)
  z <- litsearchr::fakerake(fulltext, min_n = 2, max_n = 3)
  w <- cbind(c(z, y))
  words <- litsearchr::remove_punctuation(w)
  words <- gsub("^[^a-zA-Z]+", "\\1", words)
  words <- gsub("[^a-zA-Z]+$", "\\1", words)
  words <- gsub('\\b\\w{1,3}\\b', '', words)
  words <- words[words != ""]
  dat <- as.data.frame(words)
  dat$counts <- sapply(words, function(x) stringi::stri_detect_fixed(words, x)%>%sum())
  dat$title <- logical(length(dat$words))
  for(i in seq_along(dat$words)){ 
    dat$title[i] <- grepl(dat$words[i], tolower(title), fixed = TRUE)
  }
  dat$abstract <- logical(length(dat$words))
  for(i in seq_along(dat$words)){ 
    dat$abstract[i] <- grepl(dat$words[i], tolower(abstract), fixed = TRUE)
  }
  dat$posskw <- paste(dat$title, dat$abstract)
  dat$posskw <- gsub("FALSE FALSE", "Yes, possible keyword candidate", dat$posskw)
  dat$posskw <- gsub("FALSE TRUE", "No, word exists in abstract", dat$posskw)
  dat$posskw <- gsub("TRUE FALSE", "No, word exists in title", dat$posskw)
  dat$posskw <- gsub("TRUE TRUE", "No, word exists in title and abstract", dat$posskw)
  if (suggest == TRUE){
    dat <- subset(dat, posskw == "YES")
    dat <- subset(dat, select = -c(posskw))
  }
  dat <- unique(dat)
  dat <- subset(dat, select = -c(title, abstract))
  return(dat)
}

z <- suggest_keywords(fulltext, title, abstract, suggest = FALSE)
