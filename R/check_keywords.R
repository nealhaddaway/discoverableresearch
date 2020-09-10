check_keywords <- function(title, abstract, keywords){
  
  keywordoverlap <- data.frame(matrix(ncol = 2, nrow = 0)) #create a blank dataframe to store the results
  x <- c("term","field")
  colnames(keywordoverlap) <- x
  
  for (i in keywords){ #loop through the keywords one at a time to see if they are in the title
    if(grepl(tolower(i), tolower(title))==TRUE){
      keywordoverlap <- rbind(keywordoverlap, c(tolower(i), "title"))
      names(keywordoverlap) <- x
    }
  }
  
  for (i in keywords){ #loop through the keywords one at a time to see if they are in the abstract  
    if(grepl(tolower(i), tolower(abstract))==TRUE){
      keywordoverlap <- rbind(keywordoverlap, c(tolower(i), "abstract"))
      names(keywordoverlap) <- x
    }
  }
  return(keywordoverlap)
  
}


format_keywords <- function(keywords, sep = ";"){
  if (grepl(";", keywords) == TRUE){
    keywords <- as.vector(strsplit(keywords, ";"))
    keywords <- unlist(keywords)
    keywords <- trimws(tolower(keywords))
    return(keywords)
  } else {
    keywords <- as.vector(strsplit(keywords, sep))
    keywords <- unlist(keywords)
    keywords <- trimws(tolower(keywords))
    return(keywords)
  } 
}


keywordoverlap <- check_keywords(title, abstract, keywords)

title <- "A methodology for systematic mapping in environmental sciences"
abstract <- "Systematic mapping was developed in social sciences in response to a lack of empirical data when answering questions using systematic review methods, and a need for a method to describe the literature across a broad subject of interest. Systematic mapping does not attempt to answer a specific question as do systematic reviews, but instead collates, describes and catalogues available evidence (e.g. primary, secondary, theoretical, economic) relating to a topic or question of interest. The included studies can be used to identify evidence for policy-relevant questions, knowledge gaps (to help direct future primary research) and knowledge clusters (sub-sets of evidence that may be suitable for secondary research, for example systematic review). Evidence synthesis in environmental sciences faces similar challenges to those found in social sciences. Here we describe the translation of systematic mapping methodology from social sciences for use in environmental sciences. We provide the first process-based methodology for systematic maps, describing the stages involved: establishing the review team and engaging stakeholders; setting the scope and question; setting inclusion criteria for studies; scoping stage; protocol development and publication; searching for evidence; screening evidence; coding; production of a systematic map database; critical appraisal (optional); describing and visualising the findings; report production and supporting information. We discuss the similarities and differences in methodology between systematic review and systematic mapping and provide guidance for those choosing which type of synthesis is most suitable for their requirements. Furthermore, we discuss the merits and uses of systematic mapping and make recommendations for improving this evolving methodology in environmental sciences."
keywords <- c("Systematic mapping", "Evidence-based environmental management", "Systematic evidence synthesis", "Evidence review", "Knowledge gaps", "Knowledge clusters")

format_keywords(keywords2)
