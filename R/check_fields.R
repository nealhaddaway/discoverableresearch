#' Check all field suitability
#'
#' Check given fields (title, abstract and keywords) for an article to assess discoverability based 
#' on similarities across the fields
#' @param title The article title: a short string
#' @param abstract The article abstract: a string
#' @param keywords The article keywords: a vector of strings
#' @return A dataframe displaying the presence of the terms across the title, abstract, and keywords
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
#' check <- check_fields(title, abstract, keywords)
#' check$df
#' check$tit_terms
#' check$abs_terms
#' check$key_terms
#' check$report;
#' @export
check_fields <- function(title, abstract, keywords){
  
  #extract 1-/2-/3- word ngrams from the title and abstract
  tok_tit <- fakerake(title, min_n = 1, max_n = 3)
  tok_abs <- fakerake(abstract, min_n = 1, max_n = 3)
  
  #create a list of unique terms from across the title, abstract and keywords
  unique_tokens <- unique(c(tok_tit, tok_abs, tolower(keywords))) 
  
  # create a data frame showing whether the unique terms are present in the title or abstract or keywords
  tit_words <- NA
  abs_words <- NA
  key_words <- NA
  df <- data.frame(unique_tokens, tit_words = NA, abs_words = NA, key_words = NA)
  # populate the data frame with presence/absence for each term in each field
  df$tit_words <- unique_tokens %in% tok_tit
  df$abs_words <- unique_tokens %in% tok_abs
  df$key_words <- unique_tokens %in% tolower(keywords)
  
  # order the terms alphabetically and remove row names
  df <- df[order(df$unique_tokens),]
  row.names(df) <- NULL
  
  # subset the data frame for terms present in each field
  tit_terms <- subset(df, tit_words == TRUE)
  abs_terms <- subset(df, abs_words == TRUE)
  key_terms <- subset(df, key_words == TRUE)
  
  # subset keywords that also appear in the title and abstract
  poor_keywords_tit <- subset(key_terms, tit_words == TRUE)[,1]
  poor_keywords_abs <- subset(key_terms, abs_words == TRUE)[,1]
  
  # generate a report summarising the findings
  report <- paste(
    if(length(poor_keywords_tit) < 1){
      "None of your keywords appear in the title."
      } else {
        paste("The following keywords appear in the title: ", paste(poor_keywords_tit, collapse = "; "), 
              ". These terms should be modified or replaced to improve discoverability. ", sep = "")
        },
    if(length(poor_keywords_abs) < 1){
      "None of your keywords appear in the abstract"
    } else {
      paste("The following keywords appear in the abstract: ", paste(poor_keywords_abs, collapse = "; "), 
            ". These terms should be modified or replaced to improve discoverability.", sep = "")
    }, sep = ""
    )
  
  # return the overall assessment data frame, the subset data, and the report
  return(list(df = df, tit_terms = tit_terms, abs_terms = abs_terms, key_terms = key_terms, report = report))
}

