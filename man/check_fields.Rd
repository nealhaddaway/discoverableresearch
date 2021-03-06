% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_fields.R
\name{check_fields}
\alias{check_fields}
\title{Check all field suitability}
\usage{
check_fields(title, abstract, keywords)
}
\arguments{
\item{title}{The article title: a short string}

\item{abstract}{The article abstract: a string}

\item{keywords}{The article keywords: a vector of strings}
}
\value{
A dataframe displaying the presence of the terms across the title, abstract, and keywords
}
\description{
Check given fields (title, abstract and keywords) for an article to assess discoverability based
on similarities across the fields
}
\examples{
title <- "A methodology for systematic mapping in environmental sciences"
abstract <- "Systematic mapping was developed in social sciences in response to a lack of empirical 
  data when answering questions using systematic review methods, and a need for a method to describe 
  the literature across a broad subject of interest. Systematic mapping does not attempt to answer 
  a specific question as do systematic reviews, but instead collates, describes and catalogues 
  available evidence (e.g. primary, secondary, theoretical, economic) relating to a topic or 
  question of interest. The included studies can be used to identify evidence for policy-relevant 
  questions, knowledge gaps (to help direct future primary research) and knowledge clusters (sub-
  sets of evidence that may be suitable for secondary research, for example systematic review). 
  Evidence synthesis in environmental sciences faces similar challenges to those found in social 
  sciences. Here we describe the translation of systematic mapping methodology from social sciences 
  for use in environmental sciences. We provide the first process-based methodology for systematic 
  maps, describing the stages involved: establishing the review team and engaging stakeholders; 
  setting the scope and question; setting inclusion criteria for studies; scoping stage; protocol 
  development and publication; searching for evidence; screening evidence; coding; production of a 
  systematic map database; critical appraisal (optional); describing and visualising the findings; 
  report production and supporting information. We discuss the similarities and differences in 
  methodology between systematic review and systematic mapping and provide guidance for those 
  choosing which type of synthesis is most suitable for their requirements. Furthermore, we discuss 
  the merits and uses of systematic mapping and make recommendations for improving this evolving 
  methodology in environmental sciences."
keywords <- c("Systematic mapping", 
  "Evidence-based environmental management", 
  "Systematic evidence synthesis", 
  "Evidence review", 
  "Knowledge gaps", 
  "Knowledge clusters")
check <- check_fields(title, abstract, keywords)
check$df
check$tit_terms
check$abs_terms
check$key_terms
check$report;
}
