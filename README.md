# discoverableresearch

This package provides a suite of functions to support authors designing discoverable research articles (i.e. easily findable using bibliographic databases that index titles, abstracts and keywords).

The package offers the following functions:
1) 'format_keywords()' - accepts a text string containing keywords separated by a given character and outputs a lower case vector list of strings
2) 'check_keywords()' - checks a set of keywords for an article to assess whether they are already represented in the title and abstract
3) 'check_title_length()' - checks a given title for an article to assess how discoverable it is based on its length and proportion of words that are non-stop words
4) 'compare_title()' - checks given title for an article to assess how discoverable it is based on similarity scores relative to a given sample set of titles
5) 'get_tokens()' - removes stop words from a text
6) 'suggest_title()' - suggests possible title words by extracting uni-, bi-, and tri-grams from a long text (e.g. article full text), having removed punctuation and stop words. Returns the remaining words as a vector of strings and assesses whether they are already present in the title or abstract
7) 'suggest_keywords()' - suggests possible keywords by extracting uni-, bi-, and tri-grams from a long text (e.g. article full text), having removed punctuation and stop words. Returns the remaining words as a vector of strings and assesses whether they are already present in the abstract or keywords
8) 'check_fields()' - scans ngrams (1-, 2-, and 3- word phrases) from titles, abstracts and (original, as supplied) keywords to examine overlap to identify terms in the title and keywords that could be amended or replaced to optimise 'discoverability'.


## Example
For example, consider the following record:


`title <- "On the use of keywords and the discoverability of systematic reviews"`

`abstract <- "In this research article, we investigate the use of different strategies for the development of article keywords by academic writers to improve the discoverability of systematic reviews. We appraise the utility of keyword diversity and provide suggestions of how to draft potential keywords for article indexing and cataloging in bibliographic databases in order to improve systematic review discoverability."`

`keywords <- "systematic reviews; discoverability; keywords; indexing; cataloging; bibliographic databases"`


It is immediately obvious that the term 'systematic reviews' exists in the title and the keywords, reducing the 'discoverability' of the record. This package allows the user to automatically identify terms that shoudl be replaced and identify from a full text document, which other terms might be useful replacements. The following example has higher 'discoverability':


`title <- "A research article investigating diverse keyword terminology and systematic reviews discoverability"`

`abstract <-  "In this empirical manuscript, we describe the results of a study to assess the impacts of employing different strategies for the development of article keywording by academic writers to improve the effectiveness of academic searches in retrieving relevant articles, specifically within the context of evidence syntheses. We appraise the utility of terminological diversity and provide suggestions of how to draft potential synonyms for article indexing and cataloging in bibliographic databases in order to improve findability and research impact."`

`keywords <-  "systematic reviewing; systematic literature review; abstracting; open discovery; information retrieval; informatics"`
