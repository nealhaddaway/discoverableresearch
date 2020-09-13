# discoverableresearch

A work-in-progress, this package provides a suite of functions to support authors designing discoverable research articles (i.e. easily findable using bibliographic databases that index titles, abstracts and keywords).

The package offers the following functions:
1) format_keywords() - accepts a text string containing keywords separated by a given character and outputs a lower case vector list of strings
2) check_keywords() - checks a set of keywords for an article to assess whether they are already represented in the title and abstract
3) check_title_length() - checks a given title for an article to assess how discoverable it is based on its length and proportion of words that are non-stop words
4) compare_title() - checks given title for an article to assess how discoverable it is based on similarity scores relative to a given sample set of titles
5) get_tokens() - removes stop words from a text
6) suggest_title() - suggests possible title words by extracting uni-, bi-, and tri-grams from a long text (e.g. article full text), having removed punctuation and stopwords. Returns the remaining words as a vector of strings and assesses whether they are already present in the title or abstract
7) suggest_keywords() - suggests possible keywords by extracting uni-, bi-, and tri-grams from a long text (e.g. article full text), having removed punctuation and stopwords. Returns the remaining words as a vector of strings and assesses whether they are already present in the abstract or keywords
