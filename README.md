# discoverableresearch

A work-in-progress, this package provides a suite of functions to support authors designing discoverable research articles (i.e. easily findable using bibliographic databases that index titles, abstracts and keywords).

The package offers the following functions:
1) format_keywords() - this function accepts a text string containing keywords separated by a given character and outputs a lower case vector list of strings
2) check_keywords() - this function checks a set of keywords for an article to assess whether they are already represented in the title and abstract
3) check_title_length() - this function checks a given title for an article to assess how discoverable it is based on its length and proportion of words that are non-stop words
4) compare_title() - checks given title for an article to assess how discoverable it is
