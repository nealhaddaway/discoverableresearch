#' Languages codes synthesisr can recognize
#'
#' A dataset of the languages that can be recognized by
#' synthesisr along with their short form, character encoding,
#' and whether a scientific journal indexed in 'ulrich' uses them.
#'
#' @source 'litsearchr' package on 'Github'
#' @format A database with 53 rows of 4 variables:
#' \describe{
#'   \item{Short}{the short form language code}
#'   \item{Language}{the name of the language}
#'   \item{Encoding}{which character encoding to use for a language}
#'   \item{Used}{whether or not the language is used by a scientific journal}
#' }
#' @examples
#' \donttest{
#'  possible_langs
#' }
"possible_langs"