
#' Term Frequency 
#' @description a term frequency hash for the tokens and documents of the corpus
#' @param corpus an object inheriting from class Corpus
#' @importFrom purrr flatten
#' 
#' @return a term frequency hash for the tokens and documents in the corpus
#' @export
#'
#' @examples
#' corpusEx <- Corpus(list('first' = c('a single entry document'), 'second' = c('a two entry document','with two separate entries')))
#' termFreqEx <- TermFrequency(corpusEx)
TermFrequency <- function( corpus ){
  if( !is.Corpus( corpus ) ){
    stop('Term Frequency requires corpus to operate on', call. = FALSE)
  }
  return( new_termFrequency( corpus ) )
} 

new_termFrequency <- function( corpus ){
  termfreqDict <- collections::dict()
  labels <- corpus$documentNames
  for( documentName in corpus$documentNames ){
    documentTokens <- flatten(corpus$tokens[[documentName]])
    for( token in documentTokens ) {
      key <- c( token, documentName )
      termfreqDict$set(key, (termfreqDict$get(key, 0) + 1))
    }
  }
  totals <- corpus$tokenCounts
  return( structure( list( 'Dict' = termfreqDict, 'Labels' = labels, 'LabelCounts' = totals ), class = 'TermFrequency' ) )
}