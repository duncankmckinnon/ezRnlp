
#' Term Frequency 
#' @description a term frequency hash for the tokens and documents of the corpus
#' @param corpus an object inheriting from class Corpus
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

#' Conditional Probabilities for Word by TermFrequency
#' @description this function takes a token and calculates the conditional probability 
#' of the token appearing in each classLabel.  This can then be used to get the likelihood ratio
#' of the token belonging to each classLabel.
#' @param token a single token
#' @param termFrequency an object of class TermFrequency
#'
#' @return a conditional probability for the token belonging to each class given it's term frequency
#' @export
#'
#' @examples
#' corpusEx <- Corpus(list('first' = c('a single entry document'), 'second' = c('a two entry document','with two separate entries')))
#' termFreqEx <- TermFrequency(corpusEx)
#' P_token <- conditionalProbability( 'entr', termFreqEx)
conditionalProbability <- function( token, termFrequency ){
  P_w <- rep(0, length(termFrequency$Labels))
  for( classLabel in termFrequency$Labels ){
    P_w[ classLabel ] <- ( termFreq$Dict$get(c( token, classLabel), 0) + 1 ) / ( sum( termFreq$LabelCounts[[ classLabel ]] ) )
  }
  return( P_w )
}

new_termFrequency <- function( corpus ){
  termfreqDict <- collections::dict()
  labels <- corpus$documentNames
  for( documentName in corpus$documentNames ){
    documentTokens <- purrr::flatten(corpus$tokens[[documentName]])
    for( token in documentTokens ) {
      key <- c( token, documentName )
      termfreqDict$set(key, (termfreqDict$get(key, 0) + 1))
    }
  }
  totals <- corpus$tokenCounts
  return( structure( list( 'Dict' = termfreqDict, 'Labels' = labels, 'LabelCounts' = totals ), class = 'TermFrequency' ) )
}