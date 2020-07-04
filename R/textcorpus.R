

#' Corpus
#' @description a representation of a document or set of documents that includes the tokenized document entries
#' document counts, token counts, etc.
#' @param documents a list of one or more character vectors of text belonging to one or more documents
#' @param docnames the classnames for the different documents
#'
#' @return an object of class Corpus
#' @export
#'
#' @examples
#' corpusEx <- Corpus(list('first' = c('a single entry document'), 'second' = c('a two entry document','with two separate entries')))
Corpus <- function( documents, docnames = NULL ){
  documents <- validate_corpus( documents, docnames )
  return( new_corpus( documents ) )
}

#' Classifier Corpus
#' @description a specific type of Corpus that includes samples from a 'positive' sentiment document 
#' and a 'negative' sentiment document
#' @param positive a document of text containing entries with positive sentiment
#' @param negative a document of text containing entries with negative sentiment
#'
#' @return an object of class ClassifierCorpus
#' @export
#'
#' @examples
#' classifierCorpusEx <- ClassifierCorpus(c('what a beautiful day', 'I am happy'), c('what a miserable day', 'I am unhappy'))
ClassifierCorpus <- function( positive, negative ){
  corpus <- Corpus( list(positive, negative ), c('positive','negative') )
  class( corpus ) <- c( 'Corpus', 'ClassifierCorpus' )
  return( corpus )
}

#' Print For Corpus
#' @description print implementation for a Corpus object
#' prints the class name and the number of tokens in each document
#' @param obj an object inheriting from class Corpus
#'
#' @return a printed representation of the Corpus
#' @export
print.Corpus <- function(obj){
  cat(class(obj)[-1])
  for( i in obj$documentNames ){
    cat('\n')
    cat(i, ': ', obj[['tokenCounts']][[i]][['Ntokens']])
  }
}


#' Length for Corpus
#' @description length implementation for a Corpus object
#' returns the number of documents in the Corpus
#' @param obj an object inheriting from class Corpus
#'
#' @return the number of documents in the Corpus
#' @export
length.Corpus <- function(obj){
  return( obj$length )
}

#' Uniquer for Corpus
#' @description unique function implementation for a Corpus object
#' returns the vocabulary of unique tokens in the Corpus
#' @param obj an object inheriting from class Corpus
#'
#' @return a character vector containing each token in the corpus once
#' @export
unique.Corpus <- function(obj){
  return( vocab_tokens( obj$tokens ) )
}

#' Is Corpus
#' @description check if an object inherits from class Corpus
#' @param obj an object 
#'
#' @return true if the object inherits from Corpus
#' @export
#'
#' @examples
#' x <- structure(class='Any')
#' is.Corpus(x) # -> FALSE
#' class(x) <- c(class(x), 'Corpus')
#' is.Corpus(x) # -> TRUE
is.Corpus <- function(obj){
  return( 'Corpus' %in% class(obj) )
}

new_corpus <- function( documents ){
  documentLength = length( documents )
  documentNames <- names( documents )
  documentCounts <- sapply(documents, length, USE.NAMES = TRUE, simplify = TRUE)
  tokens <- sapply(documents, tokenize_docs, USE.NAMES = TRUE, simplify = FALSE)
  tokenCounts <- sapply(tokens, count_tokens, USE.NAMES = TRUE, simplify = FALSE)
  return( structure( list('tokens' = tokens, 
                          'tokenCounts' = tokenCounts, 
                          'documentNames' = documentNames, 
                          'documentCounts' = documentCounts,
                          'length' = documentLength), 
                     class = 'Corpus'))
}


validate_corpus <- function( documents, docnames = NULL ){
  # documents must be a list of character-type elements
  if( !is.list( documents ) ){
    documents <- as.list( documents )
  }
  
  if( !all( sapply( documents, typeof, simplify = TRUE ) == 'character' ) ){
    stop( 'each document must be of type character', call. = FALSE )
  }
  
  # docnames must be either null or a character vector of the same length as documents
  if( !is.null( docnames ) ) {
    if( !all( is.character( docnames ) ) ){
      stop( 'all docnames must be of type character', call. = FALSE)
    }
    
    if( length(docnames) != length(documents) ){
      stop( 'length of docnames must match the number of documents', call. = FALSE )
    }
    
    names(documents) <- docnames
  } else if( is.null( names(documents) ) ) {
    names( documents ) <- 1:length(documents)
  }
  
  return( documents )
}
