Corpus <- function( documents, docnames = NULL ){
  documents <- validate_documents( documents )
  docnames <- validate_document_names( docnames, documents )
  return( new_corpus( documents, docnames ) )
}

ClassifierCorpus <- function( positive, negative ){
  corpus <- Corpus( list( positive, negative ), c( 'positive','negative' ) )
  class( corpus ) <- c( 'Corpus', 'ClassifierCorpus' )
  return( corpus )
}

new_corpus <- function( documents, docnames ){
  names( documents ) <- docnames
  documentCounts <- sapply(documents, length, USE.NAMES = TRUE, simplify = TRUE)
  tokens <- sapply(documents, tokenize_docs, USE.NAMES = TRUE, simplify = FALSE)
  tokenCounts <- sapply(tokens, count_tokens, USE.NAMES = TRUE, simplify = FALSE)
  return( structure( list('tokens' = tokens, 
                          'tokenCounts' = tokenCounts, 
                          'documentNames' = docnames, 
                          'documentCounts' = documentCounts,
                          'length' = length( documents )), 
                     class = 'Corpus'))
}

validate_documents <- function( documents ){
  # documents may be passes as character vector, but will be stored in object as list
  if( !is.list( documents ) ){
    documents <- as.list( documents )
  }
  
  # documents must be a list of character-type elements
  if( !all( sapply( documents, typeof, simplify = TRUE ) == 'character' ) ){
    stop( 'each document must be of type character', call. = FALSE )
  }
  return( documents )
}

validate_document_names <- function( docnames, documents ){
  
  # docnames must be either null or a character vector of the same length as documents
  if( is.missing( docnames ) || is.null( docnames ) ){
    docnames <- NULL
  } else {
    if( !all( is.character( docnames ) ) ){
      stop( 'all docnames must be of type character', call. = FALSE)
    }
    
    if( length(docnames) != length(documents) ){
      stop( 'length of docnames must match the number of documents', call. = FALSE )
    }
  }
  return( docnames )
}


print.Corpus <- function(obj){
  cat(class(obj)[-1])
  elements <- ifelse(is.null(obj$documentNames), 1:obj$length, obj$documentNames)
  for( i in elements ){
    cat('\n')
    cat(i, ': ', obj[['tokenCounts']][[i]][['N']])
  }
}

length.Corpus <- function(obj){
  return( obj$length )
}

unique.Corpus <- function(obj){
  return( vocab_tokens( obj$tokens ) )
}