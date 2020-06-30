

#' Clean Text
#' @description performs basic text cleaning on a single line of text by
#' converting all text to lowercase and removing all punctuation and removing
#' stopwords using the stopwords selection maintained in the tm package
#' @param text a line of text 
#' @param stopwords logical - should stopwords be removed
#' @importFrom magrittr `%>%`
#'
#' @return a cleaned line of text
#' @export
#'
#' @examples
#' clean_text('This is a basic line. It contains UNCLEANED text.')
clean_text <- function( text, stopwords = TRUE ) {
  
  # remove stopwords only if requested
  text <- ifelse(stopwords==TRUE, tm::removeWords(text, tm::stopwords()), text)
  return( text %>% 
            stringr::str_to_lower() %>%
            tm::removePunctuation() )
}


#' Tokenize Text
#' @description performs cleaning, word stemming, and tokenization on a single line of text
#' using clean_text and the stemming and boosting utilities maintained in the tm package
#' @param text a line of text
#' @param stopwords logical - should stopwords be removed
#'
#' @return a cleaned, stemmed and tokenized character vector
#' @export
#'
#' @examples
#' tokenize_text('This is a basic line. It contains UNCLEANED, un-stemmed, un-tokenized text.')
tokenize_text <- function( text, stopwords = TRUE ){
  return( text %>%
            clean_text( stopwords ) %>%
            tm::stemDocument() %>%
            tm::Boost_tokenizer()
  )
}


#' Tokenize Docs
#' @description performs vectorized tokenization for each entry of the document
#' @param docs a vector or list of text samples
#'
#' @return a list of tokenized results for each each entry in the doc
#' @export
#'
#' @examples
#' tokenize_docs( c('First entry of the doc,', 'Second entry of the doc.') )
tokenize_docs <- function( docs ) { 
  return( docs %>% lapply( tokenize_text ) )
}


#' Create Vocab for Docs
#' @description collect all unique tokens in the full set of docs which serve as the base vocabulary
#' for the corpus.  Returns a character vector of the unique tokens across all entries in the docs.
#' @param docs a vector or list of text samples
#'
#' @return a character vector containing each unique token entry that appears in the docs.
#' @export
#'
#' @examples
#' vocab.docs( c('First entry of the doc,', 'Second entry of the doc.') )
vocab.docs <- function( docs ){
  doc_tokens <- tokenize_docs( docs )
  return( vocab.tokens( doc_tokens ) )
}

#' Create Vocab for Tokens
#' @description collect all unique tokens in the full set of tokens.  
#' Returns a character vector of the unique tokens across all entries.
#' @param tokens a list of tokenized entries
#'
#' @return a character vector containing each unique token entry that appears in tokenized document.
#' @export
#'
#' @examples
#' vocab.tokens( list( c( 'first', 'entr', 'doc' ), c('second', 'entr', 'doc') ) )
vocab.tokens <- function( tokens ){
  vocab <- c()
  for( entry_tokens in doc_tokens ){
    vocab <- base::union( entry_tokens, vocab )
  }
  return(vocab)
}

#' Token Counts
#' @description collects the total number of tokens (Ntokens),
#' and the number of unique tokens (Utokens) in the collection of documents
#' @param doc_tokens the tokens in the document
#'
#' @return a named vector with total token count 'Ntokens' and unique token count 'Utokens'
#' @export
#'
#' @examples
#' doc_tokens <- tokenize_docs( c('First entry of the doc,', 'Second entry of the doc.') )
#' token_counts(doc_tokens)
token_counts <- function( doc_tokens ) {
  class_tokens <- purrr::flatten( doc_tokens )
  return( c( 'Ntokens' = length( class_tokens ),
             'Utokens' = length( unique( class_tokens ) ) ) )
}

