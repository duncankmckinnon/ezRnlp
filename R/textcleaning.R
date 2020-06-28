

clean_text <- function( text, stopwords = TRUE ) {
  text <- ifelse(stopwords==TRUE, removeWords(text, stopwords()), text)
  return( text %>% 
            str_to_lower() %>%
            removePunctuation() )
}


tokenize_text <- function( text, stopwords = TRUE ){
  return( text %>%
            clean_text( stopwords ) %>%
            stemDocument() %>%
            Boost_tokenizer()
  )
}

tokenize_docs <- function( docs ) docs %>% lapply( tokenize_text )

create_vocab <- function( doc_tokens ){
  vocab <- c()
  for( tokens in doc_tokens ){
    vocab <- base::union( tokens, vocab )
  }
  return(vocab)
}

class_counts <- function( class_tokens ) c( 'N' = length( class_tokens ), 
                                            'V' = length( unique( class_tokens ) ) )
