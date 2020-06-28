
create_classifier_corpus <- function(positive, negative){
  docs <- list('positive' = positive, 'negative' = negative)
  nDocs <- sapply(docs, length, USE.NAMES = TRUE, simplify = TRUE)
  tokens <- sapply(docs, tokenize_docs, USE.NAMES = TRUE, simplify = FALSE)
  NV <- sapply(tokens, class_counts, USE.NAMES = TRUE, simplify = TRUE)
  return( structure(list('tokens' = tokens, 'counts'=NV, 'N' = nDocs), class = 'classifierCorpus') )
}

print.classifierCorpus <- function(obj){
  cat('classifierCorpus\n')
  cat('positive:', obj$N['positive'], '\n')
  cat('negative:', obj$N['negative'])
}

classifierCorpus <- function( positive, negative ){
  assert_that(is.character(positive) && is.character(negative))
  return( create_classifier_corpus( positive, negative ) )
}