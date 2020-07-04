
#' Sentiment Classifier
#' @description a simple sentiment classifier object that takes a set of positive sample text 
#' and a set of negative sample text, and creates a sentiment classifier.
#' @param positive character vector with samples of positive text
#' @param negative character vector with samples of negative text
#' @param type the classifier model type (default \'linear\')
#'
#' @return a SentimentClassifier Object
#' @export
#'
#' @examples
#' SentimentClassifier(c('some positive text'), c('negative text samples'))
SentimentClassifier <- function( positive, negative, type = c('linear','bayes')){
  type <- validate_SentimentClassifier( positive, negative, type )
  corpus <- ClassifierCorpus( positive, negative )
  termFrequency <- TermFrequency( corpus )
  model <- linearSentimentClassifier( corpus, termFrequency )
  return( structure( list( 'corpus' = corpus, 'termFrequency' = termFrequency, 'model' = model, 'type' = type), class = 'SentimentClassifier') )
}

#' Predict Sentiment Classifier
#' @description a prediction method to assign a sentiment score to a set of text samples from a Sentiment Classifier
#' @param obj a SentimentClassifier model object
#' @param newdata a character vector containing text samples to be assessed
#'
#' @return Sentiment score predictions of each entry of newdata
#' @export
#'
#' @examples
#' sentimentEx <- SentimentClassifier(c('some positive text'), c('negative text samples'))
#' predict(sentimentEx, c('more negative text', most positive text'))
predict.SentimentClassifier <- function(obj, newdata){
  # data needs to be a character vector
  stopifnot(is.character(newdata))
  n <- length(newdata)  
  tokens <- tokenize_docs( newdata )
  # split by model approach
  if( T || obj$type == 'linear' ){
    P <- vector(mode = 'numeric', length = n)
    N <- vector(mode = 'numeric', length = n)
    position <- 1
    for(entry in tokens){
      P[position] <- score_entry(entry, obj$termFrequency$Dict, label='positive')
      N[position] <- score_entry(entry, obj$termFrequency$Dict, label='negative')
      position <- position + 1
    }
    classifier_data <- data.frame('P'=P,'N'=N)
    classifier_pred <- predict( obj$model, newdata = classifier_data )
  }
  return( classifier_pred )
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

score_conditional_probability <- function( doc, conditional_probabilities ){
  tokens <- tokenize_text(doc)
  score <- 0
  for( i in tokens ){
    cp <- conditional_probabilities[[i]]
    if( !is.null(cp) ){
      score <- score + log(cp['positive'] / cp['negative'])
    }
  }
  names(score) <- NULL
  return( score )
}

score_entry <- function( tokens, term_freq_dict, label = NULL ){
  score <- 0
  if( is.null( label ) ){
    for( token in tokens ){
      score <- score + term_freq_dict$get( token, 0 )
    }
  } else {
    for( token in tokens ){
      score <- score + term_freq_dict$get( c(token, label), 0 )
    }
  }
  return( score )
}

score_entries <- function( doc_tokens, term_freq_dict, label = NULL ) doc_tokens %>% sapply( score_entry, term_freq_dict, label, simplify = T)

linearSentimentClassifier <- function( corpus, termFrequency ){
  npos <- corpus$documentCounts['positive']
  nneg <- corpus$documentCounts['negative']
  Y <- c( rep(1, npos), rep(0, nneg) )
  P <- vector(mode = 'numeric', length = npos + nneg)
  N <- vector(mode = 'numeric', length = npos + nneg)
  position <- 1
  for( classlabel in c('positive', 'negative')){
    for(entry in corpus$tokens[[classlabel]]){
      P[position] <- score_entry(entry, termFrequency$Dict, label='positive')
      N[position] <- score_entry(entry, termFrequency$Dict, label='negative')
      position <- position + 1
    }
  }
  classifier_data <- data.frame('Y'=Y,'P'=P,'N'=N)
  classifier_model <- glm(Y ~ P + N, family=binomial, data=classifier_data)
  return( classifier_model )
}

validate_SentimentClassifier <- function( positive, negative, type ) {
  stopifnot( is.character(positive), is.character(negative) )
  if( is.character(type) && all(type %in% c('linear','bayes')) ){
    type <- type[1]
  } else {
    stop('type must be either \'linear\' or \'bayes\'', call. = FALSE)
  }
  return( type )
}


