
fill_term_freq <- function( doc_tokens, term_freq_dict = NULL, label = NULL ) {
  if( is.null(label) ){
    for( tokens in doc_tokens ){
      for( token in tokens ){
        term_freq_dict$set(token, term_freq_dict$get(token, 0) + 1)
      }
    }
  } else {
    for( tokens in doc_tokens ){
      for( token in tokens ){
        term_freq_dict$set( c( token, label ), term_freq_dict$get( c( token, label ), 0 ) + 1 )
      } 
    }
  }
  return( term_freq_dict )
}



fill_conditional_probability <- function( positive, negative, term_freq_dict ){
  positive_tokens <- flatten(positive)
  negative_tokens <- flatten(negative)
  pcounts <- class_counts(positive_tokens)
  ncounts <- class_counts(negative_tokens)
  
  conditional_prob <- function( token, term_freq_dict, label ){
    return( (term_freq_dict$get(c(token, label), 0) + 1 ) / sum( ifelse( label==1, pcounts, ncounts ) ) )
  }
  
  all_tokens <- unique( c( positive_tokens, negative_tokens ) )
  names(all_tokens) <- all_tokens
  probs <- all_tokens %>% sapply(FUN = function(x){
    return( sapply(c('positive' = 1, 'negative' = 0), 
           FUN = function(y) 
             conditional_prob(x, term_freq_dict, y), simplify = TRUE) )
  }, USE.NAMES = TRUE, simplify = FALSE)
  return( probs ) 
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
  indexer <- ifelse( is.null(label), function(x) x, function(x) c( x, label ) )
  score <- 0
  for( token in tokens ){
    score <- score + term_freq_dict$get( indexer( token ), 0 )
  }
  return( score )
}

score_entries <- function( doc_tokens, term_freq_dict, label = NULL ) doc_tokens %>% sapply( score_entry, term_freq_dict, label, simplify = T)

get_classifier_data <- function( positive, negative, train_pct = 0.9 ){
  ind_pos <- 1:length(positive)
  ind_neg <- length(positive) + 1:length(negative)
  all_tokens <- c( tokenize_docs(positive), tokenize_docs(negative) )
  term_freq <- collections::dict()
  term_freq <- fill_term_freq(all_tokens[ind_pos], term_freq, 1)
  term_freq <- fill_term_freq(all_tokens[ind_neg], term_freq, 0)
  
  positive_scores <- score_entries( all_tokens, term_freq, 1)
  negative_scores <- score_entries( all_tokens, term_freq, 0)
  
  model_data <- data.frame(y = c(rep(1, length(positive)), rep(0, length(negative))),
                           positive_sentiment = positive_scores,
                           negative_sentiment = negative_scores)
  
  
  train_ind <- c(sample(1:length(positive), floor(train_pct * length(positive))), sample((length(positive) + 1:length(negative)), floor(train_pct * length(negative))))
  train <- model_data[train_ind,]
  test <- model_data[-train_ind,]
  
  
  return( list( 'training' = train, 'test' = test, 'term_freq' = term_freq) )
}

predict_text_classification <- function( text, model, term_freq ){
  all_tokens <- tokenize_docs(text)
  positive_scores <- score_entries(all_tokens, term_freq, 1)
  negative_scores <- score_entries(all_tokens, term_freq, 0)
  pred_data <- data.frame(positive_sentiment = positive_scores,
                          negative_sentiment = negative_scores)
  return( ifelse( predict(model, pred_data) > 0.5, 1, 0 ) )
}

glm_classifier <- function( train, test ){
  mod <- glm( y ~ positive_sentiment + negative_sentiment, family = binomial, data = train )
  pred <- predict(mod, test)
  
  train_accuracy <- sum( ifelse( mod$fitted.values > 0.5, 1, 0) == train$y ) / length( train$y )
  test_accuracy <- sum( ifelse( pred > 0.5, 1, 0 ) == test$y ) / length( test$y )
  
  return( list( 'mod' = mod, 
                'predicted' = pred, 
                'accuracy' = list( 
                  'train' = train_accuracy, 
                  'test' = test_accuracy ) ) )
}  


build_glm_classifier <- function( positive_samples, negative_samples ){
  term_data <- get_classifier_data( positive_samples, negative_samples )
  return( glm_classifier( term_data$training, term_data$test ) )
}


