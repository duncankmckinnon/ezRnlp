

#' Minimum Edit Distance
#' @description Finds the minimum cost (in terms of edit operations) to transform the source token to the target token
#' @param source - the token starting point
#' @param target - the token to compare against
#' @param deletecost - the cost of deleting one letter from the target
#' @param insertcost - the cost of inserting one letter into the target
#' @param replacecost - the cost of replacing on letter from the target
#'
#' @return dist - the minimum edit distance between source and target, D - the filled in distance matrix steps ( for backtrace )
#' @export
#'
#' @examples
#' minimum_edit_distance('play','stay') # -> dist = 4
minimum_edit_distance <- function( source, target, deletecost=1, insertcost=1, replacecost=2){
  # validation
  stopifnot(all( sapply( c( deletecost, insertcost, replacecost ), is.numeric, simplify = T ) ) )
  
  # split source and target tokens into component letters and get length of each
  s <- sapply(c(source,target), strsplit, '', simplify = T)
  mn <- sapply(s, length, simplify = T)
  
  # set up dynamic programming matrix for finding min distance
  D <- matrix(nrow=mn[1] + 1, ncol=mn[2] + 1, dimnames = list(c('#', s[[1]]), c('#', s[[2]])))
  D[1:(mn[1]+1),1] <- 0:mn[1]
  D[1,2:(mn[2]+1)] <- 1:mn[2]
  
  # calculate min distance for each position in the matrix
  # from top-left to bottom-right 
  for( i in 1:mn[1] ){
    for( j in 1:mn[2] ){
      rep <- D[i,j] + ifelse(s[[1]][i] == s[[2]][j], 0, replacecost)
      del <- D[i, j+1] + deletecost
      ins <- D[i+1, j] + insertcost
      nextD <- min(c(del, ins, rep), na.rm = T)
      D[i+1,j+1] <- nextD
    }
  }
  
  # min distance is final entry in D, ( D may also be needed for backtracing )
  return(list('dist' = D[mn[1]+1, mn[2]+1], 'D' = D))
}

#' Token Permutations - Delete
#' @description Collects all tokens that could result from performing 1 delete operation 
#' on the given token
#' @param token - either 1 token or a pre-split set of ordered letters that make up the token
#' @param split - whether the input token needs to be split into individual letters or not - default = TRUE
#'
#' @return - a character vector with each possible permutation of the original token after 1 delete operation
#' @export
#'
#' @examples
#' token_permutations.delete('sometoken')
#' token_permutations.delete(c('s','o','m','e','t','o','k','e','n'), split = FALSE)
token_permutation.delete <- function( token, split = TRUE ){
  # if a token is passed, split into letters
  split_token <- token
  if( split ) split_token <- strsplit(token, '')[[1]]
  tlen <- length( split_token )
  
  # allocate memory for efficiency
  perms <- character( length = tlen )
  
  # for delete, remove the character once at each index
  for( i in 1:tlen ){
    perms[i] <- paste0(split_token[-i],collapse='')
  }
  return(perms)
}

#' Token Permutations - Insert
#' @description Collects all tokens that could result from inserting 1 letter anywhere in the given token
#' @param token - either 1 token or a pre-split set of ordered letters that make up the token
#' @param split - whether the input token needs to be split into individual letters or not - default = TRUE
#'
#' @return - a character vector with each possible permutation of the original token after 1 insert operation
#' @export
#'
#' @examples
#' token_permutation.insert('sometoken')
#' token_permutation.insert(c('s','o','m','e','t','o','k','e','n'), split = FALSE)
token_permutation.insert <- function( token, split = TRUE ){
  # if a token is passed, split into letters
  split_token <- token
  if( split ) split_token <- strsplit(token, '')[[1]]
  
  # one extra space needed for inserted letter
  process_token <- c('', split_token)
  tlen <- length( process_token )
  
  # allocate memory for efficiency
  perms <- character( length = tlen * 26 )
  
  # temporary store for characters
  temp_token <- character(length=tlen)
  
  # index for next entry
  n <- 1
  for( i in 1:tlen ){
    temp_token[1:tlen] <- process_token
    
    # make a space to insert letter
    if( i != 1 ){
      temp_token[1:(i-1)] <- split_token[1:(i-1)]
      if( i < tlen ){
        temp_token[(i+1):tlen] <- split_token[i:(tlen-1)]
      } else {
        temp_token[i] <- ''
      }
    }
    
    # insert each letter once at each position
    for( j in letters ){
      temp_token[i] <- j
      perms[n] <- paste0( temp_token, collapse = '')
      n <- n + 1
    }
  }
  return( perms )
}

#' Token Permutations - Replace
#' @description Collects all tokens that could result from replace 1 letter with a different letter anywhere in the given token
#' @param token - either 1 token or a pre-split set of ordered letters that make up the token
#' @param split - whether the input token needs to be split into individual letters or not - default = TRUE
#'
#' @return - a character vector with each possible permutation of the original token after 1 replace operation
#' @export
#'
#' @examples
#' token_permutation.replace('sometoken')
#' token_permutation.replace(c('s','o','m','e','t','o','k','e','n'), split = FALSE)
token_permutation.replace <- function( token, split = TRUE ){
  # if a token is passed, split into letters
  split_token <- token
  if( split ) split_token <- strsplit(token, '')[[1]]
  tlen <- length( split_token )
  
  # allocate memory for efficiency
  perms <- character( length = ( (tlen-1) * 26 ) )
  
  # temporary store for characters 
  temp_token <- character( length = tlen )
  
  # index for next entry
  n <- 1
  for( i in 1:tlen ){
    temp_token[1:tlen] <- split_token
    for( j in letters ){
      
      # only replace if not the same letter and position as token
      if( j != split_token[i] ){
        temp_token[i] <- j
        perms[n] <- paste0( temp_token, collapse = '')
        n <- n + 1
      }
    }
  }
  return(perms)
}

#' Token Permutations - All
#' @description Collects all tokens that could result from performing any edit operation on the input token using a more efficient single-pass implementation
#' @param token - either 1 token or a pre-split set of ordered letters that make up the token
#' @param split - whether the input token needs to be split into individual letters or not - default = TRUE
#' @param simplify - whether to return a simplified vector of the token permutations, 
#' or a list containing the permutations for each separate operation (delete, replace, insert) - default = FALSE
#'
#' @return - by default returns a list with the separate permutations for 1 delete, replace and insert
#' if simplify is true, returns a character vector with each possible permutation of the original token after 1 operation
#' @export
#'
#' @examples
#' token_permutation.all('sometoken')
#' token_permutation.all(c('s','o','m','e','t','o','k','e','n'), split = FALSE)
#' token_permutation.all('sometoken', simplify = TRUE)
token_permutation.all <- function( token, split = TRUE, simplify = FALSE ){
  # if a token is passed, split into letters
  split_token <- token
  if( split ) split_token <- strsplit(token, '')[[1]]
  
  # one extra space needed for insert operation
  process_token <- c('', split_token)
  tlen <- length( process_token )
  
  # allocate memory for each operation
  perms_del <- character( length = ( tlen - 1 ) )
  perms_ins <- character( length = ( ( tlen ) * 26 ) )
  perms_rep <- character( length = ( ( tlen - 2 ) * 26 ) )
  temp_token_ins <- character( length = tlen )
  temp_token_rep <- character( length = (tlen - 1) )
  
  
  # index counter into permutation stores
  n_ins <- 1
  n_rep <- 1
  
  for( i in 1:tlen ){
    temp_token_ins[1:tlen] <- process_token
    
    # make space to insert next letter
    if( i != 1 ){
      temp_token_ins[1:(i-1)] <- split_token[1:(i-1)]
    }
    
    # last iteration is only for inserting
    if( i < tlen ){
      temp_token_rep[1:(tlen-1)] <- split_token
      
      # delete once at each position
      perms_del[i] <- paste0(split_token[-i],collapse='')
      
      # make space to insert next letter
      temp_token_ins[(i+1):tlen] <- split_token[i:(tlen-1)]
      
      for( j in letters ){
        
        # only replace if not the same letter and position as token
        if( j != split_token[i] ){
          temp_token_rep[i] <- j
          perms_rep[n_rep] <- paste0( temp_token_rep, collapse = '')
          n_rep <- n_rep + 1
        }
        
        # insert in empty position
        temp_token_ins[i] <- j
        perms_ins[n_ins] <- paste0( temp_token_ins, collapse = '')
        n_ins <- n_ins + 1
      }
    } else {
      
      # set last position to empty for insert
      temp_token_ins[i] <- ''
      for( j in letters ){
        
        # insert in empty position (after token)
        temp_token_ins[i] <- j
        perms_ins[n_ins] <- paste0( temp_token_ins, collapse = '')
        n_ins <- n_ins + 1
      }
    }
  }
  
  # return either a character vector or a list of character vectors by operation
  if( simplify ) return( c( perms_del, perms_ins, perms_rep ) )
  return( list( 'delete' = perms_del, 'insert' = perms_ins, 'replace' = perms_rep ) )
}


#' Token Frequency Check - DataMuse API
#' @description makes a call to the datamuse api to check the universal frequency of the token (count per million)
#' @param token - a single token
#'
#' @return - the token frequency per 1 million words in the English language, according to the DataMuse API
#' @importFrom RCurl httpGET
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' token_frequency_check('the')
token_frequency_check <- function(token){
  # validation
  stopifnot(is.character(token), length(token) == 1)
  
  # uses datamuse api to check word frequency (count per 1e6)
  url <- paste0(c('https://api.datamuse.com/words?sp=',token,'&md=f&max=1'), collapse = '')
  
  # parse json to data.frame ( word, score, tags )
  val <- fromJSON( httpGET(url) )
  
  # will try to autocorrect result, but only accept result for given token
  if( val$word != token ){
    return( 0 )
  } else {
    
    # parse out the frequency from the string value returned in 'tags'
    freq <- strsplit( val$tags[[1]], pattern = ":" )[[1]][2]
  }
  return( as.numeric( freq ) )
}