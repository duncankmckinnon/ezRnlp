% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textcleaning.R
\name{count_tokens}
\alias{count_tokens}
\title{Token Counts}
\usage{
count_tokens(doc_tokens)
}
\arguments{
\item{doc_tokens}{the tokens in the document}
}
\value{
a named vector with total token count 'Ntokens' and unique token count 'Utokens'
}
\description{
collects the total number of tokens (Ntokens),
and the number of unique tokens (Utokens) in the collection of documents
}
\examples{
doc_tokens <- tokenize_docs( c('First entry of the doc,', 'Second entry of the doc.') )
count_tokens(doc_tokens)
}
