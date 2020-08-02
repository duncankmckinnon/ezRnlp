
describe('clean_text()',{
  teststr <- 'A $string$ of uninterestinG, and UNimportant - text'
  describe('stopwords = FALSE',{
    it('cleaned text with is lowercase, with punctuation removed', {
      cleaned <- clean_text(teststr, FALSE)
      expected <- 'a string of uninteresting and unimportant  text'
      expect_equal(cleaned, expected)
    })
    it('should validate length of text', {
      testclean <- try(clean_text(c(teststr,''), FALSE), silent = TRUE)
      expected <- "Error in clean_text(c(teststr, \"\"), FALSE) : \n  length(text) == 1 is not TRUE\n"
      expect_equal(testclean[[1]], expected)
    })
    it('should validate that text is input', {
      testclean <- try(clean_text(1, FALSE), silent = TRUE)
      expected <- "Error in clean_text(1, FALSE) : is.character(text) is not TRUE\n"
      expect_equal(testclean[[1]], expected)
    })
  })
  describe('stopwords = TRUE',{
    it('cleaned text is lowercase, with punctuation and stopwords removed', {
      cleaned <- clean_text(teststr, TRUE)
      expected <- 'a string  uninteresting  unimportant  text'
      expect_equal(cleaned, expected)
    })
  })
})

describe('tokenize_text()',{
  teststr <- 'A $string$ of uninterestinG, and UNimportant - text'
  describe('stopwords = FALSE',{
    it('should clean, stem, and tokenize the text without removing stopwords',{
      tokenized <- tokenize_text(teststr, F)
      expected <- c('a', 'string', 'of', 'uninterest', 'and', 'unimport', 'text')
      expect_equal(tokenized, expected)
    })
  })
  describe('stopwords = TRUE',{
    it('should clean, stem, and tokenize the text and remove stopwords',{
      tokenized <- tokenize_text(teststr)
      expected <- c('a', 'string', 'uninterest', 'unimport', 'text')
      expect_equal(tokenized, expected)
    })
  })
})