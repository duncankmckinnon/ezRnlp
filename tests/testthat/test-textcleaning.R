
describe('clean_text()',{
  teststr <- 'A $string$ of uninterestinG, and UNimportant - text'
  describe('stopwords = FALSE',{
    it('cleaned text with is lowercase, with punctuation removed', {
      cleaned <- clean_text(teststr, FALSE)
      expected <- 'a string of uninteresting and unimportant  text'
      expect_equal(cleaned, expected)
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
