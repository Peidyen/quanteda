corp <- corpus(texts(data_corpus_inaugural))
str(corp)

toks <- tokens(texts(data_corpus_inaugural))

expect_silent(docvars(toks, '_document'))
expect_silent(docvars(toks, '_segid'))
expect_silent(docvars(toks, '_docid'))
expect_silent(docvars(toks, '_length'))

toks2 <- tokens_select(toks, stopwords())

expect_silent(docvars(toks2, '_document'))
expect_silent(docvars(toks2, '_segid'))
expect_silent(docvars(toks2, '_docid'))
expect_silent(docvars(toks2, '_length'))


# ---------------------------------

toks <- tokens(data_corpus_inaugural)


expect_silent(docvars(toks, '_document'))
expect_silent(docvars(toks, '_segid'))
expect_silent(docvars(toks, '_docid'))
expect_silent(docvars(toks, '_length'))


expect_equal(ntoken(toks, original = FALSE), 
             ntoken(toks, original = TRUE))

dict <- dictionary(list(country = "united states",
                   law=c('law*', 'constitution'),
                   freedom=c('free*', 'libert*')))
toks1 <- tokens_lookup(toks, dict, valuetype='glob', verbose = TRUE)

# tokens_lookup do not updates original values
expect_equal(ntoken(toks, original = FALSE), 
             ntoken(toks1, original = TRUE))

# tokens_select updates original values
toks2 <- tokens_remove(toks, stopwords())
expect_equal(ntoken(toks2, original = FALSE), 
             ntoken(toks2, original = TRUE))

# tokens_compunds updates original values
toks3 <- tokens_compound(toks, dict)
expect_equal(ntoken(toks3, original = FALSE), 
             ntoken(toks3, original = TRUE))

mx <- dfm(toks)
expect_equal(ntoken(mx, original = FALSE), 
             ntoken(mx, original = TRUE))

# dfm_lookup do not updates original values
mx1 <- dfm_lookup(mx, dict)
expect_equal(ntoken(mx, original = FALSE), 
             ntoken(mx1, original = TRUE))

# dfm_tolower do updates original values
expect_equal(ntoken(dfm_tolower(mx), original = FALSE),
             ntoken(dfm_tolower(mx), original = TRUE))

