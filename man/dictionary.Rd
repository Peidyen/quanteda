% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionaries.R
\name{dictionary}
\alias{dictionary}
\title{create a dictionary}
\usage{
dictionary(x, file = NULL, format = NULL, separator = " ",
  tolower = TRUE, encoding = "auto")
}
\arguments{
\item{x}{a named list of character vector dictionary entries, including \link{valuetype} pattern
matches, and including multi-word expressions separated by \code{concatenator}.  See examples.
This argument may be omitted if the dictionary is read from \code{file}.}

\item{file}{file identifier for a foreign dictionary}

\item{format}{character identifier for the format of the foreign dictionary. 
If not supplied, the format is guessed from the dictionary file's
extension.
Available options are: \describe{ \item{\code{"wordstat"}}{format used by 
Provalis Research's Wordstat software} \item{\code{"LIWC"}}{format used by 
the Linguistic Inquiry and Word Count software} \item{\code{"yoshikoder"}}{
format used by Yoshikoder software} \item{\code{"lexicoder"}}{format used
by Lexicoder} \item{\code{"YAML"}}{the standard YAML format}}}

\item{separator}{the character in between multi-word dictionary values. 
This defaults to \code{" "}.}

\item{tolower}{if \code{TRUE}, convert all dictionary values to lowercase}

\item{encoding}{additional optional encoding value for reading in imported 
dictionaries. This uses the \link{iconv} labels for encoding.  See the 
"Encoding" section of the help for \link{file}.}
}
\value{
A dictionary class object, essentially a specially classed named list
  of characters.
}
\description{
Create a \pkg{quanteda} dictionary class object, either from a list or by importing from a 
foreign format.  Currently supported input file formats are the Wordstat,
LIWC, Lexicoder v2 and v3, and Yoshikoder formats.  The import using the 
LIWC format works with 
all currently available dictionary files supplied as part of the LIWC 2001, 
2007, and 2015 software (see References).
}
\examples{
mycorpus <- corpus_subset(data_corpus_inaugural, Year>1900)
mydict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
                          opposition = c("Opposition", "reject", "notincorpus"),
                          taxing = "taxing",
                          taxation = "taxation",
                          taxregex = "tax*",
                          country = "america"))
head(dfm(mycorpus, dictionary = mydict))

\dontrun{
# import the Laver-Garry dictionary from Provalis Research
dictfile <- tempfile()
download.file("https://provalisresearch.com/Download/LaverGarry.zip", dictfile, mode = "wb")
unzip(dictfile, exdir = (td <- tempdir()))
lgdict <- dictionary(file = paste(td, "LaverGarry.cat", sep = "/"))
head(dfm(data_corpus_inaugural, dictionary = lgdict))

# import a LIWC formatted dictionary from http://www.moralfoundations.org
download.file("https://goo.gl/5gmwXq", tf <- tempfile())
mfdict <- dictionary(file = tf, format = "LIWC")
head(dfm(data_corpus_inaugural, dictionary = mfdict))
}
}
\references{
Wordstat dictionaries page, from Provalis Research 
  \url{http://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/}.
  
  Pennebaker, J.W., Chung, C.K., Ireland, M., Gonzales, A., & Booth, R.J. 
  (2007). The development and psychometric properties of LIWC2007. [Software 
  manual]. Austin, TX (\url{www.liwc.net}).
  
  Yoshikoder page, from Will Lowe 
  \url{http://conjugateprior.org/software/yoshikoder/}.
  
  Lexicoder format, \url{http://www.lexicoder.com}
}
\seealso{
\link{dfm}
}
