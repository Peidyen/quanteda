% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tolower-functions.R
\name{char_tolower}
\alias{char_tolower}
\alias{char_toupper}
\title{convert the case of character objects}
\usage{
char_tolower(x, keep_acronyms = FALSE, ...)

char_toupper(x, ...)
}
\arguments{
\item{x}{the input object whose character/tokens/feature elements will be 
case-converted}

\item{keep_acronyms}{logical; if \code{TRUE}, do not lowercase any 
all-uppercase words (applies only to \code{*_tolower} functions)}

\item{...}{additional arguments passed to \pkg{stringi} functions, (e.g. 
\code{\link{stri_trans_tolower}}), such as \code{locale}}
}
\description{
\code{char_tolower} and \code{char_toupper} are replacements for 
\link[base]{tolower} and \link[base]{toupper} based on the \pkg{stringi} 
package.  The \pkg{stringi} functions for case conversion are superior to the
\pkg{base} functions because they correctly handle case conversion for
Unicode.  In addition, the \code{*_tolower} functions provide an option for
preserving acronyms.
}
\examples{
txt <- c(txt1 = "b A A", txt2 = "C C a b B")
char_tolower(txt) 
char_toupper(txt)

# with acronym preservation
txt2 <- c(text1 = "England and France are members of NATO and UNESCO", 
          text2 = "NASA sent a rocket into space.")
char_tolower(txt2)
char_tolower(txt2, keep_acronyms = TRUE)
char_toupper(txt2)
}
