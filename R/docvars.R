#' get or set for document-level variables
#' 
#' Get or set variables associated with a document in a \link{corpus}, or get
#' these variables from a \link{tokens} or \link{dfm} object.
#' @param x \link{corpus}, \link{tokens}, or \link{dfm} object whose
#'   document-level variables will be read or set
#' @param field string containing the document-level variable name
#' @param drop returns values as a vector for one variable if \code{TRUE}
#' @return \code{docvars} returns a data.frame of the document-level variables,
#'   dropping the second dimension to form a vector if a single docvar is
#'   returned.
#' @examples 
#' # retrieving docvars from a corpus
#' head(docvars(data_corpus_inaugural))
#' tail(docvars(data_corpus_inaugural, "President"), 10)
#' 
#' @export
#' @keywords corpus
docvars <- function(x, field = NULL, drop = TRUE) {
    UseMethod("docvars")
}

#' @noRd
#' @export
docvars.corpus <- function(x, field = NULL, drop = TRUE) {
    check_fields(x, field)
    dvars <- select_fields(documents(x), c('system', 'user'))
    if (is.null(field))
        dvars <- select_fields(dvars, 'user')
    get_docvars(dvars, field, drop)
}

#' @noRd
#' @export
docvars.tokenizedTexts <- function(x, field = NULL, drop = TRUE) {
    check_fields(x, field)
    dvars <- attr(x, "docvars")
    if (is.null(field))
        dvars <- select_fields(dvars, 'user')
    get_docvars(dvars, field, drop)    
}

#' @noRd
#' @export
docvars.dfm <- function(x, field = NULL, drop = TRUE) {
    check_fields(x, field)
    dvars <- x@docvars
    if (is.null(field))
        dvars <- select_fields(dvars, 'user')
    get_docvars(dvars, field, drop)    
}

## internal function to return the docvars for all docvars functions
get_docvars <- function(dvars, field = NULL, drop = TRUE) {
    if (is.null(field)) {
        if (is.null(dvars)) {
            return(data.frame())
        } else {
            return(dvars)
        }
    } else {
        return(dvars[, field, drop = drop])
    }
}


#' @rdname docvars
#' @param value the new values of the document-level variable
#' @note Another way to access and set docvars is through indexing of the corpus
#'   \code{j} element, such as \code{data_corpus_irishbudget2010[, c("foren", 
#'   "name"]} or for a single docvar, 
#'   \code{data_corpus_irishbudget2010[["name"]]}.  The latter also permits 
#'   assignment, including the easy creation of new document varibles, e.g. 
#'   \code{data_corpus_irishbudget2010[["newvar"]] <- 
#'   1:ndoc(data_corpus_irishbudget2010)}. See \code{\link{[.corpus}} for 
#'   details.
#'   
#'   Assigning docvars to a \link{tokens} object is not supported.  (You should
#'   only be manipulating these variables at the corpus level.)
#' @return \code{docvars<-} assigns \code{value} to the named \code{field}
#' @examples 
#' # assigning document variables to a corpus
#' corp <- data_corpus_inaugural
#' docvars(corp, "President") <- paste("prez", 1:ndoc(corp), sep = "")
#' head(docvars(corp))
#' 
#' # alternative using indexing
#' head(corp[, "Year"])
#' corp[["President2"]] <- paste("prezTwo", 1:ndoc(corp), sep = "")
#' head(docvars(corp))
#' 
#' @export
"docvars<-" <- function(x, field = NULL, value) {
    UseMethod("docvars<-")
}


#' @noRd
#' @export
"docvars<-.corpus" <- function(x, field = NULL, value) {
    if ("texts" %in% field) stop("You should use texts() instead to replace the corpus texts.")
    if (is.null(field)) {
        field <- names(value)
        if (is.null(field))
            field <- paste("docvar", seq_len(ncol(as.data.frame(value))), sep="")
    }
    documents(x)[field] <- value
    return(x)
}

## internal only
"docvars<-.tokenizedTexts" <- function(x, field = NULL, value) {
    
    if (is.null(field) && (is.data.frame(value) || is.null(value))) {
        attr(x, "docvars") <- value
    } else {
        if (!is.data.frame(attr(x, "docvars"))) {
            vars <- data.frame(value, stringsAsFactors = FALSE)
            colnames(vars) <- field
            attr(x, "docvars") <- cbind(get_system_docvars(x), vars)
        } else {
            attr(x, "docvars")[[field]] <- value
        }
    }
    return(x)
}

## internal only
"docvars<-.dfm" <- function(x, field = NULL, value) {
    
    if (is.null(field) && (is.data.frame(value) || is.null(value))) {
        x@docvars <- value
    } else {
        if (!is.data.frame(x@docvars)) {
            vars <- data.frame(value, stringsAsFactors = FALSE)
            colnames(vars) <- field
            x@docvars <- cbind(get_system_docvars(x), vars)
        } else {
            x@docvars[[field]] <- value
        }
    }
    return(x)
}

#' get or set document-level meta-data
#' 
#' Get or set document-level meta-data.  Document-level meta-data are a special 
#' type of \link{docvars}, meant to contain information about documents that 
#' would not be used as a "variable" for analysis. An example could be the 
#' source of the document, or notes pertaining to its transformation, copyright 
#' information, etc.
#' 
#' Document-level meta-data differs from corpus-level meta-data in that the 
#' latter pertains to the collection of texts as a whole, whereas the 
#' document-level version can differ with each document.
#' @param x a \link{corpus} object
#' @param field character, the name of the metadata field(s) to be queried or 
#'   set
#' @return For \code{texts}, a character vector of the texts in the corpus.
#'   
#'   For \code{texts <-}, the corpus with the updated texts.
#' @note Document-level meta-data names are preceded by an underscore character,
#'   such as \code{_language}, but when named in in the \code{field} argument, 
#'   do \emph{not} need the underscore character.
#' @examples 
#' mycorp <- corpus_subset(data_corpus_inaugural, Year > 1990)
#' summary(mycorp, showmeta = TRUE)
#' metadoc(mycorp, "encoding") <- "UTF-8"
#' metadoc(mycorp)
#' metadoc(mycorp, "language") <- "english"
#' summary(mycorp, showmeta = TRUE)
#' @seealso \code{\link{metacorpus}}
#' @export
#' @keywords corpus
metadoc <- function(x, field = NULL) 
    UseMethod("metadoc")


#' @noRd
#' @export
metadoc.corpus <- function(x, field = NULL) {
    if (!is.null(field)) {
        field <- paste0("_", field)
        check_fields(x, field)
    }
    dvars <- documents(x)[, which(substring(names(documents(x)), 1, 1) == "_"), drop = FALSE]
    get_docvars(dvars, field)
}

#' @noRd
#' @export
metadoc.tokens <- function(x, field = NULL) {
    if (!is.null(field)) {
        field <- paste0("_", field)
        check_fields(x, field)
    }
    dvars <- attr(x, "docvars")[, which(substring(names(attr(x, "docvars")), 1, 1) == "_"), drop = FALSE]
    get_docvars(dvars, field)
}

#' @noRd
#' @export
metadoc.dfm <- function(x, field = NULL) {
    if (!is.null(field)) {
        field <- paste0("_", field)
        check_fields(x, field)
    }
    dvars <- x@docvars[, which(substring(names(x@docvars), 1, 1) == "_"), drop = FALSE]
    get_docvars(dvars, field)
}

#' @rdname metadoc
#' @param value the new value of the new meta-data field
#' @export
"metadoc<-" <- function(x, field = NULL, value) 
    UseMethod("metadoc")

#' @noRd
#' @export
"metadoc<-" <- function(x, field = NULL, value) {
    # CHECK TO SEE THAT VALUE LIST IS IN VALID DOCUMENT-LEVEL METADATA LIST
    # (this check not yet implemented)
    if (is.null(field)) {
        field <- paste("_", names(value), sep="")
        if (is.null(field))
            field <- paste("_metadoc", seq_len(ncol(as.data.frame(value))), sep = "")
    } else {
        field <- paste("_", field, sep="")
    }
    documents(x)[field] <- value
    x
}

## helper function to check fields and report error message if
## a field is not a valid docvar name
check_fields <- function(x, field = NULL) {
    if (!is.null(field)) {
        if (is.corpus(x)) {
            vars <- documents(x)
        } else if (is.tokens(x)) {
            vars <- attr(x, 'docvars')
        } else if (is.dfm(x)) {
            vars <- x@docvars
        }
        is_exist <- field %in% colnames(select_fields(vars, c('user', 'system')))
        if (any(!is_exist))
            stop("field(s) ", field[!is_exist], " not found", call. = FALSE)
    }
}

## internal function to select docvara fields
select_fields <- function(x, types = c('user', 'system')) {
    if (!is.data.frame(x)) return(data.frame())
    names <- names(x)
    is_system <- stri_startswith_fixed(names, '_') 
    is_text <- stri_detect_fixed(names, 'texts') | stri_detect_fixed(names, '_texts')
    
    result <- data.frame(row.names = row.names(x))
    if ('text' %in% types) {
        result <- cbind(result, x[,is_text, drop = FALSE])
    } 
    if ('system' %in% types) {
        result <- cbind(result, x[,is_system & !is_text, drop = FALSE])
    }
    if ('user' %in% types) {
        result <- cbind(result, x[,!is_system & !is_text, drop = FALSE])
    } 
    return(result)
}

# internal function to set system field in docvars
get_system_docvars <- function(x) {
    
    vars <- NULL
    if (is.corpus(x)) {
        vars <- documents(x)
    } else if (is.tokens(x)) {
        vars <- attr(x, 'docvars')
    } else if (is.dfm(x)) {
        vars <- x@docvars
    }
    vars <- select_fields(vars, 'system')
    if (is.null(vars) || nrow(vars) != ndoc(x))
        vars <- data.frame(row.names = docnames(x))
    names <- colnames(vars)
    if (!'_document' %in% names)
        vars[['_document']] <- docnames(x)
    if (!'_docid' %in% names)
        vars[['_docid']] <- seq_len(ndoc(x))
    if (!'_segid' %in% names)
        vars[['_segid']] <- rep(1L, ndoc(x))
    if (!'_length' %in% names)
        vars[['_length']] <- ntoken(x)
    
    vars <- vars[,c('_document', '_docid', '_segid', '_length')] # sort and drop unknown
    return(vars)
}

get_user_docvars <- function(x) {

    vars <- NULL
    if (is.corpus(x)) {
        vars <- documents(x)
    } else if (is.tokens(x)) {
        vars <- attr(x, 'docvars')
    } else if (is.dfm(x)) {
        vars <- x@docvars
    }
    vars <- select_fields(vars, 'user')
    if (is.null(vars) || nrow(vars) != ndoc(x))
        vars <- data.frame(row.names = docnames(x))
    return(vars)
}

