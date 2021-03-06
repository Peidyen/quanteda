#' @rdname textstat_simil
#' @export
#' @param p The power of the Minkowski distance.
#' @details \code{textstat_dist} options are: \code{"euclidean"} (default), 
#'   \code{"Chisquared"}, \code{"Chisquared2"}, \code{"hamming"}, 
#'   \code{"kullback"}. \code{"manhattan"}, \code{"maximum"}, \code{"canberra"},
#'   and \code{"minkowski"}.
#' @references The \code{"Chisquared"} metric is from Legendre, P., & Gallagher,
#'   E. D. (2001).
#'   "\href{http://adn.biol.umontreal.ca/~numericalecology/Reprints/Legendre_&_Gallagher.pdf}{Ecologically
#'    meaningful transformations for ordination of species data}".
#'   \emph{Oecologia}, 129(2), 271–280. doi.org/10.1007/s004420100716
#'   
#'   The \code{"Chisquared2"} metric is the "Quadratic-Chi" measure from Pele,
#'   O., & Werman, M. (2010). 
#'   "\href{http://www.ariel.ac.il/sites/ofirpele/publications/ECCV2010.pdf}{The
#'   Quadratic-Chi Histogram Distance Family}". In \emph{Computer Vision – ECCV
#'   2010} (Vol. 6312, pp. 749–762). Berlin, Heidelberg: Springer, Berlin,
#'   Heidelberg. doi.org/10.1007/978-3-642-15552-9_54.
#'   
#'   \code{"hamming"} is \eqn{\sum{x \neq y)}}.
#'
#'   \code{"kullback"} is the Kullback-Leibler distance, which assumes that
#'   \eqn{P(x_i) = 0} implies \eqn{P(y_i)=0}, and in case both \eqn{P(x_i)} and
#'   \eqn{P(y_i)} equals to zero, then \eqn{P(x_i) * log(p(x_i)/p(y_i))} is
#'   assumed to be zero as the limit value.  The formula is:
#'    \deqn{\sum{P(x)*log(P(x)/p(y))}}
#'    
#'   All other measures are described in the \pkg{proxy} package.
#' @importFrom RcppParallel RcppParallelLibs
#' @author Kenneth Benoit, Haiyan Wang
#' @examples
#' # create a dfm from inaugural addresses from Reagan onwards
#' presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1990), 
#'                remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
#'                
#' # distances for documents 
#' (d1 <- textstat_dist(presDfm, margin = "documents"))
#' as.matrix(d1)
#' 
#' # distances for specific documents
#' textstat_dist(presDfm, "2017-Trump", margin = "documents")
#' textstat_dist(presDfm, "2005-Bush", margin = "documents", method = "eJaccard")
#' (d2 <- textstat_dist(presDfm, c("2009-Obama" , "2013-Obama"), margin = "documents"))
#' as.list(d1)
#' 
textstat_dist <- function(x, selection = NULL, 
                          margin = c("documents", "features"),
                          method = "euclidean",
                          upper = FALSE, diag = FALSE, p = 2) {
    UseMethod("textstat_dist")
}
    
#' @noRd
#' @export
textstat_dist.dfm <- function(x, selection = NULL, 
                          margin = c("documents", "features"),
                          method = "euclidean",
                          upper = FALSE, diag = FALSE, p = 2) {
    margin <- match.arg(margin)
    
    if (!is.null(selection)) {
        if (!is.character(selection)) 
            stop("'selection' should be character or character vector of document names or feature labels.")
        if (margin == "features") {
            selection <- intersect(selection, featnames(x))
            if (!length(selection))
                stop("The features specified by 'selection' do not exist.")
            y <- x[, selection, drop = FALSE]
        } else {
            selection <- intersect(selection, docnames(x))
            if (!length(selection))
                stop("The documents specified by 'selection' do not exist.")
            y <- x[selection, , drop = FALSE]
        }
    } else {
        y <- NULL
    }
    
    m <- if (margin == "documents") 1 else 2
    
    methods1 <- c("euclidean", "hamming", "Chisquared", "Chisquared2", "kullback", "manhattan", "maximum", "canberra")
    methods2 <- c("jaccard", "binary", "eJaccard", "simple matching")
    
    # methods1 <- char_tolower(methods1)
    # methods1 <- char_tolower(methods2)
    # method <- char_tolower(method)
    
    if (method %in% methods1) {
        temp <- get(paste0(method, "_sparse"))(x, y, margin = m)
    } else if (method == "minkowski") {
        temp <- get(paste0(method, "_sparse"))(x, y, margin = m, p = p)
    } else if (method %in% methods2) {
        if (method == "binary") method = "jaccard"
        temp <- get(paste0(method, "_sparse"))(x, y, margin = m)
    } else {
        stop(method, " is not implemented; consider trying proxy::dist().")
    }
    
    if (!is.null(selection)) {
        names <- c(colnames(temp), setdiff(rownames(temp), colnames(temp)))
        temp <- temp[names, , drop = FALSE] # sort for as.dist()
        #temp2 <- sparseMatrix(i = rep(seq_len(nrow(temp)), times = ncol(temp)),
        #                      j = rep(seq_len(ncol(temp)), each = nrow(temp)),
        #                      x = as.vector(temp), dims = c(length(names), length(names)),
        #                      dimnames = list(names, names))
    }
    
    # create a new dist object
    if (is.null(selection)) {
        result <- stats::as.dist(temp, diag = diag, upper = upper)
        attr(result, "method") <- method
        attr(result, "call") <- match.call()
        return(result)
    } else {
        result <- as.matrix(temp)
        if(!is.null(rownames(result)))
            attr(result,"Labels") <- rownames(result)
        else if(!is.null(colnames(result)))
            attr(result,"Labels") <- colnames(result)
        attr(result, "Size") <- if (margin == "documents") nrow(result) else ncol(result)
        attr(result, "method") <- method
        attr(result, "call") <- match.call()
        class(result) <- c("dist_selection")
        return(result)
    }
}


#' coerce a dist object into a list
#' 
#' Coerce a dist matrix into a list of selected target terms and similar terms,
#' in descending order of similarity.  Can be used after calling
#' \code{\link{textstat_simil}} or \code{\link{textstat_dist}}.
#' @param x dist class object
#' @param sorted sort results in descending order if \code{TRUE}
#' @param n the top \code{n} highest-ranking items will be returned.  If n is 
#'   \code{NULL}, return all items.
#' @param ... unused
#' @method as.list dist
#' @export
#' @examples 
#' \dontrun{
#' ## compare to tm
#' 
#' # tm version
#' require(tm)
#' data("crude")
#' crude <- tm_map(crude, content_transformer(tolower))
#' crude <- tm_map(crude, remove_punctuation)
#' crude <- tm_map(crude, remove_numbers)
#' crude <- tm_map(crude, stemDocument)
#' tdm <- TermDocumentMatrix(crude)
#' findAssocs(tdm, c("oil", "opec", "xyz"), c(0.75, 0.82, 0.1))
#' 
#' # in quanteda
#' quantedaDfm <- as.dfm(t(as.matrix(tdm)))
#' as.list(textstat_simil(quantedaDfm, c("oil", "opec", "xyz"), margin = "features"), n = 14)
#' 
#' # in base R
#' corMat <- as.matrix(proxy::simil(as.matrix(quantedaDfm), by_rows = FALSE))
#' round(head(sort(corMat[, "oil"], decreasing = TRUE), 14), 2)
#' round(head(sort(corMat[, "opec"], decreasing = TRUE), 9), 2)
#' } 
as.list.dist <- function(x, sorted = TRUE, n = NULL, ...) {
    # convert the matrix to a list of similarities
    if (!is.null(attr(x, "Labels"))) xLabels <- attr(x, "Labels")
    result <- lapply(seq_len(ncol(as.matrix(x))), function(i) as.matrix(x)[, i])
    attributes(x) <- NULL
    names(result) <- if (!is.null(xLabels)) xLabels
    
    # remove the element of each similarity vector equal to the item itself
    tempseq <- seq_along(result)
    names(tempseq) <- names(result)
    result <- lapply( tempseq, function(i)
        result[[i]] <- result[[i]][-which(names(result[[i]]) == names(result)[i])] )
    
    # sort each element of the list and return only first n results if n not NULL
    if (sorted == TRUE)
        result <- lapply(result, sort, decreasing=TRUE, na.last = TRUE)
    
    # truncate to n if n is not NULL
    if (!is.null(n))
        result <- lapply(result, "[", 1:n)
    
    result
    
}

#' coerce a dist into a dist
#' 
#' Internal function to guarantee that a dist remains a dist, if for some reason
#' a user wants to coerce a dist into a dist.
#' @keywords textstat internal
#' @importFrom stats as.dist
#' @method as.dist dist
#' @export
as.dist.dist <- function(m, diag = FALSE, upper = FALSE) {
    as.dist(as.matrix(m), diag = diag, upper = upper)
}


#' coerce a dist_selection object into a list
#' 
#' Coerce a dist_selection matrix into a list of selected terms and tarhet terms in
#' descending order.  Can be used after calling \code{\link{textstat_simil}} or
#' \code{\link{textstat_dist}} when selection is not NULL
#' @param x dist_selection class object
#' @param sorted sort results in descending order if \code{TRUE}
#' @param n the top \code{n} highest-ranking items will be returned.  If n is 
#'   \code{NULL}, return all items.
#' @param ... unused
#' @method as.list dist_selection
#' @keywords textstat internal
#' @export
as.list.dist_selection <- function(x, sorted = TRUE, n = NULL, ...) {
    # convert the matrix to a list of similarities
    if (!is.null(attr(x, "Labels"))) xLabels <- attr(x, "Labels")
    result <- lapply(seq_len(ncol(as.matrix(x))), function(i) as.matrix(x)[, i])
    #attributes(x) <- NULL
    names(result) <- if (!is.null(xLabels)) xLabels[1:ncol(as.matrix(x))]
    
    # remove the element of each similarity vector equal to the item itself
    tempseq <- seq_along(result)
    names(tempseq) <- names(result)
    result <- lapply( tempseq, function(i)
        result[[i]] <- result[[i]][-which(names(result[[i]]) == names(result)[i])] )
    
    # sort each element of the list and return only first n results if n not NULL
    if (sorted == TRUE)
        result <- lapply(result, sort, decreasing=TRUE, na.last = TRUE)
    
    # truncate to n if n is not NULL
    if (!is.null(n))
        result <- lapply(result, "[", 1:n)
    
    result
}

#' print a dist_selection object
#' 
#' Print method for a dist_selection object, to make it appear as a data.frame.
#' @export
#' @method print dist_selection
#' @keywords textstat internal
print.dist_selection <- function(x, ...) {
    print(as.matrix(x))
}

#' coerce a dist_selection object to a matrix
#' 
#' Coerce a dist_selection object to a plain matrix.
#' @export
#' @method as.matrix dist_selection
#' @keywords textstat internal
as.matrix.dist_selection <- function(x, ...) {
    attributes(x)[setdiff(names(attributes(x)), 
                          c("dimnames", "dim"))] <- NULL
    class(x) <- "matrix"
    x
}


## used Matrix::crossprod and Matrix::tcrossprod for sparse Matrix handling
euclidean_sparse <- function(x, y = NULL, sIndex = NULL, margin = 1){
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    marginSums <- if (margin == 2) colSums else rowSums
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod   
    n <- if (margin == 2) ncol(x) else nrow(x)
    
    if (!is.null(y)) {
        stopifnot(if (margin == 2) nrow(x) == nrow(y) else ncol(x) == ncol(y))
        an <- marginSums(x^2)
        bn <- marginSums(y^2)
        
        # number of features
        kk <- y@Dim[margin]
        tmp <- matrix(rep(an, kk), nrow = n) 
        tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
        eucmat <- sqrt( tmp - 2 * as.matrix(cpFun(x, y)) )
    } else {
        an <- marginSums(x^2)
        tmp <- matrix(rep(an, n), nrow = n) 
        tmp <-  tmp +  matrix(rep(an, n), nrow = n, byrow=TRUE)
        eucmat <- sqrt( tmp - 2 * as.matrix(cpFun(x)))
    }
    eucmat
}

# Hamming distance
# formula: hamming = sum(x .!= y)
hamming_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    
    # convert to binary matrix
    x <- tf(x, "boolean") 
    x0 <- 1 - x
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) nrow else ncol
    marginNames <- if (margin == 2) colnames else rownames
    # union 
    an <- marginSums(x)
    if (!is.null(y)) {
        y <- tf(y, "boolean")
        y0 <- 1 - y
        A <- cpFun(x, y)
        A0 <- cpFun(x0, y0)
        colNm <- marginNames(y)
    } else {
        A <- cpFun(x)
        A0 <- cpFun(x0)
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    # common values
    A <- A + A0
    hammat <- an -A
    dimnames(hammat) <- list(rowNm,  colNm)
    hammat
}

#Chi-squared distance:divide by row sums and square root of column sums, and adjust for square root of matrix total (Legendre & Gallagher 2001, Bruce McCune & James b. Grace 2002). 
#http://adn.biol.umontreal.ca/~numericalecology/Reprints/Legendre_&_Gallagher.pdf
# https://www.pcord.com/book.htm
#formula: Chi = sum((x/rowsum(x_i) - y/rowsum(y_i))^2/(colsum(i)/total))
Chisquared_sparse <- function(x, y = NULL, sIndex = NULL, margin = 1){
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    aveProfile <- if (margin == 2) sqrt(rowSums(x)/sum(x)) else sqrt(colSums(x)/sum(x))
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod   
    n <- if (margin == 2) ncol(x) else nrow(x)
    rowNm <- marginNames(x)
    colNm <- marginNames(x)
    if (margin == 1 ) {
        # convert into profiles
        x <- x/marginSums(x)
        
        # weighted by the average profiles
        x <- x %*% diag(1/aveProfile)
    } else {
        x <- x %*% diag(1/marginSums(x))
        x <- x / aveProfile
    }
    
    if (!is.null(y)) {
        stopifnot(if (margin == 2) nrow(x) == nrow(y) else ncol(x) == ncol(y))
        colNm <- marginNames(y)
        # aveProfile is same as that for x 
        if (margin == 1 ) {
            # convert into profiles
            y <- y/marginSums(y)
            
            # weighted by the average profiles
            y <- y %*% diag(1/aveProfile)
        } else {
            y <- if (dim(y)[margin] > 1) y %*% diag(1/marginSums(y)) else y %*% (1/marginSums(y))
            y <- y / aveProfile
        }
        an <- marginSums(x^2)
        bn <- marginSums(y^2)
        
        # number of features
        kk <- y@Dim[margin]
        tmp <- matrix(rep(an, kk), nrow = n) 
        tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
        chimat <- tmp - 2 * as.matrix(cpFun(x, y))
        #chimat <-  sqrt(round(chimat, 2)) 
    } else {
        an <- marginSums(x^2)
        tmp <- matrix(rep(an, n), nrow = n) 
        tmp <-  tmp +  matrix(rep(an, n), nrow = n, byrow=TRUE)
        chimat <-  tmp - 2 * as.matrix(cpFun(x))
        #chimat <-  sqrt(round(chimat, 2)) 
    }
    dimnames(chimat) <- list(rowNm,  colNm)
    chimat
}

## This chi-squared method is used for histogram: sum((x-y)^2/((x+y)))/2
##http://www.ariel.ac.il/sites/ofirpele/publications/ECCV2010.pdf
Chisquared2_sparse <- function(x, y = NULL, sIndex = NULL, margin = 1){
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    marginSums <- if (margin == 2) colSums else rowSums
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod   
    n <- if (margin == 2) ncol(x) else nrow(x)
    
    if (!is.null(y)) {
        stopifnot(if (margin == 2) nrow(x) == nrow(y) else ncol(x) == ncol(y))
        an <- marginSums(x^2)
        bn <- marginSums(y^2)
        
        # number of features
        kk <- y@Dim[1]
        tmp <- matrix(rep(an, kk), nrow = n) 
        tmp <-  tmp +  matrix(rep(bn, n), nrow = n, byrow=TRUE)
        chimat <- sqrt( tmp - 2 * as.matrix(cpFun(x, y)) )
    } else {
        an <- marginSums(x^2)
        tmp <- matrix(rep(an, n), nrow = n) 
        tmp <-  tmp +  matrix(rep(an, n), nrow = n, byrow=TRUE)
        
        a1 <- marginSums(x)
        sumij <- matrix(rep(a1, n), nrow = n) + matrix(rep(a1, n), nrow = n, byrow=TRUE)
        
        chimat <- ( tmp - 2 * as.matrix(cpFun(x)))/sumij/2
    }
    chimat
}

# Kullback-Leibler divergence: is a measure of the difference between probability distributions
# This metric is not symmetric, it is better applied with setting of "dist = FALSE" and "tri= FALSE"
# to avoid eoercing the result to a dist object.
# assumption: p(x_i) = 0 implies p(y_i)=0 and in case both p(x_i) and p(y_i) equals to zero, 
# p(x_i)*log(p(x_i)/p(y_i)) is assumed to be zero as the limit value.
# formula: sum(p(x)*log(p(x)/p(y)))
kullback_sparse <- function(x, y = NULL, margin = 1) {
    if (!(margin %in% 1:2)) stop("margin can only be 1 (rows) or 2 (columns)")
    cpFun <- if (margin == 2) Matrix::crossprod else Matrix::tcrossprod
    marginSums <- if (margin == 2) colSums else rowSums
    marginNames <- if (margin == 2) colnames else rownames
    
    # probability
    x <- x/marginSums(x)
    logx <- log(x)
    logx[is.na(logx)] <- 0L
    logx[is.infinite(logx)] <- 0L
    if (!is.null(y)) {
        y <- y/marginSums(y)
        logy <- log(y)
        logy[is.na(logy)] <- 0L
        logy[is.infinite(logy)] <- 0L
        kullmat <- marginSums(x*logx) - cpFun(x, logy)
        colNm <- marginNames(y)
    } else {
        kullmat <- marginSums(x*logx) - cpFun(x, logx)
        colNm <- marginNames(x)
    }
    rowNm <- marginNames(x)
    dimnames(kullmat) <- list(rowNm,  colNm)
    kullmat
}

# Manhattan distance: sum_i |x_i - y_i|
manhattan_sparse <- function(x, y=NULL, margin = 1){
    marginNames <- if (margin == 2) colnames else rownames
    if (!is.null(y)) {
        colNm <- marginNames(y)
        manmat <- qatd_ManhattanPara_cpp2(x, y, margin)
    } else {
        colNm <- marginNames(x)
        manmat <- qatd_ManhattanPara_cpp(x, margin)
    }
    dimnames(manmat) <- list(marginNames(x),  colNm)
    manmat
}

# Maximum/Supremum distance: max_i |x_i - y_i|
maximum_sparse <- function(x, y=NULL, margin = 1){
    marginNames <- if (margin == 2) colnames else rownames
    if (!is.null(y)) {
        colNm <- marginNames(y)
        maxmat <- qatd_MaximumPara_cpp2(x, y, margin)
    } else {
        colNm <- marginNames(x)
        maxmat <- qatd_MaximumPara_cpp(x, margin)
    }
    dimnames(maxmat) <- list(marginNames(x),  colNm)
    maxmat
}

# Canberra distance: sum_i |x_i - y_i| / |x_i + y_i|
# Weighted by num_nonzeros_elementsum/num_element
canberra_sparse <- function(x, y=NULL, margin = 1){
    marginNames <- if (margin == 2) colnames else rownames
    if (!is.null(y)) {
        colNm <- marginNames(y)
        canmat <- qatd_CanberraPara_cpp2(x, y, margin)
    } else {
        colNm <- marginNames(x)
        canmat <- qatd_CanberraPara_cpp(x, margin)
    }
    dimnames(canmat) <- list(marginNames(x),  colNm)
    canmat
}

# Minkowski distance: (sum_i (x_i - y_i)^p)^(1/p)
minkowski_sparse <- function(x, y=NULL, margin = 1, p = 2){
    marginNames <- if (margin == 2) colnames else rownames
    if (!is.null(y)) {
        colNm <- marginNames(y)
        minkmat <- qatd_MinkowskiPara_cpp2(x, y, margin, p)
    } else {
        colNm <- marginNames(x)
        minkmat <- qatd_MinkowskiPara_cpp(x, margin, p)
    }
    dimnames(minkmat) <- list(marginNames(x),  colNm)
    minkmat
}
