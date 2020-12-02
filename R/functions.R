#' Polynomial generation and multiplication
#' 
#' @description
#' Generate a polynomial object.
#'
#' @param s a vector specifying the the coefficients of the polynomial
#'
#' @details There are 2 methods for multiplication and one for printing
#' @return An object of class \code{Poly} giving the coefficients of the polynomial.
#' @references \url{https://en.wikipedia.org/wiki/Polynomial}.
#' @author Jakub Cery and Andrea Srnakova
#' @examples
#' ## Generate the sequence of the first 10 Fibonacci numbers.
#' p <- Poly(c(1,2,3))
#' ## One can now print:
#' print(p)
#' @export
Poly <- function(s) {
  x <- as.numeric(s)
  # getting rid of zeroes
  l <- length(x)
  while (l > 1 & x[l] == 0) {
    x <- x[-l]
    l <- length(x)
  }
  class(x)<-"Poly"
  x
}

#' @noRd
#' @export
print.Poly <- function(x, ...) {
  if (length(x)==1) {
    cat(x)
  } else { 
    s <- ""
    for (i in 0:(length(x)-1)) {
      # we want to print signs
      if (x[i+1]==0) next
      if (x[i+1]<0) sign<-"-"
      if (x[i+1]>0) sign<-"+"
      # first coefficient has no x
      if (i == 0) {
        s <- paste(s,sign,abs(x[i+1]),sep="")
      }
      # second coefficient has no exponent
      if (i==1) {
        # if coef=1, dont print it
        if (abs(x[i+1])==1) {temp<-""}
        else temp<-paste(abs(x[i+1]),sep="")
        # add to s
        s <- paste(s,sign,temp,"x",sep="")
      }
      # other coefficients
      if (i>1) {
        # if coef=1, dont print it
        if (abs(x[i+1])==1) {temp<-""}
        else temp<-paste(abs(x[i+1]),sep="")
        # add to s
        s <- paste(s,sign,temp,"x^",i,sep="")
      }
    }
    # remove the first +
    if (substring(s,1,1)=="+") s<-paste(substring(s,2),sep="")
    # create resulting string
    res<-""
    for (i in strsplit(s,"")[[1]]) {
      # put spaces before and after signs
      if (i=="+"||i=="-") {
        res <- paste(res," ",i, " ",sep="")
        next
      }
      # otherwise just paste together
      res <- paste(res,i, sep="")
    }
    # delete first space in case of -
    if (substring(res,1,1)==" ") {
      res <- paste("-",substring(res,4),sep="")
    }
    cat(res)
  }
}

#' @noRd
#' @export
polymult.Poly <- function(p,q) {
  res <- rep(0,length(q)+length(p)-1)
  # multiply each coef of p  
  for (i in 1:length(p)) {
    #multiply current coef of p with each coef of q
    for (j in 1:length(q)) {
      res[i+j-1] <- res[i+j-1]+p[i]*q[j]
    }
  }
  #generate result
  Poly(res)
}

#' @noRd
#' @export
#' @useDynLib ex32 polymult_c
polymult_C_c <- function(x, y) {
  #dyn.load("polymult.dll")
  z <- double(length(x) + length(y) - 1L)
  .C("polymult_c", as.double(x), as.double(y), length(x), length(y), z)[[5L]]
}
