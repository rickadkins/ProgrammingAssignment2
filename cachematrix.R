## Matrix functions to extend the basic functionality of a matrix
## The extended matrix will hold a reference to its inverse 

## Matrix extension.  
## The internal methods allow external functions to store inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## internal inverse matrix (inverse of x)
  matrix.cache <- NULL
  
  setmatrix<-function(external.matrix){
    ## set the original matrix to the external.matrix
    x<<- external.matrix
    ## reset the cached inverse matrix
    ## this is because the cached value may now be invalid
    matrix.cache<<-NULL
  }
  
  ## Functions to work with the internal matrix and the cached inverse matrix
  get<-function() {
    ## returns the original matrix
    x
  }
  setinverse<-function(external.inverse) {
    ## TODO: test to make sure the calling application passed in the inverse to x
    ## if x%*%external.inverse==identity matrix, then ok.  Else, set to null
    ## sets the internal cache value to an externally inverted matrix
    matrix.cache <<- external.inverse
  }
  getinverse<-function(){
    ## returns the cached inverted matrix
    matrix.cache
  }
  ## prints the available functions and the current value
  list(setmatrix=setmatrix, get=get,
       setinverse=setinverse, getinverse=getinverse)
}


## This function exercises the extensions created by makeCacheMatrix()
## This function shows how to use extended matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x is extended matrix created by using makeCacheMatrix
  
  ## get the inverse matrix
  m <- x$getinverse()
  
  ## If m is NOT NULL, then there is a cached value
  if(!is.null(m)) {
    message("getting cached data")
    ## A return here exits the function
    return(m)
  }
  
  ## m is NULL, so need to calculate the inverse
  data <- x$get()
  m <- solve(data, ...)
  
  ## put the inverse into cache
  x$setinverse(m)
  
  ## return the inverse
  m
}

## to test:
## my.matrix<-rbind(c(1,-1/2),c(-1/2,1))
## test.cacheMatrix<-makeCacheMatrix(my.matrix)
## my.inverse<-solve(my.matrix)
## test.cacheMatrix$setinverse(my.inverse)
## my.matrix%*%test.cacheMatrix$getinverse()

## should display the identity matrix
##        [,1] [,2]
##  [1,]    1    0
##  [2,]    0    1
