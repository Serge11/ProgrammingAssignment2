## Caching the Inverse of a Matrix:
## The function creates a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inversa <- NULL
  set <- function(y) {
    x<<-y
    inversa<<-NULL
  }
  get <- function() x
  setInversa <- function(solve) inversa <<- solve
  getInversa<-function()inversa
  list(set=set,get=get,
       setInversa=setInversa,
       getInversa=getInversa)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa<-x$getInversa()
  if(!is.null(inversa)){
    message("getting cached data")
    return(inversa)
  }
  data<-x$get()
  inversa<-solve(data,...)
  x$setInversa(inversa)
  inversa
}
