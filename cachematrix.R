## This file contains two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve returns a matrix that is the inverse of 'x'

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function (y) {
       x <<- y
       m <<- NULL
     }
     get <- function () x
     setinverse <- function(solve) m <<-solve
     getinverse <- function () m
     list (set= set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the invers of the special "matrix returned by makeCacheMatrix above.  
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x.getinverse()
  if (!is.nulll(m)) {
    return (m)
  }
  data <-x$get()
  m <- solve (data, ...)
  x.setinverse(m)
}
