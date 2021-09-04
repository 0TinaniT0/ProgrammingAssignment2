## The key use of the makeCacheMatrix function and the cacheSolve function is
## to compute the inverse of matrices. 

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse. Specifically, the function set and get the value of the 
## matrix, denoted by 'x' and the inverse, which is denoted by 'z'.  

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function()x
  set_inverse <- function(inverse) z <<- inverse
  get_inverse <- function() z
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## The cacheSolve function returns the inverse of the matrix created 
## with the above function. The function first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the set_inverse function.

cacheSolve <- function(x, ...) {
  z <- x$get_inverse()
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  mat <- x$get()
  z <- solve(mat, ...)
  x$set_inverse(z)
  z
}