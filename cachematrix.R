## The following two functions "makeCacheMatrix" and "cacheSolve" can be used together to
## avoid the repeated invocation of the time consuming "solve" operation.
## e.g. 
## m <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
## m.1 <- cacheSolve(m)

## Creates a wrapper around a matrix that can store
## the result of a time consuming operation.
## The input matrix can be read and modified with the $get and $set functions
## The cached results can be accessed with $get.cache and set with $set.cache

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  
  set.cache <- function(data) cache <<- data
  get.cache <- function() cache
  list(set = set, get = get,
       set.cache = set.cache,
       get.cache = get.cache)
}


## Solves the inverse of a matrix that has been wrapped
## with the function "makeCacheMatrix".
## The result of the solve function is returned and stored in the given matrix.
## The same result is returned on sub sequent invocations of this function.
## All additional parameters are passed to the solve function.

cacheSolve <- function(x, ...) {
  inv <- x$get.cache()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$set.cache(inv)
  inv
}
