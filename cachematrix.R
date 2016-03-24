## Getting and Cleaning Data
## Assignment 2 - Lexical Scoping
##
## Create a special matrix holder which can cache its inverted form.

## makeCacheMatrix: creates the matrix holder. It holds original matrix,
##     and after calling cacheSolve it will also hold a cached version of the inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvmatrix <- function(InvMatrix) m <<- InvMatrix
  getInvmatrix <- function() m
  list(set = set, get = get,
       setInvmatrix = setInvmatrix,
       getInvmatrix = getInvmatrix)
  
}

## cacheSolve: solves for and caches the inverted matrix.
## If already cached, just return cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getInvmatrix()              
  if(!is.null(m)) { # already have the inverse stored, just return it.
    message("getting cached data")  
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvmatrix(m) # store the inverse in the cache
  m
}

