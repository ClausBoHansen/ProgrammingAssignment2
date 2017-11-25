## Functions to handle caching of a matrix's solve.
## Once the solve is calculated, the solve will be served from cache,
## not calculated again.

## makeCacheMatrix returns an abject containing a matrix, it's solve and methods:
## set      - Sets the matrix
## get      - Gets the matrix
## setsolve - Sets the solve of the matrix
## getsolve - Gets the solve of the matrix
## This object does not calculate the solve, it only serves to store the
## matrix and it's solve
## By Claus Bo Hansen, strongly inspired by Roger D. Peng's script;-)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(sol) s <<- sol
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Returns the solve of matrix x. If solve has been previously
## calculated, it's served from cache in the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  
}
