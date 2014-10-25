## These functions serve to reduce redundant computation by cacheing
## the calculated value of an inverted square matrix. The functions
## check to see if the value of the inverted matrix needs updating before
## calculating it, otherwise the value is retrieved from the cache.

## makeCacheMatrix creates a function which sets the value of the 
## square matrix, gets the value of the matrix, sets the value of the
## inverse of the matrix, and gets the value of the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }# end of set
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}# end of makeCacheMatrix

## cacheSolve calculates the inverse of the matrix created by the function
## makeCacheMatrix after checking to see if the calculation has already been
## performed. If yes, the cacheSolve gets the inverted matrix from the cache
## in lieu of re-computing the matrix inverse. If the calculation needs to be
## updated, cacheSolve calculates the matrix inverse and sets the value in
## the cache using the setsolve function defined within makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }# end of is.null if statement
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}# end of cacheSolve
