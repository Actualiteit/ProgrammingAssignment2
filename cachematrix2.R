## Function which calculates and caches the inverse of matrices.

# makeCacheMatrix creates a special object from the provided matrix.
# This object consists of a list of functions to get, manipulate and calculate
# the provided matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}
# cacheSolve checks if "Inv" already contains the inverse of the matrix.
# if "Inv" is not "NULL", it prints the inverse from "Inv".
# if "Inv" is "NULL", it calculates the inverse by applying the function solve()
# to the matrix stored in "data", Storing the output in "Inv".

cacheSolve <- function(x, ...) {
  m <- x$getInv
  if (!is.null(m)) {
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
  }
  message("Returning cached inverse")
  m
}

