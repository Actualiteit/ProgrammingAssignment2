## Function which calculates and caches the inverse of matrices.

# makeCacheMatrix creates a special object from the provided matrix.
# This object consists of a list of functions to get, manipulate and calculate
# the provided matrix.
makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) Inv <<- solve
  getInv <- function() Inv
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
  Inv <- x$getInv()
  if (!is.null(Inv)) {
    message("Returning cached inverse")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
}

