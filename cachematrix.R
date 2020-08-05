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

