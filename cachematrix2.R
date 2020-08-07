makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x) != nrow(x)){
    stop("The provided matrix is not inversible. please provide a square matrix.")
  }
  m <- NULL
  set <- function(y) {
    if (ncol(y) != nrow(y)){
      stop("The provided matrix is not inversible. please provide a square matrix.")
    }
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

