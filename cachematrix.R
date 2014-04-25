## The functions below, together, allow the user to solve the inverse
## of a square, invertable matrix, and to cache the inverse for
## later access.  For example: pass a matrix to the makeCacheMatrix
## function for the first time, and store the result in a variable.
## Pass that variable to cacheSolve for the first time and the 
## inverse will be generated and cached.  This may be a lengthy
## process.  Subsequent calls of cacheSolve on that variable, however,
## will yield the inverse in (potentially) substantially less time.

## This function takes a square, invertable matrix as an argument
## and returns four functions (set, get, setsolve, & getsolve)
## and stores them in the variable you assign 
## (e.g., output <- makeCacheMatrix(<your matrix here>))
## The matrix itself is stored in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list( set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve )
}


## This function takes as its argument the special variable created
## in the previous function.  For example, if you stored your output 
## in the variable output, you would pass that to cacheSolve
## cacheSolve computes the inverse of the original matrix the first time
## around, and caches it.  It retrieves the cached matrix upon subsequent
## calss

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if (!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
