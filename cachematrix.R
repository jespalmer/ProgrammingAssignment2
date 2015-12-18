## You can use makeCacheMatrix and cacheSolve together to calculate, store, and return a matrix that is the inverse of 'X'. makeCacheMatrix makes a "cache matrix" and cacheSolve solves and stores the inverse.

## makeCacheMatrix is a function that takes a matrix, sets its inverse to NULL and creates other functions to set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a function that takes an input x and checks to see if the inverse is in the cache; if so it returns it. If not in the cache, it calculates, stores and then returns the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
