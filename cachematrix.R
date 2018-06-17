## This collection of functions provides a potential speed increase for 
## calculating the inverse of a matrix by caching the matrix and its 
## inverse as an R object and stores them in in the parent environment 
## through the use of lexical scoping. The "makeCacheMatrix()" encapsulates
## the matrix and its inverse and provides accessor/mutator functions 
## for editing these variables. The "cacheSolve()" function takes this 
## cached matrix and calculates its inverse, first checking whether
## the inverse has already been calculating and return that value if it
## exists.

## This function encapsulate a matrix and its inverse, and provides
## access to them through a list of accessor/mutator functions.
## If the value of the matrix is changed, the value of its inverse is 
## is set to NULL, to serve as a flag for "cacheSolve()" to recalculate 
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) { ##Set the value of the matrix and reset its inverse to NULL
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function takes a cached matrix object as its input and returns
## its inverse. The function first checks whether the inverse has previously
## been calculated and returns that value from the cached object. If it
## has not been calcualted (or the value of the matrix has changed by using the
## its "set()" function, then the inverse is recalcualted using the "solve()"
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i
}
