## 2 functions to cache the invert of a matrix.

## Create a special "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInv <- function(solveMat) {
    inv <<- solveMat
  }
  getInv <- function() {
    inv
  }
  list(set=set, get=get,
       setInverse=setInv,
       getInverse=getInv)
}


## Computes the inverse of the special “matrix”. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
