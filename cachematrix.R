## Caching the Inverse of a Matrix

## Create a special matrix with capability of caching matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  xt <- NULL
  set <- function(y) {
    x <<- y
    xt <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) xt <<- inverse
  getInverse <- function() xt
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculate matrix inverse and store it in special matrix
## Return cached matrix inverse if it has calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xt <- x$getInverse()
  if(!is.null(xt)) {
    message("getting cached data")
    return(xt)
  }
  data <- x$get()
  xt <- solve(data, ...)
  x$setInverse(xt)
  xt
}
