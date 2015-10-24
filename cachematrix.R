## Below two functions are used to store a Matrix and cache its inverse.

## The first function, `makeCacheMatric` creates a special "matrix" object, which is
## really a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
          }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
    }


## The following function computes the inverse of the special "matrix" returned by 
## the above 'makeCacheMatrix' function . If the Inverse has already been caclutaed
##(and then matrix as not changed), then the cacheSolve should retrieve the inverse
## from the Cache


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
