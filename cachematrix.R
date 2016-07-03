## In this assignment, there are 2 functions used to create a
## special "matrix" object that stores a matrix and to cache its
## inverse.

## makeCacheMatrix: This function creates a special "matrix"
## object that can cache its inverse.
## The function, creates a special "matrix", which is really a
## list containing a function to:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse
## 4. get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
