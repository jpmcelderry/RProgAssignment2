## makeCacheMatrix generates a cached matrix and cacheSolve solves for the inverse
## of that matrix given that the matrix is invertible; if a value for the inverse of the
## matrix is cached and the matrix has not since been changed, cacheSolve will return the
## cached inverse

## input argument for makeCacheMatrix and x$set as: 
## makeCacheMatrix(matrix(c( , , ), #rows, #columns))

makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set<-function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<-solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## input argument as: cacheSolve(x), where x is the name of the cached matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  } else {
    data<- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m ##Return Inverse of matrix 'x'
  }
}