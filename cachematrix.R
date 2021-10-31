## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse){inv<<- inverse}
  getInverse <- function(){inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve computes inverse of "matrix" returned by makeCacheMatrix. 
#If inverse has already been calculated (and the matrix has not changed), then 
#cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
