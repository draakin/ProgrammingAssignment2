## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#creates makecachematrix object
makeCacheMatrix <- function(vec = matrix(NA,10,10)) {
  invvec <- matrix(NA,10,10)
  set <- function(y) {
    vec <<- y
    invvec <<- matrix(NA,10,10)
  }
  get <- function() vec
  setinverse <- function(solve) invvec <<- solve
  getinverse <- function() invvec
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#gets cached inverse or caclulates inverse
cacheinverse <- function(x, ...) {
  invvec <- x$getinverse()
  if(!is.na(invvec)) {
    message("getting cached data")
    return(invvec)
  }
  data <- x$get()
  invvec <- solve(data, ...)
  x$setinverse(invvec)
  invvec
}
