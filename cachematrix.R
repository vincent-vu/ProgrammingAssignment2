## These 2 functions allow caching the inverse of a matrix within a list
## and restore the inversed matrix from cached data

## Function makeCacheMatrix returns a "cachable matrix" that is actually a list 
## This list stores the actual matrix provided by user and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
     
}


## Function cacheSolve attempts to retrieve the inverse matrix from cached data
## If inverse matrix has not been calculated, it computes the inverse matrix 
## and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
