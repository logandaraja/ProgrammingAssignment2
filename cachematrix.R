## The function makeCacheMatrix takes a numeric matrix and craete a special representation
## of a matrix with the following functions defined on them
## set the value of matrix with 'set'
## get the value of matrix with 'get'
## set the value of the inverse of the matrix with 'setinverse'
## get the value of the inverse of the matrix with 'getinverse'

## The function cacheSolve takes the special representation created by makeCacheMatrix and calculate the inverse 
## of the metrix if it has not been computer
## otherwise it return the inverse of the matrix if it is calculated previously

## usage example
## m<-rbind(c(5,2,3),c(1,-2,3),c(3,2,-2))
## newM=makeCacheMatrix(m)
## cacheSolve(newM)
## will solve the inverse and the response will be
##             [,1]       [,2]       [,3]
## [1,] -0.05555556  0.2777778  0.3333333
## [2,]  0.30555556 -0.5277778 -0.3333333
## [3,]  0.22222222 -0.1111111 -0.3333333
##
## if cacheSolve(newM) called again, then inverse wil NOT be computer, instead it be be
## reproduced from the previously stored results as
##
## getting cached data
## [,1]       [,2]       [,3]
## [1,] -0.05555556  0.2777778  0.3333333
## [2,]  0.30555556 -0.5277778 -0.3333333
## [3,]  0.22222222 -0.1111111 -0.3333333

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

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
