## Caching the Inverse of a Matrix


## This function creates a special "matrix" object that can cache its inverse
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

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Calling the functions
m <- matrix(rnorm(9),3,3)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)

#Result
#           [,1]       [,2]         [,3]
#[1,] -0.5022953  0.6154404 -0.286333206
#[2,]  1.6043606 -0.8907853 -0.003284467
#[3,]  2.3707447 -2.7510088 -0.689488907
          
