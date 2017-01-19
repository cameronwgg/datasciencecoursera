## These functions take a matrix (x) as an argument and stores its inverse in a different environment.
## The inverse can be retrieved from the cache if it has already been calculated, using the second function.

## makeCacheMatrix takes a matrix as an argument and returns a list of functions that:
## 1. Sets the value of the matrix 
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse of the matrix 
## 4. Gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inv) s <<- inv
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve takes the output of makeCacheMatrix and
## checks with an "if" statement whether the inverse has already been calculated.
## If it has, it returns the inverse from the cache.
## If it has not, it calculates and returns the inverse using the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        matrixdata <- x$get()
        s <- solve(matrixdata)
        x$setinv(s)
        s
}
