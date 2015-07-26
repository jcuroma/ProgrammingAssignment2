## rprog-030 R Programming - ProgrammingAssignment2

## The makeCacheMatrix function generates an inverse matrix
## that encompasses a list to
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     t <- NULL
    set <- function(y) {
        x <<- y
        t <<- NULL
    }
    get <- function() x
    setinv <- function(inv) t <<- inv
    getinv <- function() t
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function computes the inverse of the matrix. 
## This checks to see if the inverse has been already computed.
## In that case, it gets the inverse from the cache and skips the calculation.
## In other case, it computes the inverse of the matrix and sets the value 
## of the inverse in the cache using the setinv function.

cacheSolve <- function(x, ...) {
        t <- x$getinv()
    if(!is.null(t)) {
        message("getting cached data")
        return(t)
    }
    data <- x$get()
    t <- solve(data, ...)
    x$setinv(t)
    t
}
