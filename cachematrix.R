## Pair of functions that cache the inverse of a matrix

## makeCacheMatrix create a special "matrix" object that can cache its inverse
## Contain list a function
## 1.- set the value of matrix
## 2.- get the value of matrix
## 3.- set the value of the inverse of matrix (solve function)
## 4.- get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    } ## 1
    get <- function() x ## 2
    setinv <- function(solve) m <<- solve  ## 3
    getinv <- function() m  ## 4
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  ## Create list function

}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }   ## Verify is inverse of 'x' is already computed
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
