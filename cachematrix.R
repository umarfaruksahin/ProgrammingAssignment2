## My functions create a special matrix object and cache its inverse. If inverse of my matrix has already been calculated
## then my second fucntion get the inverse from the cache or if it's not, calculate it and give the result.

## "makeCacheMatrix" function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y, r) {
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inversematrix <<- solve
        getsolve <- function() inversematrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## "cacheSolve" function computes the inverse of the matrix returned by previous function "makeCacheMatrix". 
## If the inverse has already been calculated (and the matrix has not changed),
##  then the "cacheSolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting inversed matrix")
                return(inversematrix)
        }
        data <- x$get()
        inversematrix <- solve(data, ...)
        x$setsolve(inversematrix)
        inversematrix

}
