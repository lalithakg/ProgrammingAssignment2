## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        ##set the value of the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ##get the value of the matrix using get function
        get <- function() x
        ##set the inverse of the matrix using setsolve
        setsolve <- function(solve) s <<- solve
        ##get the inverse of the matrix using getsolve function
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        ##checks to see if the inverse has already been calculated
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        ##sets the value of inverse in the cache via the setsolve function
        x$setsolve(s)
        s
}
