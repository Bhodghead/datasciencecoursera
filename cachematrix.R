## the cacheMatrix functions create a special form of a matrix
## with a cached inversion. This allows us to store the inversion
## with the matrix and not recalculate it unless the matrix changes.

## makeCacheMatrix is a Class that wraps a matrix and which
## stores some additional data "inv" with that matrix. 
## inv is reset whenever the set method is called
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getinv <- function() inv
        setinv <- function(y){
                inv <<- y
        }
        
        list(set = set, get = get,
             getinv = getinv,
             setinv = setinv)
}


## cacheSolve extends the makeCacheMatrix class by using that
## class to invert the matrix. It uses the cached data if available.
cacheSolve <- function(x, ...) {     
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)  # use ginv() for the general case
        x$setinv(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
