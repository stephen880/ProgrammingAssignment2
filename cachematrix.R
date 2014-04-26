## makeCacheMatrix creates a special matrix object that can cache its inverse.
## It returns 4 functions: set, get, setInverse, GetInverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Set the inverse to an empty matrix
        i <- matrix()
        
        ## Allows for changing the matrix & resetting the inverse matrix
        set <- function(y) {
                x <<- y
                i <<- matrix()
        }
        
        ## Returns the matrix
        get <- function() x
        
        ## Sets the inverse matrix to the function input, n
        setInverse <- function(n) i <<- n
        
        ## Returns the inverse matrix
        getInverse <- function() i
        
        ## makeCacheMatrix returns a list of 4 functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function calculates & caches the inverse matrix, if needed
## If the inverse matrix has already been cached, the cached value is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Set m to the cached inverse matrix
        m <- x$getInverse()
        
        ## If the inverse matrix hasn't been initialized, the first value will be NA
        ## If the 1st value's not NA, display a message
        if(!is.na(m[1,1])) {
                message("getting cached data")
        
        ## If the 1st value's NA, calculate the inverse and cache it        
        } else {
        
        ## Get the initial matrix        
        data <- x$get()
        
        ## Calculate the inverse matrix
        m <- solve(data)
        
        ## Cache the inverse matrix
        x$setInverse(m)
        }
        
        ## Return the inverse matrix
        m
}
