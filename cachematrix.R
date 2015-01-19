## The following functions allow the creation of matrices caching their own inverse.
## Once constructed such a matrix via the 'makeCacheMatrix' function, 
## use 'cacheSolve' to compute the matix' inverse. If the inverse has been computed
## previously, the cached inverse is merely returned, unless the matrix was changed
## meanwhile.

# Constructs matrix which caches its own inverse.
# Argument x denotes the initial matrix that should be
# turned into a cached variant.
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    # sets new matrix and clears cache
    set <- function(y) {
        
        x <<- y
        inverse <<- NULL
    }
    
    # gets matrix and get/set inverse
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    
    # return cached matrix as list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}

# Computes inverse of cached matrix. If the inverse
# has been computed previously, the cached version of 
# the inverse is returned
cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    
    # if the inverse has already been computed
    if(!is.null(inverse)) {
        
        message("getting cached matrix")
        return(inverse)
    }
    
    # get matrix and actually compute inverse
    data <- x$get()
    inverse <- solve(data, ...)
    
    # cache inverse and return it
    x$setinverse(inverse)
    inverse
}
