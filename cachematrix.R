## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #definition of set function
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        #definition of get function
        get <- function() x
        #definition of setinverse function
        setinverse <- function(inverse) inv <<- inverse
        #definition of getinverse function
        getinverse <- function() inv
        #return a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        #if the inverse has already been computed, return the cached inverse
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        #else get the data and calculate the inverse
        data <- x$get()
        inv <- solve(data)
        #set the newly calculated inverse
        x$setinverse(inv)
        #return the calculated inverse
        inv
}
