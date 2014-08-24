## This program will take advantage of the scoping rules of the R language
## and how they can be manipulated to preserve state inside of an R object.
## It contains a pair of functions that cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	## Set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	## Get the value of the matrix
        get <- function() x
	## Set the inverse value of the matrix
        setinverse <- function(inverse) m <<- inverse
	## Get the inverse value of the matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
	## If the inverse has already been calculated (and the matrix has not
	## changed), retrieve the inverse from the cache.
        m <- x$getinverse()		
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
