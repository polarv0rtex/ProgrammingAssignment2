## These functions are able to compute and cache
## inverse of a square matrix.

## This function creates a special "matrix"
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL ## create variable m with value NULL
        
        set <- function(y){ ## set the value of the matrix
                x <<- y ## cache the value of y
                m <<- NULL ## cache NULL in m
        }
        
        get <- function() x ## get the value of the matrix
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        
        ## create a list to perform the above functions
        list(set = set, get = get, 
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix
## above. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        
        ## if m is not null, present a message and return m
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        ## invert the matrix and return m
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
