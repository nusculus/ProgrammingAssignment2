## These are functions to create and access an object containing a 
## matrix and its inverse. To save processing time, the matrix inverse is
## cached when possible for future accesses.

## Create, initialize, and return the object to store and access the matrix
# and its inverse.

makeCacheMatrix <- function(cached_matrix = matrix()) {
        
        matrix_inverse <- NULL
        
        set <- function(new_matrix) {
                cached_matrix <<- new_matrix
                matrix_inverse <<- NULL
        }
        
        get <- function() {
                cached_matrix
        }
        
        getinverse <- function() {
                matrix_inverse
        }
        
        setinverse <- function(new_inverse) {
                matrix_inverse <<- new_inverse
        }
        
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## Calculate, store, and return the inverse of a makeCacheMatrix matrix 
## and return it. If the inverse has already been calculated and cached, then
## return that stored value.

cacheSolve <- function(x, ...) {
        
        the_matrix <- x$get()
        if(is.na(the_matrix[1][1])) {
                stop("Atempting to retrieve inverse of an empty matrix")
        }
        
        result <- x$getinverse()
        if(is.null(result)) {
                result <- solve(the_matrix, ...)
                x$setinverse(result)
        } else {
                message("getting cached data")
        }

        result
}
