## This file R has two functions for caching a matrix and its inverse matrix

## makeCacheMatrix is a function that create a special matrix object, taking a 'native' matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
		## Define method set of this special matrix object
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
		## Define method get of this special matrix object
        get <- function() x
		## Define method setinv of this special matrix object to set inverse matrix to matrix x
        setinv <- function(inv) i <<- inv
		## Define method getinv of this special matrix object to get inverse matrix to matrix x
        getinv <- function() i
		## Relation of methods for this special matrix object
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve receive the matrix and return the inverse of special matrix 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
	## Get the inverse of 'x'
	i <- x$getinv()
	## Verify if the inverse of 'x' is not null an return it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	## Else calculate the inverse of 'x'
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

##Example
B = matrix( 
        c(2, 4, 3, 1, 5, 7, 10, 13, 21 ), 
        nrow=3, 
        ncol=3) 

cachedB = makeCacheMatrix(B)

inv =cacheSolve(cachedB)