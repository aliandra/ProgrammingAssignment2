## Function pair to cache the inverse of a matrix rather than computing
## it repeatedly

## create a special "matrix" object to cache inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## create empty matrix for inverse
        s <- NULL
        
        set <- function(y) {
                ## set matrix
                x <<- y
                ## clear inverse
                s <<- NULL
        }
        
        ## return matrix
        get <- function() x
        
        ## set s to computed inverse
        setsolve <- function(solve) s <<- solve
        
        ## return inverse
        getsolve <- function() s
        
        ## Matrix object
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## compute the inverse of the special "matrix" returned by 
## makeCacheMatrix above (represented by x)

cacheSolve <- function(x, ...) {
        
        ## Assign inverse to s
        s <- x$getsolve()
        
        ## Return inverse if already calculated and abort function
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        ## When s is empty
        
        ## get matrix 
        data <- x$get()
        
        ## compute inverse
        s <- solve(data, ...)
        
        ## set the inverse in the matrix object
        x$setsolve(s)
        
        ## return the inverse
        s
}
