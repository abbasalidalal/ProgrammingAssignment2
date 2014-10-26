## Following functions create a special object that stores  
## a matrix vector and cache's its inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {    ## sets the value of matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x     ## gets the value of matrix
        setinverse <- function(solve) m <<- solve    ## sets the inverse of matrix
        getinverse <- function() m      ## gets the inverse of matrix
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by  
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed),then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)    ## returns the cached inverse
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)   
        m          ## returns the computed inverse
        }

