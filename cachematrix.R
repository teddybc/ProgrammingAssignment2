## The goal is to create two functions: makeCacheMatrix and cacheSolve. 

## makeCacheMatrix creates a matrix that can cache its inverse and is a list containing a 
## function to:
##    1.set the value of the matrix
##    2.get the value of the matrix
##    3.set the value of the inverse
##    4.get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      
      set <- function(y) {    ## set value of matrix
            x <<- y
            m <<- NULL
      }
      
      get <- function() x     ## get value of matrix
      
      setinverse <- function(inverse) m <<- inverse   ## set value of inverse
      
      getinverse <- function() m    ## get value of inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cacheSolve computes inverse of matrix returned by makeCacheMatrix. If the inverse has been 
## already calculated (with no changes in the matrix), the cacheSolve retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
      
      m <- x$getinverse() ## get value of inverse
      
      if(!is.null(m)) {   ## check if inverse calculated
            m_base<-NULL
            m_base<-x$get()
            
            if(identical(dim(m), dim(m_base))) {  ## check for no changes in matrix
                  message("getting cached data")
                  m      ## retrieve cached inverse
            }          
      }
      
      data.matrix <- x$get()       ## get value of matrix
      m <- solve(data.matrix, ...) ## calculate inverse of matrix
      x$setinverse(m)              ## set value of inverse
      m                            ## return a matrix that is the inverse of 'x'
      
}
