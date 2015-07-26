##  makeCacheMAtrix function creates a special matrix which is a list
##   containing functions to:
##       1 set the value of the matrix
##       2 get the value of the matrix
##       3 set the value of the inverse
##       4 get the value of the inverse
 
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
          x <<- y
          #Matrix value changed, invalidate inverse
          inverse <<- NULL
      }
      #Get the Value of Matrix
      get <- function() x
      
      #Set the value of inverse
      setinverse <- function(inverseValue) inverse <<- inverseValue
      
      #get the Value of inverse
      getinverse <- function () inverse
      
      #Return list of functions
      list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the 
## special "matrix" created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minverse <- x$getinverse()
        
        if (!is.null(minverse)) {
              message("getting cached data")
              return(minverse)
        }
        
        
        mymatrix <- x$get()
        minverse <- solve(mymatrix)
        x$setinverse(minverse)
        minverse
}
