## Gerhard Diedericks - RProg-031 Assignment 2

## Below are two functions, makeCacheMatrix() and solveCache()

## makeCachematrix() returns a list containing functions to :
##    1. set the matrix
##    2. get the matrix
##    3. set the inverse
##    4. get the invesre

## The function cacheSolve() use the above list as input
## x is a square invertable matrix

makeCacheMatrix <- function(x = matrix()) {

      
      ## Initialise the inverse object
      inv <- NULL                         
      set <- function(y) {
            x <<- y            
            inv <<- NULL
      }
      
      ## Get and returns the matrix
      get <- function() x
      
      ## Set the inverse of the matrix
      setInverse <- function(inverse) {
            inv <<- inverse
      }
      
      ## Get the inverse of the matrix
      getInverse <- function() inv
      
      ##Create a list of the methods
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Returns the inverse of the original matrix by first checking cache
## If not in cache, it calculates the inverse of the matrix using solve
## And returns that output to makeCacheMatrix()

cacheSolve <- function(x, ...) {
      ## x is the output of the makeCacheMatrix()
      ## returns the invesre of the original matrix to makeCacheMatrix()
      
      inv <- x$getInverse()
      
      ## Checks if inverse has already been calculated
      ## If yes, gets the inverse from cache
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      ## If no, calculates the inverse
      mat <- x$get()
      inv <- solve(mat, ...)
      
      ## Returns the calculated inverse matrix to cache via setinv function
      x$setInverse(inv)
      inv
}