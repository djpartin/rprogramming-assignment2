##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    inverse_x <- NULL
    set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(mtrix) inverse_x <<- mtrix
  getinverse <- function() inverse_x
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache cacheSolve <- function(x, ...) 
cacheSolve <- function(x, ...) 
{
  ## get the inverse of the matrix        
  inverse_x <- x$getinverse()
  
  ## check if there is the matrix   
  if(!is.null(inverse_x)) 
  {
    message("Found cached inverse matrix...returning")
    return(inverse_x)
  }
  ## if not: get the inverse of the matrix   
  data <- x$get()
  inverse_x <- solve(data, ...)
  
  ## set the inverse of the matrix 
  x$setinverse(inverse_x)
  inverse_x
}
