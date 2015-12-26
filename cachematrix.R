## The following 2 functions have the purpose to to store the inverse of a given matrix
## and solve the matrix in case the inverse hasn't been calculated yet

## This function will cache the inverse of the given matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  cachedInverse <- NULL  
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## this function will check if the inverse of the matrix exists, return it if it's true,
## otherwise it will calculate the inverse, store it, and give it as output

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() ## i create an object 'inverse' and assign to it 
                            ## the inverse of 'x'
  if(!is.null(inv)) { ## here i check if the inverse of the function exists
    
    message("getting cached data")
    
    return(inv)  ## and then return the Inverse
  }
  
  data <- x$get() ## here we are in the case where there is NO cached data,
                  ## therefore i call the subfunction get and assign 'x' to data
  inv <- solve(data, ...) ## solve (calculate the inverse) of the matrix
  
  x$setInverse(inv) ## store the inverse of the matrix for future use
  
  inverse ## give the inverse as output
  }
