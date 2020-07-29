## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function allows to set & get value of matrix,
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL ##initialising i with null
  
  ##setting the matrix value
  set <- function(y) {
    x <<- y ##assignment outside 
    i <<- NULL
  }
  
  ##getting the matrix value
  get <- function() x
  
  
  ##setting the matrix inverse value
  setinverse <- function(inverse) i <<- inverse
  
  ##returns 'i' matrix
  getinverse <- function() i
  
  ##implicit return  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
##This function inverses the returned matrix by makeChachematrix.
##If inverse exists, it retrieves from cache.
##If no inverse exists it calcualtes inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ##checking cache for matrix inverse
  i <- x$getinverse()
  
  ##checking contents of i and printing
  if (!is.null(i)) {
    message("No null in i. Retrieving cached data")
    return(i)
  }
  
  ## i is null, getting data
  data <- x$get()
  
  i <- solve(data, ...)##calculating matrix inverse
  
  x$setinverse(i)##storing the value of inverse in i
  
  i ##inverted matrix will be returned
  
  
}
