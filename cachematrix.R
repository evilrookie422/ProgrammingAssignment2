## Put comments here that give an overall description of what your
## functions do

## This function sets/gets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  
 
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  
  #Stores all the functions in a list
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


##Checks to see if the inverse of m has been previously calculated
## If it has, Return the cached matrix that is the inverse of 'x'
## If it hasn't, compute the inverse of the matrix m
cacheSolve <- function(x, ...) {
 
  m <- x$getmatrix()
  
  # Checks to see if m has been previously solved
  if(!is.null(m)) { 
    message("getting cached data") 
    return(m)
  }
  
  #if m is null, solve matrix
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
