## This function first creates a matrix and caches its inverse through the solve function
## The result is a list with four elements which set the value of the matrix,
## get the value of the matrix, set the value of the inverse and get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##here it sets the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ##here it gets the value of the matrix
  get <- function() x
  ##here it sets the value of the inverse of the matrix
  setSolve <- function(solve) m <<- solve
  ##here it gets the value of the inverse of the matrix
  getSolve <- function() m
  ##The result of the function is a list with the four elements
  list(set = set, get = get,
       setSolve= setSolve,
       getSolve = getSolve)
  
}

## With this function you can return the inverse of a matrix or take it from the last
## function if it exists and it has not changed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  ## If it has been calculated before, returns cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If it has not been calculated before, does the calculation
  matrix <- x$get()
  m <- solve(matrix)
  x$setSolve(m)
  m
}

