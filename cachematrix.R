## Function 1 stores the matrix and its inverse
## Function 2 retreives and solves the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #create variable 'inv'
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } #set up function
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse #this stores the value of inverse into inv
  getInverse <- function() inv #this will tell us the inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
#the list will be printed as the result of the function
  }



cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #assigns a value to 'inv'
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } #if the value is not null, return value
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv #the above will solve and print inv
}
