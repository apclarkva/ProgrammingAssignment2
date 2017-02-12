## Put comments here that give an overall description of what your
## functions do

# The makeCacheMatrix function creates a list of functions that do
# the following:
# 1. set: sets the matrix, x, to input y, and sets the inverseM matrix to NULL
# 2. get: returns the value of the matrix, x
# 3. setInverse: sets the inverse matrix, inverseM, to an argument, inverse
# 4. getInverse: returns the inverse matrix, inverseM

makeCacheMatrix <- function(x = matrix()) {
  # Create the inverse matrix
  inverseM <- NULL
  set <- function(y){
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseM <<- inverse
  getInverse <- function() inverseM
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}



# This function accepts the "special" vector as input, with four functions that:
# set a matrix, get a matrix, set that matrix's inverse, or gets that matrix's inverse.
# This function will check the cache to see if a matrixes inverse has been calculated.
# If it has been, the inverse matrix will be returned, and a message will be printed
# to the console.
# If the inverse matrix has not been calculated, the function will calculate it, 
# save it to the cache, then output the answer.
cacheSolve <- function(x, ...) {
	# Aave the inverse matrix of x to inverseM
	inverseM <- x$getInverse()

	# Check if the inverse matrix was created (make sure it's not null)
	if(!is.null(inverseM)) {
		# if the inverse matrix has been calculated, output its value with message
		message("getting cached data")
		return(inverseM)
	}

	# If inverse matrix has not been calculated, grab the original matrix with the get function
	normalM <- x$get()

	# Calculate the inverse matrix using the solve function and save it to inverseM
	inverseM <- solve(normalM)

	# Use the setInverse function to set the value of inverseM in makeCacheMatrix function
	x$setInverse(inverseM)

	# Return the inverse matrix
	inverseM
}


