# makeCacheMatrix has four functions in the list
# use the set() function for assigning the i/p matrix
# use the get() function to return the matrix
# use the setinverse() function to save the result of inverse matrix
# use the getinverse() function if the inverse matrix is already created

makeCacheMatrix <- function(x = matrix()) {
  inverse_mat <- NULL # sets the inverse matrix to NULL
  # creating the set() function to assign the matrix
  set <- function(y)
  {
    x <<- y # assigning the matrix to x
    inverse_mat <<- NULL # sets the inverse matrix to NULL
  }
  # creating get() function to return the matrix
  get <- function() x 
  # create the setinverse() function to save the result of inverse matrix
  setinverse <- function(inverse) inverse_mat <<- inverse
  # creating the getinverse() function to return the result of inverse matrix
  getinverse <- function() inverse_mat
  # creating list with the functions defined and can be used later
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# In this function we use setinverse() function which is already created in the
# makeCacheMatrix list - which saves the result of inverse matrix

cacheSolve <- function(x, ...) {
  inverse_mat <- x$getinverse() # checks if the inverse is already generated for the matrix
  if (!is.null(inverse_mat)) # if it returns NULL - then inverse is not created
  {
    print("getting cache data")
    return(inverse_mat) # if inverse is already created than returns the value
  }
  data <- x$get() # assigning the i/p matrix to data
  inverse_mat <- solve(data) # by using the solve() function we create the inverse of the matrix
  x$setinverse(inverse_mat) # saving the result of the inverse matrix
  inverse_mat
}
