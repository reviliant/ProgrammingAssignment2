## Put comments here that give an overall description of what your
## makeCacheMatrix function is used to getting/setting the value of a matrix (cached) and cacheSolve function is used to 
# get the cached value (if it exists) or re-compute the value if the cached value does not exist
#Call cacheSolve(<matrix : Square Invertible>) Working if you run the Code

## Write a short comment describing this function
#The function returns a list of all the functions (set,get,setInverse and getInverse) desfined within

makeCacheMatrix <- function(x = matrix()) {
  
  #Setting the inverse of a matrix to NULL for future value
  inv <- NULL
  
  #Defines a function to set the matrix X to Y and resets the inverse of a matrix to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Returns the Vector X
  get <- function() x
  
  #Sets the Inverse of Matrix
  setInverse <- function(inverse) inv <<- inverse
  
  #Gets the Inverse of Matrix
  getInverse <- function() inv
  
  #List returning the special matrix containing all the functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
#The function returns a new calculated inverse of a matrix if a new matrix is passed as argument; otherwise returns the cached
#value of the previous matrix


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Check if the data is available in the cache
  inv <- makeCacheMatrix(x)$getInverse()
  
  #If the data is available in the cache, get the cached data and return (The function returns a value)
  if(!is.null(inv)) {
    return(inv)
  }
  
  #If the data is not available in the cache, get the value of the matrix from the input
  data <- makeCacheMatrix(x)$get()
  
  #Solve the Inverse for the new dataset
  inv <- solve(data)
  
  #Set the value to cache again (for future use)
  makeCacheMatrix(x)$setInverse(inv)
  
  #Return the computed value
  inv
}
