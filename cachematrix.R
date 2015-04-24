## computes the inverse of a matrix using caching properties
## if the inverse has never been computed, it solves it
## and puts it in a cache via setinverse() 
## if it has already been computed, it gets it from the cache
## via the setinverse() function

makeCacheMatrix <- function (x = matrix(c(1,2,3,4), nrow=2, ncol=2)) {
  xinv <- NULL
  set <- function(y) { # assigns the matrix to new value and initializes the inverse to NULL
    x <<- y # deep assignment arrow
    xinv <<- NULL
  }
    
  get <- function() x # returns the current value of the matrix
  setinverse <- function(inv) xinv <<- inv # simply sets the inverse to given 'inv' matrix
  getinverse <- function() xinv # returns the current inverse, these 2 are similar to the set and get 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # makes the list of available functions that will be linked to the x object
  
}

cacheSolve <- function (amatrix) { # Computes, caches, and returns matrix inverse
  xinv <- amatrix$getinverse() # to check if it is already cached
  if(!is.null(xinv)) { # if so, it returns the cached value
    message("getting cached data")
    return(xinv)
  }  # if it was not cached the inverse, it computes using the solve function
     # this is actually the only place where the computation of the inverse is done  
  data <- amatrix$get()
  xinv <- solve(data)
  amatrix$setinverse(xinv) # and once it has been calculated, it sets the inverse value in the cache
  xinv # and returns the result
}

# things to remember : 
# <-, always creates a variable in the current environment. 
# The deep assignment arrow, <<-, never creates a variable in the current environment, 
# but instead modifies an existing variable found by walking up the parent environments.

