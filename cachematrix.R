## The first function creates "special matrix" that is a list contains the functions to 
## i) to set the value of matrix
## ii) to get the value of matrix
## iii) to set the inverse of matrix
## iv) to get the inverse of matrix.
## The second function calculates the inverse of the "special matrix" created with the above functions.  
## it first checks to see if inverse has already been calculated 
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.


##creating a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y){
                    x <<- y
                    inv <<- NULL
          }
          get <- function() x
          setInverse <- function(solveMatrix) inv <<- solveMatrix
          getInverse <- function() inv
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
          inv <- x$getInverse()
          if(!is.null(inv)){
          message("getting cached data")
          return(inv)
          }
          data <- x$get()
          inv <- solve(data)
          x$setInverse(inv)
          inv      
}


