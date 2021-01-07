## Put comments here that give an overall description of what your
## functions do

## This is the programming assignment 2 of the R programming module
## The assignment is presented by Juan David Gutierrez. The assignment consists in 2 functions:
## 1. makeCacheMatrix - A function to create a special "matrix" object and 
##                      cache its inverse
## 2. cacheSolve -  A function which return an inverse of the 'matrix' object
##                  if it already exist in the cache and the object remain 
##                  unchanged. Else, the function will calculate the inverse
##                  on the special 'matrix' returned by makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() { 
      x ## return matrix
  }
  
  setinverse <- function(inverse) {
    i <<- inverse ## assign the inverted matrix to the env var
  }
  
  getinverse <- function() {
    i ##the env var inverse matrix is return
  }
  
  ##put the defined function into a list to be able to call the function within the environment
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## I expect the function will do:
## 1. Check if the inverse object exist and the matrix is unchanged by getting
##    the stored inverse matrix calling from makeCacheMatrix
## 2. Return the inverse object in cache if it satisfy the first rules.
## 3. Else, compute the inverse of the special 'matrix', store in 
##    makeCacheMatrix for future use and return the new inverse object.

cacheSolve <- function(x, ...) {
  ## 1. Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  ## 2. Verified if the inverse has already been calculated
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## 3. Else inverse matrix null or matrix change, calculates the inverse 
  data <- x$get()
  i <- solve(data, ...)
  
  # 4. sets the value of the inverse in the cache via the setinverse function.
  x$setinverse(i)
  
  ## 5. Return a matrix that is the inverse of 'x'
  i
}
  

