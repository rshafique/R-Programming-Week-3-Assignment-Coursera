## Put comments here that give an overall description of what your
## functions do

## We have been asked to write a function named makeCacheMatrix which 
## creates a special matrix object that can cache its inverse. We assume the
## matrix provided is invertible. 

## We also have to write another function named cacheSolve which computes the 
## inverse of a matrix retruned by makeCacheMatrix. If the inverse is already
## calculated then this function should retrieve the inverse from cache and save
## time. Otherwise it calculates the inverse of the matrix. 

## Write a short comment describing this function

## The following function makeCacheMatrix has four functions which are
## set, get, setmean, getmean; get function returns the vector stored in
## the function makeCacheMatrix; set function changes the vector stored in 
## the main function. setinverse sets the inverse and getinverse gets the
## inverse. 

makeCacheMatrix <- function(x = matrix()) {

      	m <- NULL
	      set <- function(y) {
      	      x <<- y
            	m <<- NULL
      	}
      
		get <- function() x
	
	      setinverse <- function(solve) m <<- solve
	
	      getinverse <- function() m
      
		list(set = set, get = get,setinverse = setinverse, 
		getinverse = getinverse)

}

## Write a short comment describing this function

## The folloing fucntion computes the inverse of the matrix returned by \
## the makeCacheMatrix. If the inverse has already been calculated then 
## this function does not calculate the inverse again; rather it retrieves
## the inverse from cache. Otherwise, it calculated the inverse of the matrix
## returned by the function makeCacheMatrix. 


cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

	      m <- x$getinverse()
      
		if(!is.null(m)) {
	            message("getting cached data")
      	      return(m)
	      }
      
		data <- x$get()
	
	      m <- solve(data, ...)
      
		x$setinverse(m)
		
		m

}
