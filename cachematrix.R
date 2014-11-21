## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Matrix data has to be update trough implemented changeContent funtion.
## Otherwise the cahceSolve will return incorrectly solved matrix.
## Data of the matrix can be retrieved by getContent function.

makeCacheMatrix <- function(x = matrix()) {
  #Creates a new cached "matrix" object.
  cached <- FALSE
  cache <- NULL
  data <- x
  setCache <- function(c){
    #Updates the cache (solved matrix) during solution.
    cached <<- TRUE
    cache <<- c
  }
  getCache <- function(){
    #Returns the cahce (solved matrix) of cached "matrix" object
    return(cache)
  }
  isCached <- function(){
    #Verifies whether the "matrix" object is chached (was solved) or not
    return(cached)
  }
  changeContent <- function(new_data){
    #Function to update data in the matrix by providing the new matrix.
    cached <<- FALSE
    data <<- new_data
  }
  getContent <- function(){
    #Returns the matrix stored in this "matrix" object.
    return(data)
  }
  list(cached=cached,
       cache=cache,
       data=data,
       setCache=setCache,
       getCache=getCache,
       isCached=isCached,
       changeContent=changeContent,
       getContent=getContent)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', where x is a object of makeCacheMatrix function.
  ## Solution is computed only if the matrix data was not solved before or modified by changeContent function since the last solution.
  
  if (x$isCached()){
    message("returning solved matrix from the cache")
    return(x$getCache())
  }
  else {
    message("computing solution")
    result <- solve(x$data)
    x$setCache(result)
    return(result)
  }
}
