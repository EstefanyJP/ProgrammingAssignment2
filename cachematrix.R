#References: Cache Mean Example given in the assignment description,
#discussion forums of the course.

#makeCacheMatrix is a function that creates a special "matrix" 
#object that can cache its inverse.

#cacheSolve is a function that computes the inverse of the special "matrix" 
#returned by makeCacheMatrix. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

#makeCaheMatrix()
#x - Matrix to be created
#Output - list of functions:
# -get: returns the data in the matrix x
# -set: sets matrix x with the data passed as parameter
# -getinversa: gets the inverse of the matrix x
# -setinversa: sets the inverse of the matrix x through the function parameter
#or arguement

makeCacheMatrix=function(x=matrix()){
  inversa=NULL
  set=function(y){
    x<<-y
    inversa<<-NULL
  }
  get=function() 
    x
  setinversa=function(inverse)
    inversa<<-inverse
  getinversa=function() 
    inversa
  list(set=set,get=get,setinversa=setinversa,getinversa=getinversa)
}

#cahceSolve()
#x - Matrix whose inverse needs to be computed
#Output - inverse of the matrix x
# -getinversa: invokes the already computed inverse
#if is NULL:
# -get: gets the matrix
# -solve:computes the inverse
# -setinversa: sets the inverse
#Else, it returns the cahced value

cacheSolve=function(x, ...){
        ## Return a matrix that is the inverse of 'x'
  inversa=x$getinversa()
  if(!is.null(inversa)){
    message("getting cahced data")
    return(inversa)
  }
  data=x$get()
  inversa=solve(data, ...)
  x$setinversa(inversa)
  inversa
}
