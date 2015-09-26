
## The first function, I call "makeCacheMatrix" that creates a special "vector", which is really 
## a list containing a function to do the following.
## 1st, I set the value of the Matrix
## Then, get the value of the Matrix
## 3rd, I set the value of the inverse of the Matrix
## lastly, get the value of the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  a<-NULL
  	set<-function(y){
    	x<<-y
    	a<<-NULL
  }
  get<-function() x	
	setmatrix<-function(solve) a<<- solve
  	getmatrix<-function() a
  		list(set=set, get=get,
        	setmatrix=setmatrix, 
		getmatrix=getmatrix)
}


## The Second function computes the inverse Matrix of the special "vector" created from the
## function above. First, it checks to see if the inverse has already been calculated. If 
## it has...then value is not computed again but, pulled from cache. If the inverse is unknown 
## it is then calculated... The inverse matrix value is now "known" and does not 
## need to recalculated if the value (inverse matrix) is called upon again.

cacheSolve <- function(x=matrix(), ...) {
  a<-x$getmatrix()
  	if(!is.null(a)){
    	message("Getting cached data")
    	return(a)
  }
  matrix<-x$get()
  	a<-solve(matrix, ...)
  	x$setmatrix(a)
  	a
}