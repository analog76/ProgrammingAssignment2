## Functions to calculate the inverse of matrix and stores its cache value.

 
## A special matrix object has been created to  cache its inverse
makeCacheMatrix <- function(x = matrix()) {

		inverseMatrix=NULL
		
		## method to set the matrix and re-initialize the inverseMatrix to NULL
 		set<-function(z){
 			m<<-z
 			inverseMatrix= NULL
 		}
 		
 		## method to get the matrix 
 		get<-function(){
 			m
 		}
 		
 		## it stores the value in the cache.
 		setInverseMatrix <- function(im){
 			inverseMatrix <<- im	
 		}
 		
 		## method to get the inverse matrix.
 		getInverseMatrix <- function(){
 			inverseMatrix
 		}
 		
 		## returns list of methods.
 		list(set=set,
 			 get=get,
 			 setInverseMatrix=setInverseMatrix,
 			 getInverseMatrix=getInverseMatrix)
 
}


## Return a matrix that is the inverse of matrix

cacheSolve <- function(x, ...) {
        
    ## Retrieves inverse of matrix from makeCacheMatrix object.
    inverseMatrix <- x$getInverseMatrix()
    
    
    ## checks if the inverse of matrix is NULL or not.
    ## if it is NOT null returns that value.
	if(!is.null(inverseMatrix)){
		message(" getting cached inverse matrix ")
		return(inverseMatrix)
	}
	
	## if the inverse of matrix is NULL then if block will not get executed.
	## it calculates the inverse of matrix here. 
	data <- x$get()
	inverseMatrix <- solve(data,...)

	## caches the value in the makeCacheMatrix object.
	x$setInverseMatrix(inverseMatrix)
	
	## returns the value .
	inverseMatrix
}
