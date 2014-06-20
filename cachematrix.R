# R File which has two functions 
# makeCacheMatrix
# cacheSolve

# Function which has a matrix and its inverse. Methods are exposed to
# get, set matrix and also its inverse (getInverse,setInverse).
# When the matrix is modified (through set method), it is also ensured that its
# inverse is invalidated.

makeCacheMatrix <- function(lMat= matrix()) {
        mInverse <- NULL

		# When matrix is changed (which can only happen with set method)
		# the inverse also initialized to NULL.
		# mInverse is NULL, signifies, that inverse is not calculated (yet) or 
		# matrix could have been changed. Checking for minverse == NULL would satisfies both conditions.
		# Other options that can be used to check if matrix has changed are
		# multiply the matrix with its inverse and see if it generates identity, if the matrix
		# is changed, identity would not be generated, but this is little costly than the above check.
		
		# Set method to change matrix, when matrix is changed we invalidate the inverse also.
		
        set <- function(y) {
                lMat <<- y
                mInverse  <<- NULL
        }

        get <- function() lMat
		
		#Methods to set and get inverse.
		
        setInverse <- function(lInverse) mInverse  <<- lInverse
        getInverse <- function() mInverse 

        list(set = set, get = get,
             getInverse = getInverse ,
             setInverse = setInverse)
}

# Method which calculates inverse of a matrix. 
# It checks if the inverse is NULL, only then calculates the inverse.
# If the inverse is NOT NULL, the same is returned.

cacheSolve <- function(matObj, ...) {
		mInverse <- matObj$getInverse()

		#Check to see if the inverse is NOT NULL and if yes return the same.
		
		if(!is.null(mInverse)) {
			message("getting cached inverse")
			
			#Return the calculated inverse.
			return(mInverse)	
		}
		#Calculate the inverse (assumption the matrix is always invertible).
		
		mInverse<-solve(matObj$get())
		
		#Store the calculated inverse for subsequent calls (caching).
		matObj$setInverse(mInverse)
		
		#Return the calculated inverse.
		mInverse
}