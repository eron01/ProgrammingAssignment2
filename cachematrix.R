## This function calculates de inverse of a matrix and puts it on cache


makeCacheMatrix <- function(x = matrix()) {
       
                myMat <- NULL                       ## sets the value of the matrix
                set <- function(y) {
                        x <<- y
                        myMat <<- NULL
                }
                get <- function() x        ## sets the value of the matrix
                setInv <- function(solve) myMat <<- solve   ## sets the inverse of the matrix
                getInv <- function() myMat                ## gest the inverse of the matrix
                list(set = set, get = get,            ## returns the list of cache items
                     setInv = setInv,
                     getInv = getInv)
        
}


## This function Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        cacheInv <- function(x, ...) { ## caches the inverse of the matrix
                myMat <- x$getInv()          
                if(!is.null(myMat)) {     ## in case no value, gets the data on cache
                        message("getting cached data")
                        return(myMat)
                }
                data <- x$get() 
                myMat <- solve(data, ...) ##solve the inverse of the matrix
                x$setInv(myMat)   ##sets the inverse of the matrix
                print(myMat)  ##prints the matrix
        }
        

}
