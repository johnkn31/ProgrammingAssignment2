## Get a matrix with no values. Set m as a object and x is a matrix object.We would like to return
## back the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) { ## Make a matrix with no values.
    m <- NULL  ##m is an object that is set to NULL.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x ## apply get function 
    setinverse <- function(solve) m <<- solve #ask for the inverse matrix. Use solve function to get inverse matrix m
    getinverse <- function() m #get the inverse matrix m and store it
    list(set = set, get = get,   #make a list that would store the set, get, setinverse, and getinverse
         setinverse = setinverse, #The list contains the method function set(),get(),setinverse(), and getinverse()
         getinverse = getinverse)
}
    



## The cacheSolve will get the inverse matrix by using the solve function

cacheSolve <- function(x, ...) {
    m <- x$getinverse() ##the object m is store to the value of the inverse matrix
    if(!is.null(m)) { #if matrix m is not NULL then return matrix m
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...) # if matrix is NULL, then use solve function to get inverse matrix 
    x$setinverse(m) #store inverse matrix
    m  ## Return a matrix that is the inverse of 'x'
}
