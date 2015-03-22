## Taking advantage of the R scoping rules, the following functions 
## compute and store the time-consuming computation associated with
## calculating the inverse of a matrix.

## makeCacheMatrix uses lists to set and return the value of a vector
## and it's inverse
 

makeCacheMatrix<-function(y = matrix()) {

    n <- NULL
    set <- function(z){
        y <<-  z
        n <<- NULL
    }
    get <-function() y
    setinverse<- function(solve) n<<- solve
    getinverse<- function() n

    list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
            )
}


## If the inverse of matrix has already been calculated and stored using the 
## makeCacheMatrix function, from above, this function will returned the cached
## value.  Otherwise, the inverse will be computed and stored.

cacheSolve <- function(x, ...){
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return (m)   
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
