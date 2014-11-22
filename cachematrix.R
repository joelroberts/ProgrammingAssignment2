
## sets up a cache matrix object.

makeCacheMatrix <- function(x=matrix()) {
		m<-NULL
		set <-function(y){
		    x <<- y
		    m <<- NULL
		}
		get <-function()x
                setinv <- function(solve) m <<-solve
                getinv <- function() m
		list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## computes matrix inverse and caches it, retrieving it on subsequent calls.
## lexical scoping, yay!

cacheSolve <- function(x, ...) {
        m <-x$getinv()
	if(!is.null(m)){
           message("getting cached data")
	   return(m)
	}
	y <-x$get()
	m <-solve(y)
	x$setinv(m)
	m
}
