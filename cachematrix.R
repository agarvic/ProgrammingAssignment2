##Esta función calcula la inversa de una matriz, almacenando en caché el resultado para evitar tener que recalcularla cada ocasión
##This function calculates the inverse of a matrix, caching the result to prevent unnecessary calculations
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Set Inversa as NULL
  Inversa <- NULL
  #The following function sets the matrix to calculate its inverse
  set <- function(y) {
    x <<- y
    Inversa <<- NULL ##Resets its inverse
  }
  get <- function() x ##Function that returns the currently cached matrix
  set_inversa <- function(inversion) Inversa <<- inversion ##Function that stores the inverse of the matrix
  get_inversa <- function() Inversa #Function that returns the currently stored inverse matrix
  
  list(set = set, get = get, set_inversa = set_inversa, get_inversa = get_inversa)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
  {
    ## Return a matrix that is the inverse of 'x'
  
    Inversa <- x$get_inversa() #We try to get the cached inverse matrix
  
    #If we have the inverse in cache we return it...
    if(!is.null(Inversa)) {
      message("getting cached data/obteniendo datos en caché")
      return(Inversa)
    }
    message("Calculating the inverse matrix/calculando la matriz inversa")
    data <- x$get() #We get the matrix to calculate its inverse matrix
    Inversa <- solve(data, ...) ##solve() calculates the inverse of a given mtrix
    x$set_inversa(Inversa) ##We store the previously calculated matrix
    Inversa #Return the inverse matrix
}
