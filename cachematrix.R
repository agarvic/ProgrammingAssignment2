##Esta función calcula la inversa de una matriz, almacenando en caché el resultado para evitar tener que recalcularla cada ocasión

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # sets x equal to an empty numeric vector
  Inversa <- NULL
  set <- function(y) {
    x <<- y
    Inversa <<- NULL
  }
  get <- function() x
  set_inversa <- function(inversion) Inversa <<- inversion
  get_inversa <- function() Inversa
  list(set = set, get = get, set_inversa = set_inversa, get_inversa = get_inversa)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
  {
    ## Return a matrix that is the inverse of 'x'
    Inversa <- x$get_inversa()
  
    if(!is.null(Inversa)) {
      message("getting cached data/obteniendo datos en caché")
      return(Inversa)
    }
    essage("Calculating the inverse matrix/calculando la matriz inversa")
    data <- x$get()
    Inversa <- solve(data, ...)
    x$set_inversa(Inversa)
    Inversa
}
