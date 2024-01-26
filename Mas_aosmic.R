
# Funciones aosmic que invierten

#Negative exponential - Monomulecolar growth
monoGrowthMean <- function(predictor, a, b, c) {
  x <- predictor
  a - (a - b) * exp (- c * x)
}

monoGrowthInit <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  plateau <- max(y) * 1.05
  
  ## Linear regression on pseudo y values
  pseudoY <- log( 1 - (y / plateau ) )
  coefs <- coef( lm(pseudoY ~ x) )
  temp <- exp(coefs[1])
  b <- plateau * (1 - temp)
  c <- - coefs[2]
  a <- plateau
  value <- c(a, b, c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLSmonoGrowth <- selfStart(monoGrowthMean, monoGrowthInit, parameters=c("a", "b", "c"))

#Logistic growth - 1 ###############################
logiGrowth1Mean <- function(predictor, a, b, c) {
  x <- predictor
  a / (1 + exp(- b * x + c))
}

logiGrowth1Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  a <- max(y) * 1.05
  
  ## Linear regression on pseudo y values
  pseudoY <- log( (a / y ) - 1 )
  coefs <- coef( lm(pseudoY ~ x) )
  b <- - coefs[2]
  c <- coefs[1]
  value <- c(a, b, c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLSlogiGrowth.1 <- selfStart(logiGrowth1Mean, logiGrowth1Init, parameters=c("a", "b", "c"))


#Logistic Growth 2
logiGrowth2Mean <- function(predictor, a, b, c) {
  x <- predictor
  a / (1 + b * exp(- c * x))
}

logiGrowth2Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  a <- max(y) * 1.05
  
  ## Linear regression on pseudo y values
  pseudoY <- log( (a / (y + 0.0001) ) - 1 )
  coefs <- coef( lm(pseudoY ~ x) )
  c <- - coefs[2]
  k <- coefs[1]
  b <- exp(k)
  value <- c(a, b, c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLSlogiGrowth.2 <- selfStart(logiGrowth2Mean, logiGrowth2Init, parameters=c("a", "b", "c"))

#Logistic Growth 3
logiGrowth3Mean <- function(predictor, init, m, plateau) {
  x <- predictor
  init * plateau / (init + (plateau - init) * exp( - m * x))
}

logiGrowth3Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  plateau <- max(y) * 1.05
  
  ## Linear regression on pseudo y values
  pseudoY <- log( (plateau / (y + 0.0001) ) - 1 )
  coefs <- coef( lm(pseudoY ~ x) )
  b <- exp(coefs[1])
  init <- plateau / (1 + b)
  m <- - coefs[2]
  value <- c(init, m, plateau)
  names(value) <- mCall[c("init", "m", "plateau")]
  value
}

NLSlogiGrowth.3 <- selfStart(logiGrowth3Mean, logiGrowth3Init, parameters=c("init", "m", "plateau"))

#Logistic Growth 4
logiGrowth4Mean <- function(predictor, t50, m, plateau) {
  x <- predictor
  plateau / (1 + exp(- m * (x - t50)))}

logiGrowth4Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  plateau <- max(y) * 1.05
  
  ## Linear regression on pseudo y values
  pseudoY <- log( (plateau / (y + 0.0001) ) - 1 )
  coefs <- coef( lm(pseudoY ~ x) )
  m <- - coefs[2]
  t50 <- coefs[1] / m
  value <- c(t50, m, plateau)
  names(value) <- mCall[c("t50", "m", "plateau")]
  value
}

NLSlogiGrowth.4 <- selfStart(logiGrowth4Mean, logiGrowth4Init, parameters=c("t50", "m", "plateau"))

#Logistic function 5
logistic5Mean <- function(predictor, a, b, c, d) {
  x <- predictor
  c + (d-c) / (1 + b * exp(a * x))
}

logistic5Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  d <- max(y) * 1.05
  c <- min(y) -0.0001
  ## Linear regression on pseudo y values
  pseudoY <- log((d - y) / (y - c ))
  coefs <- coef( lm(pseudoY ~ x) )
  a <- coefs[2]
  k <- coefs[1]
  b <- exp(k)
  value <- c(a, b, c, d)
  names(value) <- mCall[c("a", "b", "c", "d")]
  value
}

NLSlogistic.1 <- selfStart(logistic5Mean, logistic5Init, parameters=c("a", "b", "c", "d"))

#Logistic function 6
logistic6Mean <- function(predictor, a, b) {
  x <- predictor
  1 / (1 + b * exp(- a * x))
}

logistic6Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  ## Linear regression on pseudo y values
  pseudoY <- log((1 - y) / y)
  coefs <- coef( lm(pseudoY ~ x) )
  a <- - coefs[2]
  k <- coefs[1]
  b <- exp(k)
  value <- c(a, b)
  names(value) <- mCall[c("a", "b")]
  value
}

NLSlogistic.2 <- selfStart(logistic6Mean, logistic6Init, parameters=c("a", "b"))

#LOG_LOGISTIC FUNCTIONS##########################################################


#GOMPERTZ MODELS################################################################

#Gompertz growth - 1
gompGrowth1Mean <- function(predictor, a, m, c) {
  x <- predictor
  a * exp( - (m/c) * exp (-c * x))
}

gompGrowth1Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  plateau <- max(y) * 1.05
  
  ## Linear regression on pseudo y values
  pseudoY <- log( - log( y / plateau ) )
  coefs <- coef( lm(pseudoY ~ x) )
  k <- coefs[1]; c <- - coefs[2]
  b <- exp(k) 
  m <- b * c
  a <- plateau
  value <- c(a, m, c)
  names(value) <- mCall[c("a", "m", "c")]
  value
}

NLSgompGrowth.1 <- selfStart(gompGrowth1Mean, gompGrowth1Init, parameters=c("a", "m", "c"))

#Gompertz growth 2
gompGrowth2Mean <- function(predictor, a, b, c) {
  x <- predictor
  a * exp( - exp (b - c*x))
}

gompGrowth2Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  a <- max(y) * 1.05
  
  ## Linear regression on pseudo y values
  pseudoY <- log( - log( y / a ) )
  coefs <- coef( lm(pseudoY ~ x) )
  
  k <- coefs[1]
  c <- - coefs[2]
  b <- k
  value <- c(a, b, c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLSgompGrowth.2 <- selfStart(gompGrowth2Mean, gompGrowth2Init, parameters=c("a", "b", "c"))

#Gompertz growth - 3
gompGrowth3Mean <- function(predictor, a, b, c) {
  x <- predictor
  a * exp( -b * exp (-c*x))
}

gompGrowth3Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  a <- max(y) * 1.05
  
  ## Linear regression on pseudo y values
  pseudoY <- log( - log( y / a ) )
  coefs <- coef( lm(pseudoY ~ x) )
  
  k <- coefs[1]
  c <- - coefs[2]
  b <- exp(k)
  value <- c(a, b, c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLSgompGrowth.3 <- selfStart(gompGrowth3Mean, gompGrowth3Init, parameters=c("a", "b", "c"))


#Extreme value
extremeValueMean <- function(predictor, a, b, c) {
  x <- predictor
  a * (1 - exp( - exp (b - c*x)))
}

extremeValueInit <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  
  a <- max(y) * 1.05
  
  ## Linear regression on pseudo y values
  pseudoY <- log( - log( (a - y ) / a ) )
  coefs <- coef( lm(pseudoY ~ x) )
  
  k <- coefs[1]
  c <- - coefs[2]
  b <- k
  value <- c(a, b, c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLSextremeValue <- selfStart(extremeValueMean, extremeValueInit, parameters=c("a", "b", "c"))

#WEIBULL TYPE MODELS
#Weibull-1

#Modified Mitscherlich equation for A vs PFD relationships
AvsPFDMean <- function(predictor, Rd, Amax, Qapp) {
  x <- predictor
  Rd+Amax*(1-exp((-Qapp/Amax)*x))
}

AvsPFDInit <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  interc <- min(y)
  plateau <- max(y) * 1.05 - interc        
  ## Linear regression on pseudo y values
  pseudoY <- log( 1 - (((y - interc) / plateau ) ))
  coefs <- coef( lm(pseudoY ~ x - 1) )
  Amax <- plateau
  Rd <- interc
  b <-  coefs[1]
  Qapp <- -b*plateau
  value <- c(Rd,Amax,Qapp)
  names(value) <- mCall[c("Rd", "Amax", "Qapp")]
  value
}

NLSAvsPFD <- selfStart(AvsPFDMean, AvsPFDInit, parameters=c("Rd", "Amax", "Qapp"))

#Inverse polynomial
polyInv.3mean <- function(predictor, a, b, c) {
  x <- predictor
  1/(a + b*x + c*x^2)
}

polyInv.3Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  ## Linear regression on pseudo y values
  #pseudoY <- 1/y
  coefs <- coef(glm(y ~ x + I(x^2), family=gaussian(link="inverse")))
  a <- coefs[1]; b <- coefs[2]; c <- coefs[3]
  
  value <- c(a,b,c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLSpolyInv.3 <- selfStart(polyInv.3mean, polyInv.3Init, parameters=c("a", "b", "c"))

#Inverse polynomial 2
polyInv.4mean <- function(predictor, a, b, c) {
  x <- predictor
  x/(a + b*x + c*x^2)
}

polyInv.4Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  ## Linear regression on pseudo y values
  pseudoY <- y/x
  coefs <- coef(glm(pseudoY ~ x + I(x^2), family=gaussian(link="inverse")))
  a <- coefs[1]; b <- coefs[2]; c <- coefs[3]
  print(a);print(b);print(c)
  value <- c(a,b,c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLSpolyInv.4 <- selfStart(polyInv.4mean, polyInv.4Init, parameters=c("a", "b", "c"))



# Bragg's equation
logBragg.3.fun <- function(X, b, d, e){
  d * exp(- b * (log(X + 0.000001) - e)^2)
}

# Da fare
DRC.logBragg.3 <- function(){
  fct <- function(x, parm) {
    logBragg.3.fun(x, parm[,1], parm[,2], parm[,3])
  }
  ssfct <- function(data){
    # Get the data     
    x <- log(data[, 1] + 0.000001)
    y <- data[, 2]
    
    d <- max(y)
    e <- x[which.max(y)]
    
    ## Linear regression on pseudo-y and pseudo-x
    pseudoY <- log( (y + 0.0001) / d )
    pseudoX <- (x - e)^2
    coefs <- coef( lm(pseudoY ~ pseudoX - 1) )
    b <- - coefs[1]
    start <- c(b, d, e)
    return( start )
  }
  names <- c("b", "d", "e")
  text <- "log-Bragg equation with three parameters"
  
  ## Returning the function with self starter and names
  returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
  class(returnList) <- "drcMean"
  invisible(returnList)
}


# Negative exponential cumulative distribution #############
negExpDist.fun <- function(predictor, c) {
  x <- predictor
  1 - exp (- c * x)
}

negExpDist.Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  ## Linear regression on pseudo y values
  pseudoY <- log( 1 - y )
  coefs <- coef( lm(pseudoY ~ x - 1) )
  c <- - coefs[1]
  value <- c(c)
  names(value) <- mCall[c("c")]
  value
}

NLS.negExpDist <- selfStart(negExpDist.fun, negExpDist.Init, 
                            parameters=c("c"))

DRC.negExpDist <-
  function(fixed = NA, names = c("c"))
  {
    ## Checking arguments
    numParm <- 1
    if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
    if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
    
    ## Fixing parameters (using argument 'fixed')
    notFixed <- is.na(fixed)
    parmVec <- rep(0, numParm)
    parmVec[!notFixed] <- fixed[!notFixed]
    
    ## Defining the non-linear function
    fct <- function(x, parm)
    {
      parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
      parmMat[, notFixed] <- parm
      
      c <- parmMat[, 1]
      1 - exp (- c * x)
    }
    
    ## Defining self starter function
    ssfct <- function(dataf)
    {
      x <- dataf[, 1]
      y <- dataf[, 2]
      
      ## Linear regression on pseudo y values
      pseudoY <- log( 1 - y )
      coefs <- coef( lm(pseudoY ~ x - 1) )
      c <- - coefs[1]
      
      return(c(c)[notFixed])
    }
    
    ## Defining names
    pnames <- names[notFixed]
    
    ## Defining derivatives
    deriv1 <- function(x, parms){
      parmMat <- matrix(parmVec, nrow(parms), 
                        numParm, byrow = TRUE)
      parmMat[, notFixed] <- parms
      
      # Approximation by using finite differences
      a <- as.numeric(parmMat[,1])
      
      d1.1 <- negExpDist.fun(x, a)
      d1.2 <- negExpDist.fun(x, (a + 10e-7))
      d1 <- (d1.2 - d1.1)/10e-7
      d1
      # cbind(d1)[notFixed]
    }
    
    ## Defining the first derivative (in x=dose)
    derivx <- function(x, parm)
    {
      parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
      parmMat[, notFixed] <- parm
      
      a <- as.numeric(parmMat[,1])
      
      d1.1 <- negExpDist.fun(x, a)
      d1.2 <- negExpDist.fun((x + 10e-7), a)
      d1 <- (d1.2 - d1.1)/10e-7
      d1
    }
    
    ## Defining the ED function
    
    ## Defining the inverse function
    
    ## Defining descriptive text
    text <- "Negative exponential cumulative distribution function"
    
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, ssfct = ssfct, names = pnames, 
                       text = text, noParm = sum(is.na(fixed)),
                       deriv1 = deriv1, derivx = derivx)
    
    class(returnList) <- "drcMean"
    invisible(returnList)
  }

#Rational function ################################################
# Ratio of two polynomials ############################
Rational.fun <- function(predictor, a, b, c) {
  x <- predictor
  (b + c*x) / (1 + a*x)
}

Rational.Init <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  pseudoY <-  y 
  pseudoX <- x
  pseudoXY <- x*y
  coefs <- coef( lm(pseudoY ~ pseudoX + pseudoXY) )
  b <- coefs[1]        
  c <- coefs[2]
  a <- - coefs[3]
  value <- c(a, b, c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLS.Rational <- selfStart(Rational.fun, Rational.Init, parameters=c("a", "b", "c"))

"DRC.Rational" <-
  function(fixed = c(NA, NA, NA), names = c("a", "b", "c"))
  {
    ## Checking arguments
    numParm <- 3
    if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
    if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
    
    ## Fixing parameters (using argument 'fixed')
    notFixed <- is.na(fixed)
    parmVec <- rep(0, numParm)
    parmVec[!notFixed] <- fixed[!notFixed]
    
    ## Defining the non-linear function
    fct <- function(x, parm)
    {
      parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
      parmMat[, notFixed] <- parm
      
      a <- parmMat[, 1]; b <- parmMat[, 2]; c <- parmMat[, 3]
      (b + c*x)/(1 + a*x)
    }
    
    ## Defining self starter function
    ssfct <- function(dataf)
    {
      x <- dataf[, 1]
      y <- dataf[, 2]
      xy <- dataf[,1]*dataf[,2]
      
      ## Linear regression on pseudo y values
      
      coefs <- coef(lm(y ~ x + xy) )
      b <- coefs[1]
      c <- coefs[2]
      a <- - coefs[3]
      
      return(c(a, b, c)[notFixed])
    }
    
    ## Defining names
    pnames <- names[notFixed]
    
    ## Defining derivatives
    
    ## Defining the ED function
    
    ## Defining the inverse function
    
    ## Defining descriptive text
    text <- "Inverse polynomial 1"
    
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, ssfct = ssfct, names = pnames, text = text, noParm = sum(is.na(fixed)))
    
    class(returnList) <- "drcMean"
    invisible(returnList)
  }

# Hill function
hillCurveMean <- function(predictor, a, b, c) {
  (a * predictor^c)/(b + predictor^c)
}

hillCurveInit <- function(mCall, LHS, data, ...) {
  xy <- sortedXyData(mCall[["predictor"]], LHS, data)
  x <-  xy[, "x"]; y <- xy[, "y"]
  a <- max(y) * 1.05
  pseudoY <-  log(( a - y )/ y)
  pseudoX <- log(x)
  lmFit <- lm(pseudoY ~ pseudoX )
  coefs <- coef(lmFit)
  b <- exp(coefs[1])
  c <- - coefs[2]
  value <- c(a, b, c)
  names(value) <- mCall[c("a", "b", "c")]
  value
}

NLShillCurve <- selfStart(hillCurveMean, hillCurveInit, parameters=c("a", "b", "c"))

"hill" <-
  function(fixed = c(NA, NA, NA), names = c("a", "b", "c"))
  {
    ## Checking arguments
    numParm <- 3
    if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
    if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
    
    ## Fixing parameters (using argument 'fixed')
    notFixed <- is.na(fixed)
    parmVec <- rep(0, numParm)
    parmVec[!notFixed] <- fixed[!notFixed]
    
    ## Defining the non-linear function
    fct <- function(x, parm)
    {
      parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
      parmMat[, notFixed] <- parm
      
      a <- parmMat[, 1]; b <- parmMat[, 2]; c <- parmMat[, 3]
      (a * x^c)/(b + x^c)
    }
    
    ## Defining self starter function
    ssfct <- function(dataf)
    {
      x <- dataf[, 1]
      y <- dataf[, 2]
      
      a <- max(y) * 1.05
      
      ## Linear regression on pseudo y values
      pseudoY <-  log(( a - y )/ y)
      pseudoX <- log(x)
      coefs <- coef( lm(pseudoY ~ pseudoX ) )
      b <- exp(coefs[1])
      c <- - coefs[2]
      
      return(c(a, b, c)[notFixed])
    }
    
    ## Defining names
    pnames <- names[notFixed]
    
    ## Defining derivatives
    
    ## Defining the ED function
    
    ## Defining the inverse function
    
    ## Defining descriptive text
    text <- "Hill function (Morgan-Mercer-Flodin)"
    
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, ssfct = ssfct, names = pnames, text = text, noParm = sum(is.na(fixed)))
    
    class(returnList) <- "drcMean"
    invisible(returnList)
  }

"hillMax" <-
  function(fixed = c(NA, NA, NA), names = c("a", "b", "c"))
  {
    ## Checking arguments
    numParm <- 3
    if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
    if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
    
    ## Fixing parameters (using argument 'fixed')
    notFixed <- is.na(fixed)
    parmVec <- rep(0, numParm)
    parmVec[!notFixed] <- fixed[!notFixed]
    
    ## Defining the non-linear function
    fct <- function(x, parm)
    {
      parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
      parmMat[, notFixed] <- parm
      
      a <- parmMat[, 1]; b <- parmMat[, 2]; c <- parmMat[, 3]
      (8 * 0.8 * (1 + b * exp ( -c * log(a))))/(1 + b * exp ( -c * log(x)))
    }
    
    ## Defining self starter function
    
    ## Defining names
    pnames <- names[notFixed]
    
    ## Defining descriptive text
    text <- "Hill function (Morgan-Mercer-Flodin) with expected value parameters replacement"
    
    ## Returning the function with self starter and names
    returnList <- list(fct = fct, names = pnames, text = text, noParm = sum(is.na(fixed)))
    
    class(returnList) <- "drcMean"
    invisible(returnList)
  }


powerCurveNO.fun <- function(predictor, a, b, c) {
  a * ( predictor ^ b ) + c
}


"DRC.powerCurveNO" <- function(fixed = c(NA, NA, NA), names = c("a", "b", "c"))
{
  ## Checking arguments
  numParm <- 3
  if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
  if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
  
  ## Fixing parameters (using argument 'fixed')
  notFixed <- is.na(fixed)
  parmVec <- rep(0, numParm)
  parmVec[!notFixed] <- fixed[!notFixed]
  
  ## Defining the non-linear function
  fct <- function(x, parm)
  {
    
    
    parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
    parmMat[, notFixed] <- parm
    
    a <- parmMat[, 1]; b <- parmMat[, 2]; c <- parmMat[, 3]
    a * x ^(b) + c
  }
  
  ## Defining self starter function
  # ssfct <- function(dataf)
  # {
  #   x <- dataf[, 1]
  #   y <- dataf[, 2]
  #   
  #   #regression on pseudo y values
  #   pseudoY <- log( y + 0.00001)
  #   pseudoX <- log(x)
  #   coefs <- coef( lm(pseudoY ~ pseudoX) )
  #   a <- exp(coefs[1])
  #   
  #   b <- coefs[2]
  #   
  #   return(c(a, b)[notFixed])
  # }
  
  ## Defining names
  pnames <- names[notFixed]
  
  ## Defining derivatives
  
  ## Defining the ED function
  
  ## Defining the inverse function
  
  ## Defining descriptive text
  text <- "Power curve not passing for origin"
  
  ## Returning the function with self starter and names
  returnList <- list(fct = fct, names = pnames, text = text, noParm = sum(is.na(fixed)))
  
  class(returnList) <- "drcMean"
  invisible(returnList)
}