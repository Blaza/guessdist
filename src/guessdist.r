library(fitdistrplus)
# we need to change dir to be src so we can source, it's monkey patching but
# in the end we will merge all scripts
dir <- dirname(parent.frame(2)$ofile)
setwd(dir)
source('distros.r')
source('tests.r')

# function to process one distribution, i.e. generate MLEs afrom the sample smp
# and run tests from 'tests' which support the current distro. It takes the
# argument distro as a distro object from 'distros.r' and t.codes is a vector of
# strings (test codes) which says which tests we would like to run if possible.
# We returna list containing a vector with the name of the distribution,
# estimated parameters, AIC value, BIC value and p values of wanted tests. We
# also return the fitted fitdist object
process.distro <- function(smp, distro, t.codes) {
    # the filter we use to get tests which support current distro.
    flt <- function(t) {
        # allows tests which contain the type of the distro or the distro itself
        return(distro$type %in% t$distros || distro$code %in% t$distros)
    }
    # get tests that support distro
    allowed.tests <- Filter(flt, tests)
    # get codes vector of allowed.tests
    allowed.t.codes <- sapply(allowed.tests, function(t){ t$code })

    # start the result vector, beginning with distro name, which we know
    result <- c(distro$name)

    # get fitdist object which has estimates and is used for tests
    fit <- fitdist(smp, distro$code, method='mle')

    # get the MLEs
    mles <- fit$estimate
    # convert this vector to a string like:
    # par1.name: est1.value
    # par2.name: est2.value
    mle.string <- paste( sapply(1:length(mles), function(i){
                                    paste(names(mles)[i], mles[i], sep=': ')
                                }),
                         collapse='\n' )

    # append this string to result
    result <- c(result, mle.string)

    # append the AIC and BIC values
    result <- c(result, fit$aic, fit$bic)

    # now we run the tests and append p values to result
    p.values <- sapply(t.codes,
                       function(code) {
                           if(code %in% allowed.t.codes){
                               test <- get.test(code)
                               return(test$pval(fit))
                           } else {
                               return(NA)
                           }
                       })
    result <- c(result, p.values)

    # we create the vector of column names for the returned vector. We start
    # with known values then add the test names which we get with the
    # get.test.name function from the tests.r  file
    col.names <- c('Distribution', 'MLEs', 'AIC', 'BIC')
    test.names <- sapply(t.codes, get.test.name)
    col.names <- c(col.names, test.names)
    names(result) <- col.names

    # return the result with nice column names and the fitted fitdist object
    return(list('result'=result, 'fitobj'=fit))
}
