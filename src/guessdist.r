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

    # return the result with nice column names and the fitted fitdist object
    return(list('result'=result, 'fitobj'=fit))
}

# The main function which will fit data smp to selected distributions and use
# selected tests to calculate p values. Best is chosen by the crit criterion.
# Returns a list with a dataframe containing a table with results and a string
# containing the approximate guess for the best fit distribution.
fit.data <- function(smp, d.codes, t.codes, crit, plots=FALSE) {
    # the filter we use to get distros which were chosen.
    flt <- function(d) {
        return(d$code %in% d.codes)
    }
    # get distro objects which correspond to d.codes selected distros
    chosen.distros <- Filter(flt, distros)

    # process every chosen distro and get results as a list
    results.list <- lapply(chosen.distros,
                           function(d) {
                               return(process.distro(smp, d, t.codes))
                           })

    # if plots is TRUE, plot the results
    if(plots){
        for(res in results.list){
            par(mar=c(5.1,4.1,8.1,2.1))
            plot(res$fitobj)
            title(res$result[1], outer=TRUE, line=-2)
        }
    }

    # get the vector containing the result component for each chosen distro
    results <- sapply(results.list, function(r){r$result})

    # and put it in a dataframe. We take the transpose so we get results
    # displayed by row, i.e. in the form:
    # distribution 1 | estimates | ....
    # distribution 2 | estimates | ....
    dframe=as.data.frame(t(results))

    # we create the vector of column names for the returned vector. We start
    # with known values then add the test names which we get with the
    # get.test.name function from the tests.r  file
    col.names <- c('Distribution', 'MLEs')
    test.names <- sapply(t.codes, get.test.name)
    col.names <- c(col.names, test.names)
    colnames(dframe) <- col.names

    # get test object corresponding to criterion crit
    crit.obj <- get.test(crit)
    # get the index of the best guess using the crit$best function
    guess.index <- crit.obj$best(dframe[[crit.obj$name]])
    # get the row associated with that guess
    guess.row <- dframe[guess.index, ]
    # guessed distribution name
    guess <- toString(guess.row[['Distribution']])
    # parameters to be displayed in end message, replace newline with ", "
    params <- gsub('\\n', ', ', guess.row$MLEs)

    # we return the dataframe which should be displayed as a final result and
    # the guess concluded by the analysis.
    return(list('table'=dframe,
                'guess'=paste('Our guessed distribution is', guess,
                              'with parameters:', params)))
}

