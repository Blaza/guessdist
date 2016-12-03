library(goftest)
library(KScorrect)
library(fitdistrplus)

# We will work with objects of class fitdist from fitdistrplus
# test class interface:
#   name='long name'
#   code='shorthand id'
#   distros=c(for,which,distros,can,be,applied)
#   warning='warning message regarding this test'
#   fun=function(fit){return(pval)}

tests <- list(
              list(
                   name='Chi-squared',
                   code='chisq',
                   distros=c('discrete','continuous'),
                   warning='',
                   fun=function(fit){
                       gof <- gofstat(fit)
                       return(gof$chisqpvalue)
                   }
                  ),
              list(
                   name='Kolmogorov-Smirnov',
                   code='ks',
                   distros=c('continuous'),
                   warning='K-S test may give very inaccurate p-values, because
                            the parameters are estimated. This should be
                            possible to correct using the Lilliefors-Corrected
                            version of the test, but for a limited number of
                            distributions.',
                   fun=function(fit){
                   }
                  ),
              list(
                   name='Lilliefors-Corrected Kolmogorov-Smirnov',
                   code='lcks',
                   distros=c('norm','lnorm','unif','exp','gamma','weibull'),
                   warning='Lilliefors-Corrected K-S test can only be applied
                            to normal, log-normal, uniform, exponential, gamma
                            and Weibull distributions.',
                   fun=function(fit){
                   }
                  ),
              list(
                   name='Anderson-Darling',
                   code='ad',
                   distros=c('continuous'),
                   warning='Anderson-Darling test may give inaccurate p-values
                            as the parameters are estimated. For a limited
                            number of distributions, Lilliefors-Corrected
                            Kolmogorov-Smirnov test may give better results.',
                   fun=function(fit){
                       pdf.name <- paste('p', fit$distname, sep='')
                       args <- append(list(fit$data, pdf.name),
                                      as.list(fit$estimate))
                       test <- do.call('ad.test', args)
                       return(test$p.value)
                   }
                  ),
              list(
                   name='Cramer-von Mises',
                   code='cvm',
                   distros=c('continuous'),
                   warning='Cramer-von Mises test may give inaccurate p-values
                            as the parameters are estimated. For a limited
                            number of distributions, Lilliefors-Corrected
                            Kolmogorov-Smirnov test may give better results.',
                   fun=function(fit){
                       pdf.name <- paste('p', fit$distname, sep='')
                       args <- append(list(fit$data, pdf.name),
                                      as.list(fit$estimate))
                       test <- do.call('cvm.test', args)
                       return(test$p.value)
                   }
                  )
             )

get.test <- function(code) {
    # find tests with given code
    matched.test <- Filter(function(t){t$code == code}, tests)
    # only one test will be matched, so get the first element of the filtered
    # list or NULL if no match is found
    if(length(matched.test) != 0) {
        result <- matched.test[[1]]
    } else {
        result <- NULL
    }
    return(result)
}

# function to get full test name from test code (wrapper for get.test)
get.test.name <- function(code) {
    test <- get.test(code)
    return( ifelse( is.null(test), '', test$name ) )
}
