library(actuar)
distros = list(
               list(
                    name='Beta',
                    code='beta',
                    params=2,
                    type='continuous'
                    ),
               list(
                    name='Binomial',
                    code='binom',
                    params=2,
                    type='not supported'
                    ),
               list(
                    name='Cauchy',
                    code='cauchy',
                    params=2,
                    type='continuous'
                    ),
               list(
                    name='Exponential',
                    code='exp',
                    params=1,
                    type='continuous'
                    ),
               list(
                    name='F distribution',
                    code='f',
                    params=2,
                    type='not supported'
                    ),
               list(
                    name='Gamma',
                    code='gamma',
                    params=2,
                    type='continuous'
                    ),
               list(
                    name='Geometric',
                    code='geom',
                    params=1,
                    type='discrete'
                    ),
               list(
                    name='Hypergeometric',
                    code='hyper',
                    params=3,
                    type='not supported'

                    ),
               list(
                    name='Log-normal',
                    code='lnorm',
                    params=2,
                    type='continuous'
                    ),
               list(
                    name='Negative binomial',
                    code='nbinom',
                    params=2,
                    type='discrete'
                    ),
               list(
                    name='Normal',
                    code='norm',
                    params=2,
                    type='continuous'
                    ),
               list(
                    name='Poisson',
                    code='pois',
                    params=1,
                    type='discrete'
                    ),
               list(
                    name='t-distribution',
                    code='t',
                    params=1,
                    type='not supported'
                    ),
               list(
                    name='Uniform',
                    code='unif',
                    params=2,
                    type='continuous'
                    ),
               list(
                    name='Weibull',
                    code='weibull',
                    params=2,
                    type='continuous'
                    ),
               list(
                    name='Pareto',
                    code='pareto',
                    params=2,
                    type='continuous'
                    )
               )

# function to get distro object, given code
get.distro <- function(code) {
    # find distros with given code
    matched.dist <- Filter(function(d){d$code == code}, distros)
    # only one distro will be matched, so get the first element of the filtered
    # list or NULL if no match is found
    if(length(matched.dist) != 0) {
        result <- matched.dist[[1]]
    } else {
        result <- NULL
    }
    return(result)
}

# function to get full distribution name from the code (wrapper for get.distro)
get.distro.name <- function(code) {
    distro <- get.distro(code)
    return( ifelse( is.null(distro), '', distro$name ) )
}
