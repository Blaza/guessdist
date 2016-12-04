Warner <- local({
  warnings = c()
  list(
    warn=function(w) {
        if(nchar(w) != 0) {
            warnings <<- unique(c(warnings, w))
        }
    },
    getWarnings=function() {
      return(warnings)
    })
})
