# we need to get source dir so we can source
script.dir <- dirname(sys.frame(1)$ofile)
source(paste(script.dir, 'guessdist.r', sep='/'))

# wrapper for fit.data function in guessdist.r to adapt it for spss
guess.dist <- function(variable, d.codes, t.codes, crit, plots=FALSE) {
    # get spss data as data.frame
    smp.df <- spssdata.GetDataFromSPSS(variables=c(variable),
                                       missingValueToNA=TRUE)
    # get first column of the dataframe, basicaly turning dataframe to vector
    smp <-smp.df[,1]

    return(fit.data(smp, unlist(d.codes), unlist(t.codes), crit, plots))
}

Run <- function(args){
    cmdname = args[[1]]
    args <- args[[2]]
    oobj <- spsspkg.Syntax(templ=list(
                spsspkg.Template("VARIABLE", subc="",  ktype="existingvarlist", var="variable", islist=FALSE),
                spsspkg.Template("DISTRIBUTION", subc="",  ktype="str", var="d.codes", islist=TRUE),
                spsspkg.Template("TEST", subc="",  ktype="str", var="t.codes", islist=TRUE),
                spsspkg.Template("CHOOSEBY", subc="",  ktype="str", var="crit", islist=FALSE),
                spsspkg.Template("PLOT", subc="OPTIONS", ktype="bool", var="plots")
            ))

    spsspkg.StartProcedure("Guess distribution")

    res <- spsspkg.processcmd(oobj,args,"guess.dist")

    spsspivottable.Display(res$table,
                           title="Fitting results table",
                           format=formatSpec.GeneralStat,
                           hiderowdimlabel=TRUE,
                           hidecoldimlabel=TRUE)

    spss.TextBlock("Distribution guess:",
                   res$guess)

    spsspkg.EndProcedure()
}
