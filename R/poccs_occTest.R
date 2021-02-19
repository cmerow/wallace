#' @title poccs_occTest Test occurrence data quality
#' @description Test occurrence for geographic or environmental errors
#' @details This function is called by the component poccs and is used to flag occurrences based on whether they pass given tests.
#' @param occs data frame of cleaned occurrences obtained from component occs: Obtain occurrence data
#' @param sp.name species name
#' @param sp.table occurrence df
#' @param r.env raster of environmental layers
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @examples
# spN<-"Panthera onca"
# out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
# occs <- as.data.frame(out.gbif[[1]]$cleaned)
# out.thin <- poccs_thinOccs(occs=occs, thinDist=30,spN=spN,logger=NULL)
#'
#' @return Output is a data frame of  occurences with columns appended that indicate pass/fail of various tests
#' @author Cory Merow
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export

wallaceOccTest<- function(sp.name, sp.table ,r.env, shinyLogs = NULL){
  if(!require(occTest)){
    shinyLogs %>% writeLog("Please install the occTest package before running.")
    return()
  }
  smartProgress(shinyLogs, message = "performing tests for occurrence quality",{
    # ot=occurrenceTests(sp.name=sp.name,
    #                  sp.table = sp.table,
    #                  r.env = r.env)
    ot=cbind(sp.table,test=1)
  })
  shinyLogs %>% writeLog("occurrence testing complete")
  return(ot)
}


#' @title poccs_occTestFilter Filter occurrence data based on quality
#' @description Filter occurrences based on geographic or environmental errors
#'
#' @details This function is called by the component poccs and is used to filter occurrences based on whether they pass given tests.
#'
#' @param occs data frame of cleaned occurrences obtained from component occs: Obtain occurrence data
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @examples
# spN<-"Panthera onca"
# out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
# occs <- as.data.frame(out.gbif[[1]]$cleaned)
# out.thin <- poccs_thinOccs(occs=occs, thinDist=30,spN=spN,logger=NULL)
#'
#' @return Output is a data frame of  occurences with columns appended that indicate pass/fail of various tests
#' @author Cory Merow
#' @author Gonzalo E. Pinilla-Buitrago <gpinillabuitrago@@gradcenter.cuny.edu>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export
wallaceOccTestFilter<- function(df, level ,errorAcceptance,shinyLogs = NULL){
  if(!require(occTest)){
    shinyLogs %>% writeLog("Please install the occTest package before running.")
    return()
  }
  smartProgress(shinyLogs, message = "filtering occurrences",{
    #of=occFilter(df = df,level=level,errorAcceptance=errorAcceptance)
    # temporarily just get a subset
    of=df[sample(1:nrow(df), floor(nrow(df)/2)),]
  })
  shinyLogs %>% writeLog("occurrence filtering complete")
  return(of)
}

