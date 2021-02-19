poccs_occTest_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    span("Step 1:", class = "step"),
    span("Test for problematic occurrences", class = "stepText"), br(), br(),
    tags$div(
      title = "Types of tests",
      checkboxInput(ns("geoTests"), label = strong("Geographic Tests"),
                    value = FALSE), # Check default (value = FALSE),
      checkboxInput(ns("envTests"), label = strong("Environmental Tests"),
                    value = FALSE), # Check default (value = FALSE),
      checkboxInput(ns("humanTests"), label = strong("Human Modification Tests"),
                    value = FALSE)#, # Check default (value = FALSE),
      #checkboxInput(ns("timeTests"), label = strong("Time Tests"),
      #              value = FALSE) # Check default (value = FALSE)
    ),
    tags$div(
      title = "Add Batch guidance text here (**)",
      checkboxInput(ns("batch2"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
    actionButton(ns("goTestOccs"), "Test Occurrences"),
    tags$hr(),

    span("Step 2:", class = "step"),
    span("Filter Occurrences", class = "stepText"), br(), br(),
    strong(paste0('Remove occurrences that fail quality tests')), br(), br(),
    tags$div(
      # CM: these currently won't do anything; all tests are performed
      title = "Tests to filter by:",
      checkboxInput(ns("geoTests"), label = strong("Geographic Tests"),
                    value = FALSE), # Check default (value = FALSE),
      checkboxInput(ns("envTests"), label = strong("Environmental Tests"),
                    value = FALSE), # Check default (value = FALSE),
      checkboxInput(ns("humanTests"), label = strong("Human Modification Tests"),
                    value = FALSE)#, # Check default (value = FALSE),
      #checkboxInput(ns("timeTests"), label = strong("Time Tests"),
      #              value = FALSE) # Check default (value = FALSE)
    ),
    #CM: what is the value of this? the list elements?
    radioButtons(ns("occFilterRule"), "Filter Rule:",
                 choices = list("Strict (passes all tests)",
                                "Majority (passes half tests)",
                                "Relaxed (passes any tests")),
    tags$div(
      title = "Add Batch guidance text here (**)",
      checkboxInput(ns("batch2"), label = strong("Batch"), value = FALSE) # Check default (value = FALSE)
    ),
    actionButton(ns("goFilterOccs"), "Filter Occurrences")
  )
}

poccs_occTest_module_server <- function(input, output, session, common) {

  logger <- common$logger
  spp <- common$spp
  allSp <- common$allSp
  curSp <- common$curSp
  envs.global <- common$envs.global
  envs <- common$envs
  #bgExt <- common$bgExt
  occs <- common$occs

  # observeEvent(input$run, { # outline
  #   # WARNING ####
  #   # FUNCTION CALL ####
  #   # LOAD INTO SPP ####
  #   # METADATA ####
  # })
  observeEvent(input$goTestOccs, {
    common$update_component(tab = "Map")
    # ERRORS #### Not needed yet
    # if (is.null(envs())) {
    #   logger %>% writeLog(type = 'error', hlSpp(curSp()),
    #                       'Environmental variables missing.',
    #                       '. Obtain them in component 3.')
    #   return()
    # }
    req(curSp(), occs())

    # loop over all species if batch is on
    if (input$batch1 == TRUE) spLoop <- allSp() else spLoop <- curSp()

    for (sp in spLoop) {
      # FUNCTION CALL ####
      # CM: eventually use the settings from input$geoTests, input$envTests, input$humanTests
      occsTested = wallaceOccTest(sp.name=sp, sp.table=spp[[sp]]$occs,
                                  r.env=spp[[sp]]$envs, shinyLogs=logger)
      req(occsTested) # cm:what does this do?

      # LOAD INTO SPP ####
      spp[[sp]]$occs <- occsTested

      # METADATA
      # CM: can change these to settings if user can control them
      spp[[sp]]$rmm$dataPrep$geographic$geographicStandardization=T
      # spp[[sp]]$rmm$data$occurrence$backgroundSampleSizeRule <-
      #   paste0(input$bgSel, ', ', input$bgBuf, ' degree buffer')

      ##Creating these to facilitate RMD generation.
      #CM: if we add the optiuon to turn test blocks on/off, then indicate which test blocks were run
      #spp[[sp]]species$rmm$code$wallace$filterRule='something'
    }
  })

  observeEvent(input$goFilterOccs, {
  #   # WARNING #### CM: probably need a warning if someone tries to run this event without running step 1
  #   if (input$bgPtsNum < 1) {
  #     logger %>% writeLog(type = 'warning',
  #                         "Enter a non-zero number of background points.")
  #     return()
  #   }
    req(curSp(), occs())

    # loop over all species if batch is on
    if (input$batch1 == TRUE) spLoop <- allSp() else spLoop <- curSp()

    for (sp in spLoop) {
      # FUNCTION CALL ####
      #CM: what is the value of this? the list elements?

     if(input$occFilterRule=="Strict (passes all tests)") filterRule='stringent'
     if(input$occFilterRule=="Majority (passes half tests)") filterRule='majority'
     if(input$occFilterRule=="Relaxed (passes any tests") filterRule='relaxed'

      # CM: eventually use the settings from input$ to set the level
      occsFiltered = wallaceOccTestFilter(df=spp[[sp]]$occs, level=1,
                                          errorAcceptance=filterRule, shinyLogs=logger)
      req(occsFiltered) # cm:what does this do?

      # LOAD INTO SPP ####
      spp[[sp]]$procOccs$occsPreOccTest=spp[[sp]]$occs
      spp[[sp]]$procOccs$occsPostOccTest=occsFiltered
      # CM: check that i duplicate the new occs in order to track all the changes
      spp[[sp]]$occs <- occsFiltered

      # METADATA #### CM: i think not needed
      # CM: replace the number of presences samples because i think there isn't a field to report cleaning. (the metadata doesn't care about how you go to this number; that's recorded in dataPrep fields)
      spp[[sp]]$rmm$data$occurrence$presenceSampleSize=nrow(occsFiltered)

      ##Creating these to facilitate RMD generation.
      # CM:: whats needed for this?
      spp[[sp]]$species$rmm$code$wallace$filterRule=filterRule
    }
    common$update_component(tab = "Map") # CM: needed?
  })
  #CM: output some aspect of the test table as to results
  # Define output as a table
  # output$envCorrTable <- renderText({
  #   # Result
  #   knitr::kable(spp[[curSp()]]$procEnvs$envCorrs, format = 'html')
  # })


  #CM: not sure what i need to do here. do i need to update the value of occs table?
  return(list(
  #   save = function() {
  #     # Save any values that should be saved when the current session is saved
  #     list(
  #       bgSel = input$bgSel,
  #       bgBuf = input$bgBuf,
  #       bgPtsNum = input$bgPtsNum
  #     )
  #   },
  #   load = function(state) {
    # CM: I'm not sure where the state variables are defined to know what names i need to give them
  #     # Load
  #     updateRadioButtons(session, "bgSel", selected = state$bgSel)
  #     updateNumericInput(session, "bgBuf", value = state$bgBuf)
  #     updateNumericInput(session, "bgPtsNum", value = state$bgPtsNum)
  #   }
  ))
  common$update_component(tab = "Map")

}

# FROM TEMPLATE
#CM: maybe eventually add a standard table that comes out of occTest show the number lost to each test
# poccs_occTest_module_result <- function(id) {
#   ns <- NS(id)
#
#   # Result UI
#   verbatimTextOutput(ns("result"))
# }
# EXample
# correlations_module_result <- function(id) {
# ns <- NS(id)
# #spp <- common$spp
# #curSp <- common$curSp
# # Result UI as html
# htmlOutput(ns("envCorrTable"))
# }

poccs_occTest_module_map <- function(map, common) {
  # Map logic
  curSp <- common$curSp
  spp <- common$spp
  req(spp[[curSp()]]$occs)
  occs <- common$occs
  # if you've thinned already, map thinned points blue
  # and kept points red
  if (!is.null(spp[[curSp()]]$procOccs$occsPostOccTest)) {
    occs.preThin <- spp[[curSp()]]$procOccs$occsPreOccTest
    map %>% clearAll() %>%
      addCircleMarkers(data = occs.preThin, lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "blue",
                       fillOpacity = 1, weight = 2, popup = ~pop) %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 1, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs()) %>%
      addLegend("bottomright", colors = c('red', 'blue'), title = "Occ Records",
                labels = c('retained', 'removed'), opacity = 1)
  } else {
    # if you haven't thinned, map all points red
    map %>% clearAll() %>%
      addCircleMarkers(data = occs(), lat = ~latitude, lng = ~longitude,
                       radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                       fillOpacity = 0.2, weight = 2, popup = ~pop) %>%
      zoom2Occs(occs()) %>%
      leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
  }
}

#CM: Not sure what i need to do here
# which tests blocks were done and which filter rule was used are the only user inputs used
#
# poccs_occTest_module_rmd <- function(species) {
#   # Variables used in the module's Rmd code
#   list(
#     poccs_occTest_knit = species$rmm$code$wallace$someFlag,
#     var1 = species$rmm$code$wallace$someSetting1,
#     var2 = species$rmm$code$wallace$someSetting2
#   )
# }
#
