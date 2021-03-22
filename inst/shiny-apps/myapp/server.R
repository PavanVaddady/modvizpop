## User defined functions

# convert to numeric
anac <- function(x) as.numeric(as.character(x))

# convert to character
ac <- function(x) as.character(x)

# create a list of table followed by caption for latex
table4Latex <- function(x) {
                table <- xtable(x$table, caption = x$caption)
                tableArray <- sub('\\[ht\\]', '\\[H\\]',
                              readLines(textConnection(print(table, include.rownames=FALSE), 'r')))
                return(tableArray)
}

scaled80Table4Latex <- function(x) {
  table <- xtable(x$table, caption = x$caption)
  tableArray <- sub('\\[ht\\]', '\\[H\\]',
                    readLines(textConnection(print(table, scalebox = 0.8, include.rownames=FALSE), 'r')))
  return(tableArray)
}


scaled75RTable4Latex <- function(x) {
  table <- xtable(x$table, caption = x$caption)
  tableArray <- sub('\\[ht\\]', '\\[H\\]',
                    readLines(textConnection(print(table, scalebox = 0.75, include.rownames=TRUE), 'r')))
  return(tableArray)
}

geoMean <- function(x) exp(mean(log(x)))

geoCV <- function(x) 100 * sqrt(exp(sd(log(x))^2) - 1)


#############################
####### Shiny server ########
#############################

shinyServer(function(input, output, session) {

userName <- Sys.info()[7]
UID   <- paste0(userName, ceiling(runif(1, 1, 100000)))

## Logout Option
output$loggedUser <- renderText({
  return(paste0("Welcome ", userName, "!"))
})

observeEvent(input$login, {
  stopApp()
})

# Save user session information
userData <- data.frame(ID=UID, START = Sys.time())

# Reactive value to refresh session
value <- reactiveVal(0)
cumValues <- reactiveVal("0")

observeEvent(input$runsim, {
  newValue <- 1
  value(newValue)
  newCumValues <- paste0(value())
  cumValues(newCumValues)
  })

observeEvent(input$updatePlot, {
  newValue <- 2
  value(newValue)
  newCumValues <- paste(cumValues(), value(), sep=",")
  cumValues(newCumValues)
})

observeEvent(input$calculateNCA, {
  newValue <- 3
  value(newValue)
  newCumValues <- paste(cumValues(), value(), sep=",")
  cumValues(newCumValues)
})


## Wait for some events to happen before some buttons are active
observe({
  toggleState("runsim", input$loadModel!=0 && !is.null(modelLoadMessage())  && !is.null(selectedModel()))
  toggleState("updatePlot", length(grep("1",cumValues()))==1)
  toggleState("calculateNCA", length(grep("1",cumValues()))==1)
  toggleState("dwnModel", length(grep("1",cumValues()))==1)
  toggleState("dwnPlot", length(grep("2",cumValues()))==1)
  toggleState("dwnSimData", length(grep("1",cumValues()))==1)
  toggleState("dwnNCARaw", length(grep("3",cumValues()))==1)
  toggleState("dwnNCASumm", length(grep("3",cumValues()))==1)
  toggleState("dwnReport", length(grep("1",cumValues()))==1 && length(grep("2",cumValues()))==1 && length(grep("3",cumValues()))==1)
  toggleState("dwnSession", length(grep("1",cumValues()))==1)
  toggleState("modelSelect", input$advOptions=="None")
  toggleState("doseSelect", input$eventData=="No")
})

mysessdir <- paste0("scratch/",UID,"/dwnSession")
dir.create(mysessdir, showWarnings = FALSE,  mode = "0777", recursive = TRUE)

## Create a session folder (updated)
sessionPath <- function() {
  Sys.setenv(LOCALE=paste("en_US.UTF-8"))
  return(file.path(mysessdir))
}

## Create a temporary working folder for each user
tempPath <- reactive ({

  #create a unique ID for each session and library
  tmpCode <- paste0(input$sources, ceiling(runif(1, 1, 100000000)))

  Sys.setenv(LOCALE=paste("en_US.UTF-8"))
  mymaindir <- paste0("scratch/",UID,"/",tmpCode)
  return(file.path(mymaindir))
}) #tempPath


## Create the model text based on user input (LIBODE)
modelText <- reactive({

    if(input$route=="Select" || input$cmpt=="Select" || input$PDmodel=="Select")
    return()

    if(input$route %in% c("IVB","IVI") && input$cmpt!="Select" && input$PDmodel=="None") {
    modelText <- paste0("PKM-IV-", input$cmpt)
    }

    if(input$route %in% c("IVB","IVI") && input$cmpt!="Select"
      && !input$PDmodel %in% c("Select","None")) {
    modelText <- paste0("PKPDM-IV-", input$cmpt,"-",input$PDmodel)
    }

    if(input$route=="ABS" && input$cmpt!="Select" && input$PDmodel=="None") {
    modelText <- paste0("PKM-ABS-", input$cmpt)
    }

    if(input$route=="ABS" && input$cmpt!="Select"
       && !input$PDmodel %in% c("Select","None")) {
    modelText <- paste0("PKPDM-ABS-", input$cmpt,"-",input$PDmodel)
    }

    return(modelText)
}) #modelText


### For Project Repository (PROJ)
shinyFileChoose(input,'inFile',roots = c(Projects="Models_PROJ_Format/"),session = session,
                  restrictions = system.file(package = 'base'))

projRep <- reactive({
  if(is.null(input$inFile)) return(list(NULL, NULL))
  inFileO <- parseFilePaths(roots=c(Projects="Models_PROJ_Format/"),input$inFile)
  projModel <- ac(inFileO$datapath)
  modelName <- ac(gsub("(^.+/[A-Z]+)(\\w+/)(.+)(\\.cpp)", "\\3", projModel))
  return(list(projModel, modelName))
})

## mread model
selectedModel <- eventReactive(input$loadModel, {

    if(is.null(modelText()) && input$sources=="LIBODE")
    return()

    if (is.null(input$userModelFile) && input$sources=="USER")
    return()

    if (is.null(projRep()[2]) && input$sources=="PROJ")
    return()


    # ODE library
    if(input$sources=="LIBODE" && !is.null(modelText())) {

    dir.create(tempPath(), showWarnings = FALSE, mode = "0777", recursive = TRUE)

    file.copy(paste0("Models_ODE_Format/",modelText(), ".cpp"),
                     tempPath(),
                     overwrite = TRUE)

    withProgress(message = "Loading Model...Please Wait!", value = 0, {
      mod <- mread(modelText(), project=tempPath())
      setProgress(message="Model Loaded", 3)
      Sys.sleep(1)
     })
    }


    # User defined
    if(!is.null(input$userModelFile) && input$sources=="USER") {
    inFile <- input$userModelFile
    userModel <- inFile$datapath

    dir.create(tempPath(), showWarnings = FALSE, mode = "0777", recursive = TRUE)

    file.copy(userModel, tempPath(),  overwrite = TRUE)

    withProgress(message = "Loading Model...Please Wait!", value = 0, {
      mod <- mread("0", project = tempPath())
      setProgress(message="Model Loaded", 3)
      Sys.sleep(1)
     })
    }


    # Project Repository
    if(!is.null(projRep()[2]) && input$sources=="PROJ") {

    dir.create(tempPath(), showWarnings = FALSE, mode = "0777", recursive = TRUE)

    file.copy(ac(projRep()[1]), tempPath(), overwrite = TRUE)

    withProgress(message = "Loading Model...Please Wait!", value = 0, {
    mod <- mread(ac(projRep()[2]), project=tempPath())
    setProgress(message="Model Loaded", 3)
    Sys.sleep(1)
       })
    }
    return(mod)
}) #selectedModel


#modelLoadMessage
modelLoadMessage <- eventReactive(input$loadModel, {
  if(input$sources=="LIBODE") {
    if(is.null(modelText())) {
      modelMessageText <- paste0("Please select a model.")
    }
    if(!is.null(modelText())) {
    modelMessageText <- paste0("PKPD Library Model: ", modelText(), ".cpp model loaded.")
    }
  }
  if(input$sources=="USER") {
    modelMessageText <- ifelse(!is.null(input$userModelFile[1]),
                               paste0("User Defined Model: ", input$userModelFile[1], " model loaded."),
                               "No model file loaded.")
  }
  if(input$sources=="PROJ") {
    modelMessageText <- paste0("Project Library Model: ", ac(as.data.frame(input$inFile)[,2]), "/",
                               ac(as.data.frame(input$inFile)[,3]), " model loaded.")
  }
  return(modelMessageText)
})


output$modelLoadMessageO <- renderUI({
    return(modelLoadMessage())
})


# User provided dataset/event dataset
eventDataInput <- function() {
  if(is.null(input$inputEventData)) return()
  if(input$eventData=="Yes" && !is.null(input$inputEventData)) {
    eventInFile <-  input$inputEventData
    dataset <- read.csv(eventInFile$datapath) %>%
      mutate(endTime=max(anac(time), ii*(addl+1)+24))
  if(!"dgrp" %in% names(dataset)) dataset$dgrp <- 1
  return(dataset)
  }
}

# Parameter pop up box
output$fixedPop <- renderUI ({

  bsModal("fixedPopUp", "Fixed Parameters Selection", "fixedSelect", size = "large",
          if(is.null(selectedModel())){
            HTML(paste(""))
          },

          if(!is.null(selectedModel())) {

            mod <-  selectedModel()
            w <- ""
            col1 <- ""
            col2 <- ""
            col3 <- ""
            col4 <- ""
            for(i in 1:ceiling(length(param(mod))/2)) {
                      col1 <- paste(col1, textInput(paste0(names(param(mod))[i],"I"), p(paste0("Select ",names(param(mod))[i]),
                                                                                         style="color: #20786d;
                                                                                         font-weight: bold;"),
                              value=param(mod)[i]))
            }
            for(i in 1:ceiling(length(param(mod))/2)) {
                      col2 <- paste(col2, textInput(paste0(names(param(mod))[i],"U"), p(paste0(names(param(mod))[i], " Units"),
                                                                                        style="color: #79d2c0;
                                                                                        font-weight: bold;"),
                              value=as.list(mod)$details$data$unit[i]))
            }
            for(i in (ceiling(length(param(mod))/2)+1):length(param(mod))) {
                       col3 <- paste(col3,textInput(paste0(names(param(mod))[i],"I"), p(paste0("Select ",names(param(mod))[i]),
                                                                                         style="color: #20786d;
                                                                                         font-weight: bold;"),
                               value=param(mod)[i]))
            }
            for(i in (ceiling(length(param(mod))/2)+1):length(param(mod))) {
                      col4 <- paste(col4, textInput(paste0(names(param(mod))[i],"U"), p(paste0(names(param(mod))[i], " Units"),
                                                                                        style="color: #79d2c0;
                                                                                        font-weight: bold;"),
                              value=as.list(mod)$details$data$unit[i]))
            }
          w <- paste(w, tags$div(class="parambox",
                                   tags$div(class="left4_col", HTML(col1)),
                                   tags$div(class="left4_col", HTML(col2)),
                                   tags$div(class="left4_col", HTML(col3)),
                                   tags$div(class="right4_col", HTML(col4))))
          HTML(w)
          })

}) # fixedPop


## Omega Matrix
omegaMat <-  reactive ({

  if(is.null(selectedModel())){
    return()
  }

  if(!is.null(selectedModel())) {
  mod <-  selectedModel()

  myOmgMat <- as.matrix(revar(mod)$omega)
  omgNames <- as.list(mod)$random$omega_labels[[1]]
  rownames(myOmgMat) <- omgNames
  colnames(myOmgMat) <- omgNames
  }
  return(myOmgMat)

})

omegaVal <- reactiveValues(data=as.matrix(0, nrow=1, ncol=1))

observe({
  input$loadModel
  omegaVal$data <- omegaMat()
})

observe({
  if(!is.null(input$omegaTab))
  omegaVal$data <- hot_to_r(input$omegaTab)
})

output$omegaTab <- renderRHandsontable({
  rownames(omegaVal$data) <- colnames(omegaVal$data)
  rhandsontable(omegaVal$data, rowHeaderWidth=100)  %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_cols(manualColumnResize = TRUE, format = "0.000")
}) #omegaMat


##  Sigma Matrix
sigmaMat <- reactive ({

  if(is.null(selectedModel())){
    return()
  }

  if(!is.null(selectedModel())) {
    mod <-  selectedModel()
    mySigMat <- as.matrix(revar(mod)$sigma)
    sigNames <- as.list(mod)$random$sigma_labels[[1]]
    rownames(mySigMat) <- sigNames
    colnames(mySigMat) <- sigNames
  }
  return(mySigMat)
})

sigmaVal <- reactiveValues(data=as.matrix(0,nrow=1, ncol=1))

observe({
  input$loadModel
  sigmaVal$data <- sigmaMat()
})

observe({
  if(!is.null(input$sigmaTab))
    sigmaVal$data <- hot_to_r(input$sigmaTab)
})

output$sigmaTab <- renderRHandsontable({
  rownames(sigmaVal$data) <- colnames(sigmaVal$data)
  rhandsontable(sigmaVal$data, rowHeaderWidth=100)  %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_cols(manualColumnResize = TRUE, format = "0.000")
})#sigmaMat


# Endtime Calculation
endTimeCalc <-  function(){
if(input$eventData=="No") {
  if(is.na(anac(input$ii)) || is.na(anac(input$nDoses))) {
    return(24)
    }
    return(anac(input$nDoses)*anac(eval(parse(text=input$ii))))
 }

if(input$eventData=="Yes" && !is.null(eventDataInput())) {
  return(max(anac(eventDataInput()$endTime)))
  }
}


# nSub Calculation
nSubO <- function(){
  ifelse(input$randomOptions!="None" && input$eventData=="No", 10,
         ifelse(input$randomOptions!="None" && input$eventData=="Yes", 1, 1))
}

output$simOptions <- renderUI ({
  bsModal("simOptionsPopUp", "Simulation Options", "simSelect", size = "large",
    tags$div(class="parambox",
       textInput("startTime","Select Start Time", 0),
       textInput("dur", "Select End Time", endTimeCalc()),
       textInput("timePoints","Specify Timepoints", paste0("seq(0, ", endTimeCalc(), ", 0.25)")),
       if(input$randomOptions!="None") textInput("seed","Select Seed", 123456),
       textInput("nsub","Select Number of Subjects", nSubO()))
  )
})

# Update the runsim into the user session information
observeEvent(input$runsim, {
  userData <- userData %>% mutate(SOURCE=ac(input$sources), N=anac(input$runsim))
  write.table(userData, paste0("scratch/",UID,"/","userData.tab"),
              append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
})


# Check Par
parCheck <- eventReactive(input$runsim,{

  if(is.null(selectedModel())) return()

  mod <- selectedModel()

  errText <- NULL

  # Fixed parameters
  for(i in 1:length(param(mod))) {
    evalFixedParams <- try(eval(parse(text=input[[paste0(names(param(mod))[i],"I")]])), silent=T)

    if(is(evalFixedParams,"try-error")) {
      errText <- paste0(errText, tags$p(paste0("Fixed Parameters: Please select a valid ", names(param(mod))[i], " value.\n"), style="color: red;"))
    }
  }


  # Random parameters
  if(!isSymmetric(omegaVal$data)) {
    errText <- paste0(errText, tags$p(paste0("Random Parameters: Please select a symmetric omega matrix.\n"), style="color: red;"))
  }
  if(!isSymmetric(sigmaVal$data)) {
    errText <- paste0(errText, tags$p(paste0("Random Parameters: Please select a symmetric sigma matrix.\n"), style="color: red;"))
  }


  # Dosing options
  evalAmt <- try(eval(parse(text=paste0("c(",input$doses,")"))), silent=T)
  if(is(evalAmt,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Dosing Options: Please select a valid entry for doses.\n"), style="color: red;"))
  }


  evalLtinf <- try(eval(parse(text=input$Ltinf)), silent=T)
  if(is(evalLtinf,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Dosing Options: Please select a numeric value for duration of infusion.\n"), style="color: red;"))
  }

  evalUtinf <- try(eval(parse(text=input$Utinf)), silent=T)
  if(is(evalUtinf,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Dosing Options: Please select a numeric value for duration of infusion.\n"), style="color: red;"))
  }


  evalII <- try(eval(parse(text=input$ii)), silent=T)
  if(is(evalII,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Dosing Options: Please select a numeric value for dosing interval.\n"), style="color: red;"))
  }

  evalNDoses <- try(eval(parse(text=input$nDoses)), silent=T)
  if(is(evalNDoses,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Dosing Options: Please select a numeric value for number of doses.\n"), style="color: red;"))
  }

  evalCmt <- try(eval(parse(text=input$cmt)), silent=T)
  if(is(evalCmt,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Dosing Options: Please select a numeric value for dosing compartment.\n"), style="color: red;"))
  }

  # Simulation Options
  evalStartTime <- try(eval(parse(text=input$startTime)), silent=T)
  if(is(evalStartTime,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Simulation Options: Please select a numeric value for simulation start time.\n"), style="color: red;"))
  }

  evalEndTime <- try(eval(parse(text=input$dur)), silent=T)
  if(is(evalEndTime,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Simulation Options: Please select a numeric value for simulation end time.\n"), style="color: red;"))
  }

  evalTimePoints <- try(eval(parse(text=paste0("c(",input$timePoints,")"))), silent=T)
    if(is(evalTimePoints,"try-error")) {
      errText <- paste0(errText, tags$p(paste0("Simulation Options: Please select a valid entry for timepoints.\n"), style="color: red;"))
    }

  if(input$randomOptions!="None") {
  evalSimSeed <- try(eval(parse(text=input$seed)), silent=T)
  if(is(evalSimSeed,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Simulation Options: Please select a numeric value for simulation seed.\n"), style="color: red;"))
    }
  }

  evalNSub <- try(eval(parse(text=input$nsub)), silent=T)
  if(is(evalNSub,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Simulation Options: Please select a numeric value for number of subjects.\n"), style="color: red;"))
  }

  if(!is(evalNSub,"try-error")) {
    if(anac(input$nsub)>10000) {
    errText <- paste0(errText, tags$p(paste0("Simulation Options: Number of subjects cannot exceed 10000. Please select a value less than 10000.\n"), style="color: red;"))
    }
    if(!is(evalAmt,"try-error")) {
      if(anac(input$nsub)*
         length(eval(parse(text=paste0("c(",input$doses,")")))) > 50000) {
         errText <- paste0(errText, tags$p(paste0("Total number of subjects (number of subjects x number of dose groups) cannot exceed 50000.
                                                  Please select a different combination of doses (Dosing Options) and number of subjects (Simulation Options).\n"), style="color: red;"))
      }
      if(!is(evalTimePoints,"try-error")) {
        if(anac(input$nsub)*
           length(eval(parse(text=paste0("c(",input$doses,")"))))*
           length(eval(parse(text=paste0("c(",input$timePoints,")")))) > 500000) {
           errText <- paste0(errText, tags$p(paste0("Total number of obervations (number of subjects x number of dose groups x number of time points per subject) cannot exceed 500000.
                                                    Please select a different combination of doses (Dosing Options), number of subjects and time points (Simulation Options).\n"), style="color: red;"))
        }
      }
    }
  }


  # Advanced options
  evalParSweepVal <- try(eval(parse(text=paste0("c(",input$parSweepVal,")"))), silent=T)
  if(is(evalParSweepVal,"try-error")) {
    errText <- paste0(errText, tags$p(paste0("Advanced Options: Please select a valid entry for parametric sweep values.\n"), style="color: red;"))
  }

  return(errText)
})

output$parCheckE <- renderUI({
  HTML(parCheck())
})

## Updated model parameter list
newPar  <- eventReactive(input$runsim,{

  if(!is.null(parCheck())) return()

  if(is.null(selectedModel())) return()

   mod <- selectedModel()

   ## Create a list of parameter names and their values
   VAL1 <- lapply(1:length(param(mod)), function(i) {
                anac(eval(parse(text=input[[paste0(names(param(mod))[i],"I")]])))
          })

   PAR1 <- mapply(list, names(param(mod)), VAL1, SIMPLIFY=T)

   VAL2 <- lapply(1:length(param(mod)), function(i) {
           input[[paste0(names(param(mod))[i],"U")]]
           })

   PAR2 <- mapply(list, names(param(mod)), VAL2, SIMPLIFY=T)

   VAL3 <- lapply(1:length(param(mod)), function(i) {
           mod@annot$data$desc[i]
           })

   PAR3 <- mapply(list, names(param(mod)), VAL3, SIMPLIFY=T)

   PAR <- rbind(rbind(PAR1, PAR2[2,]), PAR3[2,])

   return(PAR)

}) #newPar

## updated model with updated parameters
runMod  <-  eventReactive(input$runsim,{

       if(!is.null(parCheck())) return()

       if(is.null(selectedModel())) return()

       mod <- selectedModel()

       mod <- param(mod, newPar()[2,])

       if(input$randomOptions=="None")
       mod <- mod %>% zero.re()

       if(input$randomOptions=="Between Subject Only")
       mod <- mod %>% omat(hot_to_r(input[["omegaTab"]])) %>% zero.re("sigma")

       if(input$randomOptions=="Residual Only")
       mod <- mod %>% smat(hot_to_r(input[["sigmaTab"]])) %>% zero.re("omega")

       if(input$randomOptions=="Both")  {
       mod <- mod %>% omat(hot_to_r(input[["omegaTab"]]))
       mod <- mod %>% smat(hot_to_r(input[["sigmaTab"]]))
       }

       mod@annot$data$unit[mod@annot$data$block=="PARAM"] <- t(as.data.frame(newPar()[3,]))

       return(mod)
 }) #runMod


## Running the model with events
runOut <- eventReactive(input$runsim,{

     if(!is.null(parCheck())) return()

      rate <- 0
      tinf <- 0

      if(input$eventData=="No") {

           amt <- anac(eval(parse(text=paste0("c(",input$doses,")"))))

           addl <- anac(input$nDoses)-1
           ii <- anac(eval(parse(text=input$ii)))

           if(input$route=="IVI" && !input$sources %in% c("USER","PROJ")) {
           tinf <- anac(eval(parse(text=input$Ltinf)))
           }

           if(input$sources %in% c("USER","PROJ")) {
           tinf <- anac(eval(parse(text=input$Utinf)))
           }

           cmt <- anac(input$cmt)

      } #Only when eventData is not chosen


      end <- anac(eval(parse(text=input$dur)))

      nsub <- ifelse(input$randomOptions!="None" && input$eventData=="No", 10,
                     ifelse(input$randomOptions!="None" && input$eventData=="Yes", 1, 1))
      if(!is.null(input$nsub)) nsub <- anac(eval(parse(text=input$nsub)))

      seed <- 123456
      if(input$randomOptions!="None" && !is.null(input$seed)) seed <- anac(eval(parse(text=input$seed)))

      delta <- end

      timePoints <- NULL
      evalTimePoints <- try(eval(parse(text=paste0("c(",input$timePoints,")"))), silent=T)
      if(!is(evalTimePoints,"try-error")) {
      timePoints <- eval(parse(text=paste0("c(", input$timePoints, ")")))
      }

      # User specified time points
      if (is.null(timePoints)) design <-  NA
      if (!is.null(timePoints)) design <- timePoints[!is.na(timePoints)]

      if(input$advOptions=="Parameter Sweep") {

           parSweepVal <-  eval(parse(text=paste0("c(", input$parSweepVal, ")")))
           parSweep <- input$parSweep

           if(input$eventData=="No") {
           dataset <- expand.ev(ID=1:nsub, amt=amt, ii=ii, addl=addl, cmt=cmt, tinf=tinf, parVal=parSweepVal, dgrp=1) %>%
                      mutate(dose=amt, rate=ifelse(tinf==0, 0, dose/tinf))
           }

          if(input$eventData=="Yes" && !is.null(input$inputEventData)) {
            dataset <- eventDataInput()
            dataset <- bind_rows(replicate(nsub*length(parSweepVal), dataset, simplify = FALSE), .id="Rep") %>%
                       mutate(ID=max(dataset$ID)*(anac(Rep)-1)+ID,
                              parVal=rep(parSweepVal, each=nsub*nrow(dataset))) %>%
                       select(-Rep) %>%
                       mutate(dose=amt)
          }

           names(dataset)[match(c("parVal"), names(dataset))] <- parSweep

           txt <- paste0(names(dataset), collapse=",")

           set.seed(seed)
           out <- runMod() %>%
                  data_set(dataset) %>%
                  mrgsim(delta=delta, end=end, add=c(design), carry.out=txt, recsort=3)
        }

       if(input$advOptions!="Parameter Sweep") {

           if(input$eventData=="No") {
             dataset <- expand.ev(ID=1:nsub, amt=amt, ii=ii, addl=addl, tinf=tinf, cmt=cmt, dgrp=1) %>%
                        mutate(dose=amt, rate=ifelse(tinf==0, 0, dose/tinf))
               }

           if(input$eventData=="Yes" && !is.null(input$inputEventData)) {
             dataset <- eventDataInput()
             dataset <- bind_rows(replicate(nsub, dataset, simplify = FALSE), .id="Rep") %>%
                        mutate(ID=max(dataset$ID)*(anac(Rep)-1)+ID) %>%
                        select(-Rep) %>%
                        mutate(dose=amt)
           }

           txt <- paste0(names(dataset), collapse=",")

           set.seed(seed)
           out <- runMod() %>%
                  data_set(dataset) %>%
                  mrgsim(delta=delta, end=end, add=c(design), carry.out=txt, recsort=3)
          }

      return(out)

}) #runOut


## Create outputs of simulation and save them for reference
runSim  <- eventReactive(input$runsim, {

  if(!is.null(parCheck())) return()


  withProgress(message = "Running Simulation...Please Wait!", value = 0, {

  if(input$advOptions=="None") {
       sims <- runOut() %>% as_tibble %>%
               select(-match(names(init(selectedModel())), names(runOut()))) %>%
               mutate(TYPE="Simulation") %>%
               filter(time >= anac(input$startTime))

       simData <- sims
       write.csv(sims, file=paste0(tempPath(),"/","refSim.csv"), quote=F, row.names=F)
    }

  if(input$advOptions=="Overlay External Data") {

    inFile <- input$inputData
    if (is.null(inFile))
        return(NULL)

    obsData <- read.csv(inFile$datapath) %>%
               mutate(TYPE="Overlay")

    sims <- runOut() %>% as_tibble %>%
            select(-match(names(init(selectedModel())), names(runOut()))) %>%
            mutate(TYPE="Simulation") %>%
            filter(time >= anac(input$startTime))

    write.csv(sims, file=paste0(tempPath(),"/","refSim.csv"), quote=F, row.names=F)

    simData <- suppressWarnings(suppressMessages(full_join(sims, obsData)))
  }


  if(input$advOptions=="Set As Reference") {

    sims <- runOut() %>% as_tibble %>%
            select(-match(names(init(selectedModel())), names(runOut()))) %>%
            mutate(TYPE="Simulation") %>%
            filter(time >= anac(input$startTime))

    refData <- read.csv(file=paste0(tempPath(),"/","refSim.csv")) %>%
               mutate(TYPE="Reference")

    simData <- suppressWarnings(suppressMessages(full_join(sims, refData)))
  }

  if(input$advOptions=="Parameter Sweep") {
       sims <- runOut() %>% as_tibble %>%
               select(-match(names(init(selectedModel())), names(runOut()))) %>%
               mutate(TYPE="Simulation") %>%
               filter(time >= anac(input$startTime))

       simData <- sims
    }

    setProgress(message="Simulation Complete", 3)
    Sys.sleep(1)
    return(simData)
    })
}) #runSim

# Create an output datatable
output$simData <- DT::renderDataTable({
  input$runsim
  if(is.null(runSim())) return()
  if(!is.null(runSim())) {
  simDataO <- runSim()
  is.num <- sapply(simDataO, is.numeric)
  simDataO[is.num] <- lapply(simDataO[is.num], signif, 4)

  datatable(simDataO, class="compact",
    callback=JS('$("a.buttons-colvis").css({"background": "#43b3a5",
                                            "color": "#000",
                                            "fontWeight": "bold",
                                            "fontSize": "14px"});
                 $("a.buttons-colvis").text("Choose Columns");
                    return table;'),
    options=list(
      initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#43b3a5', 'color': '#000'});",
      "}"),
      pageLength = 10, autoWidth=T,
      dom = 'Bfrtip', buttons = I('colvis')),
      rownames=F,
      filter="bottom", style="default", selection='none',
      extensions = 'Buttons')
  }
})

## Output parameters
outVar <- reactive({

  input$runsim
  if(!is.null(parCheck())) return()

  allVars <- names(runSim())

  captured <- c(unlist(as.list(runMod())$details$data %>%
                         filter(block=="CAPTURE") %>%
                         select(name), use.names=F), "time")

  cmpts <- unlist(as.list(runMod())$details$data %>%
                    filter(block=="CMT") %>%
                    select(name), use.names=F)

  remVars <- allVars[!allVars %in% c("evid", "amt", "tinf", "cmt", "ii", "addl", "rate", "endTime", captured, cmpts)]

  return(list(allVars, captured, cmpts, remVars))
}) #outVar


# Parameters for sweep
output$parSweep <- renderUI({
      selectizeInput("parSweep","Select Parameter",
                     names(param(selectedModel())))
})

## Plot Options
output$plotOptions <- renderUI({
  bsModal("plotOptionsPopUp", "Plot Options", "customPlotOptions", size = "large",
      tags$div(class="parambox",
          tags$div(class="left3_col",
                   selectizeInput("xVar","Select X",
                                  choices=unlist(outVar()[2]), selected="time") ,
                   selectizeInput("yVar","Select Y",
                                  choices=c(unlist(outVar()[2])[!unlist(outVar()[2]) %in% ("time")],"ALL COMPARTMENTS"),
                                  selected="ALL COMPARTMENTS"),
                   selectizeInput("groupBy","Select Grouping",
                                  choices=unlist(outVar()[4]), selected="ID"),
                   if(input$eventData=="No") {
                     selectizeInput("colorBy","Select Color Variable",
                                    choices=unlist(outVar()[4]), selected="dose")
                   },
                   if(input$eventData=="Yes" && !is.null(input$inputEventData)) {
                     selectizeInput("colorBy","Select Color Variable",
                                    choices=unlist(outVar()[4]), selected="dgrp")
                   },
                   selectizeInput("facetBy","Select Faceting",
                                  choices=unlist(outVar()[4]), selected="", multiple=T),
                   textInput("filterData","Subset"),
                   radioButtons("predIntPlot", "Plot Prediction Intervals", c("Yes","No"), "No", inline=T),
                   textInput("predInt","Prediction Interval (%)", 95),
                   br(), br(), br(), br()),
          tags$div(class="left3_col",
                   radioButtons("logX", "X-axis", c("Linear","Log"), "Linear", inline=T),
                   radioButtons("logY", "Y-axis", c("Linear","Log"), "Linear", inline=T),
                   br(),
                   textInput("xlabI","X-axis Label"),
                   textInput("titleI","Title"),
                   textInput("xRef","Horizontal Reference Line"),
                   textInput("iHeight","Plot Height (px)","500"),
                   textInput("baseSize", "Plot Text Font Size","11"),
                   textInput("nrowDef","Number of Figure Rows"),
                   if(input$advOptions=="Overlay External Data") textInput("pointSizeOverlay","Overlay Point Size","2"),
                   if(input$advOptions=="Set As Reference") textInput("lineSizeRef","Reference Line Size","1.1"),
                   br(), br(), br(), br()),
          tags$div(class="right3_col",
                   radioButtons("scales", "Scales",
                                c("Fixed","Free X","Free Y","Free Both"), "Free Y", inline=T),
                   radioButtons("includePoint","Include Points", c("Yes","No"), "No", inline=T),
                   textInput("ylabI","Y-axis Label"),
                   textInput("footnoteI","Footnote"),
                   textInput("yRef","Vertical Reference Line"),
                   textInput("iWidth","Plot Width (px)","670"),
                   textInput("legendNRow","Number of Rows for Legend", 1),
                   if(input$advOptions=="Overlay External Data") textInput("shapeOverlay","Overlay Shape","1"),
                   if(input$advOptions=="Set As Reference") textInput("ltyRef","Reference Linetype","dotted"),
                   br(), br(), br(), br())
          ))
}) #plotOptions


plotOptionsCheck <- eventReactive(input$updatePlot, {

  if(!is.null(parCheck())) return()

  if(is.null(runSim())) {
    return()
  }

  plotErrText <- NULL

  if(input$predIntPlot=="Yes") {
  evalpredInt <- try(eval(parse(text=paste0(input$predInt))), silent=T)
  if(is(evalpredInt,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for prediction interval.\n", style="color: red;"))
    }
  }

  evalxRef <- try(eval(parse(text=paste0(input$xRef))), silent=T)
  if(is(evalxRef,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for horizontal reference line.\n", style="color: red;"))
  }

  evalyRef <- try(eval(parse(text=paste0(input$yRef))), silent=T)
  if(is(evalyRef,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for vertical reference line.\n", style="color: red;"))
  }

  evaliHeight <- try(eval(parse(text=paste0(input$iHeight))), silent=T)
  if(is(evaliHeight,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for plot height.\n", style="color: red;"))
  }

  evaliWidth <- try(eval(parse(text=paste0(input$iWidth))), silent=T)
  if(is(evaliWidth,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for plot width.\n", style="color: red;"))
  }

  evalnRowDef <- try(eval(parse(text=paste0(input$nrowDef))), silent=T)
  if(is(evalnRowDef,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for the number of figure rows.\n", style="color: red;"))
  }

  evalbaseSize <- try(eval(parse(text=paste0(input$baseSize))), silent=T)
  if(is(evalbaseSize,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for the plot font size.\n", style="color: red;"))
  }

  evallegendNRow <- try(eval(parse(text=paste0(input$legendNRow))), silent=T)
  if(is(evallegendNRow,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for the number of rows for legend.\n", style="color: red;"))
  }

  if(input$advOptions=="Overlay External Data") {
    evalpointSizeOverlay <- try(eval(parse(text=paste0(input$pointSizeOverlay))), silent=T)
  if(is(evalpointSizeOverlay,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for overlay point size.\n", style="color: red;"))
    }

    evalshapeOverlay <- try(eval(parse(text=paste0(input$shapeOverlay))), silent=T)
    if(is(evalshapeOverlay,"try-error")) {
      plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for shape overlay.\n", style="color: red;"))
    }
  }

  if(input$advOptions=="Set As Reference") {
    evallineSizeRef <- try(eval(parse(text=paste0(input$lineSizeRef))), silent=T)
  if(is(evallineSizeRef,"try-error")) {
    plotErrText <- paste0( plotErrText, tags$p("Plot Options: Please select a numeric value for reference line size.\n", style="color: red;"))
    }
  }

  return(plotErrText)

})


output$plotOptionsCheckE <- renderUI({
  HTML(plotOptionsCheck())

})



## Generate custom simulation plot
plotSim <- eventReactive(input$updatePlot, {

  if(!is.null(plotOptionsCheck())) return()

  if(is.null(runSim())) {
    return()
  }

    plotData <- runSim()

    xVar <- ac(input$xVar)
    yVar <- ifelse(ac(input$yVar)=="ALL COMPARTMENTS","value",ac(input$yVar))
    xlabI <- ifelse(ac(input$xlabI)=="",xVar, ac(input$xlabI))
    ylabI <- ifelse(ac(input$ylabI)=="",yVar, ac(input$ylabI))

    groupBy <- ac(input$groupBy)
    facetBy <- input$facetBy
    colorBy <- ac(input$colorBy)
    filterData <- ifelse(ac(input$filterData)=="", "time>=0", ac(input$filterData))

    nrowDef <- anac(input$nrowDef)
    titleI <- ac(input$titleI)
    footnoteI <- ac(input$footnoteI)

    pi <- anac(input$predInt)
    piLow <- ((100-pi)/2)/100
    piHigh <- 1 - piLow

    subtitleText <- ""
    if(ac(input$filterData)!="") subtitleText <- paste0("Subset: ", ac(input$filterData))
    if(input$predIntPlot=="Yes") subtitleText <- paste0(subtitleText, "\n",
                                                        "Shaded regions include median with simulated ", piLow*100, "th and ", piHigh*100, "th percentiles")

    subtitleI <- subtitleText
    legendNRow <- anac(input$legendNRow)

    scalesO <- switch(ac(input$scales),
                      "Fixed" = "fixed",
                      "Free X" = "free_x",
                      "Free Y" = "free_y",
                      "Free Both" = "free")

    baseSize <- anac(input$baseSize)

    if(input$advOptions=="Overlay External Data") {
    pointSizeOverlay <- anac(input$pointSizeOverlay)
    shapeOverlay <- anac(input$shapeOverlay)
    }

    if(input$advOptions=="Set As Reference") {
    lineSizeRef <- anac(input$lineSizeRef)
    ltyRef <- ac(input$ltyRef)
    }

    if(input$advOptions=="Parameter Sweep") parSweep <- isolate(input$parSweep)
    if(input$advOptions!="Parameter Sweep") parSweep <- ""

    if(xVar=="time" && yVar=="value") {
      plotData <- runSim() %>%
        gather(cmpt, value, unlist(outVar()[2])[!unlist(outVar()[2]) %in% c("time")]) %>%
        filter_(.dots=filterData)
    }

    if(yVar!="value") {
      plotData <- runSim() %>%
        filter_(.dots=filterData)
    }

    if(nrow(plotData)>0) {
      p0 <- ggplot(plotData %>% filter(time>=0, TYPE=="Simulation"),
                   aes(get(xVar), get(yVar))) +
        theme_bw(base_size = baseSize) +
        ylab(paste0(ylabI,"\n")) + xlab(paste0("\n",xlabI)) +
        labs(title=titleI, subtitle=subtitleI, caption=footnoteI) +
        scale_color_discrete(colorBy) +  guides(color=guide_legend(nrow=legendNRow, byRow=T)) +
        theme(legend.position="top",
              plot.title=element_text(hjust=0.5),
              plot.subtitle=element_text(hjust=0.5, face="italic"))

      xRef <- anac(input$xRef)
      yRef <- anac(input$yRef)

      if(!is.na(xRef)) {
      p0 <- p0 + geom_hline(yintercept=xRef, color="grey60", alpha=0.75) +
        annotate("text", x=0.01, y=xRef, label=xRef, vjust=-0.3, color="grey20", alpha=0.75)
      }

      if(!is.na(yRef)) {
      p0 <- p0 + geom_vline(xintercept=yRef, color="grey60", alpha=0.75) +
        annotate("text", x=yRef, y=0.01, label=yRef, hjust=-0.3, color="grey20", alpha=0.75)
      }

      p1 <- p0 + geom_line(aes(group=get(groupBy), color=factor(get(colorBy)), linetype=TYPE)) +
            scale_linetype_discrete(guide=F)

     if(xVar!="time" && yVar!="value") {
      p1 <- p0 + geom_path(aes(group=get(groupBy), color=factor(get(colorBy)),linetype=TYPE)) +
        scale_linetype_discrete(guide=F)
     }
    }

    if(input$predIntPlot=="Yes") {
      p1 <- p0
    }

    if(yVar=="value") {

      nrowDef <- ifelse(is.na(nrowDef),
                        nrow(plotData %>% select(match("cmpt", names(.))) %>% distinct()),
                        nrowDef)

      if(length(facetBy)==0) {
        p1 <- p1 + facet_wrap(~ cmpt, nrow=nrowDef, scales=scalesO, labeller = "label_both")
       }

      if(length(facetBy)!=0) {
        p1 <- p1 + facet_wrap(formula(paste("~ cmpt+", paste0(facetBy,collapse="+"))),
                              nrow=nrowDef, scales=scalesO, labeller = "label_both")
        }
    }

    if(yVar!="value") {

      if(length(facetBy)==0) {
        p1 <- p1
      }

      if(length(facetBy)!=0) {

       nrowDef <- ifelse(is.na(nrowDef),
                        nrow(plotData %>% select(match(facetBy[1], names(.))) %>% distinct()),
                        nrowDef)
       p1 <- p1 + facet_wrap(formula(paste("~ ", paste0(facetBy,collapse="+"))),
                            nrow=nrowDef, scales=scalesO, labeller = "label_both")
      }
    }


    if(input$advOptions=="Overlay External Data") {

      p1 <- p1 +
        geom_point(data=plotData %>%
                     filter(time>=0, TYPE=="Overlay") %>%
                     mutate(color=factor(get(colorBy), levels=sort(unique(anac(get(colorBy)))),
                                         labels=sort(unique(anac(get(colorBy)))))),
                   aes(group=get(groupBy), color=factor(get(colorBy)),
                       shape="Input Data"), size=pointSizeOverlay, alpha=0.5) +
        scale_shape_manual(name="",values=c("Input Data"=shapeOverlay)) +
        theme(legend.box="horizontal")
    }


    if(input$predIntPlot=="Yes") {
      p1 <- p1 +
        stat_summary(mapping=aes(get(xVar), get(yVar),
                                 fill=factor(get(colorBy)),
                                 color=factor(get(colorBy)),
                                 linetype="Simulation"), alpha=0.5,
                     fun.ymax=function(x) quantile(x, probs=piHigh),
                     fun.ymin=function(x) quantile(x, probs=piLow),
                     geom="ribbon") +
        stat_summary(mapping=aes(get(xVar), get(yVar),
                                color=factor(get(colorBy)),
                                linetype="Simulation"), size=1.1,
                    fun.y=function(x) quantile(x, probs=0.5),
                    geom="line") +
        scale_fill_discrete(colorBy) +
        scale_linetype_discrete(guide=F)
    }


    if(input$advOptions=="Set As Reference") {
      if(input$predIntPlot=="No")  {
      p1 <- p1 +
        geom_line(data=plotData %>%
                    filter(time>=0, TYPE=="Reference"),
                  aes(group=get(groupBy), color=factor(get(colorBy)),
                      linetype="Reference"), size=lineSizeRef) +
        scale_linetype_manual(name="",values=c("Simulation"="solid", "Reference"=ltyRef)) +
        theme(legend.box="horizontal")
      }

      if(input$predIntPlot=="Yes") {
      p1 <- p1 +
            stat_summary(data=plotData %>%
                         filter(time>=0, TYPE=="Reference"),
              mapping=aes(get(xVar), get(yVar),
                          fill=factor(get(colorBy)),
                          color=factor(get(colorBy)),
                          linetype="Reference"), alpha=0.5,
              fun.ymax=function(x) quantile(x, probs=piHigh),
              fun.ymin=function(x) quantile(x, probs=piLow),
              geom="ribbon") +
            stat_summary(data=plotData %>%
                       filter(time>=0, TYPE=="Reference"),
              mapping=aes(get(xVar), get(yVar),
                         color=factor(get(colorBy)),
                         linetype="Reference"), size=1.1,
              fun.y=function(x) quantile(x, probs=0.5),
              geom="line") +
              scale_color_discrete("")  + scale_fill_discrete("") +
              scale_linetype_manual(name="", values=c("Simulation"="solid", "Reference"=ltyRef))  +
              guides(linetype = guide_legend(override.aes=list(color="black", fill="transparent")))
      }
    }


     if(input$logY=="Linear" && input$logX=="Linear") p1 <- p1
     if(input$logY=="Log" && input$logX=="Linear") p1 <- p1 + scale_y_log10()
     if(input$logY=="Linear" && input$logX=="Log") p1 <- p1 + scale_x_log10()
     if(input$logY=="Log" && input$logX=="Log") p1 <- p1 + scale_y_log10() + scale_x_log10()
     if(input$includePoint=="Yes") p1 <- p1 + geom_point()

    # Automatic saving of the files
    png(paste0(sessionPath(),"/SimPlot-",gsub(":","-", Sys.time()),".png"),
        type="cairo-png", height=anac(input$iHeight), width=anac(input$iWidth))
    print(p1)
    dev.off()

    return(p1)

   }) #plotSim


outPlotO <- reactive({

if(value()==1) {
  return()
}

if(length(grep("2",cumValues()))==1) {
  plotSim()
}
})

iWidth <- eventReactive(input$updatePlot, {
    if(is.na(anac(input$iWidth))) return(670)
    return(anac(input$iWidth))
})

iHeight <- eventReactive(input$updatePlot, {
  if(is.na(anac(input$iHeight))) return(500)
  return(anac(input$iHeight))
})


## Render simulation plot
observe({
output$outPlot <- renderPlot({
  outPlotO()
}, height=iHeight(), width=iWidth()) #outPlot
})


########## Tables ##########

## Dosing table
dosingTable <- eventReactive(input$runsim,{

      if(!is.null(parCheck())) return()

      doseO <- data.frame(Dose = paste(anac(eval(parse(text=paste0("c(",input$doses,")")))),collapse=", "),
                          Tau = eval(parse(text=input$ii)))

                if(input$route=="IVI" && !input$sources %in% c("USER","PROJ")) {
                    doseO$InfusionTime = eval(parse(text=input$Ltinf))
                }

                if(input$sources %in% c("USER","PROJ")) {
                    doseO$InfusionTime = eval(parse(text=input$Utinf))
                }
      doseO$nDoses <- input$nDoses
      doseO$DosingCompartment <- input$cmt

      doseO <- data.frame(lapply(doseO, ac), stringsAsFactors=FALSE)  %>%
                gather("Parameter","Value")


    doseO <- doseO %>%
             mutate(Units = ifelse(Parameter=="Dose", input$doseUnits,
                                ifelse(Parameter=="Tau", input$iiUnits,
                                   ifelse(input$route=="IVI" &&
                                          !input$sources %in% c("USER","PROJ") &&
                                          Parameter=="InfusionTime", input$LtinfUnits,
                                      ifelse(input$sources %in% c("USER","PROJ") &&
                                             Parameter=="InfusionTime", input$UtinfUnits,"")))))

  if(input$advOptions=="None" || input$advOptions=="Overlay External Data" || input$advOptions=="Parameter Sweep") {
    doseReturn <- doseO
    write.csv(doseReturn, file=paste0(tempPath(),"/","refDose.csv"), row.names=F)

  }

  if(input$advOptions=="Set As Reference") {

    refDose <- read.csv(paste0(tempPath(),"/","refDose.csv")) %>%
               mutate(Reference=ac(Value),
                      Reference_Units=ac(Units)) %>%
               select(Parameter, Reference, Reference_Units)

    doseReturn <- suppressWarnings(suppressMessages(full_join(doseO %>% select(-Units),
                                             refDose %>% mutate(Units=Reference_Units))))
    doseReturn <- doseReturn  %>%  select(Parameter, Value, Reference, Units)
  }

  return(doseReturn)
}) #dosingTable

## Run an instance of the dosingTable to refresh
rundosingTable <- observeEvent(input$runsim,{
  if(!is.null(parCheck())) return()
  dosingTable()
}) #rundosingTable


## Render dosing table
output$dosingOutputTable <- renderTable({
  dosingTable()
}, include.rownames=FALSE) #dosingOutputTable


## Parameter Table
paramTable <- eventReactive(input$runsim, {
    if(!is.null(parCheck())) return()

    paramO <- data.frame(t(data.frame(t(param(runMod())[1:length(names(param(runMod())))]))))
    paramO$Parameter <- row.names(paramO)
    names(paramO) <- c("Value","Parameter")
    paramO$Value <- vapply(paramO$Value, paste, collapse = ", ", character(1L))
    paramO$Units <- ac(runMod()@annot$data$unit[runMod()@annot$data$block=="PARAM"])

  if(input$advOptions=="None" || input$advOptions=="Overlay External Data") {
    paramReturn <- paramO %>% select(Parameter, Value, Units)
    write.csv(paramReturn, file=paste0(tempPath(),"/","refParam.csv"), row.names=F)
  }

  if(input$advOptions=="Set As Reference") {

    refParam <- read.csv(paste0(tempPath(),"/","refParam.csv")) %>%
                mutate(Reference=ac(Value),
                       Reference_Units=ac(Units)) %>%
                select(Parameter, Reference, Reference_Units)

    paramReturn <- suppressWarnings(suppressMessages(full_join(paramO %>% select(-Units),
                                              refParam %>% mutate(Units=Reference_Units))))
    paramReturn <- paramReturn  %>%  select(Parameter, Value, Reference, Units)
  }

  if(input$advOptions=="Parameter Sweep") {
    paramO$Value[paramO$Parameter==ac(input$parSweep)] <- paste(eval(parse(text=paste0("c(", input$parSweepVal, ")"))), collapse=",")
    paramReturn <- paramO %>% select(Parameter, Value, Units)
  }

  return(paramReturn)
  }) #paramTable


## Run an instance of the paramTable to refresh
runparamTable <- observeEvent(input$runsim,{
  if(!is.null(parCheck())) return()
  paramTable()
}) #runparamTable

## Render paramTable
output$paramOutputTable <- renderTable({

  paramTable()

  }, include.rownames=FALSE) #paramOutputTable


# Random Effects Table
randomTable <- eventReactive(input$runsim, {
  if(!is.null(parCheck())) return()

    omgO <- omegaMat()
    omgO[!is.na(omgO)] <- "0.00"
    sigO <- sigmaMat()
    sigO[!is.na(sigO)] <- "0.00"
    refOmg <- omgO
    refSig <- sigO

  if(input$randomOptions %in% c("Between Subject Only", "Both")) omgO <- isolate(data.frame(omegaVal$data))
  if(input$randomOptions %in% c("Residual Only", "Both"))   sigO <- isolate(data.frame(sigmaVal$data))

  if(input$advOptions!="Set As Reference") {
    write.csv(omgO, file=paste0(tempPath(),"/","refOmg.csv"), row.names=T)
    write.csv(sigO, file=paste0(tempPath(),"/","refSig.csv"), row.names=T)
    }

    if(input$advOptions=="Set As Reference") {
      refOmg <- read.csv(paste0(tempPath(),"/","refOmg.csv"))
      rownames(refOmg) <- refOmg[,1]
      refOmg <- refOmg[,-1]

      refSig <- read.csv(paste0(tempPath(),"/","refSig.csv"))
      rownames(refSig) <- refSig[,1]
      refSig <- refSig[,-1]
    }

  return(list(omgO, sigO, refOmg, refSig))
}) #randomTable

## Run an instance of the randomTable to refresh
runrandomTable <- observeEvent(input$runsim,{
  if(!is.null(parCheck())) return()
  randomTable()
}) #runrandomTable

## Render randomTable
output$omgOutputTable <- renderTable({
    randomTable()[[1]]
}, spacing="xs", rownames = T) #omgOutputTable

output$sigOutputTable <- renderTable({
    randomTable()[[2]]
}, spacing="xs", rownames = T) #sigOutputTable

output$omgRefOutputTable <- renderTable({
   randomTable()[[3]]
}, spacing="xs", rownames = T) #omgRefOutputTable

output$sigRefOutputTable <- renderTable({
   randomTable()[[4]]
}, spacing="xs", rownames = T) #sigRefOutputTable


## SimulationOPtions table
simOptionsTable <- eventReactive(input$runsim,{
  if(!is.null(parCheck())) return()

  simO <- data.frame(StartTime = ifelse(!is.null(input$startTime), input$startTime, 0),
                    EndTime = anac(eval(parse(text=input$dur))),
                    TimePoints = ifelse(!is.null(input$timePoints), input$timePoints, "-"),
                    Seed = ifelse(input$randomOptions!="None" && !is.null(input$seed), input$seed, 123456),
                    N = ifelse(input$randomOptions!="None" && !is.null(input$nsub), input$nsub, 1))


  simO <- data.frame(lapply(simO, ac), stringsAsFactors=FALSE)  %>%
          gather("Parameter","Value")

  simO <- simO %>%
          mutate(Units = ifelse(Parameter=="StartTime", input$iiUnits,
                            ifelse(Parameter=="EndTime", input$iiUnits,
                              ifelse(Parameter=="TimePoints", input$iiUnits,""))))
                                #ifelse(Parameter=="StepSize", input$iiUnits,



  if(input$advOptions=="None" || input$advOptions=="Overlay External Data" || input$advOptions=="Parameter Sweep") {
    simReturn <- simO
    write.csv(simReturn, file=paste0(tempPath(),"/","refSimOptions.csv"), row.names=F)

  }

  if(input$advOptions=="Set As Reference") {

    refSimOptions <- read.csv(paste0(tempPath(),"/","refSimOptions.csv")) %>%
      mutate(Reference=ac(Value),
             Reference_Units=ac(Units)) %>%
      select(Parameter, Reference, Reference_Units)

    simReturn <- suppressWarnings(suppressMessages(full_join(simO %>% select(-Units),
                                                              refSimOptions %>% mutate(Units=Reference_Units))))
    simReturn <- simReturn  %>%  select(Parameter, Value, Reference, Units)
  }

  return(simReturn)
}) #simOptionsTable

## Run an instance of the simOptionsTable to refresh
runsimOptionsTable <- observeEvent(input$runsim,{
  if(!is.null(parCheck())) return()
  simOptionsTable()
}) #rundosingTable


## Render dosing table
output$simOptionsOutputTable <- renderTable({
  simOptionsTable()
}, include.rownames=FALSE) #simOptionsOutputTable

#initTable
initTable <- eventReactive(input$runsim, {
  if(!is.null(parCheck())) return()

  initO <- data.frame(t(data.frame(t(init(runMod())[1:length(names(init(runMod())))]))))
  initO$cmt <- row.names(initO)
  names(initO) <- c("InitialValue","Compartment")
  initO$InitialValue <- vapply(signif(anac(initO$InitialValue), 5), paste, collapse = ", ", character(1L))

  if(input$advOptions=="None" || input$advOptions=="Overlay External Data" || input$advOptions=="Parameter Sweep") {
    initReturn <- initO %>% select(Compartment, InitialValue)
    write.csv(initReturn, file=paste0(tempPath(),"/","refInit.csv"), row.names=F)
  }


  if(input$advOptions=="Set As Reference") {

    refInit <- read.csv(paste0(tempPath(),"/","refInit.csv")) %>%
                mutate(Reference=ac(InitialValue)) %>%
                select(Compartment, Reference)

    initReturn <- suppressWarnings(suppressMessages(full_join(initO,refInit)))
    initReturn <- initReturn  %>%  select(Compartment, InitialValue, Reference)
  }

  return(initReturn)


  }) #initTable


## Run an instance of the initTable to refresh
runinitTable <- observeEvent(input$runsim,{
  if(!is.null(parCheck())) return()
  initTable()
}) #runinitTable


## Render initOUtput table
output$initOutputTable <- renderTable({

  initTable()

  }, include.rownames=FALSE) #initOutputTable



## ODEs for Model tab
odePrint <- eventReactive(input$runsim, {
  if(!is.null(parCheck())) return()

    modelTextI <- readLines(as.list(runMod())$cfile)
    startODE <- grep("\\$ODE",modelTextI)+1
    dollarPresent <- grep(".*\\$", modelTextI)
    endODE <- dollarPresent[grep(startODE-1, dollarPresent) + 1]-1
    odeBlock <- modelTextI[startODE:endODE]
    noCommentsODEBlock <- odeBlock[grep("^//", odeBlock, invert=T)]

    modelTextOut1 <- ""
    for(i in grep("^dxdt_", noCommentsODEBlock)) {
    modelTextOut1 <- paste0(modelTextOut1, strsplit(noCommentsODEBlock[i], ";")[[1]][1],"<br/>")
    }
    modelTextOut2 <- ""
    for(i in grep("^double", noCommentsODEBlock)) {
    modelTextOut2 <- paste0(modelTextOut2, strsplit(strsplit(noCommentsODEBlock[i], "double")[[1]][2], ";")[[1]][1],"<br/>")
    }

    HTML(paste0(modelTextOut1,"<br/> <h5> Dependent/Output Equations: <h5/>", modelTextOut2,"<br/>"))

  }) #odePrint

## Render ODE
output$odePrintOut <- renderUI({

 odePrint()

}) #odePrintOut



## Derived PK Tables
derivedPKTableAll <- eventReactive(input$runsim, {

if(is.null(runSim())) return()

tau <- isolate(anac(eval(parse(text=input$ii))))
nDoses <- isolate(anac(input$nDoses))
route <- isolate(input$route)
parSweep <- isolate(input$parSweep)

simData <- runSim() %>% as_tibble() %>% filter(TYPE!="Overlay")

my.results <- NULL


if(!c("CENTRAL") %in% unlist(outVar()[2])) {
  return(my.results)
}

if(any(simData$CENTRAL<0)) {
return(my.results)
}

if(input$eventData=="Yes") {
return(my.results)
}

if(simData %>% filter(evid==0) %>% summarize(n()) > 100000) {
return(my.results)
}

my.conc <- PKNCAconc(simData %>% filter(amt>=0) %>%
                       # Minor modification to the time to avoid duplication of timepoints for event and observation records
                       mutate(time=ifelse(amt>0, time+0.00000001, time)) %>%
                       select(amt,CENTRAL,dose,time,ID,TYPE,match(parSweep, names(.))),
                       formula(paste0("CENTRAL~time|TYPE+dose",
                                    ifelse(is.null(parSweep),"+",paste0("+",parSweep,"+")),
                                    "ID")))

my.dose <- suppressWarnings(suppressMessages(
              PKNCAdose(simData %>% filter(amt>0) %>%
                        # Minor modification to the time to avoid duplication of timepoints for event and observation records
                        mutate(time=ifelse(amt>0, time+0.00000001, time)) %>%
                        select(amt,CENTRAL,dose,time,ID,TYPE,match(parSweep, names(.))),
                        formula(paste0("amt~time|TYPE+dose",
                                    ifelse(is.null(parSweep),"+",paste0("+",parSweep,"+")),
                                    "ID")))))

if(nDoses==1) {
my.intervals <- data.frame(start=0,
                           end=tau,
                           cmax=TRUE,
                           tmax=TRUE,
                           aucinf.pred=TRUE,
                           auclast=TRUE)
}

if(nDoses>1) {
my.intervals <- data.frame(start=c(0, tau*(nDoses-1)),
                           end=c(tau-0.000001, tau*nDoses),
                           cmax=c(TRUE, TRUE),
                           tmax=c(TRUE, TRUE),
                           auclast=c(TRUE, TRUE),
                           cav=c(TRUE,TRUE))
}


if(input$eventData=="No") {
  my.data.manual <- PKNCAdata(my.conc, my.dose,
                              intervals=my.intervals)

  my.results <- pk.nca(my.data.manual)

  my.results$result$PPORRES <- round(my.results$result$PPORRES,3)

  # Automatic saving of the files
  write.table(as.data.frame(my.results) %>% as_tibble(row.names=F) %>%
                mutate(Parameter=PPTESTCD, Value=PPORRES, Exclude=exclude) %>%
                select(-PPTESTCD,-PPORRES, -exclude),
              paste0(sessionPath(),"/NCAIndividual-", gsub(":","-", Sys.time()), ".tab"),
              row.names=F, quote=F, sep="\t")

  return(my.results)
}

}) ##derivedPKTableAll


derivedPKTable  <- eventReactive(input$calculateNCA, {

  my.results <- NULL

  if(is.null(derivedPKTableAll())) return(my.results)

  parSweep <- isolate(input$parSweep)

  my.results <- derivedPKTableAll()

  if(is.null(parSweep)) {
  my.summary <- summary(my.results) %>% as_tibble(row.names=F) %>%
    select(type=TYPE, dose, N, start, end, cmax, tmax, auclast, everything())
    }

  if(!is.null(parSweep)) {
  my.summary <- summary(my.results) %>% as_tibble(row.names=F) %>%
                select(type=TYPE, match(parSweep, names(.)), dose, N, start, end, cmax, tmax, auclast, everything())

  }

  # Automatic saving of the files
  write.table(my.summary,
              paste0(sessionPath(),"/NCASummary-", gsub(":","-", Sys.time()), ".tab"),
              row.names=F, quote=F, sep="\t")

  return(my.summary)
}) #derivedPKTable


#derivedPKOutput
output$derivedPKOutput <- renderUI({

  if(value()==1)  return(HTML(""))

  if(length(grep("3",cumValues()))!=1) return(HTML(""))

  if(length(grep("3",cumValues()))==1) {

  ncaErrText <- ""

  footerText <- ""

  if(is.null(derivedPKTable())) {

    footerText <- ""

    if(!c("CENTRAL") %in% unlist(outVar()[2])) {
      ncaErrText <- paste0(tags$p("PK-NCA will not be evaluated when \'CENTRAL\' doesnt exist in the model capture outputs", style="color: red;"))
    }

    if(any(runSim() %>% as_tibble() %>% filter(TYPE!="Overlay", evid==0) %>% select(CENTRAL) < 0)) {
      ncaErrText <- paste0(tags$p("PK-NCA will not be evaluated when simulation results in negative CENTRAL concentrations", style="color: red;"))
    }

    if(input$eventData=="Yes") {
      ncaErrText <- paste0(tags$p("PK-NCA will not be calculated for user provided input data", style="color: red;"))
    }

    if(runSim() %>% as_tibble() %>% filter(TYPE!="Overlay", evid==0) %>%
       summarize(n()) > 100000) {
      ncaErrText <- paste0(tags$p("PK-NCA will not be calculated if total observations exceed 100000", style="color: red;"))
    }
  }

  if(!is.null(derivedPKTable())) {
  footerText <- paste0(tags$p("Note: All NCA parameters except tmax are summarized by the geometric mean [geometric CV].
                      tmax is summarized by median [range].", style="font-size: 14px;"))
  }

  HTML(paste0(tags$blockquote(strong("PK Non-Compartmental Analysis Summary"),
                                style="border: 0px invisible;
                                border-left: 0px solid #eee;
                                padding: 4px;
                                font-size: 16px;
                                background-color: #43b3a5;")),

  ncaErrText,

  paste0(tableOutput("derivedPKOutputTable")),

  footerText
  )
  }
}) #derivedPKOutput


## Render derived PK table
output$derivedPKOutputTable <- renderTable({
    derivedPKTable()
}, include.rownames=FALSE) #derivedPKOutputTable



########## Downloads ###########

## Model text for ouput
modelTextO <- function() {
    if (is.null(runMod()))
    return(NULL)
    # read the original model file
    modelTextI <- readLines(as.list(runMod())$cfile)
    # remove commented lines
    modelTextI <- modelTextI[grep("^//", modelTextI, invert=T)]
    # remove empty lines
    modelTextI <- modelTextI[grep("^[ \t\r\n]*$", modelTextI, invert=T)]

    # Parameters
    newPARAM <- ac(lapply(1:ncol(newPar()), function(i)
                paste0(newPar()[1,][i]," : ", newPar()[2,][i]," : ",newPar()[4,][i], " (",newPar()[3,][i],")")))

    startPARAM <- grep("\\$PARAM",modelTextI)+1
    # Find all occurrence of $
    dollarPresent <- grep(".*\\$", modelTextI)
    endPARAM <- dollarPresent[grep(startPARAM-1, dollarPresent) + 1]-1

    for(i in 1:length(newPARAM)) {
    # Substitute entire line
    modelTextI[startPARAM+i-1] <- sub(".*$",newPARAM[i],modelTextI[startPARAM+i-1])
    }

    ### Omega
    startOMEGA <- grep("\\$OMEGA",modelTextI)+1
    # Find all occurrence of $
    dollarPresent <- grep(".*\\$", modelTextI)
    endOMEGA <- dollarPresent[grep(startOMEGA-1, dollarPresent) + 1]-1

    if(input$randomOptions=="Between Subject Only" || input$randomOptions=="Both") {
    for(i in 1:nrow(omegaVal$data)) {
    # Substitute entire line
    myOmgSub <- paste(omegaVal$data[i,][lower.tri(omegaVal$data, diag=T)[i,]], collapse=" ")
    modelTextI[startOMEGA+i-1] <- sub("\\:(.*?)\\:", paste0(": ", myOmgSub," :"), modelTextI[startOMEGA+i-1])
    }}
    if(input$randomOptions=="None" || input$randomOptions=="Residual Only") {
    for(i in 1:nrow(omegaMat())) {
    # Substitute entire line
    myOmgSub <- paste0(rep(0, i), collapse=" ")
    modelTextI[startOMEGA+i-1] <- sub("\\:(.*?)\\:", paste0(": ", myOmgSub," :"), modelTextI[startOMEGA+i-1])
    }}

    ### Sigma
    startSIGMA <- grep("\\$SIGMA",modelTextI)+1
    # Find all occurrence of $
    dollarPresent <- grep(".*\\$", modelTextI)
    endSIGMA <- dollarPresent[grep(startSIGMA-1, dollarPresent) + 1]-1

    if(input$randomOptions=="Residual Only" || input$randomOptions=="Both") {
    for(i in 1:nrow(sigmaVal$data)) {
    # Substitute entire line
    mySigSub <- paste(sigmaVal$data[i,][lower.tri(sigmaVal$data, diag=T)[i,]], collapse=" ")
    modelTextI[startSIGMA+i-1] <- sub("\\:(.*?)\\:", paste0(": ", mySigSub," :"), modelTextI[startSIGMA+i-1])
    }}
    if(input$randomOptions=="None" || input$randomOptions=="Between Subject Only") {
    for(i in 1:nrow(sigmaMat())) {
    # Substitute entire line
    mySigSub <- paste0(rep(0, i), collapse=" ")
    modelTextI[startSIGMA+i-1] <- sub("\\:(.*?)\\:", paste0(": ", mySigSub," :"), modelTextI[startSIGMA+i-1])
    }}

  return(modelTextI)
} #modelTextO

## Automatically save model file (updated)
observeEvent(input$runsim, {
  fileConn <- paste0(sessionPath(),"/ModelCode-", gsub(":","-", Sys.time()), ".cpp")
  writeLines(modelTextO(), fileConn)
})


## Write model as .md file
writeRmdFile <- function() {
  rmdTextI <- readLines("modelCodeTemplate.rmd")
  index <- grep("\`\`\`", rmdTextI)[1]
  rmdTextI <- c(rmdTextI[1:index],
                modelTextO(),
                rmdTextI[(index + 1): length(rmdTextI)])
  writeLines(rmdTextI, paste0(tempPath(), "/", "modelCode.md"))
}

## Output Model Code
output$modelCode <- renderUI({
  writeRmdFile()
  file <- file.path(tempPath(), "modelCode.md")
  includeMarkdown(file)
})

## Download model
output$dwnModel <- downloadHandler(

  filename = function() {
    paste0("ModViz-Model-", gsub(":","-", Sys.time()),".cpp")
    },
  content = function(file) {

    writeLines(modelTextO(), file)

    }
) #dwnModel

outputOptions(output, "dwnModel", suspendWhenHidden=FALSE)


## Download plot
output$dwnPlot <- downloadHandler(

      filename <- function() {
        paste0("ModViz-SimPlot-",gsub(":","-", Sys.time()),".png")
        },
      content = function(file) {
        png(file, type="cairo-png", height=iHeight(), width=iWidth())
        print(plotSim())
        dev.off()
        }
      ) #dwnPlot

outputOptions(output, "dwnPlot", suspendWhenHidden=FALSE)


# Download simulated data
output$dwnSimData <- downloadHandler(
   filename <- function() {
     paste0("ModViz-SimData-", gsub(":","-", Sys.time()), ".csv")
     },
    content = function(file) {
    write.csv(runSim(),file,row.names=F,quote=F)
    }
  ) #dwnSimData

outputOptions(output, "dwnSimData", suspendWhenHidden=FALSE)


# Download Raw NCA Results
output$dwnNCARaw <- downloadHandler(
  filename <- function() {
    paste0("ModViz-NCAIndividual-", gsub(":","-", Sys.time()), ".csv")
  },
  content = function(file) {
    write.csv(as.data.frame(derivedPKTableAll()) %>% as_tibble(row.names=F) %>%
              mutate(Parameter=PPTESTCD, Value=PPORRES, Exclude=exclude) %>%
              select(-PPTESTCD,-PPORRES, -exclude),
              file, row.names=F, quote=F)
  }
) #dwnNCARaw

outputOptions(output, "dwnNCARaw", suspendWhenHidden=FALSE)


# Download NCA Summary
output$dwnNCASumm <- downloadHandler(
  filename <- function() {
    paste0("ModViz-NCASummary-", gsub(":","-", Sys.time()), ".tab")
  },
  content = function(file) {
    write.table(derivedPKTable(), file, row.names=F, quote=F, sep="\t")
  }
) #dwnNCASumm

outputOptions(output, "dwnNCASumm", suspendWhenHidden=FALSE)

# Download report
output$dwnReport <- downloadHandler(

  filename <- function() {
      paste0("ModViz-PKPDReport-",gsub(":","-", Sys.time()), ".pdf")
    },
  content = function(file) {

  file.copy(paste0("report.tex"),
            tempPath(), overwrite = TRUE)

  # Load report template:
  reportArray <- readLines(paste0(tempPath(),"/report.tex"))

  # Intro
  index <- grep("%%INTRO%%", reportArray)

  if(input$sources=="USER") {

  fileText <- input$userModelFile[1]
  fileText <- gsub("_", "-", fileText)

  intro <- paste0("\t\tThis report summarizes the simulation input parameters, outputs and associated model code
                    of the user defined model (",fileText,"). Section 2 contains user specified simulation input parameters.
                    Section 3 contains simulated outputs. Please refer to section 4 for more details on the model
                    and associated code.")
  }

  if(input$sources=="PROJ") {
  fileName <- input$inFile[1]
  fileText <- paste0(ac(as.data.frame(fileName)[,2]),"/",
                     ac(as.data.frame(fileName)[,3]))
  fileText <- gsub("_", "-", fileText)
  intro <- paste0("\t\tThis report summarizes the simulation input parameters, outputs and associated model code
                    of a repository project model (",fileText,"). Section 2 contains user specified simulation input parameters.
                    Section 3 contains simulated outputs. Please refer to section 4 for more details on the model
                    and associated code.")
  }

  if(!input$sources %in% c("USER","PROJ")) {

  pRoute <- ifelse(input$route=="ABS","first order absorption","intravenous")

  pComp <- ifelse(input$cmpt=="1CMT","one","two")

  pBol <- ifelse(input$route=="IVI", "infusion",
            ifelse(input$route=="IVB","bolus",
              ifelse(input$route=="ABS","","")))

  pPKorPKPD <-  ifelse(input$PDmodel=="None","PK","PK/PD")

  pPD <- ifelse(input$PDmodel %in% c("None"), paste0(""),
          ifelse(input$PDmodel %in% c("EMAX"), paste0(", where the Emax PD model describes the direct link between plasma drug concentration and response at the site of action using a simple Emax or a sigmoid Emax relationship"),
            ifelse(input$PDmodel %in% c("EFF"), paste0(", where the effect compartment PD model involves a hypothetical effect compartment (Ceffect) which acts as a link between the plasma drug concentration and pharmacodynamic response"),
              ifelse(input$PDmodel %in% c("IRM1"), paste0(", where the indirect response (Type I) PD model describes drug response resulting from inhibition of the factors regulating the production of the response variable (Kin), where inhibition processes operate according to the inhibition function characterized by a sigmoid function"),
                ifelse(input$PDmodel %in% c("IRM2"), paste0(", where the indirect response (Type II) PD model describes drug response resulting from inhibition of the factors governing the dissipation of response variable (kout), where inhibition processes operate according to the inhibition function characterized by a sigmoid function"),
                  ifelse(input$PDmodel %in% c("IRM3"), paste0(", where the indirect response (Type III) PD model describes drug response resulting from stimulation of the factors regulating the production of the response variable (kin), where stimulation processes operate according to the stimulation function characterized by a sigmoid function"),
                    ifelse(input$PDmodel %in% c("IRM4"), paste0(", where the indirect response (Type IV) PD model describes drug response resulting from stimulation of the factors controlling the dissipation of the response variable (kout), where stimulation processes operate according to the stimulation function characterized by a sigmoid function","")
          )))))))



  intro <- paste0("\t\tThis report summarizes the simulation input parameters, outputs and associated model code of a ",
                   paste0(pComp), " compartment ", paste0(pRoute," "),
                   paste0(pBol), paste0(" ",pPKorPKPD), " model", paste0(pPD),
                   ".\n",
                   "Section 2 contains user specified simulation input parameters.
                   Section 3 contains simulated outputs.
                   Please refer to section 4 for more details on the model and associated code.")
  }

  reportArray <- c(reportArray[1:(index - 1)],
                   intro,
                   reportArray[(index + 1): length(reportArray)])

  ###Insert tables:

  # Input Dosing Table
  dosingTableList <- list(list(table = dosingTable(),
                         caption = "Dosing Scheme"))

  dosingTempList <- lapply(dosingTableList, function(x) {table4Latex(x)})

  index <- grep("%%DOSING%%", reportArray)
  reportArray <- c(reportArray[1:(index - 1)],
                   do.call(c, dosingTempList),
                   reportArray[(index + 1): length(reportArray)])


 # Fixed Effect Parameters
  paramTableList <- list(list(table = paramTable(),
                             caption = "Input PK/PD Parameters"))

  paramTempList <- lapply(paramTableList, function(x) {table4Latex(x)})

  index <- grep("%%FIXED_INPUT%%", reportArray)
  reportArray <- c(reportArray[1:(index - 1)],
                   do.call(c, paramTempList),
                   reportArray[(index + 1): length(reportArray)])

  # Random Effect Parameters

  omgLength <- length(as.list(selectedModel())$random$omega_labels[[1]])
  if(omgLength<=11) {
  randomTableList <- list(list(table = randomTable()[[1]],
                               caption = "Between Subject Variability (Covariance Matrix)"),
                          list(table =  randomTable()[[2]],
                               caption = "Residual Variability (Covariance Matrix)"))

  if(input$advOptions=="Set As Reference") {
  randomTableList <- list(list(table = randomTable()[[1]],
                               caption = "Between Subject Variability (Covariance Matrix)"),
                          list(table =  randomTable()[[2]],
                               caption = "Residual Variability (Covariance Matrix)"),
                          list(table = randomTable()[[3]],
                              caption = "Between Subject Variability (Covariance Matrix) - Reference"),
                          list(table = randomTable()[[4]],
                              caption = "Residual Variability (Covariance Matrix) - Reference")
                         )}
  }

  if(omgLength>11) {
   randomTableList <- list(list(table = data.frame("Random Parameters Table Too Big To Display" = NA),
                                caption = "Between Subject Variability (Covariance Matrix)"),
                            list(table =  randomTable()[[2]],
                                 caption = "Residual Variability (Covariance Matrix)"))


   if(input$advOptions=="Set As Reference") {
     randomTableList <- list(list(table = data.frame("Random Parameters Table Too Big To Display" = NA),
                                  caption = "Between Subject Variability (Covariance Matrix)"),
                             list(table =  randomTable()[[2]],
                                  caption = "Residual Variability (Covariance Matrix)"),
                             list(table = data.frame("Random Parameters Table Too Big To Display" = NA),
                                  caption = "Between Subject Variability (Covariance Matrix) - Reference"),
                             list(table = randomTable()[[4]],
                                  caption = "Residual Variability (Covariance Matrix) - Reference")
     )}
  }

  randomTempList <- lapply(randomTableList, function(x) {scaled75RTable4Latex(x)})

  index <- grep("%%RANDOM_INPUT%%", reportArray)
  reportArray <- c(reportArray[1:(index - 1)],
                   do.call(c, randomTempList),
                   reportArray[(index + 1): length(reportArray)])

  # Simulation Options Table
  simOptionsTableList <- list(list(table = simOptionsTable(),
                               caption = "Simulation Options"))

  simOptionsTempList <- lapply(simOptionsTableList, function(x) {table4Latex(x)})

  index <- grep("%%SIMOPTIONS%%", reportArray)
  reportArray <- c(reportArray[1:(index - 1)],
                   do.call(c, simOptionsTempList),
                   reportArray[(index + 1): length(reportArray)])

  # Insert plots:
  pdf(paste0(tempPath(),"/","simplot.pdf"), height=(iHeight()/72), width=(iWidth()/72))
  print(plotSim())
  dev.off()

  reportArray <- sub('%%SIMULATION_PLOT%%', paste0(tempPath(),"/","simplot.pdf"), reportArray)

  # Insert Secondary PK
  if(!is.null(derivedPKTable())) {

  secondaryPKTableList <- list(list(table =  derivedPKTable(),
                                    caption = "PK Non-Compartmental Analysis Summary"))

  secondaryPKTempList <- lapply(secondaryPKTableList, function(x) {table4Latex(x)})
  }

  if(is.null(derivedPKTable())) {
  secondaryPKTableList <- list(list(table = data.frame("PK-NCA is not evaluated" = NA),
                                    caption = "PK Non-Compartmental Analysis Summary"))

  secondaryPKTempList <- lapply(secondaryPKTableList, function(x) {table4Latex(x)})
  }

  index <- grep("%%SECONDARYPK%%", reportArray)
  reportArray <- c(reportArray[1:(index - 1)],
                   do.call(c, secondaryPKTempList),
                   reportArray[(index + 1): length(reportArray)])



  ## Insert ODE Section
  # Insert Intial conditions
  index <- grep("%%INIT_COND%%", reportArray)

  initTableList <- list(list(table = initTable(),
                        caption = "Initial Conditions"))

  initTempList <- lapply(initTableList, function(x) {table4Latex(x)})

  reportArray <- c(reportArray[1:(index - 1)],
                   do.call(c, initTempList),
                   reportArray[(index + 1): length(reportArray)])


  # Insert Equations
  modelTextTemp <- modelTextO()
  startODE <- grep("\\$ODE",modelTextTemp)+1
  dollarPresent <- grep(".*\\$", modelTextTemp)
  endODE <- dollarPresent[grep(startODE-1, dollarPresent) + 1]-1
  odeBlock <- modelTextTemp[startODE:endODE]
  noCommentsODEBlock <- odeBlock[grep("^//", odeBlock, invert=T)]

  modelTextOut1 <- ""
  for(i in grep("^dxdt_", noCommentsODEBlock)) {
  modelTextOut1 <- paste0(modelTextOut1, strsplit(noCommentsODEBlock[i], ";")[[1]][1], "\n")
  }
  modelTextOut2 <- ""
  for(i in grep("^double", noCommentsODEBlock)) {
  modelTextOut2 <- paste0(modelTextOut2, strsplit(strsplit(noCommentsODEBlock[i], "double ")[[1]][2], ";")[[1]][1], "\n")
  }

  index <- grep("%%ODE1%%", reportArray)
  reportArray <- c(reportArray[1:(index - 1)],
                   modelTextOut1,
                   reportArray[(index + 1): length(reportArray)])


  index <- grep("%%ODE2%%", reportArray)
  reportArray <- c(reportArray[1:(index - 1)],
                   modelTextOut2,
                   reportArray[(index + 1): length(reportArray)])

  # Insert Model Code
  index <- grep("%%CODE%%", reportArray)

  reportArray <- c(reportArray[1:(index - 1)],
                   modelTextO(),
                   reportArray[(index + 1): length(reportArray)])

  # Construct the LaTeX document:
  texFile <- paste0(tempPath(),"/","reportO")
  writeLines(reportArray, paste0(texFile, ".tex"))

  print(file=stderr(), tempPath())
  system(paste0("pdflatex ", paste0("-output-directory ./", tempPath(), " "), texFile))
  system(paste0("pdflatex ", paste0("-output-directory ./", tempPath(), " "), texFile))

  file.copy(paste0(texFile, ".pdf"), file)

}) #dwnReport

outputOptions(output, "dwnReport", suspendWhenHidden=FALSE)

# Download session
output$dwnSession <- downloadHandler(
  filename = function() {
    paste0("ModViz-SessionDownload-", gsub(":","-", Sys.time()), ".zip")
  },
  content = function(fname) {
    setwd(sessionPath())
    zipFiles <- dir(full.names = TRUE)
    if(length(list.files(all.files = TRUE, include.dirs = FALSE, no.. = TRUE))!=0) {
      zip(zipfile=fname, files=zipFiles)
    }
    if(length(list.files(all.files = TRUE, include.dirs = FALSE, no.. = TRUE))==0) {
      return(NULL)
    }
    setwd("../../../")
  },
  contentType = "application/zip"
)


# This code will be run after the user session is disconnected
session$onSessionEnded(function() {
  unlink(paste0("scratch/",UID), recursive=TRUE)
})

}) #shinyServer




