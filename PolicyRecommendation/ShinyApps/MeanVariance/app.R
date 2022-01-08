#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(googleAuthR)
library(googledrive)
library(googlesheets4)
library(shinyjs)
library(lubridate)
library(rstanarm)
library(mlogit)
library(Hmisc)
library(survival)
library(DoE.base)
library(DoE.wrapper)
library(Metrics)
library(mgcv)
library(support.CEs)
library(mlmRev)
library(lme4)
library(rlist)
library(Matrix)
library(matrixStats)
library(pROC)


withBusyIndicatorCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
"

withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Decarbonization Policy Group Negotiation"),
  dashboardSidebar(
    sidebarMenu(
      )
      
    ),
  
  dashboardBody(
    useShinyjs(),
    h2("Timer"),
    tags$br(),
    fluidRow(
      box(width = 12,
          actionButton('start','Start', width = 120),
          actionButton('stop','Stop', width = 120),
          actionButton('reset','Reset', width = 120),
          numericInput('seconds','Seconds:',value=120,min=0,max=99999,step=1, width = 120),
          textOutput('timeleft'),
          tags$head(tags$style("#timeleft{font-size: 25px;
                               color: DodgerBlue;}")))
    ),
    h2("Policy Alternatives"),
    tags$br(),
    textOutput("counterText"),
    tags$head(tags$style("#counterText{font-size: 25px;
                               }")),
    tags$br(),
    fluidRow(
      tabBox(title = "Policy Alternatives", width = 12, 
             tabPanel(title = "Stage 1 Votes",      
                      tableOutput("RecommendationGroup1")),
             tabPanel(title = "Validation",
                      tableOutput("validationQuestion")))
    ),
    tags$br(),
    actionButton("updateAlternatives", "Refresh", width = 120)
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  options(gargle_oauth_cache= ".secrets")
  gargle::gargle_oauth_cache()
  
  options(
    gargle_oauth_cache = ".secrets",
    gargle_oauth_email = "nilesguo@gmail.com"
  )
  
  drive_auth()
  drive_auth(email = "nilesguo@gmail.com")
  sheets_auth(token = drive_token())
  
  left.alternatives <- as_sheets_id("https://docs.google.com/spreadsheets/d/1oeiXzpyQYqc6AFSn_pjs4RAgvfq1YFwhMcyes1bItto/edit#gid=0")
  right.alternatives <- as_sheets_id("https://docs.google.com/spreadsheets/d/1Etda1ouNUfmLMFTGMBHDgj6fMImDKbHDs7jn0TaEiRM/edit#gid=0")
  
  voting <- as_sheets_id("https://docs.google.com/spreadsheets/d/1Gsr7xmv5PCbvKY7_vTRir6XBJKI4ME7VlAQ5YEVJv7Q/edit#gid=0")
  final.recommendation <- as_sheets_id("https://docs.google.com/spreadsheets/d/1QlypWKlVGIHDKg8BSBAuwOSKA6QpmaEvMW_tBzC-keE/edit#gid=0")
  predictedprob <- as_sheets_id("https://docs.google.com/spreadsheets/d/1M0YFUoZzQcgF_wbsFDxK3JMoCcN7jotyuu5-1RJQE6s/edit#gid=0")


  
  create.col.nuc <- function(df, n){
    varname <- paste0("nuc.power0", n)
    mutate(df, !!varname := ifelse(nuc.power0 == n, 1,0))
  }
  
  create.col.ban <- function(df, n){
    varname <- paste0("ban.fuel.explore0", n)
    mutate(df, !!varname := ifelse(ban.fuel.explore0 == n, 1,0))
  }
  
  create.col.RFS <- function(df, n){
    varname <- paste0("RFS0", n)
    mutate(df, !!varname := ifelse(RFS0 == n, 1,0))
  }
  
  create.col.nuc1 <- function(df, n){
    varname <- paste0("nuc.power1", n)
    mutate(df, !!varname := ifelse(nuc.power1 == n, 1,0))
  }
  
  create.col.ban1 <- function(df, n){
    varname <- paste0("ban.fuel.explore1", n)
    mutate(df, !!varname := ifelse(ban.fuel.explore1 == n, 1,0))
  }
  
  create.col.RFS1 <- function(df, n){
    varname <- paste0("RFS1", n)
    mutate(df, !!varname := ifelse(RFS1 == n, 1,0))
  }
  
  alt1.test <- fac.design(factor.names = list(nuc.power =c("Keep current nuclear power plants but do not build any new plants.", 
                                                           "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 
                                                           "Shut down nuclear power plants that are not making money, reducing nuclear power by 14-29% than today. Do not build nuclear plants in the next eight years."),
                                              CO2Price = c("0", "30", "60", "90", "120", "150"),
                                              ban.fuel.explore = c("Unregulated access to federal lands and waters for fossil fuel exploration.", 
                                                                   "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 
                                                                   "Fully ban fossil fuel exploration on public lands."),
                                              RFS = c("Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 
                                                      "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 
                                                      "Starting from 2021, reach 100% clean energy by 2035 with a yearly increase of 7.5% of clean energy in the grid.")))
  
  
  alt.left.num.test <- alt1.test%>%
    mutate(nuc.power = ifelse(nuc.power == "Keep current nuclear power plants but do not build any new plants.", 0, ifelse(
      nuc.power == "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 1, 2)
    ))%>%
    mutate(CO2Price = ifelse(CO2Price == "0", 0, ifelse(
      CO2Price == "30", 1, ifelse(
        CO2Price == "60", 2, ifelse(
          CO2Price =="90", 3, ifelse(
            CO2Price == "120", 4, 5
          )
        )
      ) 
    )))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Unregulated access to federal lands and waters for fossil fuel exploration.", 0, ifelse(
      ban.fuel.explore == "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 1, 2
    )))%>%
    mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 0, ifelse(
      RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 1, 2)))
  
  alt.right.num.test <- alt.left.num.test%>%
    mutate(nuc.power = (nuc.power + 1)%%3)%>%
    mutate(ban.fuel.explore = (ban.fuel.explore + 1)%%3)%>%
    mutate(RFS = (RFS + 1)%%3)%>%
    mutate(CO2Price = (CO2Price + 1)%%6)%>%
    as.data.frame()%>%
    dplyr::select(nuc.power, CO2Price, ban.fuel.explore, RFS)
  
  alt.left.txt <- alt.left.num.test%>%
    mutate(CO2Price = ifelse(CO2Price == "0", 0, ifelse(
      CO2Price == "1", 30, ifelse(
        CO2Price == "2", 60, ifelse(
          CO2Price =="3", 90, ifelse(
            CO2Price =="4", 120, 150
          )
        )
      )
    )))%>%
    mutate(nuc.power0 = nuc.power)%>%
    mutate(CO2Price0 = CO2Price)%>%
    mutate(ban.fuel.explore0 = ban.fuel.explore)%>%
    mutate(RFS0 = RFS)%>%
    as.data.frame()%>%
    dplyr::select(nuc.power0, CO2Price0, ban.fuel.explore0, RFS0)
  
  
  alt.right.txt <- alt.right.num.test%>%
    mutate(CO2Price = ifelse(CO2Price == "0", 0, ifelse(
      CO2Price == "1", 30, ifelse(
        CO2Price == "2", 60, ifelse(
          CO2Price =="3", 90, ifelse(
            CO2Price =="4", 120, 150
          )
        )
      )
    )))%>%
    mutate(nuc.power1 = nuc.power)%>%
    mutate(CO2Price1 = CO2Price)%>%
    mutate(ban.fuel.explore1 = ban.fuel.explore)%>%
    mutate(RFS1 = RFS)%>%
    as.data.frame()%>%
    dplyr::select(nuc.power1, CO2Price1, ban.fuel.explore1, RFS1)
  
  total.design.test <- cbind(alt.left.txt, alt.right.txt)
  
  total.design.test.1 <- total.design.test%>%
    create.col.nuc(1)%>%
    create.col.nuc(2)%>%
    create.col.ban(1)%>%
    create.col.ban(2)%>%
    create.col.RFS(1)%>%
    create.col.RFS(2)%>%
    create.col.nuc1(1)%>%
    create.col.nuc1(2)%>%
    create.col.ban1(1)%>%
    create.col.ban1(2)%>%
    create.col.RFS1(1)%>%
    create.col.RFS1(2)%>%
    dplyr::select(nuc.power01, nuc.power02, CO2Price0, ban.fuel.explore01, ban.fuel.explore02, RFS01, RFS02, nuc.power11, nuc.power12, CO2Price1, ban.fuel.explore11, ban.fuel.explore12, RFS11, RFS12)
  
  ut.matrix <- total.design.test.1%>%
    dplyr::select(nuc.power01, nuc.power02, CO2Price0, ban.fuel.explore01, ban.fuel.explore02, RFS01, RFS02)%>%
    mutate(id = rep(1:nrow(total.design.test.1)))
  
  
  
  colnames(ut.matrix) <- c("nuc.power1", "nuc.power2", "CO2Price", "ban.fuel.explore1", "ban.fuel.explore2", "RFS1", "RFS2", "id")
  
  utility.matrix <- as.matrix(total.design.test.1[,1:7] - total.design.test.1[,8:14])
  
  colnames(utility.matrix) <- c("nuc.power1", "nuc.power2", "CO2Price", "ban.fuel.explore1", "ban.fuel.explore2", "RFS1", "RFS2")
  
  utility.calculation <- function(ut.matrix, b1, b2, b3, b4, b5, b6, b7, b8){
    
    total.design.ut <- as.data.frame(ut.matrix)%>%
      mutate(id = rep(1:nrow(ut.matrix)))%>%
      mutate(utility = b2*nuc.power1 + b3*nuc.power2 + b4*CO2Price + b5*ban.fuel.explore1 + b6*ban.fuel.explore2 + b7*RFS1 + b8*RFS2 + b1)
    
    return(total.design.ut)
    
  }
  
  constGLM1multiple <- function(X, y, Xtest){
    # constraints
    # Ubar: 0.999 < theta1 < 1.001
    # SD: -1/sqrt(2) < theta2 < 1/sqrt(2)
    ui <- matrix(c(1, 0, -1, 0, 0, 1, 0, -1), nrow = 4, ncol = 2, byrow = TRUE)
    ci <- c(0.999, -1.001, -1/sqrt(10), -1/sqrt(10))
    
    # constrained log-likelihood
    CLL <- function(par, X, y){
      eta <- X %*% par
      p <- exp(eta)/(1 + exp(eta))
      ll <- sum(y*log(p) + (1-y)*log(1-p))
      return(-ll)
    }
    
    # Optimize
    optims <- constrOptim(c(1, 0), CLL, ui = ui, ci = ci, X = X, y = y, grad = NULL)
    pars <- optims$par
    print(pars)
    
    # Return predicted probabilities
    etatest <- Xtest %*% pars
    output.list = list(pars, exp(etatest)/(1+exp(etatest)), etatest)
    return(output.list)
  }
  
  result.ids <- read_sheet(voting, sheet = 1, range = cell_rows(1), col_names = FALSE)
  
  result1 <- drive_get(as.character(result.ids[1]))%>%
    as_sheets_id()
  
  leftalt1 <- sheets_read(result1, sheet = 2)%>%
    mutate(nuc.power = ifelse(nuc.power == "Keep current nuclear power plants but do not build any new plants.", 0, ifelse(
      nuc.power == "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 1, 2)
    ))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Unregulated access to federal lands and waters for fossil fuel exploration.", 0, ifelse(
      ban.fuel.explore == "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 1, 2
    )))%>%
    mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 0, ifelse(
      RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 1, 2)))
  
  leftalt.1 <- leftalt1%>%
    mutate(nuc.power_0 = nuc.power)%>%
    mutate(CO2Price_0 = CO2Price) %>%
    mutate(ban.fuel.explore_0 = ban.fuel.explore)%>%
    mutate(RFS_0 = RFS)
  
  left1 <- leftalt.1[5:8]
  
  left1 <- left1%>%
    mutate(nuc.power_0 = as.factor(nuc.power_0))%>%
    mutate(ban.fuel.explore_0 = as.factor(ban.fuel.explore_0))%>%
    mutate(RFS_0 = as.factor(RFS_0))%>%
    mutate(CO2Price_0 = as.numeric(CO2Price_0))
  
  
  rightalt1 <- sheets_read(result1, sheet = 3)%>%
    mutate(nuc.power = ifelse(nuc.power == "Keep current nuclear power plants but do not build any new plants.", 0, ifelse(
      nuc.power == "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 1, 2)
    ))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Unregulated access to federal lands and waters for fossil fuel exploration.", 0, ifelse(
      ban.fuel.explore == "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 1, 2
    )))%>%
    mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 0, ifelse(
      RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 1, 2)))
  
  rightalt.1 <- rightalt1%>%
    mutate(nuc.power_1 = nuc.power)%>%
    mutate(CO2Price_1 = CO2Price)%>%
    mutate(ban.fuel.explore_1 = ban.fuel.explore)%>%
    mutate(RFS_1 = RFS)
  
  right1 <- rightalt.1[5:8]
  
  right1 <- right1%>%
    mutate(nuc.power_1 = as.factor(nuc.power_1))%>%
    mutate(ban.fuel.explore_1 = as.factor(ban.fuel.explore_1))%>%
    mutate(RFS_1 = as.factor(RFS_1))%>%
    mutate(CO2Price_1 = as.numeric(CO2Price_1))
  
  answers1 <- sheets_read(result1, sheet =4, col_names = FALSE)
  
  names(answers1) <- c("Choice")
  
  data.1 <- cbind(answers1, left1, right1)
  
  id.1 <- rep(1,length(answers1))
  
  dataset.1 <- cbind(id.1, data.1)
  
  dataset.ready1 <- mlogit.data(dataset.1, shape = "wide", choice = "Choice", varying = c(3:10), sep = "_")
  
  ut1 <- glm(Choice ~ nuc.power + (CO2Price) + ban.fuel.explore + RFS, data = dataset.ready1, family = binomial(link = "logit"))
  
  p1.utility <- utility.calculation(ut.matrix, ut1$coefficients[1], ut1$coefficients[2], ut1$coefficients[3], ut1$coefficients[4], ut1$coefficients[5],
                                    ut1$coefficients[6], ut1$coefficients[7], ut1$coefficients[8])%>%
    dplyr::select(id, utility)
  
  result2 <- drive_get(as.character(result.ids[2]))%>%
    as_sheets_id()
  
  leftalt2 <- sheets_read(result2, sheet = 2)%>%
    mutate(nuc.power = ifelse(nuc.power == "Keep current nuclear power plants but do not build any new plants.", 0, ifelse(
      nuc.power == "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 1, 2)
    ))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Unregulated access to federal lands and waters for fossil fuel exploration.", 0, ifelse(
      ban.fuel.explore == "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 1, 2
    )))%>%
    mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 0, ifelse(
      RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 1, 2)))
  
  leftalt.2 <- leftalt2%>%
    mutate(nuc.power_0 = nuc.power)%>%
    mutate(CO2Price_0 = CO2Price) %>%
    mutate(ban.fuel.explore_0 = ban.fuel.explore)%>%
    mutate(RFS_0 = RFS)
  
  left2 <- leftalt.2[5:8]
  
  left2 <- left2%>%
    mutate(nuc.power_0 = as.factor(nuc.power_0))%>%
    mutate(ban.fuel.explore_0 = as.factor(ban.fuel.explore_0))%>%
    mutate(RFS_0 = as.factor(RFS_0))%>%
    mutate(CO2Price_0 = as.numeric(CO2Price_0))
  
  
  
  rightalt2 <- sheets_read(result2, sheet = 3)%>%
    mutate(nuc.power = ifelse(nuc.power == "Keep current nuclear power plants but do not build any new plants.", 0, ifelse(
      nuc.power == "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 1, 2)
    ))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Unregulated access to federal lands and waters for fossil fuel exploration.", 0, ifelse(
      ban.fuel.explore == "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 1, 2
    )))%>%
    mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 0, ifelse(
      RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 1, 2)))
  
  rightalt.2 <- rightalt2%>%
    mutate(nuc.power_1 = nuc.power)%>%
    mutate(CO2Price_1 = CO2Price)%>%
    mutate(ban.fuel.explore_1 = ban.fuel.explore)%>%
    mutate(RFS_1 = RFS)
  
  right2 <- rightalt.2[5:8]
  
  right2 <- right2%>%
    mutate(nuc.power_1 = as.factor(nuc.power_1))%>%
    mutate(ban.fuel.explore_1 = as.factor(ban.fuel.explore_1))%>%
    mutate(RFS_1 = as.factor(RFS_1))%>%
    mutate(CO2Price_1 = as.numeric(CO2Price_1))
  
  
  answers2 <- sheets_read(result2, sheet =4, col_names = FALSE)
  
  names(answers2) <- c("Choice")
  
  data.2 <- cbind(answers2, left2, right2)
  
  id.2 <- rep(1, length(answers2))
  
  dataset.2 <- cbind(id.2, data.2)
  
  dataset.ready2 <- mlogit.data(dataset.2, shape = "wide", choice = "Choice", varying = c(3:10), sep = "_")
  
  ut2 <- glm(Choice ~ nuc.power + (CO2Price) + ban.fuel.explore + RFS, data = dataset.ready2, family = binomial(link = "logit"))
  
  p2.utility <- utility.calculation(ut.matrix, ut2$coefficients[1], ut2$coefficients[2], ut2$coefficients[3], ut2$coefficients[4], ut2$coefficients[5],
                                    ut2$coefficients[6], ut2$coefficients[7], ut2$coefficients[8])%>%
    dplyr::select(id, utility)
  
  result3 <- drive_get(as.character(result.ids[3]))%>%
    as_sheets_id()
  
  leftalt3 <- sheets_read(result3, sheet = 2)%>%
    mutate(nuc.power = ifelse(nuc.power == "Keep current nuclear power plants but do not build any new plants.", 0, ifelse(
      nuc.power == "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 1, 2)
    ))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Unregulated access to federal lands and waters for fossil fuel exploration.", 0, ifelse(
      ban.fuel.explore == "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 1, 2
    )))%>%
    mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 0, ifelse(
      RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 1, 2)))
  
  leftalt.3 <- leftalt3%>%
    mutate(nuc.power_0 = nuc.power)%>%
    mutate(CO2Price_0 = CO2Price) %>%
    mutate(ban.fuel.explore_0 = ban.fuel.explore)%>%
    mutate(RFS_0 = RFS)
  
  left3 <- leftalt.3[5:8]
  
  left3 <- left3%>%
    mutate(nuc.power_0 = as.factor(nuc.power_0))%>%
    mutate(ban.fuel.explore_0 = as.factor(ban.fuel.explore_0))%>%
    mutate(RFS_0 = as.factor(RFS_0))%>%
    mutate(CO2Price_0 = as.numeric(CO2Price_0))
  
  
  
  rightalt3 <- sheets_read(result3, sheet = 3)%>%
    mutate(nuc.power = ifelse(nuc.power == "Keep current nuclear power plants but do not build any new plants.", 0, ifelse(
      nuc.power == "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 1, 2)
    ))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Unregulated access to federal lands and waters for fossil fuel exploration.", 0, ifelse(
      ban.fuel.explore == "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 1, 2
    )))%>%
    mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 0, ifelse(
      RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 1, 2)))
  
  rightalt.3 <- rightalt3%>%
    mutate(nuc.power_1 = nuc.power)%>%
    mutate(CO2Price_1 = CO2Price)%>%
    mutate(ban.fuel.explore_1 = ban.fuel.explore)%>%
    mutate(RFS_1 = RFS)
  
  right3 <- rightalt.3[5:8]
  
  right3 <- right3%>%
    mutate(nuc.power_1 = as.factor(nuc.power_1))%>%
    mutate(ban.fuel.explore_1 = as.factor(ban.fuel.explore_1))%>%
    mutate(RFS_1 = as.factor(RFS_1))%>%
    mutate(CO2Price_1 = as.numeric(CO2Price_1))
  
  
  answers3 <- sheets_read(result3, sheet =4, col_names = FALSE)
  
  names(answers3) <- c("Choice")
  
  data.3 <- cbind(answers3, left3, right3)
  
  id.3 <- rep(1, length(answers3))
  
  dataset.3 <- cbind(id.3, data.3)
  
  dataset.ready3 <- mlogit.data(dataset.3, shape = "wide", choice = "Choice", varying = c(3:10), sep = "_")
  
  ut3 <- glm(Choice ~ nuc.power + (CO2Price) + ban.fuel.explore + RFS, data = dataset.ready3, family = binomial(link = "logit"))
  
  p3.utility <- utility.calculation(ut.matrix, ut3$coefficients[1], ut3$coefficients[2], ut3$coefficients[3], ut3$coefficients[4], ut3$coefficients[5],
                                    ut3$coefficients[6], ut3$coefficients[7], ut3$coefficients[8])%>%
    dplyr::select(id, utility)
  
  result4 <- drive_get(as.character(result.ids[4]))%>%
    as_sheets_id()
  
  leftalt4 <- sheets_read(result4, sheet = 2)%>%
    mutate(nuc.power = ifelse(nuc.power == "Keep current nuclear power plants but do not build any new plants.", 0, ifelse(
      nuc.power == "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 1, 2)
    ))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Unregulated access to federal lands and waters for fossil fuel exploration.", 0, ifelse(
      ban.fuel.explore == "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 1, 2
    )))%>%
    mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 0, ifelse(
      RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 1, 2)))
  
  leftalt.4 <- leftalt4%>%
    mutate(nuc.power_0 = nuc.power)%>%
    mutate(CO2Price_0 = CO2Price) %>%
    mutate(ban.fuel.explore_0 = ban.fuel.explore)%>%
    mutate(RFS_0 = RFS)
  
  left4 <- leftalt.4[5:8]
  
  left4 <- left4%>%
    mutate(nuc.power_0 = as.factor(nuc.power_0))%>%
    mutate(ban.fuel.explore_0 = as.factor(ban.fuel.explore_0))%>%
    mutate(RFS_0 = as.factor(RFS_0))%>%
    mutate(CO2Price_0 = as.numeric(CO2Price_0))
  
  
  
  rightalt4 <- sheets_read(result4, sheet = 3)%>%
    mutate(nuc.power = ifelse(nuc.power == "Keep current nuclear power plants but do not build any new plants.", 0, ifelse(
      nuc.power == "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.", 1, 2)
    ))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "Unregulated access to federal lands and waters for fossil fuel exploration.", 0, ifelse(
      ban.fuel.explore == "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.", 1, 2
    )))%>%
    mutate(RFS = ifelse(RFS == "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", 0, ifelse(
      RFS == "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", 1, 2)))
  
  rightalt.4 <- rightalt4%>%
    mutate(nuc.power_1 = nuc.power)%>%
    mutate(CO2Price_1 = CO2Price)%>%
    mutate(ban.fuel.explore_1 = ban.fuel.explore)%>%
    mutate(RFS_1 = RFS)
  
  right4 <- rightalt.4[5:8]
  
  right4 <- right4%>%
    mutate(nuc.power_1 = as.factor(nuc.power_1))%>%
    mutate(ban.fuel.explore_1 = as.factor(ban.fuel.explore_1))%>%
    mutate(RFS_1 = as.factor(RFS_1))%>%
    mutate(CO2Price_1 = as.numeric(CO2Price_1))
  
  
  answers4 <- sheets_read(result4, sheet =4, col_names = FALSE)
  
  names(answers4) <- c("Choice")
  
  data.4 <- cbind(answers4, left4, right4)
  
  id.4 <- rep(1, length(answers4))
  
  dataset.4 <- cbind(id.4, data.4)
  
  dataset.ready4 <- mlogit.data(dataset.4, shape = "wide", choice = "Choice", varying = c(3:10), sep = "_")
  
  ut4 <- glm(Choice ~ nuc.power + (CO2Price) + ban.fuel.explore + RFS, data = dataset.ready4, family = binomial(link = "logit"))
  
  p4.utility <- utility.calculation(ut.matrix, ut4$coefficients[1], ut4$coefficients[2], ut4$coefficients[3], ut4$coefficients[4], ut4$coefficients[5],
                                    ut4$coefficients[6], ut4$coefficients[7], ut4$coefficients[8])%>%
    dplyr::select(id, utility)
  
  group.utility <- inner_join(ut.matrix, p1.utility, by = "id")%>%
    inner_join(p2.utility, by = "id")%>%
    inner_join(p3.utility, by ="id")%>%
    inner_join(p4.utility, by = "id")
  
  group.names <- c("nuc.power1", "nuc.power2", "CO2Price", "ban.fuel.explore1", "ban.fuel.explore2", "RFS1", "RFS2", "id", 
                   "p1.utility", "p2.utility", "p3.utility", "p4.utility")
  
  names(group.utility) <- group.names
  
  group.score <- rowMeans(group.utility[9:12])
  group.sd <- rowSds(as.matrix(group.utility[9:12]))
  group.utility <- cbind(group.utility, group.score, group.sd)%>%
    arrange(desc(group.score))
  
  
  optimal.design <- Dopt.design(15, data = group.utility, 
                                formula = ~group.score + group.sd)
  
  design.left <- optimal.design%>%
    dplyr::mutate(CO2Price = ifelse(CO2Price == 30, 1, ifelse(CO2Price == 60, 2,ifelse(CO2Price == 90, 3,
                                                                                       ifelse(CO2Price == 120, 4, ifelse(CO2Price == 150, 5, 0))))))%>%
    dplyr::select(1:8)%>%
    dplyr::mutate(nuc.power = ifelse(nuc.power1 == 1, 1, ifelse(nuc.power2 ==1, 2, 0)))%>%
    dplyr::mutate(ban.fuel.explore = ifelse(ban.fuel.explore1 == 1, 1, ifelse(ban.fuel.explore2 ==1, 2, 0)))%>%
    dplyr::mutate(RFS = ifelse(RFS1 == 1, 1, ifelse(RFS2 == 1, 2, 0)))%>%
    dplyr::select(nuc.power, CO2Price, ban.fuel.explore, RFS)
  
  design.right <- design.left%>%
    mutate(nuc.power = (nuc.power + 1)%%3)%>%
    mutate(CO2Price = (CO2Price + 1)%%6)%>%
    mutate(ban.fuel.explore = (ban.fuel.explore + 1)%%3)%>%
    mutate(RFS = (RFS + 1)%%3)
  
  left.group <- design.left%>%
    mutate(nuc.power = ifelse(nuc.power == "0", "Keep current nuclear power plants but do not build any new plants.", ifelse(
      nuc.power == "1", "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.",
      "Shut down nuclear power plants that are not making money, reducing nuclear power by 14-29% than today. Do not build nuclear plants in the next eight years."
    )))%>%
    mutate(CO2Price = ifelse(CO2Price == "0", "0", ifelse(CO2Price == "1", "30", ifelse(CO2Price == "2", "60", ifelse(CO2Price == "3", "90", ifelse(
      CO2Price == "4", "120", "150"
    ))))))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "0", "Unregulated access to federal lands and waters for fossil fuel exploration.", ifelse(
      ban.fuel.explore == "1", "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.",
      "Fully ban fossil fuel exploration on public lands."
    )))%>%
    mutate(RFS = ifelse(RFS == "0", "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", ifelse(
      RFS == "1", "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", "Starting from 2021, reach 100% clean energy by 2035 with a yearly increase of 7.5% of clean energy in the grid."
    )))
  
  right.group <- design.right%>%
    mutate(nuc.power = ifelse(nuc.power == "0", "Keep current nuclear power plants but do not build any new plants.", ifelse(
      nuc.power == "1", "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.",
      "Shut down nuclear power plants that are not making money, reducing nuclear power by 14-29% than today. Do not build nuclear plants in the next eight years."
    )))%>%
    mutate(CO2Price = ifelse(CO2Price == "0", "0", ifelse(CO2Price == "1", "30", ifelse(CO2Price == "2", "60", ifelse(CO2Price == "3", "90", ifelse(
      CO2Price == "4", "120", "150"
    ))))))%>%
    mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "0", "Unregulated access to federal lands and waters for fossil fuel exploration.", ifelse(
      ban.fuel.explore == "1", "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.",
      "Fully ban fossil fuel exploration on public lands."
    )))%>%
    mutate(RFS = ifelse(RFS == "0", "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", ifelse(
      RFS == "1", "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", "Starting from 2021, reach 100% clean energy by 2035 with a yearly increase of 7.5% of clean energy in the grid."
    )))
  
  sheet_write(left.alternatives, data = left.group, sheet = 1)
  sheet_write(right.alternatives, data = right.group, sheet = 1)
  sheet_write(final.recommendation, data = optimal.design, sheet = 2)


  
  # Initialize the timer, 10 seconds, not active.
  timer <- reactiveVal(10)
  active <- reactiveVal(FALSE)
  counter <- reactiveVal(2)
  counter_refresh <- reactiveVal(2)
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time Remaining: ", seconds_to_period(timer()))
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    invalidateLater(1000)
    isolate({
      if(active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          active(FALSE)
          counter(counter()+1)
          showModal(modalDialog(
            title = "Important message",
            "Time is Up!"
          ))
        }
      }
    })
  })
  
  # observers for actionbuttons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$stop, {active(FALSE)})
  observeEvent(input$reset, {timer(input$seconds)})
   
 column.names <- c("Characteristics", "Alternative Left", "Alternative Right")
 attribute.names <- c("Nuclear Power in the Power Grid", "Price on Carbon($/ton)", "Fossil Fuel Exploration Rules on Federal Land", "Clean Energy Standard Target")
 
 observeEvent(input$updateAlternatives,{
   
   counter_refresh(counter())
   output$RecommendationGroup1 <- renderTable({
     if(counter_refresh() <=16){
       
       policy.left <- sheets_read(left.alternatives, range = cell_rows(counter_refresh()), sheet = 1, col_names = FALSE)
       policy.right <- sheets_read(right.alternatives, range = cell_rows(counter_refresh()), sheet = 1, col_names = FALSE)
       
       questions <- as.data.frame(cbind(attribute.names, t(policy.left), t(policy.right)))
       names(questions) <- column.names
       questions  
       
     }else if(counter_refresh() ==17){
       
       
       final.recommendation <- as_sheets_id(final.recommendation)
       all.responses <- sheets_read(voting, sheet = 1)
       
       y.recommendation <- array(dim = c(15,4))
       
       for(userResponseCount in 1:15){
         for(userCount in 1:4){
           
           y.recommendation[userResponseCount, userCount] <- ifelse(all.responses[userResponseCount, userCount] == "Left", 1,0)
           
         }
         
       }
       
       y.test <- as.vector(y.recommendation)
       
       left.uber <- sheets_read(final.recommendation, sheet = "Optimal Design", range="A1:H16")
       #uber.tot <- left_join(left.uber, group.utility, by = c("nuc.power1", "nuc.power2", "CO2Price", "ban.fuel.explore1", "ban.fuel.explore2", "RFS1", "RFS2"))
       
       ubar <- as.data.frame(left.uber)[,"group.score"]
       
       sdrec <- as.data.frame(left.uber)[,"group.sd"]
       
       ubar_test <- rep(ubar, times = 4)
       sd_test <- rep(sdrec, times =4)
       
       ubar_total <- as.data.frame(group.utility)[,13]
       sd_total <- as.data.frame(group.utility)[,14]
       
       ubar_total_repeat <- rep(ubar_total, times  = 4)
       sd_total_repeat <- rep(sd_total, times = 4)
       cGLM1 <- constGLM1multiple(X=cbind(ubar_test, sd_test), y = y.test, Xtest = cbind(ubar_total_repeat, sd_total_repeat))
       
       predicted.probability <- cGLM1[[2]][1:162]
       
       row.recommendation <- which.max(predicted.probability)
       
       row.recommendation 
       
       new.group.recommendations <- cbind(group.utility, predicted.probability)%>%
         arrange(desc(predicted.probability))
       
       recommendation <- as.data.frame(group.utility[row.recommendation, 1:7])%>%
         dplyr::mutate(nuc.power = ifelse(nuc.power1 == 1, 1, ifelse(nuc.power2 ==1, 2, 0)))%>%
         dplyr::mutate(ban.fuel.explore = ifelse(ban.fuel.explore1 == 1, 1, ifelse(ban.fuel.explore2 ==1, 2, 0)))%>%
         dplyr::mutate(RFS = ifelse(RFS1 == 1, 1, ifelse(RFS2 == 1, 2, 0)))%>%
         dplyr::select(nuc.power, CO2Price, ban.fuel.explore, RFS)%>%
         mutate(nuc.power = ifelse(nuc.power == "0", "Keep current nuclear power plants but do not build any new plants.", ifelse(
           nuc.power == "1", "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.",
           "Shut down nuclear power plants that are not making money, reducing nuclear power by 14-29% than today. Do not build nuclear plants in the next eight years."
         )))%>%
         mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "0", "Unregulated access to federal lands and waters for fossil fuel exploration.", ifelse(
           ban.fuel.explore == "1", "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.",
           "Fully ban fossil fuel exploration on public lands."
         )))%>%
         mutate(RFS = ifelse(RFS == "0", "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", ifelse(
           RFS == "1", "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", "Starting from 2021, reach 100% clean energy by 2035 with a yearly increase of 7.5% of clean energy in the grid."
         )))
       
       sheet_write(data = new.group.recommendations, ss = predictedprob, sheet = 1)
       
       sheet_write(final.recommendation, data = recommendation, sheet = 1)

       
       column.names1 <- c("Characteristics", "Final Recommendation")
       attribute.names1 <- c("Nuclear Power in the Power Grid", "Price on Carbon($/ton)", "Fossil Fuel Exploration Rules on Federal Land", "Clean Energy Standard Target")
       
       rec.table <- as.data.frame(cbind(attribute.names1, t(recommendation)))
       names(rec.table) <- column.names1
       rec.table
       
     } else{
       
       column.names1 <- c("Characteristics", "Final Recommendation")
       attribute.names1 <- c("Nuclear Power in the Power Grid", "Price on Carbon($/ton)", "Fossil Fuel Exploration Rules on Federal Land", "Clean Energy Standard Target")
       
       rec.final <- sheets_read(final.recommendation, sheet = 1)
       rec.table1 <- as.data.frame(cbind(attribute.names1, t(rec.final)))
       names(rec.table1) <- column.names1
       rec.table1
       
     }
     
     
     
   })
   
   output$validationQuestion <- renderTable({
     if(counter_refresh() <= 17){
       
       A <- c("A", "B", "C")
       
     }else if(counter_refresh() == 18){
       

       
       test.1 <- sample(2:18, 1)
       test.2 <- sample(19:35, 1)
       test.3 <- sample(36:42, 1)
       test.4 <- sample(43:59, 1)
       test.5 <- sample(60:76, 1)
       test.6 <- sample(77:93, 1)
       test.7 <- sample(94:110, 1)
       test.8 <- sample(111:127, 1)
       test.9 <- sample(128:144, 1)
       test.10 <- sample(145:162, 1)
       
       test.qnumbers <- c(test.1, test.2, test.3, test.4, test.5, test.6, test.7, test.8, test.9, test.10)
       
       validationQs <- sheets_read(final.recommendation, sheet = 1)
       random.questions <- sheets_read(predictedprob, sheet = 1)
       random.questions <- random.questions[c(test.qnumbers),]
         
         
       random.questions.reformed <- random.questions%>%
         dplyr::mutate(nuc.power = ifelse(nuc.power1 == 1, 1, ifelse(nuc.power2 ==1, 2, 0)))%>%
         dplyr::mutate(ban.fuel.explore = ifelse(ban.fuel.explore1 == 1, 1, ifelse(ban.fuel.explore2 ==1, 2, 0)))%>%
         dplyr::mutate(RFS = ifelse(RFS1 == 1, 1, ifelse(RFS2 == 1, 2, 0)))%>%
         dplyr::select(nuc.power, CO2Price, ban.fuel.explore, RFS)%>%
         mutate(nuc.power = ifelse(nuc.power == "0", "Keep current nuclear power plants but do not build any new plants.", ifelse(
           nuc.power == "1", "Keep current nuclear power plants and provide money to increase nuclear power by 14% over the next eight years.",
           "Shut down nuclear power plants that are not making money, reducing nuclear power by 14-29% than today. Do not build nuclear plants in the next eight years."
         )))%>%
         mutate(ban.fuel.explore = ifelse(ban.fuel.explore == "0", "Unregulated access to federal lands and waters for fossil fuel exploration.", ifelse(
           ban.fuel.explore == "1", "Tighter fracking regulation on public lands that increases storage safety standards and transparency of what chemicals are used.",
           "Fully ban fossil fuel exploration on public lands."
         )))%>%
         mutate(RFS = ifelse(RFS == "0", "Starting from 2021, reach 100% clean energy by 2100 with a yearly increase of 1.3% of clean energy in the grid.", ifelse(
           RFS == "1", "Starting from 2021, reach 100% clean energy by 2050 with a yearly increase of 3.5% of clean energy in the grid.", "Starting from 2021, reach 100% clean energy by 2035 with a yearly increase of 7.5% of clean energy in the grid."
         )))
       
       sheet_write(random.questions.reformed, ss = final.recommendation, sheet = "Random Questions")
       sheet_write(data = random.questions, ss = final.recommendation, sheet = "Raw Random")
       
       validation.table <- as.data.frame(cbind(attribute.names, t(validationQs), t(random.questions.reformed[1,])))
       names(validation.table) <- column.names
       validation.table
     }else{
       
      
       rowCountQs <- counter_refresh()
       attribute.names <- c("Nuclear Power in the Power Grid", "Price on Carbon($/ton)", "Fossil Fuel Exploration Rules on Federal Land", "Clean Energy Standard Target")
       actual.row <- rowCountQs - 17
       validationRandom <- sheets_read(final.recommendation, sheet = "Random Questions")
       validation.final <- sheets_read(final.recommendation, sheet = "Sheet1")
       
       validation.table1 <- as.data.frame(cbind(attribute.names, t(validation.final), t(validationRandom[actual.row,])))
       names(validation.table1) <- column.names
       validation.table1
       
     }
     
     
   })
   
   output$counterText <- renderText({
     
     current.counter <- counter_refresh()-1
     paste0("Current Comparison: ", current.counter)
     
   })
   
   
 })
 
 

}

# Run the application 
shinyApp(ui = ui, server = server)

