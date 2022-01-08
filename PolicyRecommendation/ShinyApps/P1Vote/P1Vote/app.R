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
library(shinyWidgets)
library(httr)
library(gargle)

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
   
   # Application title
   dashboardHeader(title = "Person 1 Vote"),
   dashboardSidebar(
     sidebarMenu(id = "sidetabs",
     menuItem("Consent Form", tabName = "consent", icon = icon("check-square")),
     menuItemOutput("all.items"))

   ),
   dashboardBody(
     useShinyjs(),
     tags$head(tags$style(".shiny-notification {height: 100px; width: 100%; position: fixed; 
                         right: 0; bottom: 0; border: 3px solid #73AD21; font-size: 26px;}")),
     title = "Your responses",
     tabItems(
       tabItem(tabName = "consent",
               h2("Consent Form"),
               fluidRow(
                 box(
                   title = "Consent Form", solidHeader = TRUE, width = 12,
                   h3("Please read the consent form below carefully and answer the three questions at the end of the form and click on the submit button. 
                      Your consent will only be considered as given if you answer Yes to all three questions. 
                      If you do not answer Yes to all three questions, the browser will close once you click on the submit button.
                      However if you give consent to the study, please navigate to the Group Discussion tab by clicking on the Group Discussion tab on the side menu."),
                   tags$br(),
                   tags$br(),
                   "This task is part of a research study conducted by Niles Guo at Carnegie Mellon University and is funded by US Army Contracting Command.",
                   tags$br(),
                   tags$b("Principal Investigator:"), "Niles Xunan Guo, PhD Student in Engineering and Public Policy", tags$br(),
                   "Engineering and Public Policy, 5000 Forbes Ave, Baker Hall 129, Pittsburgh, PA, 15213", tags$br(),
                   "Phone: 206-495-1364", tags$br(),
                   "Email: nilesxug@andrew.cmu.edu", tags$br(),
                   tags$br(),
                   tags$b("Faculty Adviser: "), "Dr. Alex Davis, Assistant Professor in Engineering and Public Policy", tags$br(),
                   "Email: ald1@andrew.cmu.edu", tags$br(),
                   h3("Purpose"), 
                   "This survey is part of a research study conducted by Dr. Alexander Davis and Niles Guo at Carnegie Mellon University. The purpose of the research is to learn how experts determine what components should be consolidated in a metal additive manufacturing (MAM) process, and what parts are appropriate for MAM.",
                   tags$br(),
                   h3("Summary"),
                   "As a participant, we will ask you to evaluate potential candidates for MAM, and their suitability for consolidation using your expert judgment. We will first ask you to make these judgments individually, then invite you to come back and make these judgments in a group of other experts.",
                   tags$br(),
                   h3("Procedure"),
                   "You are being asked to participate in this stage 2 study because you previously participated in stage 1 (survey) of the study. In this stage 2 study, you will be in a group with up to 3 other participants. As a group, you will be asked to select an the most appropriate component for MAM conducted over Zoom. The study should last ~45 minutes.",
                   tags$br(),
                   tags$br(),
                   "No video or audio recording will be collected during the study. However, observations about your interactions with other participants would be collected. 
                   A researcher will be present during the group session to observe and take notes of participants' interactions. 
                   This information will be anonymized and aggregated for qualitative results.",
                   tags$br(),
                   h3("Participant Requirements"),
                   "Participation in this study is limited to individuals age 18 and older and who are conversational English speakers and are experts in MAM.",
                   tags$br(),
                   h3("Risks"),
                   "For this second stage, the risks and discomfort associated with participation in this study are no greater than those ordinarily encountered in daily life or during discussions with colleagues while using a computer. There is a potential risk of breach of confidentiality. We cannot guarantee any confidentiality about what is discussed in the group as group members may repeat the information they hear to others.",
                   tags$br(),
                   h3("Benefits"),
                   "There may be no personal benefit from your participation in the study but the knowledge received may be of value to humanity.",
                   tags$br(),
                   h3("Compensation & Costs"),
                   "There is no compensation for participating. There will be no cost to you if you participate in this study.",
                   tags$br(),
                   h3("Future Use of Information"),
                   "In the future, once we have removed all identifiable information from your data, we may use the data for our future research studies, or we may distribute the data to other researchers for their research studies.  We would do this without getting additional informed consent from you (or your legally authorized representative).  Sharing of data with other researchers will only be done in such a manner that you will not be identified.",
                   tags$br(),
                   h3("Confidentiality"),
                   "By participating in this research, you understand and agree that Carnegie Mellon may be required to disclose your consent form, data and other personally identifiable information as required by law, regulation, subpoena or court order.  In addition, the sponsor (US Army Contracting Command) may have access to research records. Otherwise, your confidentiality will be maintained in the following manner:",
                   tags$br(),
                   tags$br(),
                   "Your data and consent form will be kept separate. Your consent form will be stored in a secure location on Carnegie Mellon property and will not be disclosed to third parties. By participating, you understand and agree that the data and information gathered during this study may be used by Carnegie Mellon and published and/or disclosed by Carnegie Mellon to others outside of Carnegie Mellon.  However, your name, address, contact information and other direct personal identifiers will not be mentioned in any such publication or dissemination of the research data and/or results by Carnegie Mellon. Note that per regulation all research data must be kept for a minimum of 3 years.", 
                   tags$br(),
                   tags$br(),
                   "If you are participating via Zoom, please note that Carnegie Mellon University does not control Zoom's terms and conditions or how Zoom uses any data that Zoom collects from its account holders.",
                   tags$br(),
                   tags$br(),
                   "The researchers will take the following steps to protect participants' identities during this study: (1) Each participant will be assigned a number; (2) The researchers will record any data collected during the study by that number, not by name; (3) Any original data files will be stored in a secured location accessed only by authorized researchers.",
                   tags$br(),
                   h3("Rights"),
                   "Your participation is voluntary.  You are free to stop your participation at any point.  Refusal to participate or withdrawal of your consent or discontinued participation in the study will not result in any penalty or loss of benefits or rights to which you might otherwise be entitled.  The Principal Investigator may at his/her discretion remove you from the study for any of a number of reasons.  In such an event, you will not suffer any penalty or loss of benefits or rights which you might otherwise be entitled.",
                   tags$br(),
                   h3("Rights to Ask Questions & Contact Information"),
                   "If you have any questions about this study, you should feel free to ask them by contacting the Principal Investigator, Niles Guo, PhD Student, Engineering and Public Policy, 5000 Forbes Ave, Baker Hall 129, Pittsburgh, PA, 15213, nilesxug@andrew.cmu.edu. If you have questions later, desire additional information, or wish to withdraw your participation please contact the Principal Investigator by mail, phone or e-mail in accordance with the contact information listed above.",
                   tags$br(),
                   tags$br(),
                   "If you have questions pertaining to your rights as a research participant; or to report concerns about this study, you should contact the Office of Research Integrity and Compliance at Carnegie Mellon University.  Email: irb-review@andrew.cmu.edu . Phone: 412-268-1901 or 412-268-5460.",
                   tags$br(),
                   h3("Voluntary Participation"),
                   "Your participation in this research is voluntary.  You may discontinue participation at any time during the research activity.  You may print a copy of this consent form for your records.",
                   tags$br(),
                   tags$br(),
                   selectInput("plus18","I am age 18 and older", c("Yes", "No")),
                   tags$br(),
                   selectInput("understand", "I have read and understand the information above", c("Yes" = "Yes", "No" = "No")),
                   tags$br(),
                   selectInput("wishPart", "I want to participate in this research and continue with the activity", c("Yes" = "Yes", "No" = "No")),
                   tags$br(),
                   actionButton("submitConsent", "Submit Consent Form"),
                   tags$br(),
                   tags$br(),
                   textOutput("consentOutputMessage"),
                   tags$br(),
                   uiOutput("linkOutput")
                   
                   )
                 )),
       tabItem(tabName = "voting",
               h2 = "Group Discussion",
               fluidRow(
                 box(title = "Attribute Information",width = 12, collapsible = TRUE, collapsed = TRUE,
                     htmlOutput("inst")  
                 )
                 
                 
               ),            
               tags$br(),
               h3("Instructions"),
               "During this task, we are going to show you pairs of parts in the Improved Ribbon Bridge M16 Ramp Bay and M17 Interior Bay. 
               The improved ribbon bridge is a modular bridge made up of a series of foldable, floating pontoon segments. 
               A complete ribbon bridge consists of a ramp bay at each bank and a number of longitudinally connected interior bays spanning between them. Once deployed, vehicles and personnel can safely cross the bridge. 
               Each candidate will show you a different part in this ribbon bridge. Some candidates will be suitable for MAM, some will not be suitable. Your task is first to select the candidate that you believe the group believes is more suitable for MAM. 
               We are interested here in part performance, economic case for moving to MAM, and military specific criteria on MAM decisions. For experts who have extensive military experience, when evaluating these parts on their suitability for MAM, we would recommend you thinking about these parts' suitability in the context of either your or your unit's last deployment.",
               tags$br(),
               lapply(1:15, function(i){
                 
                 fluidRow(
                   box(width = 12, title = paste('Comparison', i, sep = " "), 
                       uiOutput(paste0("Comparison", i))))
                 
               }),
               fluidRow(
                 box(width = 12, title = "Final Recommendation",
                     uiOutput("recommendationOutput"))
               ),
               lapply(1:10, function(g){
                 fluidRow(
                   box(width = 12, title = paste('Validation', g, sep = " "), 
                       uiOutput(paste0("Validation", g))))
                 
               })
               )
     )
     
     
   )
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

   # options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/drive.file",
   #                                         "https://www.googleapis.com/auth/drive",
   #                                         "https://www.googleapis.com/auth/drive.readonly",
   #                                         "https://www.googleapis.com/auth/spreadsheets.readonly",
   #                                         "https://www.googleapis.com/auth/spreadsheets",
   #                                         "https://www.googleapis.com/auth/xapi.zoo"))
  # options("googleAuthR.webapp.client_id" = "896066592625-9b26euhim8plpf9jqn2968mgd6dque1a.apps.googleusercontent.com")
  # options("googleAuthR.client_secret" = "Gh_SXMEy8ZnMXU1hy6lKSlr6")
  # 
  # 
  # drive_deauth()
  # google_app <- httr::oauth_app("CADS Study",
  #                               key = "896066592625-9b26euhim8plpf9jqn2968mgd6dque1a.apps.googleusercontent.com",
  #                               secret = "Gh_SXMEy8ZnMXU1hy6lKSlr6")
  # 
  # drive_auth_configure(api_key = "AIzaSyCMy0-hV904esi55f2l5eidrvm-O1cSks8")
  # 
  # drive_auth_configure(google_app)

  options(gargle_oauth_cache= ".secrets")
  gargle::gargle_oauth_cache()
  
  options(
    gargle_oauth_cache = ".secrets",
    gargle_oauth_email = "nilesguo@gmail.com"
  )
  
  drive_auth()
  drive_auth(email = "nilesguo@gmail.com")
  gs4_auth(token = drive_token())
  
  voting_url <- as_sheets_id("https://docs.google.com/spreadsheets/d/1Gsr7xmv5PCbvKY7_vTRir6XBJKI4ME7VlAQ5YEVJv7Q/edit#gid=0") 
  
  observeEvent(input$idSubmit,{
    googlesheets4::range_write(voting_url, data = as.data.frame(input$idInput), range = "A1", col_names = FALSE)
     
  })
  
  getPage <- function() {
    return(includeHTML("Attributes.htm"))
  }
  
  observeEvent(input$submitConsent, {
    
    if(input$plus18 == "Yes" & input$understand == "Yes" & input$wishPart == "Yes"){
      
      output$all.items <- renderMenu(
        sidebarMenu(id = "fulltabs",
                    menuItem("Group Discussion", tabName = "voting", icon = icon("briefcase"))
        )
      )
      output$linkOutput <- renderUI(
        
        actionLink("link_instructions", a("Group Discussion", href = "#shiny-tab-voting", "data-toggle" = "tab"))
      )
      output$consentOutputMessage <- renderText(
        
        "Your Consent Form has been submitted. Please click on the Group Discussion link below."
        
      )
      
    } else{
      
      stopApp()
      
    }
    
    
  })
  
  output$inst <- renderUI({
    getPage()
  })
  
  lapply(1:15, function(i){
    output[[paste0("Comparison",i)]] <- renderUI({
      fluidRow(
        box(width = 12,
          column(12,
                   prettyRadioButtons(inputId = paste0('Comparison', i), label = "Your Vote:",
                               choices = c("Left" = "Left",
                                "Right" = "Right"),
                                selected = character(0),
                               shape = "square",
                               inline = TRUE,
                               animation = "pulse",
                               status = "info"
                               )))
        
      )
    })
  })
  
  lapply(1:15, function(i){
    
    observeEvent(input[[paste0('Comparison',i)]], {
      
      rownums <- i +1
      range_write(voting_url, data = as.data.frame(input[[paste0('Comparison', i)]]), range = paste0("A",rownums), col_names = FALSE)
      
    })
    
  })
  
  lapply(1:10, function(g){
    
    output[[paste0("Validation",g)]] <- renderUI({
      
      fluidRow(
        box(width = 12,
            column(12,
                   prettyRadioButtons(inputId = paste0('Validation', g), label = "Your Vote:",
                               choices = c(
                                 "Left" = "Left",
                                 "Right" = "Right"),
                               selected = character(0),
                               shape = "square",
                               inline = TRUE,
                               animation = "pulse",
                               status = "info")))
        
      )
      
    })
    
  })
  
  lapply(1:10, function(g){
     
    observeEvent(input[[paste0('Validation', g)]], {
      rownums1 <- g+17
      range_write(voting_url, data = as.data.frame(input[[paste0('Validation', g)]]), range = paste0("A", rownums1), col_names = FALSE)
      
    })
  })
  
  output$recommendationOutput <- renderUI({
    
    fluidRow(
      box(width = 12,
          column(12,
                 prettyRadioButtons(inputId = 'recommendationVote', label = "Your Vote:",
                             choices = c(
                               "Yes" = "Yes",
                               "No" = "No"), 
                             selected = character(0),
                             shape = "square",
                             inline = TRUE,
                             animation = "pulse",
                             status = "info")))
    )
    
  })
  
  observeEvent(input$recommendationVote,{
    
    range_write(voting_url, data = as.data.frame(input$recommendationVote), range = "A17", col_names = FALSE)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

