# Load the required packages
library(DT)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(sitools)
library(gridExtra)
library(readxl)
library(tidyr)
library(writexl)
library(stringr)
library(tidyverse)
library(dplyr)
library(readxl)
library(treemap)
library(flexdashboard)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(waffle)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(shinythemes) 
library(htmltools)
library(webshot)
webshot::install_phantomjs()##newly added

School_list <- read_excel("C:/Users/maureen.otini_eviden/Documents/R/Form P/2022 School List.xlsx")

jscode.autoHeightDT <- '
  autoHeightDT = function() {
    var offset = 100; // pixels used for other elements like title, buttons, etc

    // compute the number of rows to show in window
    var n = Math.floor(($(window).height() - offset) / $("#table_name tr").height());

    // set the new number of rows in table
    t = $("#table_name .dataTable").DataTable().page.len(n).draw();
  }

  // to adjust the height when the app starts, it will wait 0.8 seconds
  setTimeout(autoHeightDT, 800);

  // to react to changes in height of window 
  $(window).resize(function() {
    autoHeightDT();
  });'

ui <- fluidPage(
  tags$script(jscode.autoHeightDT), # includes JavaScript code
  tags$head(
    tags$style(HTML("
            code {
                 border: 1px solid black;  
                  padding-right: 30px;  
                   background-color: white; 
                   font-color: black;
            }"))),
  tags$head(
  tags$style(HTML("
            h5 {
                 text-align: right; 
                 font-size: 70px;
            }"))),
  
  titlePanel("Form P"),
  sidebarLayout(
    sidebarPanel( width = 3,
                  uiOutput("county"),
                  uiOutput("county_id"),
                  uiOutput("sub_county"),
                  uiOutput("sub_county_id"),
                  uiOutput("division"),
                  uiOutput("division_id"),
                  downloadButton("download", "Download Plots and Tables")
                  
    ) ,
    mainPanel(
      hr(),
      br(), br(), 
      img(src='P.png', align = "right"),
      img(src='Pill.png', align = "left"),
      HTML("<h1><center> <b> FORM P: DIVISION PLANNING </b> </center></h1>"),
      HTML("<h2><center> <b> (Primary School List) </b> </center></h2>"),
          br(),
      (uiOutput("County")), class = "active",
      br(),
      br(),
      HTML(" > Review the list of public and private primary schools below. Complete the enrolment data carefully and neatly </>"),
      br(),
      HTML(" > Indicate if any school has closed with a √ in the 'closed' column"),
      br(),
      HTML(" > Confirm that all eligible schools (private and public) are entered in the right division/ward. All schools on form P are Only primary schools, not ECD centers/pre- primary   listed."),
      br(),
      HTML(" > Strike off any closed schools and do not complete the table for them. Example: <SPAN STYLE='text-decoration:line-through'><code>Hope Academy  </SPAN STYLE='text-decoration:line-through'></code><SPAN STYLE='text-decoration:line-through'><code>001-001-HQ05</SPAN STYLE='text-decoration:line-through'></code><SPAN STYLE='text-decoration:line-through'><code>Public  </SPAN STYLE='text-decoration:line-through'></code><code> √ </code>"),
      br(),
      HTML(" > Add new schools at the end of the form. Complete the programme assigned school ID using 
           
           the first 2 letters of school name. Example: <code>BARAKA P.S</code><code>001-001- BA 50</code>"),
      br(),
      br(),
      HTML("<h3><b> 2. SCHOOL LIST </b> </h3>"),
      box(width =12, DT::dataTableOutput("table2")),
      box(width =12, DT::dataTableOutput("table3")) ,
      
      br(),
      br(),
      hr(),
      br(), br(), 
      img(src='P.png', align = "right"),
      img(src='Pill.png', align = "left"),
      HTML("<h1><center> <b> FORM P: WARD PLANNING </b> </center></h1>"),
      HTML("<h2><center> <b> (Programme Activties) </b> </center></h2>"),
      br(), br(), 
      HTML("<h9><center>  Using Form P (school list) please complete the planning exercise below for your Ward.</center></h9>"),
      box(  HTML("<h4> <b> 1. Ward/Division Summary: Please add up all Form P sheet totals to give a summary for this Ward/Division</b> </h4>"),
          DT::dataTableOutput("table4") , width = 12, height = 200)
           ,
      tags$script(HTML("$('.box').eq(0).css('border', '2px solid #000000');")),
      
      br(), br(),
         fluidRow( box( 
              height = 620,
              solidHeader = TRUE,
              status = "warning",
              width = 12,
          tags$table(border = 2, cellpadding = 100, cellspacing = 100, style="height: 600px;",
                      tags$tbody(
                        tags$tr(
                          tags$td(colspan = 8, HTML("<h3> <b>  3. Plan scheduling and select venues for teacher training:</b> </h3>")),
                          tags$tr(
                          tags$td(colspan = 8, "As a Ward plan the teacher training sessions according to the number needed
Discuss which schools may attend each training and provide an approximate number of schools expected at each TT session.")),
                        tags$tr(
                          tags$td(colspan = 1, rowspan = 2, ""),
                          tags$td(colspan = 1, rowspan = 2, "Training Venue"),
                          tags$td(colspan = 1, rowspan = 2, "Estimated Training date(DD/MM/YY)"),
                          tags$td(colspan = 2, rowspan = 1, "Assigned Responsible MoE and MoH Officer"),
                          tags$td(colspan = 3, rowspan = 1, "No. of Schools Attending")),
                        tags$tr(
                          tags$td("Name"),
                          tags$td("Phone Number"),
                          tags$td("Non-Bilharzia (A)"),
                          tags$td("Bilharzia (B)"),
                          tags$td("Total (A+B)")),
                        tags$tr(
                            tags$td(rowspan = 2,"1" ),
                            tags$td(rowspan = 2, ""),
                            tags$td(rowspan = 2, ""),
                            tags$td(rowspan = 1, ""),
                            tags$td(rowspan = 1, ""),
                            tags$td(rowspan = 2, ""),
                            tags$td(rowspan = 2, ""),
                            tags$td(rowspan = 2, "")),
                        tags$tr(
                          tags$td(""),
                          tags$td("")),
                        tags$tr(
                          tags$td(rowspan = 2,"2" ),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, "")),
                        tags$tr(
                          tags$td(""),
                          tags$td("")),
                        tags$tr(
                          tags$td(rowspan = 2,"3" ),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, "")),
                        tags$tr(
                          tags$td(""),
                          tags$td("")),
                        tags$tr(
                          tags$td(rowspan = 2,"4" ),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, "")),
                        tags$tr(
                          tags$td(""),
                          tags$td("")),
                        tags$tr(
                          tags$td(rowspan = 2,"5" ),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, "")),
                        tags$tr(
                          tags$td(""),
                          tags$td("")),
                        tags$tr(
                          tags$td(rowspan = 2,"6" ),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, "")),
                        tags$tr(
                          tags$td(""),
                          tags$td("")),
                        tags$tr(
                          tags$td(rowspan = 2,"7" ),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, "")),
                        tags$tr(
                          tags$td(""),
                          tags$td("")),
                        tags$tr(
                          tags$td(rowspan = 2,"8" ),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, "")),
                        tags$tr(
                          tags$td(""),
                          tags$td("")),
                        tags$tr(
                          tags$td(rowspan = 2,"9" ),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 1, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, ""),
                          tags$td(rowspan = 2, "")),
                        tags$tr(
                          tags$td(""),
                          tags$td(""))
                            
                        )
      )),
      tags$script(HTML("$('.box2').eq(0).css('border', '2px solid #000000');"))),
      br(),br(),br(),
      tags$div(style="height:50px;"), # Add 20px of vertical space
      fluidRow( 
        box(width = 12,  
          tags$table(border = 2, cellpadding = 100, cellspacing = 1200, width = "100%",
                     tags$tbody(
                       tags$tr(
                         tags$td( colspan = 12, rowspan = 1, HTML("<h4> <b> 4. Sub-County please agree on and record the following dates: </b> </h4>")),
                         tags$tr(
                           
                           tags$td( colspan = 4,rowspan = 1, "Event"),
                           tags$td( colspan = 4, rowspan = 1, "Guidance on Date (As agreed by County)"),
                           tags$td(colspan = 4,  rowspan = 1, "Agreed date(DD/MM/YY)")),
                         tags$tr(
                           tags$td(colspan = 4, rowspan = 1,"Deworming Day"),
                           tags$td(colspan = 4, rowspan = 1,"About 1 week after teacher training"),
                           tags$td(colspan = 4,rowspan = 1, "_____/_____/ 20")),
                         tags$tr(
                           tags$td(colspan = 4,rowspan = 1,"Return Form 517C to CSO" ),
                           tags$td(colspan = 4,rowspan = 1, "About 1 week after deworming day"),
                           tags$td(colspan = 4,rowspan = 1, "_____/_____/ 20")),
                         tags$tr(
                           tags$td(colspan = 4,rowspan = 1,"CSO Returns Form MOH517C and D to SCDE" ),
                           tags$td(colspan = 4,rowspan = 1, "About 2 weeks after deworming day"),
                           tags$td(colspan = 4,rowspan = 1, "_____/_____/ 20")),
                         tags$tr(
                           tags$td(colspan = 4,rowspan = 1,"SCDE returns MOH 517C, D & E(Sub-County
Summary) to National Secretariat." ),
                           tags$td(colspan = 4,rowspan = 1, "About 3 weeks after deworming day"),
                           tags$td(colspan = 4,rowspan = 1, "_____/_____/ 20"))
                         
                         
                       )))),
      tags$script(HTML("$('.box3').eq(0).css('border', '2px solid #000000');")))
      

      
  
      
    
  ))
))