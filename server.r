library(readxl)
library(shiny)
library(tidyverse)
library(knitr)
library(kableExtra)
library(DT)
library(plotly)
library(purrr)
library(htmltools)
library(data.table)
library(webshot)

School_list <- read_excel("C:/Users/maureen.otini_eviden/Documents/R/Form P/2022 School List.xlsx")

# a custom table container for school list
sketch = htmltools::withTags(table(
  class = 'display',
  thead(  tr(
    
      th(style = "border-top: solid 2px", rowspan = 2, 'No.'),
      th(style = "border-top: solid 2px",rowspan = 2, 'Primary School Name (Write in Full)'),
      th(style = "border-top: solid 2px",rowspan = 2, 'School MOEST NEMIS Code'),
      th(style = "border-top: solid 2px",rowspan = 2, 'Primary School Type (Public/ private/other)'),
      th(style = "border-top: solid 2px",rowspan = 2, 'School Closed (Tick if Closed )'),
      th(style = "border-top: solid 2px",colspan = 2, 'Enrolment in this school (children in register book) including attached ECD'), 
      th(style = "border-top: solid 2px",rowspan = 2, 'No. of standalone ECD centres in School Catchment Area'),
      th(style = "border-top: solid 2px",rowspan = 2, 'Estimated enrolment in stand- alone ECD centres. (C)'),
      th(style = "border-top: solid 2px",rowspan = 2, 'School treating for Bilharzia ? (Yes/No)?'),
      th(style = "border-top: solid 2px",rowspan = 2,"Total Enrolment (A+B+C)") 
      ),
    tr(
      lapply(rep(c('Primary school enrolment (A)', 'ECD attached enrolment (B)')), th)
    )
  )
))
print(sketch)

sketch2 = htmltools::withTags(table(
  class = 'display',
  thead(  tr(
    
    th(style = "border-top: solid 2px", rowspan = 1, HTML("<h3> <b>3. Total this <br>sheet<h3><b>")),
    th(style = "border-top: solid 2px",rowspan = 1, HTML("<h4><b>Total no. of schools:<br>(do not count closed)<h4><b>")),
    th(style = "border-top: solid 2px",rowspan = 1,  HTML("<h3><b> Count<h3>")),
    th(style = "border-top: solid 2px",rowspan = 1, HTML("<h4><b>Count:<h4>")),
    th(style = "border-top: solid 2px",rowspan = 1, HTML("<h4><b>Sum of A:<h4>")),
    th(style = "border-top: solid 2px",rowspan = 1, HTML("<h4>Sum of B:<h4>")),
    th(style = "border-top: solid 2px",rowspan = 1, HTML("<h4>Sum:<h4>")),
    th(style = "border-top: solid 2px",rowspan = 1, HTML("<h4>Sum:<h4>")),
    th(style = "border-top: solid 2px",rowspan = 1, HTML("<h4>Count <br> yes:<h4>")),
    th(style = "border-top: solid 2px",rowspan = 1,HTML("<h4>Sum<h4>")),
  ),
  
  )
  )
)
print(sketch2)

###Form P Ward Planning table outline
sketch3 = htmltools::withTags(table(
  class = 'display',
  thead(  tr(
    
    th(style = "border-top: solid 2px", rowspan = 2, 'Division/Ward Name:'),
    th(style = "border-top: solid 2px",rowspan = 2, 'Total no. of schools:)'),
    th(style = "border-top: solid 2px",colspan = 2, 'Enrolment in schools'),
    th(style = "border-top: solid 2px",rowspan = 2, 'Total No. of Stand-alone centres'),
    th(style = "border-top: solid 2px",rowspan = 2, 'Total Enrolment in standalone ECD centres (C):'),
    th(style = "border-top: solid 2px",rowspan = 2, 'No. Bilharzia Schools:'),
    th(style = "border-top: solid 2px",rowspan = 2, 'Total Enrolment (A+B+C)')
  ),
  tr(
    lapply(rep(c('Primary school enrolment (A)', 'ECD attached enrolment (B)')), th)
  )
  
  )
)
)
print(sketch3)


headerCallback <- c(
  "function(thead, data, start, end, display){",
  "  $(thead).closest('thead').find('th').css('border-right', 'solid 2px');",
  "  $(thead).closest('thead').find('th').eq(0).css('border-left', 'solid 2px');",
  "}"
)

School_list$trt_type <- with(School_list, ifelse(treatment_type == "STH", 'No', 'Yes'))

x<- c("x1","x2","x3","x4","x5","x6","x7","x8")
ward = data.frame(matrix(nrow = 0, ncol = length(x))) 
colnames(ward) = x
ward[ nrow(ward) + 1 , ] <- NA


add_blank <- function(x, n=10) {
  tibble::add_row(x, School_name=rep(NA, n), NEMIS_code=rep(NA, n), School_Type=rep(NA, n),
                  School_Closed=rep(NA, n),  Primary_school_enrollment=rep(NA, n),  ECD_attached_enrollment=rep(NA, n), No_standalone_ecds=rep(" ", n),
                  Standalone_ecd_enrollment=rep(NA, n), trt_type=rep(NA, n),  total_enrollment=rep(NA, n))
}
School_list2<- School_list%>%
  group_by(County, Sub_County,Division)%>%
  group_modify(~add_blank(., 10))%>%
  mutate(n= row_number())%>%
  ungroup


School_list2 <- setcolorder(School_list2, c("n","School_name","NEMIS_code","School_Type","School_Closed", "Primary_school_enrollment", "ECD_attached_enrollment","No_standalone_ecds","Standalone_ecd_enrollment","trt_type","total_enrollment"  ))
  


summary <- School_list2%>%
  filter(n==0)
#summary[ nrow(summary) + 1 , ] <- NA


shinyServer(function(input, output, session) {

  
  
  #County Details Filters
  
  
  data <-School_list
  output$table <- DT::renderDataTable({
    if(is.null(data)){return()}
    DT::datatable(data, options = list(scrollX = F, scrollY= F))
  })
  
  
  
  output$county <- renderUI({
    selectInput(inputId = "county", "Select County", choices =  var_county(), multiple = F)
  })
  
  output$county_id <- renderUI({
    selectInput(inputId = "county_id", "Select County ID", choices =  var_county_id(), multiple = F)
  })
  
  
  output$sub_county <- renderUI({
    selectInput(inputId = "sub_county", "Select Sub County", choices =  var_subcounty(), multiple = F)
  })
  
  output$sub_county_id <- renderUI({
    selectInput(inputId = "sub_county_id", "Select Sub County ID", choices =  var_subcounty_id(), multiple = F)
  })
  
  output$division <- renderUI({
    selectInput(inputId = "division", "Select Division/Ward", choices =  var_division(), multiple = F)
  })
  
  output$division_id <- renderUI({
    selectInput(inputId = "division_id", "Select Division/Ward ID", choices =  var_division_id(), multiple = F)
  })
  
# Get filters
  
  county <- reactive({
    if (is.null(input$county)) unique(School_list$County) else input$county
  })
  
  county_id <- reactive({
    if (is.null(input$county_id)) unique(School_list$County_id) else input$county_id
  })
  
  sub_county2 <- reactive({
    if (is.null(input$sub_county)) unique(School_list$Sub_County) else input$sub_county
  })
  
  sub_county_id <- reactive({
    if (is.null(input$sub_county_id)) unique(School_list$Subcounty_id) else input$sub_county_id
  })
  
  division <- reactive({
    if (is.null(input$division)) unique(School_list$Division) else input$division
  })
  
  division_id <- reactive({
    if (is.null(input$division_id)) unique(School_list$Division_id) else input$division_id
  })
  
  # Get available categories
  var_county <- reactive({
    file1 <- data
    if(is.null(data)){return()}
    as.list(unique(file1$County))
  })
  
  var_county_id <- reactive({
    filter(data, County %in% county()) %>% 
      pull(County_id) %>% 
      unique()
  })
  
  var_subcounty <- reactive({
    filter(data, County %in% county()) %>% 
      pull(Sub_County) %>% 
      unique()
  })
  
  var_subcounty_id <- reactive({
    filter(data, County %in% county(), Sub_County %in% sub_county2()) %>% 
      pull(Subcounty_id) %>% 
      unique()
  })
  
  var_division <- reactive({
    filter(data, County %in% county(), Sub_County %in% sub_county2()) %>% 
      pull(Division) %>% 
      unique()
  })
  
  var_division_id <- reactive({
    filter(data, County %in% county(), Sub_County %in% sub_county2(), Division %in% division()) %>% 
      pull(Division_id) %>% 
      unique()
  })
  
  
 
  
  output$County <- renderUI({
    tagList(tags$strong("1. COUNTY:   "), input$county, tags$strong("  COUNTY ID:    "), input$county_id, "   ",tags$strong("SUB COUNTY:   "), input$sub_county,tags$strong("  SUB COUNTY ID :   "), input$sub_county_id,tags$strong("   DIVISION:  "),input$division, tags$strong("  DIVISION ID:  "),input$division_id)
  })
  
  #School List  
  formp_school_list = reactive({
    School_list2 %>%
      filter(County == input$county & Sub_County == input$sub_county & Division == input$division )%>%
      select(-County, -year, -County_id, -Sub_County, -Subcounty_id, -Division, -Division_id,  -treatment_type )
    
    
  })
  
  output$table2 = renderDataTable({
    
    DT::datatable(formp_school_list(),
                  class = 'cell-border display table-border',
                  container = sketch,  rownames = F,
                  selection = 'none',  filter = 'none',
                  
                  options = list(headerCallback = JS(headerCallback),
                                 
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'border-top' : '5px solid black','color': 'black','font-size': '100%'});",
                                   "}"),
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '200px', targets = c(1))),
                                 
                                 dom = 'lBfrtip',
                                 columnDefs = list(list(targets = c(0, 2), visible = TRUE)),
                                 paging = F, searching = F, info = FALSE,
                                 sort = TRUE, scrollX = F, scrollY = NULL, fixedRow =  2, fixedColumns = list(leftColumns = 2)))%>% 
      formatStyle(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13), `border-right` = "solid 0px",`border-bottom` ="solid 2px", `border-left` ="solid 2px")
    
  })
  
  output$table3 = renderDataTable({
    
    DT::datatable(summary,
                 container = sketch2, 
                 selection = 'none',  filter = 'none',
                 
                 options = list(headerCallback = JS(headerCallback),
                   lengthMenu = c(5, 10, 15),
                   pageLength = 5,
                   searching = F,
                   ordering = F,
                   dom = 't',
                   scrollX = T,
                   width = '200%' # set width to 100%
                 ),
                 rownames = FALSE)
  })
  
  
  output$table4 = renderDataTable({
    
    DT::datatable(ward,
                  container = sketch3, selection = 'none',  filter = 'none',
                  
                  
                  options = list(headerCallback = JS(headerCallback),lengthMenu = c(5, 10, 15),
                                 pageLength = 5,
                                 searching = F,
                                 ordering = F,
                                 dom = 't',
                                 scrollX = T,
                                 width = '200%' # set width to 100%
                  ),
                  rownames = FALSE)
  })
  
 

  
})