#### ikoala ###### To start click on "RunApp"
#### in case the app does not start, please quit the session ####



library(DT)
library(rhandsontable)
library(shiny)
library(shinyWidgets)
library(shinyjs)
shinyApp(
  ui = fluidPage(
    useShinyjs(),  # Set up shinyjs
    setBackgroundImage(
      src = "./ikoala_report_title2.png"
    ),
    textInput(inputId = "report_name", "Report name:", "ikoala report"),
    selectizeInput(inputId = "database", label="Database: ", choices=list.files("database/", pattern = "accdb")),
    selectizeInput(inputId = "selrestricted", label="Type of report:", choices=c("monitoring","general")),
    
    selectizeInput(inputId = "selsites", label="Sites: ", choices=list.files("sites/", pattern = "csv")),
    selectizeInput(inputId = "selstudyarea", label="Study area:", choices=c("Coastal","Southern Tablelands","Palerang", "Kooraban/Gulaga","Yurammie","Escarpment/Hinterlands", "Tantawangalo","Escarpment/Hinterland","Strzelecki Ranges", "Bermagui","Devils Holes","Southern tablelands","Hinterland")),
    h5(strong('Periods')),
    shiny::p('for "general" exactly ',strong('one'),' period'), 
    shiny::p('for monitoring at least ',strong('two'),' periods.'),
    rHandsontableOutput('table'),
    selectizeInput(inputId = "selperiod", label="Periods: ", choices=list.files("periods/", pattern = "csv")),
    
    textInput(inputId ="projection", "Projection: ","+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
    downloadButton("report", "Generate ikoala report", icon="images/ikoala.ico")
  ),
  server = function(input, output) {
    
    
    observe({
      #toggleState(id="selstudyarea", condition= input$selrestricted=="monitoring")
      if (input$selrestricted=="monitoring") {disable("selstudyarea"); enable("selsites")}
      if (input$selrestricted=="general") 
      {enable("selstudyarea")
        disable("selsites")
        DF = hot_to_r(input$table)
        output$table <- renderRHandsontable({rhandsontable(DF[1,])})}
      })
    
    
    observeEvent(input$selperiod, {
      output$table <- renderRHandsontable(rhandsontable(read.csv(file.path("periods",input$selperiod)))) 
      })
    
    
      output$table <- renderRHandsontable({
      if (is.null(input$table)) {
        DF = read.csv(file.path("periods",input$selperiod))
        
      } else {
        
        DF = hot_to_r(input$table)
       
      }
       rhandsontable(DF)
  
    })
  
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "ikoala_report.docx",
      content = function(file) {
        params <- list(report_name= input$report_name,
                       database= file.path("database",input$database),
                       study_periods= file.path("periods","periods.csv"),
                       study_site= file.path("sites",input$selsites),
                       report_type= input$selrestricted,
                       study_area=  input$selstudyarea,
                       results_folder= "results",
                       projection= input$projection)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        print(params)
        #write periods to tempfile (only one string can be passed to YAML)
        write.csv(hot_to_r(input$table), "./periods/periods.csv", row.names = F)
        rmarkdown::render("ikoala_monitoring_server.rmd", output_file = file,params=params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)
