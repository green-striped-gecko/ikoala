library(DT)
library(rhandsontable)
shinyApp(
  ui = fluidPage(
    textInput(inputId = "report_name", "Report name:", "ikoala report"),
    selectizeInput(inputId = "database", label="Database: ", choices=list.files("database/", pattern = "accdb")),
    selectizeInput(inputId = "selperiod", label="Periods: ", choices=list.files("periods/", pattern = "csv")),
    rHandsontableOutput('table'),
    selectizeInput(inputId = "selsites", label="Sites: ", choices=list.files("sites/", pattern = "csv")),
    selectizeInput(inputId = "selrestricted", label="Restrict plots by sites or region: ", choices=c("site","region")),
    
    textInput(inputId ="projection", "Projection: ","+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
    downloadButton("report", "Generate ikoala report", icon="images/ikoala.ico")
  ),
  server = function(input, output) {
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
                       study_periods_name=input$selperiod,
                       study_periods=hot_to_r(input$table),
                       study_site= file.path("sites",input$selsites),
                       restricted= input$selrestricted,
                       results_folder= "results",
                       max_distance= (if (input$selrestricted=="site")   250 else 501),
                       projection= input$projection)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render("ikoala_monitoring_server.rmd", output_file = file,params=params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)
