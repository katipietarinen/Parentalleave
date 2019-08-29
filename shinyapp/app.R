# setwd("/home/kati/Documents/Birmingham_City_University/Lopputyo/rdata/coef")
library(wesanderson)
coef = read.csv("coeffinal2.csv", stringsAsFactors = FALSE)

# Define the UI
ui = fluidPage(
  
  # Give the page a title
  titlePanel("What decreases / increases length of leave?"),
  hr(),
  
  selectInput("leavetype", "Paid leave as fully-paid months:", 
              choices=names(coef)[c(3,5)]),
  hr(),
  hr(),
  plotOutput("bar"),
  
  textOutput("Mother")
  
)

#Define server
server = function(input, output) {
  
  # Fill in the spot we created for a plot
  output$bar <- renderPlot({
    
    # Render a barplot
    par(mar = c(8, 4, 2, 2))
    y=barplot(coef[,input$leavetype],
            main=input$leavetype,
            xlab="<-- Decreases | Increases -->", col=wes_palette("Royal2"), #("Cavalcanti1"),
            xlim=c(-1, 1),
            #names.arg = coef$Explanations, 
            las=2,  cex.main=2, cex.axis=1.2, cex.lab = 1.5, horiz = TRUE)
    x = 0 #0.5*coef[,input$leavetype]
    text(x,y,coef$Explanations, srt=0, cex=1.5)
  }
  
  )
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)