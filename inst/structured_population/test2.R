library(shiny)
library(ggplot2)
library(gridExtra)

u <- shinyUI(fluidPage(
  titlePanel("title panel"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("donum1", "Make #1 plot", value = T),
                             checkboxInput("donum2", "Make #2 plot", value = F),
                             checkboxInput("donum3", "Make #3 plot", value = F),
                             sliderInput("wt1","Weight 1",min=1,max=10,value=1),
                             sliderInput("wt2","Weight 2",min=1,max=10,value=1),
                             sliderInput("wt3","Weight 3",min=1,max=10,value=1)
                ),
                mainPanel("main panel",
                          column(6,plotOutput(outputId="plotgraph", width="500px",height="400px"))
                ))))

s <- shinyServer(function(input, output) 
{
  set.seed(123)
  pt1 <- reactive({
    if (!input$donum1) return(NULL)
    qplot(rnorm(500),fill=I("red"),binwidth=0.2,main="plotgraph1")
  })
  pt2 <- reactive({
    if (!input$donum2) return(NULL)
    qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="plotgraph2")
  })
  pt3 <- reactive({
    if (!input$donum3) return(NULL)
    qplot(rnorm(500),fill=I("green"),binwidth=0.2,main="plotgraph3")
  })
  output$plotgraph = renderPlot({
    ptlist <- list(pt1(),pt2(),pt3())
    wtlist <- c(input$wt1,input$wt2,input$wt3)
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    wtlist <- wtlist[to_delete]
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
  })
})
shinyApp(u,s)