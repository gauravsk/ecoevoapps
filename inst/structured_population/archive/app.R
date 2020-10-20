library(shiny); library(diagram)

# Define UI for application that draws a histogram
ui = fluidPage(
  
  # Application title
  titlePanel("Age-structured population growth"),
  
  # Sidebar with a slider input for distance 
  sidebarLayout(
    sidebarPanel(
      # Initial size
      numericInput(inputId = "N1",
                  label = "Initial number of individuals at age 1",
                  min = 0,
                  max = NA,
                  value = 100),
      numericInput(inputId = "N2",
                   label = "Initial number of individuals at age 2",
                   min = 0,
                   max = NA,
                   value = 100),
      numericInput(inputId = "N3",
                   label = "Initial number of individuals at age 3",
                   min = 0,
                   max = NA,
                   value = 100),
      
      # Leslie matrix
      numericInput(inputId = "F1",
                  label = "Fecundity at age 1",
                  min = 0,
                  max = NA,
                  value = 0),
      numericInput(inputId = "F2",
                  label = "Fecundity at age 2",
                  min = 0,
                  max = NA,
                  value = 8),            
      numericInput(inputId = "F3",
                  label = "Fecundity at age 3",
                  min = 0,
                  max = NA,
                  value = 1),
      sliderInput(inputId = "S12",
                  label = "Survival from age 1 to 2 (%)",
                  min = 1,
                  max = 100,
                  value = 10),
      sliderInput(inputId = "S23",
                  label = "Survival from age 2 to 3 (%)",
                  min = 0,
                  max = 100,
                  value = 80),
      width=4),
    
    # Show the plots
    mainPanel(
      p("Leslie matrix"),
      tableOutput(outputId = "leslie"),
      p("_____________________"),
      p("Population growth rate"),
      textOutput(outputId = "eigenvalue"),
      p("_____________________"), 
      p("Stable population structure"),
      textOutput(outputId = "eigenvector"),
      plotOutput(outputId = "plots"),
      width=8
      )
    )
  )


# Define server logic
server = function(input, output) {
  
  # Leslie matrix
  output$leslie = renderTable({
    L = matrix(
      c("Age 1", input$F1     ,      input$F2, input$F3,
        "Age 2", input$S12/100,             0,        0,
        "Age 3",             0, input$S23/100,        0),
      ncol = 4, 
      byrow = T, 
      dimnames = list(NULL, c(" ","Age 1","Age 2", "Age 3"))
      )
    })
  
  # Eigen value
  output$eigenvalue = renderText({
    L = matrix(
      c(input$F1     ,      input$F2, input$F3,
        input$S12/100,             0,        0,
        0, input$S23/100,        0),
      ncol = 3, byrow = T )
    eigenval = round(eigen(L)$values,2)
    paste0("Eigenvalue = ",eigenval[1])
    })
  
  # Eigen vector
  output$eigenvector = renderText({
    L = matrix(
      c(input$F1     ,      input$F2, input$F3,
        input$S12/100,             0,        0,
                    0, input$S23/100,        0),
      ncol = 3, byrow = T )
    eigenvec = round(eigen(L)$vectors[,1]*100/sum(eigen(L)$vectors[,1]),2)
    paste0("Eigenvector = ",eigenvec[1],"; ",eigenvec[2],"; ",eigenvec[3])
    })
  
  # Plots
  output$plots = renderPlot({
    # Leslie matrix
    L = matrix(
      c(input$F1     ,      input$F2, input$F3,
        input$S12/100,             0,        0,
                    0, input$S23/100,        0),
      ncol = 3, byrow = T )
    
    # Population growth simulation
    pop = matrix(c(input$N1,input$N2,input$N3,sum(c(input$N1,input$N2,input$N3))),nrow=4,ncol=1) # start the matrix with initial population sizes
    lambda = numeric() # discrete population growth
    for(i in 1:1000){
      pop = cbind(pop, rbind(round(L %*% pop[1:3,i,drop=F]),NA)) # run one time-step of the model
      pop[4,i+1] = sum(pop[1:3,i+1])
      lambda[i] = pop[4,i+1]/pop[4,i]
      if(i>10){if(round(lambda[i],3)==round(lambda[i-1],3)){break}} 
      }

    # Stable age structure
    #round(pop[1:3,ncol(pop)]*100/sum(pop[1:3,ncol(pop)]),2)
    
    par(mfrow = c(1,2))
    
    # Diagram
    name = c(expression(Age[1]), expression(Age[2]), expression(Age[3]) )
    plotmat(A = L, pos = 3, curve = 0.6, name = name, lwd = 1.5, my = 0,
            arr.len = 0.2, arr.width = 0.25, arr.lwd = 2, arr.type = "simple",
            self.lwd = 2, self.shiftx = 0.115, self.shifty = 0.1, self.cex = .5,
            box.size = 0.1, dtext = 0.2, box.lwd = 3.5, 
            )
    
    # Plot
    plot(x = 1, y = 1, type = "n", log = "y",
         xlab = "time steps", ylab = "number of individuals + 1",
         xlim = c(0,i), ylim = c(1,max(pop)),
         main = "Simulation")
    #abline(v = 0:i, col = "grey", lty = 3)
    points(x = 0:i, pop[1,]+1 ,type = "b", lty = 3, pch =  1, cex = .7) 
    points(x = 0:i, pop[2,]+1 ,type = "b", lty = 4, pch = 15, cex = .7) 
    points(x = 0:i, pop[3,]+1 ,type = "b", lty = 2, pch =  6, cex = .7) 
    points(x = 0:i, pop[4,]+1 ,type = "l", lty = 1, pch = 14, cex = .7, lwd=2)
    mtext(side = 3, at = par("usr")[1], "Age:", adj = 0, line = .1)
    par(fig=c(0, 1, 0, 1), oma=c(0, 0, 2.5, 2.5), mar=c(0, 0, 0, 0), new=TRUE)
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend("topright", lty=c(3,4,2,1), 
           pch=c(1,15,6,NA), lwd = c(1,1,1,2), bty = "n",
           c("1","2","3","All"), horiz = T)

    })
  
  }


# Run the application 
shinyApp(ui = ui, server = server)



