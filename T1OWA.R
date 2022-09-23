library(shiny)
library(shinydashboard)
library(shinyjs)

source('./www/gaussmf.R')
source('./www/trimf.R')
source('./www/genTrapezoidalMFs.R')
source('./www/trapezoidalMFs.R')

source('./www/qualifierMostItvType2.R')
source('./www/fuzzySetWeightsGen.R')

source('./www/agg_gen.r')

#Set up the user interface with the appropriate widgets. 
ui <- dashboardPage(
  dashboardHeader(disable=TRUE), #Disable header
  dashboardSidebar(disable=TRUE), #Disable sidebar 
  dashboardBody(
  # Fluidpage() selected to ensure auto-scaling 
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style2.css"),
      ),
      
# User interface for Tab 1 for setting up the aggregated objects  
      navbarPage(title="T1OWA", fluid=TRUE,
                 
        tabPanel("Aggregated Objects",
                 div(
                   div(
                     sliderInput("slider", h5("Select Domain Range"),
                                 min = 0, max = 500, value = c(0, 200)),
                     
                     div(
                        selectInput("select", h5("Select Type"), 
                                    choices = list("triangular", "trapezoid", "gaussian"),
                                    selected = "triangular"),
                        style="margin-top: -15px;"
                     ),
                     
                     numericInput("num1", label = h5("Param 1"), value = 25),
                     numericInput("num2", label = h5("Param 2"), value = 65),
                     numericInput("num3", label = h5("Param 3"), value = 90),
                     
 #conditional panel to indicate that when Trapezoid is selected, more parameters are to added                    
                     conditionalPanel(
                       condition = 'input.select == "trapezoid"',
                       numericInput("num4", label = h5("Select Param 4"), value = 120),
                       numericInput("num5", label = h5("Select Param 5"), value = 0.5)
                     ),
 #UI for Action button to produce output plot from user inputs from type, domain and numeric inputs                  
                     actionButton("action", label = " Generate Plot", class="btnLm"),
                     class="subDiv1"
                   ),
#Main Panel output                   
                   div(
                     plotOutput("PlotMF"),
                     class="subDiv2"
                   ),
                   
                   class="mainDiv"
                 )
        ),
                 
  #tab 2 for setting up the fuzzy set weights              
        tabPanel("Fuzzy Set Weights",
                 div(
                   div(
                     div(
                       selectInput("select2", h5("Select Singleton Weight Type"), 
                                   choices = list("Meet", "Join", "Mean", "Not use Singleton weight"),
                                   selected = "Meet"),
                     ),
#Conditional panel to dictate that if user picks to not use singleton weights, alternate options for linguistic categories(LW) should be presented                     
                     conditionalPanel(
                       condition = 'input.select2 == "Not use Singleton weight"',
                       
                       selectInput("select3", h5("Select Linguistic Category"), 
                                   choices = list("MeetLike", "JoinLike", "AverageLike"),
                                   selected = "MeetLike"),
                     ), 
#With each linguistic category chosen, a  different set of partition type options should be presented based on inition LW choice                     
                     conditionalPanel(
                       condition = 'input.select2 == "Not use Singleton weight" && input.select3 == "MeetLike"',
                       
                       selectInput("select4", h5("Select Domain Partition Type"), 
                                   choices = list("unEqual Domain Partition", "Equal Domain Partition", "type-2 quantifier"),
                                   selected = "unEqual Domain Partition"),
                     ),
                      
                     conditionalPanel(
                       condition = 'input.select2 == "Not use Singleton weight" && input.select3 == "JoinLike"',
                       
                       selectInput("select5", h5("Select Partition Type"), 
                                   choices = list("unEqual Domain Partition", "Equal Domain Partition"),
                                   selected = "unEqual Domain Partition"),
                     ),
                     
                     conditionalPanel(
                       condition = 'input.select2 == "Not use Singleton weight" && input.select3 == "AverageLike"',
                       
                       selectInput("select6", h5("Select Type"), 
                                   choices = list("unEqual Domain Partition"),
                                   selected = "unEqual Domain Partition"),
                     ),
                     
                     actionButton("action2", label = "Plot", class="btnLm"),
                     
                     class="subDiv1"
                   ),
                   
                   div(
                     plotOutput("PlotFS"),
                     class="subDiv2"
                   ),
                   
                   class="mainDiv"
                 )
        ),
        
#Final tab for plotting the aggregation results        
        tabPanel("Aggregation Results",
                 div(
                   div(
                     actionButton("action3", label = "Plot Aggregation Results", class="btnLm"),
                     class="subDiv1"
                   ),
                   
                   div(
                     plotOutput("PlotAgg"),
                     class="subDiv2"
                   ),
                  
                   class="mainDiv"
                ),
        ), 
      )
    )
  )
)

server <- function(input, output, session) {

#Storing the inputs in reactive values    
  numObj = 3
  control <- reactiveValues(i = 0, j = 0, k = 0)
  btnCntrl <- reactiveValues(i = 0)
  numObjj = reactiveValues(i=1)
  
  tri <- reactiveValues(i = vector("list"))
  tra <- reactiveValues(i = vector("list"))
  gau <- reactiveValues(i = vector("list"))
  A <- reactiveValues(i = vector("list", numObj))
  B <- reactiveValues(i = vector("list", numObj))
  C <- reactiveValues(i = vector("list", numObj))
  
#Adding reactivity  
  membershipVals <- eventReactive(input$action, {
    runif(input$slider)
    btnCntrl$i <- btnCntrl$i + 1
    if(btnCntrl$i > 0){
      numObjj$i <- btnCntrl$i
    }
    
    if(control$i > 0 && control$j > 0 && control$k > 0){
    
      if(input$select == "triangular"){
        print("working triangular")
        parFS = vector("list", numObj);
        
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          A$i[[i]]$mfValue = trimf(domainX,parFS[[i]]);
          A$i[[i]]$domain = domainX;	
          A$i[[i]]$type = c("triMF")
          A$i[[i]]$para = parFS[[i]]	
        }
        plot(domainX, seq(0, 1, length=length(domainX)), xlab="X", ylab="Grade of membership degree", font.lab=1, type ="n") 
        control$i = control$i + 1
        tri$i[[control$i]] = A$i
      }
      
      if(input$select == "trapezoid"){
        print("working trapezoid")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[2]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[3]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        
        for(i in 1:numObj)
        {
          B$i[[i]]$mfValue = trapezoidalMFs(domainX, parFS[[i]]);
          B$i[[i]]$domain = domainX;	
          B$i[[i]]$type = c("trapMF")
          B$i[[i]]$para = parFS[[i]]		
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab="X", ylab = "Grade of membership degree", font.lab = 1, type = "n")
        control$j = control$j + 1
        tra$i[[control$j]] = B$i
      }
      
      if(input$select == "gaussian"){
        print("working gaussian")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length = 100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj) 
        {
          C$i[[i]]$mfValue = gaussmf(domainX, parFS[[i]]);
          C$i[[i]]$domain = domainX;	
          C$i[[i]]$type = c("gaussmf")
          C$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab = "X", ylab = "Grade of membership degree", font.lab=1, type ="n")
        control$k = control$k + 1
        gau$i[[control$k]] = C$i
      }
      
      for(i in 1:length(tri$i)){ 
        for(j in 1:numObj)
        {
          lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
        }
      }
      
      for(i in 1:length(tra$i)){ 
        for(j in 1:numObj)
        {
          lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
        }
      }
      
      for(i in 1:length(gau$i)){ 
        for(j in 1:numObj)
        {
          lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
        }
      }
    }
    
    else if(control$i == 0 && control$j == 0 && control$k == 0){
      
      if(input$select == "triangular"){
        print("working triangular 0")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          A$i[[i]]$mfValue = trimf(domainX,parFS[[i]]);
          A$i[[i]]$domain = domainX;	
          A$i[[i]]$type = c("triMF")
          A$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, width = 20, height = 10, length=length(domainX)), xlab="X", ylab="Grade of membership degree", font.lab=1, type ="n") 
        control$i = control$i + 1 
        tri$i[[control$i]] = A$i
        
        for(i in 1:numObj)
        {
          lines(domainX, A$i[[i]]$mfValue, lwd=2)
        }
      }
      
      if(input$select == "trapezoid"){
        print("working trapezoid")
        parFS = vector("list", numObj);
        
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[2]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[3]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        
        for(i in 1:numObj)
        {
          B$i[[i]]$mfValue = trapezoidalMFs(domainX, parFS[[i]]);
          B$i[[i]]$domain = domainX;	
          B$i[[i]]$type = c("trapMF")
          B$i[[i]]$para = parFS[[i]]		
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab="X", ylab = "Grade of membership degree", font.lab = 1, type = "n") 
        control$j = control$j + 1
        tra$i[[control$j]] = B$i
        
        for(i in 1:numObj)
        {
          lines(domainX, B$i[[i]]$mfValue, lwd=2, col="blue")
        }
      }
      
      if(input$select == "gaussian"){
        print("working gaussian")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length = 100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          C$i[[i]]$mfValue = gaussmf(domainX, parFS[[i]]);
          C$i[[i]]$domain = domainX;	
          C$i[[i]]$type = c("gaussmf")
          C$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab = "X", ylab = "Grade of membership degree", font.lab=1, type ="n") 
        control$k = control$k + 1
        gau$i[[control$k]] = C$i
        
        for(i in 1:numObj)
        {
          lines(domainX, C$i[[i]]$mfValue, col="green", lwd=2)
        }
      }
      
    }
    
    else if(control$i > 0 && control$j == 0 && control$k == 0){
      
      if(input$select == "triangular"){
        print("working triangular")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          A$i[[i]]$mfValue = trimf(domainX,parFS[[i]]);
          A$i[[i]]$domain = domainX;	
          A$i[[i]]$type = c("triMF")
          A$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length=length(domainX)), xlab="X", ylab="Grade of membership degree", font.lab=1, type ="n") 
        control$i = control$i + 1
        tri$i[[control$i]] = A$i
        
        for(i in 1:length(tri$i)){
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
      }
      
      if(input$select == "trapezoid"){
        print("working trapezoid")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[2]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[3]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        
        for(i in 1:numObj)
        {
          B$i[[i]]$mfValue = trapezoidalMFs(domainX, parFS[[i]]);
          B$i[[i]]$domain = domainX;	
          B$i[[i]]$type = c("trapMF")
          B$i[[i]]$para = parFS[[i]]		
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab="X", ylab = "Grade of membership degree", font.lab = 1, type = "n") 
        control$j = control$j + 1
        tra$i[[control$j]] = B$i
        
        for(i in 1:length(tra$i)){
          for(j in 1:numObj)
          {
            lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
          }
        }
          
        for(i in 1:length(tri$i)){
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
      }
      
      if(input$select == "gaussian"){
        print("working gaussian")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length = 100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          C$i[[i]]$mfValue = gaussmf(domainX, parFS[[i]]);
          C$i[[i]]$domain = domainX;	
          C$i[[i]]$type = c("gaussmf")
          C$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab = "X", ylab = "Grade of membership degree", font.lab=1, type ="n") 
        control$k = control$k + 1
        gau$i[[control$k]] = C$i
        
        for(i in 1:length(gau$i)){
          for(j in 1:numObj)
          {
            lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
          }
        }
        
        for(i in 1:length(tri$i)){
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
      }
      
    }
    
    else if(control$i == 0 && control$j > 0 && control$k == 0){
      
      if(input$select == "triangular"){
        print("working triangular")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          A$i[[i]]$mfValue = trimf(domainX,parFS[[i]]);
          A$i[[i]]$domain = domainX;	
          A$i[[i]]$type = c("triMF")
          A$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length=length(domainX)), xlab="X", ylab="Grade of membership degree", font.lab=1, type ="n") 
        
        for(i in 1:numObj)
        {
          lines(domainX, A$i[[i]]$mfValue, lwd=2)
        }
        for(i in 1:numObj)
        {
          lines(domainX, B$i[[i]]$mfValue, col="blue", lwd=2)
        }
        control$i = control$i + 1
        tri$i[[control$i]] = A$i
      }
      
      if(input$select == "trapezoid"){
        print("working trapezoid")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[2]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[3]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        
        for(i in 1:numObj)
        {
          B$i[[i]]$mfValue = trapezoidalMFs(domainX, parFS[[i]]);
          B$i[[i]]$domain = domainX;	
          B$i[[i]]$type = c("trapMF")
          B$i[[i]]$para = parFS[[i]]		
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab="X", ylab = "Grade of membership degree", font.lab = 1, type = "n") 
        control$j = control$j + 1
        tra$i[[control$j]] = B$i
        
        for(i in 1:length(tra$i)){
          for(j in 1:numObj)
          {
            lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
          }
        }
      }
      
      if(input$select == "gaussian"){
        print("working gaussian")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length = 100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          C$i[[i]]$mfValue = gaussmf(domainX, parFS[[i]]);
          C$i[[i]]$domain = domainX;	
          C$i[[i]]$type = c("gaussmf")
          C$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab = "X", ylab = "Grade of membership degree", font.lab=1, type ="n") 
        
        for(i in 1:numObj)
        {
          lines(domainX, C$i[[i]]$mfValue, col="green", lwd=2)
        }
        
        for(i in 1:numObj)
        {
          lines(domainX, B$i[[i]]$mfValue, col="blue", lwd=2)
        }
        control$k = control$k + 1
        gau$i[[control$k]] = C$i
      }
      
    }
    
    else if(control$i == 0 && control$j == 0 && control$k > 0){
      
      if(input$select == "triangular"){
        print("working triangular")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          A$i[[i]]$mfValue = trimf(domainX,parFS[[i]]);
          A$i[[i]]$domain = domainX;	
          A$i[[i]]$type = c("triMF")
          A$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length=length(domainX)), xlab="X", ylab="Grade of membership degree", font.lab=1, type ="n") 
        
        for(i in 1:numObj)
        {
          lines(domainX, A$i[[i]]$mfValue, lwd=2)
        }
        for(i in 1:numObj)
        {
          lines(domainX, C$i[[i]]$mfValue, col="green", lwd=2)
        }
        control$i = control$i + 1
        tri$i[[control$i]] = A$i
      }
      
      if(input$select == "trapezoid"){
        print("working trapezoid")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[2]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[3]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        
        for(i in 1:numObj)
        {
          B$i[[i]]$mfValue = trapezoidalMFs(domainX, parFS[[i]]);
          B$i[[i]]$domain = domainX;	
          B$i[[i]]$type = c("trapMF")
          B$i[[i]]$para = parFS[[i]]		
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab="X", ylab = "Grade of membership degree", font.lab = 1, type = "n") 
        
        for(i in 1:numObj)
        {
          lines(domainX, B$i[[i]]$mfValue, col="blue", lwd=2)
        }
        for(i in 1:numObj)
        {
          lines(domainX, C$i[[i]]$mfValue, col="green", lwd=2)
        }
        control$j = control$j + 1
        tra$i[[control$j]] = B$i
      }
      
      if(input$select == "gaussian"){
        print("working gaussian")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length = 100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          C$i[[i]]$mfValue = gaussmf(domainX, parFS[[i]]);
          C$i[[i]]$domain = domainX;	
          C$i[[i]]$type = c("gaussmf")
          C$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab = "X", ylab = "Grade of membership degree", font.lab=1, type ="n") 
        control$k = control$k + 1
        gau$i[[control$k]] = C$i
        
        for(i in 1:length(gau$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
          }
        }
      }
      
    }
    
    else if(control$i > 0 && control$j > 0 && control$k == 0){
      
      if(input$select == "triangular"){
        print("working triangular")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          A$i[[i]]$mfValue = trimf(domainX,parFS[[i]]);
          A$i[[i]]$domain = domainX;	
          A$i[[i]]$type = c("triMF")
          A$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length=length(domainX)), xlab="X", ylab="Grade of membership degree", font.lab=1, type ="n") 
        control$i = control$i + 1
        tri$i[[control$i]] = A$i
        
        for(i in 1:length(tri$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
        
        for(i in 1:length(tra$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
          }
        }
        
      }
      
      if(input$select == "trapezoid"){
        print("working trapezoid")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[2]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[3]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        
        for(i in 1:numObj)
        {
          B$i[[i]]$mfValue = trapezoidalMFs(domainX, parFS[[i]]);
          B$i[[i]]$domain = domainX;	
          B$i[[i]]$type = c("trapMF")
          B$i[[i]]$para = parFS[[i]]		
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab="X", ylab = "Grade of membership degree", font.lab = 1, type = "n") 
        control$j = control$j + 1
        tra$i[[control$j]] = B$i
        
        for(i in 1:length(tra$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
          }
        }
        
        for(i in 1:length(tri$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
      }
      
      if(input$select == "gaussian"){
        print("working gaussian")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length = 100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          C$i[[i]]$mfValue = gaussmf(domainX, parFS[[i]]);
          C$i[[i]]$domain = domainX;	
          C$i[[i]]$type = c("gaussmf")
          C$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab = "X", ylab = "Grade of membership degree", font.lab=1, type ="n") 
        control$k = control$k + 1
        gau$i[[control$k]] = C$i
        
        for(i in 1:length(gau$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
          }
        }
         
        for(i in 1:length(tra$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
          }
        }
        
        for(i in 1:length(tri$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
      }
      
    }
    
    else if(control$i > 0 && control$j == 0 && control$k > 0){
      
      if(input$select == "triangular"){
        print("working triangular")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          A$i[[i]]$mfValue = trimf(domainX,parFS[[i]]);
          A$i[[i]]$domain = domainX;	
          A$i[[i]]$type = c("triMF")
          A$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length=length(domainX)), xlab="X", ylab="Grade of membership degree", font.lab=1, type ="n") 
        control$i = control$i + 1
        tri$i[[control$k]] = A$i
        
        for(i in 1:length(tri$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
        
        for(i in 1:length(gau$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
          }
        }
      }
      
      if(input$select == "trapezoid"){
        print("working trapezoid")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[2]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[3]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        
        for(i in 1:numObj)
        {
          B$i[[i]]$mfValue = trapezoidalMFs(domainX, parFS[[i]]);
          B$i[[i]]$domain = domainX;	
          B$i[[i]]$type = c("trapMF")
          B$i[[i]]$para = parFS[[i]]		
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab="X", ylab = "Grade of membership degree", font.lab = 1, type = "n") 
        control$j = control$j + 1
        tra$i[[control$j]] = B$i
        
        for(i in 1:length(tra$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
          }
        }
        
        for(i in 1:length(tri$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
        
        for(i in 1:length(gau$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
          }
        }
      }
      
      if(input$select == "gaussian"){
        print("working gaussian")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length = 100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          C$i[[i]]$mfValue = gaussmf(domainX, parFS[[i]]);
          C$i[[i]]$domain = domainX;	
          C$i[[i]]$type = c("gaussmf")
          C$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab = "X", ylab = "Grade of membership degree", font.lab=1, type ="n") 
        control$k = control$k + 1
        gau$i[[control$k]] = C$i
        
        for(i in 1:length(gau$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
          }
        }
        
        for(i in 1:length(tri$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
      }
      
    }
    
    else if(control$i == 0 && control$j > 0 && control$k > 0){
      
      if(input$select == "triangular"){
        print("working triangular")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          A$i[[i]]$mfValue = trimf(domainX,parFS[[i]]);
          A$i[[i]]$domain = domainX;	
          A$i[[i]]$type = c("triMF")
          A$i[[i]]$para = parFS[[i]]	
        }
        
        plot(domainX, seq(0, 1, length=length(domainX)), xlab="X", ylab="Grade of membership degree", font.lab=1, type ="n") 
        control$i = control$i + 1
        tri$i[[control$i]] = A$i
        
        for(i in 1:length(tri$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tri$i[[i]][[j]]$mfValue, lwd=2)
          }
        }
        
        for(i in 1:length(tra$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
          }
        }
          
        for(i in 1:length(gau$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
          }
        }
      }
      
      if(input$select == "trapezoid"){
        print("working trapezoid")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length=100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[2]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        parFS[[3]] = c(input$num1, input$num2, input$num3, input$num4, input$num5);
        
        for(i in 1:numObj)
        {
          B$i[[i]]$mfValue = trapezoidalMFs(domainX, parFS[[i]]);
          B$i[[i]]$domain = domainX;	
          B$i[[i]]$type = c("trapMF")
          B$i[[i]]$para = parFS[[i]]		
        }
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab="X", ylab = "Grade of membership degree", font.lab = 1, type = "n") 
        control$j = control$j + 1
        tra$i[[control$j]] = B$i
        
        for(i in 1:length(tra$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
          }
        }
        for(i in 1:length(gau$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
          }
        }
      }
      
      if(input$select == "gaussian"){
        print("working gaussian")
        parFS = vector("list", numObj);
        domainX = seq(input$slider[1], input$slider[2], length = 100);
        
        parFS[[1]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[2]] = c(input$num1, input$num2, input$num3);
        parFS[[3]] = c(input$num1, input$num2, input$num3);
        
        for(i in 1:numObj)
        {
          C$i[[i]]$mfValue = gaussmf(domainX, parFS[[i]]);
          C$i[[i]]$domain = domainX;	
          C$i[[i]]$type = c("gaussmf")
          C$i[[i]]$para = parFS[[i]]	
        }
        control$k = control$k + 1
        gau$i[[control$k]] = C$i
        
        plot(domainX, seq(0, 1, length = length(domainX)), xlab = "X", ylab = "Grade of membership degree", font.lab=1, type ="n") 
        
        for(i in 1:length(gau$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, gau$i[[i]][[j]]$mfValue, col="green", lwd=2)
          }
        }
        
        for(i in 1:length(tra$i)){ 
          for(j in 1:numObj)
          {
            lines(domainX, tra$i[[i]][[j]]$mfValue, col="blue", lwd=2)
          }
        }
      }
      
    }
    
  })
  
  output$PlotMF <- renderPlot({
    
    membershipVals()
    
  }, height=560)
  
  #Making Tab2 reactive
  #Storing swOpt & lwOpt
  swOpt = reactiveValues(i=1)
  lwOpt = reactiveValues(i=0)
  specificWt = reactiveValues(i=0)
  
  observeEvent(input$select2, {
    if(input$select2 == "Meet"){
      swOpt$i = 1
    }
    else if(input$select2 == "Join"){
      swOpt$i = 2
    }
    else if(input$select2 == "Mean"){
      swOpt$i = 3
    }
    else{
      swOpt$i = 4
    }
  })
  
  observeEvent(input$select3, {
    if(input$select3 == "MeetLike"){
      lwOpt$i = 1
    }
    else if(input$select3 == "JoinLike"){
      lwOpt$i = 2
    }
    else if(input$select3 == "AverageLike"){
      lwOpt$i = 3
    }
  })
  
  observeEvent(input$select4, {
    if(input$select4 == "unEqual Domain Partition"){
      specificWt$i = 1
    }
    else if(input$select4 == "Equal Domain Partition"){
      specificWt$i = 2
    }
    else if(input$select4 == "type-2 quantifier"){
      specificWt$i = 3
    }
  }) 
  
  observeEvent(input$select5, {
    if(input$select5 == "unEqual Domain Partition"){
      specificWt$i = 1
    }
    else if(input$select5 == "Equal Domain Partition"){
      specificWt$i = 2
    }
  })
  
  observeEvent(input$select6, {
    if(input$select6 == "unEqual Domain Partition"){
      specificWt$i = 1
    }
  })
  
  task2 <- eventReactive(input$action2, {
    fuzzySetWeightsGen(numObjj$i, swOpt$i, lwOpt$i, specificWt$i)
  })
  
  output$PlotFS <- renderPlot({
    task2()
  }, height=560)
  
  
  #plotting the aggregation results
  task3 <- eventReactive(input$action3, {
    aggregateGen(numObjj$i, swOpt$i, lwOpt$i, specificWt$i, tri, tra, gau)
  })
  
  output$PlotAgg <- renderPlot({
    task3()
  }, height=560)
  
}

shinyApp(ui = ui, server = server)

