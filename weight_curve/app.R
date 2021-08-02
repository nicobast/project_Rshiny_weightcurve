#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#TODO: 
#- load from list_data
#- look up percentiles automatically (see saved percentiles in folder)
#- publish on shinyapps.io

library(shiny)
library(ggplot2)
default_weight<-40
if(!exists('list_data')){list_data<<-list()} #if it does not exist, create list od data
#if(!exists('weight')){weight<<-as.numeric()} #--> create on first call
weight<<-as.numeric() #delete previous weight data on call

norm_data<-read.delim('bmi_girls_perc_WHO2007.txt',header=T)

# Define UI for application that draws a plot
ui <- fluidPage(
  
  # App title ----
  titlePanel("Gewichtskurve"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput('selectbox_data', 'gespeicherte Daten', choices=names(list_data)),
      
      splitLayout(
        textInput("name", "Vorname:", 'Unbekannt'),
        dateInput("birth.date", 'Geburtsdatum', format = "dd.mm.yyyy")
      ),
    
      dateInput("start_date", 'Datum des Startgewicht', format = "dd.mm.yyyy"),
      numericInput("start.weight", "Startgewicht (in kg):", default_weight, step=0.1),
      numericInput("height", "Koerpergroesse (in cm)", default_weight, step=0.1),
      
      splitLayout(
        numericInput("underweight", "Untergewicht (BMI<17.5):", default_weight+2),
        numericInput("target.weight", "Zielgewicht:", default_weight+4)
      ),
      
      splitLayout(
        numericInput("min_gain_week", "minimale Zunahme (kg/W):", 0.5, step=0.1),
        numericInput("max_gain_week", "maximale Zunahme (kg/W):", 1, step=0.1)
      ),
      
      numericInput("number_weight", "Woche Gewichtsmessung:", NA, min=2),
      sliderInput("add_weight", "naechstes Gewicht:", min=default_weight-5, max=default_weight+15, value=default_weight+0.5, step=0.1),
      # Include clarifying text ----
      helpText("Note: App berechnet Behandlungsdauer anhand einer woechentlichen",
               "Gewichtszunahme von 0.5 kg bis zum Zielgewicht",
               "und zusaetzlich vier Wochen Haltephase")
    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: plot ----
      plotOutput("weightPlot",width='800px', height='600px')
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # #reactive input based on additonal weight measurements
    updateWeight <- function(x,y){
      x[input$number_weight]<-input$add_weight
      x[1]<-input$start.weight
      
        if(length(x)<y){ #if treatment_length changes - adapt length of weight vector accordingly - otherwise this may causes errors with plot drawing
          x<-c(x,rep(NA,y-length(x)))
        }
        if(length(x)>y){ #if treatment_length changes - adapt length of weight vector accordingly - otherwise this may causes errors with plot drawing
          x<-x[1:(length(x)-(length(x)-y))]
        }
      return(x)
      }
    
    updateData <- function(x,y){
      y$date<-format(seq(input$start_date,by=7,length.out = length_treatment()),"%d.%m.%Y") #label 
      x[[input$name]]<-y #double brackets as x is list
      return(x)
    }
    
  #calculate data
  #TODO: calculate automated based on lookup table / norms
  underweight<-reactive({round(input$underweight)})   #https://www.pedz.de/de/bmi.html : calculate BMI 17.5 for weight and height
  target_weight<-reactive({round(input$target.weight)})
  length_treatment<-reactive({round((input$target.weight-input$start.weight)/input$min_gain_week)+4})
  start_weight<-reactive({input$start.weight})
  birth_weight<-reactive({input$birth.date})
  
  
   output$weightPlot <- renderPlot({
    #required variable otherwise,below is not computes - no error is displayed
     req(input$start.weight)
     req(input$underweight)
     req(input$target.weight)

     
     #if(!exists('weight')){weight<<-as.numeric(rep('NA',length_treatment()))} #--> create on first call
     
     
     
     weight<<-updateWeight(weight,length_treatment())
     time<-1:length(weight)
     weight_data<-data.frame(time,weight)
     
     list_data<<-updateData(list_data,weight_data)
     weight_data<-list_data[[input$selectbox_data]] #load data from select box
     
     print(getwd())
     
     #actual weight plot
      ggplot(data=weight_data,aes(x=time,y=weight))+geom_point()+geom_line()+
            #basic graphics
            geom_abline(intercept=weight[1]-input$min_gain_week,slope=input$min_gain_week,color='darkblue')+  #expected minimal gain
            geom_abline(intercept=weight[1]-input$max_gain_week,slope=input$max_gain_week,color='darkblue')+  #expected maximal gain
            geom_hline(yintercept=weight[1],color='red')+ #actual weight / starting weight
            geom_hline(yintercept=underweight(),color='darkorange')+ #out of underweight
            geom_hline(yintercept=target_weight(),color='darkgreen')+ #target weight
            theme_bw(base_size = 16)+
            annotate("text", x = tail(time,1), y = weight[1]-0.2, label = "Ausgangsgewicht", color='red', hjust=1)+
            annotate("text", x = tail(time,1), y = underweight()-0.2, label = "Untergewicht", color='darkorange', hjust=1)+
            annotate("text", x = tail(time,1), y = target_weight()-0.2, label = "Zielgewicht", color='darkgreen', hjust=1)+
            annotate("text", x = time, y = weight+0.2, label = as.character(weight))+
            labs(x='Datum',y='Gewicht (kg)',title=paste('Gewichtskurve von',input$name))+
            ylim(min(weight)-1,max(weight)+2)+  
            scale_y_continuous(breaks = round(seq(min(weight,na.rm=T)-1, target_weight()+2, by = 0.5)))+
            theme(axis.text.x = element_text(angle=45,hjust=1))+ #angle of x axis label - alligned to tick
            scale_x_discrete(name ="Datum", #label x axis with (start) date based on treatment length
                             limits=as.character(format(seq(input$start_date,by=7,length.out = length_treatment()),"%d.%m.%Y"))) #output format according to GER standards
                  
   })
   
   onStop(function(){
     cat("Session stopped\n")
     })
   
 }

# Run the application 
shinyApp(ui = ui, server = server)

