library(shiny)
library(janitor)
library(ggplot2)
library(forcats)
library(shinythemes)

ui=fluidPage(
  
  theme=shinytheme("yeti"),
  
  titlePanel("Spanish wine Dashboard"),
  
  sidebarLayout(
    sidebarPanel(width=3, height=3,
      sliderInput("percentile", "Please select the cutoff", min=0.01, max=1, value=0.9), 
      numericInput("num.most.expensive", "Select the number of type", min=1, max=22, value=10),
      #radio button and drop-down menu, choices - name of the categorical varibles 
      #radioButtons("cat.var", "Select the Variable", choices=c("Winery"="winery", "Region"="region", "Type"="Type")),
      selectInput("cat.var", "Select the Variable", choices=c("Winery"="winery", "Region"="region", "Type"="Type")),
      sliderInput("min.wines", "Select Number of Wines", min=50, max=300, value=250, step=25)), 
    
    #designate row, use fluidrow
    mainPanel(
      fluidRow(
        column(6, plotOutput("pricehistogram")), 
        column(6, plotOutput("barchart"))),
      
      fluidRow(
        column(6, tableOutput("table")), 
        column(6, plotOutput("stackbar")))
    )
  #plotOutput("pricehistogram"), 
  #plotOutput("barchart"),
  #tableOutput("table"), 
  #plotOutput("stackbar"))
  )
)

#page 
server=function(input, output, session){
  knitr::opts_knit$set(root.dir = '~/Downloads/Field Project/class 2')  
  wines=read.csv("Spanish_wines copy.csv")
  type_mapping=read.csv("Wine_type_mapping copy.csv")
  
  wines.1=merge(wines, type_mapping, by.x="type", by.y="Code", all.x = TRUE)
  
  wines=wines.1
  rm(wines.1)
  
  wines$type=NULL
  
  #Fill in/replace the missing values with the word 'Unknown'
  
  wines$Type[is.na(wines$Type)]="Unknown"
  
  #renderPlot(hist(wines$price[wines$price<quantile(wines$price,input$percentile)], col="beige", xlab="Price", main="Price Histogram")) 
  # Run = Error in input$percentile : Can't access reactive value 'percentile' outside of reactive consumer.
  # wrap renderPlot around the hist, it's a reactive function, correlated with plotOutput, i.e. tableOutput, renderTable
  
  output$pricehistogram=renderPlot(hist(wines$price[wines$price<quantile(wines$price,input$percentile)], col="beige", xlab="Price", main="Price Histogram"))
  
  
  
  ave.price.by.type=aggregate(price~Type, wines, FUN=mean)
  type.table=tabyl(wines, Type) #tabyl function comes from the 'janitor' package
  
  ave.price.table=merge(ave.price.by.type, type.table, by="Type")
  ave.price.table=ave.price.table[order(ave.price.table$price, decreasing = TRUE),]
  
  output$barchart=renderPlot({
    
    req(input$num.most.expensive) #only recompute whatever in the renderplot function only if something is selected - legitimate selection 
    
    top=ave.price.table[1:input$num.most.expensive,] #We'll change this in Shiny
  
    ggplot(top, aes(x=reorder(Type, -price), y=price, fill=n))+
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 90))+
      xlab("Types")+
      ggtitle("Barplot of Type vs Price")+
      geom_text(aes(label=n), vjust=-0.1)
    
    })
  
  
output$table=renderTable({
    
   mytable=tabyl(wines, input$cat.var) #tabyl function comes from the 'janitor' package
   mytable=mytable[order(mytable$n, decreasing = TRUE),]
   mytable[1:10,]
  })
  

med.price=median(wines$price)
wines$price.bin=ifelse(wines$price>med.price, "Above", "Below")

output$stackbar=renderPlot({
  wines$winery.top=fct_lump_min(wines$winery, input$min.wines)
  
  ggplot(wines, aes(x = winery.top, fill = price.bin)) +
    geom_bar(position = "fill") +
    xlab("Winery") +
    ylab("Proportion")+
    ggtitle("Above vs Below by Winery")+
    theme(axis.text.x=element_text(angle=90))+
    labs(fill="Price Category")
})

  
}

#put things together
shinyApp(ui, server)

