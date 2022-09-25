library(ggplot2)
library(dplyr)
library(shiny)
bc1<-read.csv("./liq.csv",stringsAsFactors = FALSE)

ui <- fluidPage(

  
    titlePanel("BC Liquor Store prices"),
    sidebarLayout(
        sidebarPanel(
          sliderInput("priceip",
                      "Price",
                      min = 0,
                      max = 100,
                      value = c(25,40),
                      pre = "$"),
          radioButtons("typeip",
                       "Product Type",
                       choices = c("Beer", "Refreshment Beverages", "Spirits", "Wine"),
                       selected = "Beer"),
          uiOutput("countryOutput")
        ),
        mainPanel(plotOutput("coolplot"),
                  br(),br(),
                  tableOutput("results")
        )
    )
)


server <- function(input, output) {
  
  filt<-reactive({
    bc1 %>% 
      filter(PRODUCT_PRICE>=input$priceip[1],
             PRODUCT_PRICE<=input$priceip[2],
             ITEM_CATEGORY_NAME==input$typeip,
             PRODUCT_COUNTRY_ORIGIN_NAME==input$country) %>% 
      rename(CATEGORY=ITEM_CATEGORY_NAME,COUNTRY=PRODUCT_COUNTRY_ORIGIN_NAME,PRICE=PRODUCT_PRICE,PRODUCT_NAME=PRODUCT_LONG_NAME,ALCOHOL_CONTENT=PRODUCT_ALCOHOL_PERCENT)
  })
  
  
  output$coolplot<-renderPlot({
    
    if (is.null(filt())) {
      return()
    }
    ggplot(filt(),aes(ALCOHOL_CONTENT))+
      geom_histogram(col="black",fill="coral",bins=10)+
      ggtitle("Histogram")+ylab("Count")
    
  })
  
  output$results<-renderTable({
      
     f1<-filt() %>% 
        select(PRODUCT_NAME,CATEGORY,COUNTRY,PRICE,ALCOHOL_CONTENT)
        
        f1
  })
  
  output$countryOutput <- renderUI({
    selectInput("country", "Country",
                sort(unique(bc1$PRODUCT_COUNTRY_ORIGIN_NAME)),
                selected = "CANADA")
  })
  
  
}



shinyApp(ui = ui, server = server)
