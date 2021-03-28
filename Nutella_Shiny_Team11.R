## app.R ##
library(shiny)
library(shinydashboard)
library(DT)

#install.packages("shiny")
#install.packages("shinydashboard")

ui <- dashboardPage( skin = "black",
        dashboardHeader(title = "Nutella Analysis"),
        dashboardSidebar(
          sidebarMenu(
            menuItem("General", tabName = "general", icon = icon("dashboard")),
            menuItem("Token Frequency", tabName = "frequency", icon = icon("wave-square")),
            menuItem("Phrase Analysis", tabName = "correlogram", icon = icon("chart-bar")), #th
            menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("smile"))
          )
        ),
  dashboardBody(
    tabItems(
      
      #general tab content
      tabItem( tabName = "general",h2("Survey Overview"),
               
               fluidRow(
               valueBox(34, subtitle = "Survey Participants", icon = icon("users")),
               valueBox(26, subtitle = "People that want to buy Nutella", icon = icon("thumbs-up")),
               valueBox(round((26 / 34)*100, 2), subtitle = "Nutella Acceptance (%)", icon = icon("check"),color= "green")
               ),
               fluidRow(
                 
               )
      ),
      
      # First tab content
      tabItem(tabName = "frequency",  h2("Token Frequency in Responses"),
        # Boxes need to be put in a row (or column)
        fluidRow(
          column(width = 8,
        box(title = "Histogram", status = "primary", solidHeader = TRUE, collapsible = T,
            plotly::plotlyOutput('Frequency_Tokens'), width = 12),
        box(title = "Frequency Table", status = "primary", solidHeader = T, width = 12,
            tableOutput('Frequency_Token'), collapsible = T)
          ),
        column(width = 4,
        box(
          title = "Controls", status = "warning", solidHeader = TRUE, width = 12,
          sliderInput('keywords', 'Number of Keywords', 1, 20, 10))
        )
        )
        
        
      ),
  
      # Second tab content
      tabItem(tabName = "correlogram",
              h2("Phrase Analysis"),
        
              fluidRow( 
        box(title = "Correlogram", status="primary", solidHeader = TRUE, collapsible = TRUE,
            plotly::plotlyOutput('Correlogram'), width = 8),
        box(title = "Explanation", status = "warning", width = 4,
            "The keywords over the diagonal line are used significantly more in 
            answers of people that do not want to buy nutella.
            The keywords underneith the diagonal line are used by people that like to buy Nutella.")),
        
              fluidRow(
        box(title = "Trigrams",status= "primary", solidHeader = TRUE, width = 8, collapsible = TRUE,
            plotOutput('Trigram')),
        box(title = "Explanation", status = "warning", width = 4,
            "Interesting phrases that stand out are: 1. Chocolare, greasy, stuff, 
            Hamburgers and fries. 2. Glazed, banana, whipped ")),
        
              fluidRow(
        box(title = "Correlation Network", status = "primary", solidHeader = T, width = 8,
            plotOutput('Corr_Network'), collapsible = TRUE),
        box(title= "Controls", status="warning", solidHeader=T, width = 4,
            sliderInput('corr', "Minimum Correlation", 0, 0.7, 0.5))
              )
        
      ),
  
      # Third tab content
      tabItem(tabName = "sentiment",
            h2("Sentiment Analysis"),
          
            fluidRow(
              box(title = "Frecuency Sentiment", status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  tableOutput('Frequency_Sentiment')),
              
              box(
                title = "Controls", status = "warning", solidHeader = TRUE,
                selectInput("positive_negative", "Select Sentiment", c("Positive", "Negative")))
                
            ),
            
            fluidRow(
              box(title = "Sentiment Words", status = "primary", 
                  solidHeader = TRUE, collapsible = TRUE,
                  tableOutput('Word_Sentiment')),
              
              box(
                title = "Controls", status = "warning", solidHeader = TRUE,
                selectInput("joy_sad", "Show top words that bring", c("Joy", "Sadness")))
            ) 
          
      )
    )
  )
)

server <- function(input, output) {
  frequency_tokens <- function (){
    tidy_nutella %>%
    count(word, sort=TRUE) %>%
    head(input$keywords) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col()
    }
 
  output$Frequency_Tokens <- plotly::renderPlotly({frequency_tokens()})
  
  frequency_token <-function(){
    tidy_nutella %>%
    count(word, sort=TRUE) %>%
    head(input$keywords)
  }
  output$Frequency_Token <- renderTable({frequency_token()})
  
  correlogram <- function() {
    ggplot(frequency, aes(x=proportion, y=`Biz Failure`, 
                          color = abs(`Biz Failure`- proportion)))+
      geom_abline(color="grey40", lty=2)+
      geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
      geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
      scale_x_log10(labels = percent_format())+
      scale_y_log10(labels= percent_format())+
      scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
      facet_wrap(~author, ncol=4)+
      theme(legend.position = "none")+
      labs(y= "Biz Failure", x=NULL)
  }
  output$Correlogram <-plotly::renderPlotly({correlogram()})
  
  trigram <- function(){
    ggraph(trigram_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                     arrow = a, end_cap = circle(.07, 'inches')) +
      geom_node_point(color = "lightsalmon2", size = 3) +
      geom_node_text(aes(label = name, size = input$size), vjust = 1, hjust = 1) +
      theme_void()
    
  }
  output$Trigram <- renderPlot({trigram()})
  
  corr_network <- function(){
    word_cors %>%
      filter(correlation >input$corr) %>%  
      graph_from_data_frame() %>%
      ggraph(layout = "fr")+
      geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
      geom_node_point(color = "lightsalmon1", size= 6)+
      geom_node_text(aes(label=name), repel=T)+
      theme_void()
  }
  output$Corr_Network <- renderPlot({corr_network()})
  
  sentiment_table <- function() {
    if (input$positive_negative == "Positive"){
      og_nutella_success %>%
        inner_join(afinn) %>%
        mutate(tokenfreqsentiment = n*value) %>%
        arrange(desc(tokenfreqsentiment)) %>%
        head(10)
    }
    else {
      og_nutella_success %>%
        inner_join(afinn) %>%
        mutate(tokenfreqsentiment = n*value) %>%
        arrange(desc(-tokenfreqsentiment)) %>%
        head(10)
    }
  }
  output$Frequency_Sentiment <-renderTable({sentiment_table()})
  
  joy_sadness <-function(){
    if (input$joy_sad == "Joy") {
      nrc <- get_sentiments("nrc") %>%  
        filter(sentiment == "joy")
      
      og_nutella_success %>%
        inner_join(nrc) %>%  			
        count(word, sort=T) %>%
        head(5)
    }
    else {
      nrc <- get_sentiments("nrc") %>%  
        filter(sentiment == "sadness")
      
      og_nutella_success %>%
        inner_join(nrc) %>%  			
        count(word, sort=T) %>%
        head(5)
    }
  }
  output$Word_Sentiment <- renderTable({joy_sadness()})
  
}

shinyApp(ui, server)