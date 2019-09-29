library(shiny)
#install.packages("d3heatmap")
library(d3heatmap)
library(shinythemes)
library(RColorBrewer)

fluidPage(
  theme=shinytheme("sandstone"),
  navbarPage(
    
      # <--- To use a theme, uncomment this
    "Smashing Toolbox",
    
    #TAB UPLOAD      
    tabPanel("Data Upload",
             
             titlePanel("Uploading Files"),
             
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Select a file ----
                 fileInput("file1", "Choose CSV File",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Checkbox if file has header ----
                 checkboxInput("header", "Header", TRUE),
                 
                 # Input: Select separator ----
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 
                 # Input: Select quotes ----
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select number of rows to display ----
                 radioButtons("disp", "Display",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head")
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Data file ----
                 DT::dataTableOutput("contents")
                 
               )
               
             )
             
             
             
             
    ),# finish TAB UPLOAD
    
    #TODO: function: data preparation and cleasing
    
    #TAB K-MEANS      
    tabPanel("K-means", 
             
             #SIDEBAR PANEL
             sidebarPanel(
               
               selectInput('xcol', 'X Variable', ""),
               selectInput('ycol', 'Y Variable', ""),


               sliderInput("slider_k", "Number of clusters to be analysed:", 1, 20 , 15),
               sliderInput("slider_nstart", "Choose a nstart:", 1, 100,50),
               sliderInput('clusters', "Choose a cluster", 1, 20, 3)
             ),
             
             #MAIN PANEL
             mainPanel(
               tabsetPanel(
                 tabPanel("Plots",
                          #h4("Scree Plot"),
                          plotOutput("distPlot"),
                          #h4("K-mean Clustering Plot"),
                          plotOutput("dfClusterPlot")
                          
                          
                 ),
                 tabPanel("Informations",
                          
                          h4("What it is:"),
                          "Unsupervised, parametric method (need to pre-specify K number of clusters).",
                          
                          
                          h4("When:"),
                          "First exploration of multidimensional data (few assumptions needed, i.e., K).",
                          h4("What it does:"),
                          "Groups data based on their similarity, the resulting clusters have similar properties
                          Push the data into K pre-determined categories (can be an advantage in certain situations)",
                          h4("How it does this"),
                          
                          "Choose a number K of cluster centers
                          Place the centers randomly in the data space (initial cluster assignment)
                          Iteratively move the centers to minimize the total within cluster variance (data-center distance) "  
                          
                 )
               ) #end tabsetPanel
               ) #end main panel
               ), #end TAB K-MEAN
    
    
    
    
    
  
    
    #TAB HC
    tabPanel("HC",
             #SIDEBAR PANEL
             sidebarPanel(
               
               selectInput("method_dis", "Distance methods:",
                           c("Euclidean" = "euclidean",
                             "Maximum" = "maximum",
                             "Manhattan" = "manhattan",
                             "Canberra" = "canberra",
                             "Binary"= "binary",
                             "Minkowski"="minkowski")),

               selectInput("method_link", "Linkage methods:",
                           c("ward.D" = "ward.D",
                             "ward.D2" = "ward.D2",
                             "single" = "single",
                             "complete" = "complete",
                             "average"= "average",
                             "mcquitty"="mcquitty",
                             "median"="median",
                             "centroid"="centroid")),    

               
               checkboxInput("transpose", "Check it to get a tree for the predictors (table column)", FALSE)
               
             ), #end SIDEBAR PANEL
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Plots",
                          
                          plotOutput("dendPlot"),
                          
                          #d3heatmapOutput("heatmap", height="8000px", width="100%")
                          plotOutput("heatmatPlot")
                  
                          
                 ),
                 tabPanel("Informations",
                          
                          h4("What it is:"),
                          "Unsupervised, parametric method (need to pre-specify K number of clusters).",
                          
                          
                          h4("When:"),
                          "First exploration of multidimensional data (few assumptions needed, i.e., K).",
                          h4("What it does:"),
                          "Groups data based on their similarity, the resulting clusters have similar properties
                          Push the data into K pre-determined categories (can be an advantage in certain situations)",
                          h4("How it does this"),
                          
                          "Choose a number K of cluster centers
                          Place the centers randomly in the data space (initial cluster assignment)
                          Iteratively move the centers to minimize the total within cluster variance (data-center distance) "  
                          
                 )
               ) #end tabsetPanel
               ) #end main panel
               
            
             )#end tabPanel TAB HC
    
             )
    
)
  
  
  
  
  
  
  
  
  
  
  
  