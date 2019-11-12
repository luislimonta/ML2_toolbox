library(shiny)
#install.packages("d3heatmap")
library(d3heatmap)
library(shinythemes)
library(RColorBrewer)

fluidPage(
  theme=shinytheme("readable"),
  navbarPage(
    "Smashing Toolbox",
    
    # TAB UPLOAD ----     
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
             
            
             
    ), # finish TAB UPLOAD ----
    
    
    
    
    # TAB K-MEANS   ----   
    tabPanel("K-means", 
             
             # Sidebar panel for inputs  ----
             absolutePanel(
               bottom = 20, right = 20, width = 200,
               draggable = TRUE,
               style = "opacity: 0.92",
               
               selectInput('xcol', 'X Variable', ""),
               selectInput('ycol', 'Y Variable', ""),


               sliderInput("slider_k", "Number of clusters to be analysed:", 1, 20 , 15),
               sliderInput("slider_nstart", "Choose a nstart:", 1, 100,50),
               sliderInput('clusters', "Choose a cluster", 1, 20, 3)
             ),
             
             # MAIN PANEL ----
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
    
    
    
    
    
  
    
    
    #TAB HC ----
    tabPanel("HC",
             #SIDEBAR PANEL ----
             absolutePanel(
               bottom = 20, right = 20, width = 150,
               draggable = TRUE,
               style = "opacity: 0.92",
               
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
             
             # MAIN PANEL ----
             mainPanel(
               tabsetPanel(
                 # tab Plots ----
                 tabPanel("Plots",
                          
                          plotOutput("dendPlot",width = "120%",height="800px"),
                          
                          #d3heatmapOutput("heatmap", height="8000px", width="100%"),
                          plotOutput("heatmatPlot",height="800px",width = "120%")
                  
                          
                 ),
                 # tab 2 ----
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
               
            
             ),#end tabPanel TAB HC
    
    
    #TAB PCA ----
    tabPanel("PCA",
             # # Sidebar panel for inputs ----
             # sidebarPanel(
             #   "sidepanel"
             # ), #end SIDEBAR PANEL
             
             # MAIN PANEL ----
             mainPanel(
               tabsetPanel(
                 # Tab plot ----
                 tabPanel("Plots",
                          plotOutput("biPlot",width = "150%",height="800px"),
                          sliderInput("slider_midpoint", "midpoint:", 0, 10 , 0.5,step = 0.1),
                          plotOutput("scree.pca.Plot"),
                          plotOutput("pvePlot"),
                          plotOutput("cumulative.pvePlot"),
                          verbatimTextOutput("rotation.matrix")
                          #plotOutput("starPlot")
                  
                 ),
                # Tab Info ----
                 tabPanel("Informations",
                          h4("What it is:"),
                          "Unsupervised, linear, non-parametric method.",
                          
                          h4("When:"),
                          "First exploration of multidimensional data (no assumptions needed). Data driven!",
                          
                          h4("What it does:"),
                          "PCA reduces the dimensionality of a dataset in order to be more understandable representation.",
                          
                          h4("How it does this"),
                          "Find new axes (principal components) that represent the data space in a reduced set of dimensions 
                          in order capture the most important information in the data.",
                          h4(""),
                          "Capture the maximal variance of the data.",
                          h4(""),
                          "Highlight the global patterns in the data set." 
  
                 ) #end tabPanel
               ) #end tabsetPanel
               ) #end main panel
    ), #end PCA panel
    
    
    #TAB TSNE ----      
    tabPanel("tSNE",
             #SIDEBAR PANEL
             absolutePanel(
               bottom = 20, right = 20, width = 150,
               draggable = TRUE,
               style = "opacity: 0.92",
               selectInput('category.tSNE', 'Select a category', ""),
               sliderInput("slider_dim", "Dimension:", 1, 3 , 2),
               sliderInput("slider_perplexity", "Perplexity:", 1, 50 , 10),
               sliderInput("slider_max_iter", "Max iteration:", 1, 2000 , 500)
             ), #end SIDEBAR PANEL
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Plots",
                          plotOutput("tSNEPlot",width = "130%",height="800px")
                 ),
                 tabPanel("Informations",
                          h4("What it is:"),
                          "Unsupervised, linear, non-parametric method.",
                          
                          h4("When:"),
                          "First exploration of multidimensional data (no assumptions needed). Data driven!",
                          
                          h4("What it does:"),
                          "PCA reduces the dimensionality of a dataset in order to be more understandable representation.",
                          
                          h4("How it does this"),
                          "Find new axes (principal components) that represent the data space in a reduced set of dimensions 
                          in order capture the most important information in the data.",
                          h4(""),
                          "Capture the maximal variance of the data.",
                          h4(""),
                          "Highlight the global patterns in the data set." 
                          
                 ) #end tabPanel
               ) #end tabsetPanel
             ) #end main panel
    ), #end tSNEpanel        
               
    
    #TAB SOMs ----
    tabPanel("SOMs",
             #SIDEBAR PANEL
             sidebarPanel(
               selectInput('category.SOMs', 'Select a set of feature to be classified', "",multiple = TRUE),
               sliderInput("slider_xdim", "xdim", 1, 50 , 4),
               sliderInput("slider_ydim", "ydim:", 1, 50 , 4),
               checkboxInput("check_scaled", label = "Check if you want scaled data", value = TRUE),
               #sliderInput("slider_max_iter", "Max iteration:", 1, 2000 , 500)
             ), #end SIDEBAR PANEL

             mainPanel(
               tabsetPanel(
                 tabPanel("Plots",
                          plotOutput("somsPlot.change"),
                          plotOutput("somsPlot.count"),
                          plotOutput("somsPlot.mapping"),
                          plotOutput("somsPlot.dist"),
                          plotOutput("somsPlot.codes"),
                          selectInput('property.SOMs', 'Select a feature to be classified', ""),
                          plotOutput("somsPlot.property"),
                          plotOutput("somsPlot.tree",width = "150%"),
                          numericInput("soms.tree.h", "Height (h) to cut the tree", 1, min = 1, max = 10000),
                          plotOutput("somsPlot.map.hc")
                 ),
                 tabPanel("Informations",
                          h4("What it is:"),
                          "Unsupervised, linear, non-parametric method.",

                          h4("When:"),
                          "First exploration of multidimensional data (no assumptions needed). Data driven!",

                          h4("What it does:"),
                          "PCA reduces the dimensionality of a dataset in order to be more understandable representation.",

                          h4("How it does this"),
                          "Find new axes (principal components) that represent the data space in a reduced set of dimensions
                          in order capture the most important information in the data.",
                          h4(""),
                          "Capture the maximal variance of the data.",
                          h4(""),
                          "Highlight the global patterns in the data set."

                 ) #end tabPanel
               ) #end tabsetPanel
             ) #end main panel
    ) #end SOMs panel

             )#end navbarpage
)#end Fluid page
  
  
  
  
  
  
  
  
  
  
  
  