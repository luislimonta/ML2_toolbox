#Import packages
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(d3heatmap)
library(ggplot2)
library(Rtsne)
library(plotly)
library(kohonen)
library(d3heatmap)
library(devtools)
library(factoextra)

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
                 #checkboxInput("reduceThFactors", "Reduce factors to values", FALSE),
                 #checkboxInput("replaceNull", "Replace NULLs to medians", FALSE),
                 #checkboxInput("reduceThNull", "Reduce NULL columns to a threshold", FALSE),
                 
                 
                 # Input: Select separator ----
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ";"),
                 
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
                 h4("Data Source"),
               "The database is about the quality of the wines and its characteristics which include 13 parameters about containing sugar, pH, acidity etc.. Further information about the open data source and its characteristics can be found here: https://s3.amazonaws.com/udacity-hosted-downloads/ud651/wineQualityInfo.txt. The 13th variable is about the wine type (white=1, red=2) as we have combined the white and red data set. Reason: Interesting correlations might be missed otherwise and differentiating because of colours seems somewhat old fashioned.
",
               tags$hr(),
                 # Output: Data file ----
                 tableOutput("contents"),
               tags$hr(),
               h4("Data Set"),
               "The data set doesn't contain NA values. All the values are numeric. It contains 14 columns (including the index).",
               "Unfortunatelly this app accepts only numerical data set without NAs.",
               h4("IMPORTANT"),
               "All the methods used are computally expensive. For this reason, it takes time until the plots are shown.",
               "It could be that before the plots are shown, a error is to be seem. This error disappears as soon as the run process of a method is over.",
               "Just have patience!"
                 
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
                          h4("K-means"),
                          "K-Means is an unsupervised, parametric method (need to only pre-specify K number of clusters. The possibility to define the number of clusters can be an advantage in certain situations.

It is a simple way to group data based on similar properties as only a few assumptions are needed. Due to a certain randomness of the algorithm, output can be different with each time the user runs the code. This needs to be considered for interpretation reasons.
",
                          tags$hr(),
                          #h4("Scree Plot"),
                          plotOutput("distPlot"),
                          pre(includeText("KPlotScree.txt")),
                          #h4("K-mean Clustering Plot"),
                          plotOutput("dfClusterPlot"),
                          pre(includeText("KPlotCluster.txt"))
                          
                          
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
                          h4("HC"),
                          "HC is short for Hierarchical Clustering. It is an Unsupervised, non-parametric method (no labelled data needed) where no assumptions are needed. In comparison to K-Means, there is  no need to specify K number of clusters beforehand. 

The Transposed Dendogram plot shows the distance between the predictors: The lower the altitude of a branch is, the closer the predictors are to each other. Choice of where to cut the dendrogram is not always clear

Height of cut has comparable role as the K in K-means and controls the number of clusters obtained.

With the Matrix of Dissimilarity the similarity in the dataset can be evaluated. Diagonal members equal zero dissimilarity",
                          tags$hr(),
                          
                          plotOutput("dendPlot",width = "120%",height="800px"),
                          
                          #d3heatmapOutput("heatmap", height="8000px", width="100%"),
                          plotOutput("heatmatPlot",height="800px",width = "120%"),
                          pre(includeText("HCPlot.txt"))
                          
                 ),
                 # tab 2 ----
                 tabPanel("Informations",
                          
                          h4("What it is:"),
                          "Hierarchical Clustering in short (HC)
          Unsupervised, non-parametric method (no need labelled data)
          Definition: Hierarchical clustering, also known as hierarchical cluster analysis, 
          is an algorithm that groups similar objects into groups called clusters. 
          The endpoint is a set of clusters, where each cluster is distinct from each other cluster, 
          and the objects within each cluster are broadly similar to each other.
          'Better' than K-means clustering, no need to specify K number of clusters a priori (goes through all Ks).
          Source: https://www.displayr.com/what-is-hierarchical-clustering/",
                          h4("When to use it:"),
                          "First exploration of multidimensional data (no assumptions needed). 'Data driven!'",
                          h4("What it does:"),
                          "Groups data based on their similarity, the resulting clusters have similar properties",
                          h4("How it does this"),
                          "- Bottom-up agglomerative clustering
          - Can use different metric (Euclidean-, Pearson distance) 
          - Generates dendrograms that highlight similar patterns in the data set
          - Often combined to heatmaps graphical representation"  
                          
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
                          h4("PCA"),
                          "Unsupervised, linear, non-parametric method. This method is especially meaningful with datasets of more than three dimension as it  reduces dimensionality of a dataset. It is used for grouping variables with similar behaviour and there’s no need for assumptions. The output is easily interpretable as the dimensions got reduced. ",
                          tags$hr(),
                          
                          
                          
                          
                          plotOutput("biPlot",width = "100%",height="800px"),
                          plotOutput("circPlot",width = "100%",height="800px"),
                          
                          sliderInput("slider_midpoint", "midpoint:", 0, 10 , 0.5,step = 0.1),
                          pre(includeText("pcaPlotBiplot.txt")),
                          
                          plotOutput("scree.pca.Plot"),
                       
                          plotOutput("pvePlot"),
                       
                          plotOutput("cumulative.pvePlot"),
                          pre(includeText("pcaPlotCPVE.txt")),
                          #plotOutput("starPlot"),
                          #pre(includeText("pcaPlotStar.txt"))
                  
                 ),
                # Tab Info ----
                 tabPanel("Informations",
                          h4("What it is:"),
                          "Principal Component Analysis (PCA)
           Unsupervised, linear, non-parametric method
          Definition: Principal Component Analysis (PCA) is an unsupervised, non-parametric statistical 
          technique primarily used for dimensionality reduction in machine learning. High dimensionality 
          means that the dataset has a large number of features. ... PCA can also be used to filter 
          noisy datasets, such as image compression. Source: https://medium.com/apprentice-journal/pca-application-in-machine-learning-4827c07a61db",
                          h4("When to use it:"),
                          "First exploration of multidimensional data (no assumptions needed). 'Data driven'!",
                          h4("What it does:"),
                          "PCA reduces the dimensionality of a dataset to a more understandable representation",
                          h4("How it does this"),
                          "- Find new axes (principal components) that represent the data space in a reduced set of 
            dimensions to capture the most important information in the data
         - Capture the maximal variance of the data
         - Highlight the global patterns in the data set
         Conclusion: Data organization method for grouping variables with similar behaviour"  
  
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
                          h4("tSNE"),
                          "Here we have an unsupervised, non-linear, parametric method for dimensionality reduction useful for Exploration & visualization of data and well-suited for high-dimensional data. tSNE minimizes the difference between the similarity of points in high & in low-dimensional space. It is easy to apply but not always intuitive to interpret the plots. The output can be different with every time the user runs the code.Distances have (almost) no meaning.",
                          tags$hr(),
                          plotlyOutput("tSNEPlot"),
                          pre(includeText("tSNEPlottSNE.txt"))
                 ),
                 tabPanel("Informations",
                          h4("What it is:"),
                          "t-Distributed Stochastic Neighbor Embedding (TSNE)
         Unsupervised, non-linear, parametric method for dimensionality reduction
         Definition: t-Distributed Stochastic Neighbor Embedding (t-SNE) 
         is a non-linear technique for dimensionality reduction that is 
         particularly well suited for the visualization of high-dimensional datasets. 
         It is extensively applied in image processing, NLP, genomic data and speech processing.
         Source: https://www.datacamp.com/community/tutorials/introduction-t-sne",
                          h4("When to use it:"),
                          "Exploration & visualization of data, well-suited for high-dimensional data",
                          h4("What it does:"),
                          "- Embeds high-dimensional data in a low-dimensional space to visualize
          - tSNE minimizes the difference between the similarity of points in high & in low-dimensional space
            (according to a conditional probability ruled by a probability distribution)",
                          h4("How it does this"),
                          " -Minimize distributions divergence (Kullback-Leibler divergence, relative entropy) between
             - a similarity distribution of multi-dimensional input objects and
             - a similarity distribution of the corresponding low-dimensional points using a gradient descent
             - using a gradient descent
            Conclusion: Data organization method for grouping variables with similar behaviour (as in PCA, HC, K-means)"  
                          
                 ) #end tabPanel
               ) #end tabsetPanel
             ) #end main panel
    ), #end tSNEpanel        
               
    
    #TAB SOMs ----
    tabPanel("SOMs",
             #SIDEBAR PANEL
             sidebarPanel(
               selectInput('category.SOMs', 'Delete the features that should not be classified', "",multiple = TRUE),
               sliderInput("slider_xdim", "xdim", 1, 50 , 4),
               sliderInput("slider_ydim", "ydim:", 1, 50 , 4),
               checkboxInput("check_scaled", label = "Check if you want scaled data", value = TRUE),
               #sliderInput("slider_max_iter", "Max iteration:", 1, 2000 , 500)
             ), #end SIDEBAR PANEL

             mainPanel(
               tabsetPanel(
                 tabPanel("Plots",
                          h4("SOMs"),
                          "SOM is an unsupervised, nonlinear, parametric method and can be considered as an artificial neural network. It includes mapping from a higher-dimensional input space to a lower-dimensional map space with competitive learning and therefore reduces dimensionality of a datasets. Neural network uses competitive learning. It is used for data visualization of high-dimensional data. This method is somewhat similar to K-means (SOMs with a small number of nodes behave similar to K-means) and similar to PCA as it can be considered to be a nonlinear generalization of PCA.",
                          tags$hr(),
                
                          plotOutput("somsPlot.change"),
                          pre(includeText("somsPlotChange.txt")),
                          
                          plotOutput("somsPlot.count"),
                          pre(includeText("somsPlotCount.txt")),
         
                          plotOutput("somsPlot.dist"),
                          pre(includeText("somsPlotDist.txt")),
                    
                          
                          selectInput('property.SOMs', 'Select a feature to be classified', ""),
                          plotOutput("somsPlot.property"),
                          pre(includeText("somsPlotProperty.txt")),
                          
                          plotOutput("somsPlot.codes"),
                          pre(includeText("somsPlotCodes.txt")),
                          
                          plotOutput("somsPlot.tree",width = "150%"),
                          pre(includeText("somsPlotTree.txt")),
                          
                          
                          plotOutput("somsPlot.mapping"),
                          pre(includeText("somsPlotMapping.txt")),
                  

                          numericInput("soms.tree.h", "Height (h) to cut the tree", 1, min = 1, max = 10000),
                          plotOutput("somsPlot.map.hc"),
                          pre(includeText("somsPlotMapHC.txt"))
                 ),
                 tabPanel("Informations",
                          h4("What it is:"),
                          "Self-Organizing Maps
          - Unsupervised, nonlinear, parametric method
          - Type of artificial neural network
          - Somewhat similar to K-means (SOMs with a small number of nodes behave similar to K-means) 
          - Somewhat similar to PCA (can be considered a nonlinear generalization of PCA)",
                          h4("When to use it:"),
                          "For data visualization of high-dimensional data",
                          h4("What it does:"),
                          "- Reduces the dimensionality of a dataset - similar to PCA, tSNE
          - Trains a neural network such that parts of it become specifically responsive to certain input patterns 
          - Produces a low-dimensional, discrete representation (= map) of the input space of the training samples 
          - Mapping from a higher-dimensional input space to a lower-dimensional map space",
                          h4("How it does this"),
                          "- Uses competitive learning (unlike other artificial neural networks, that use error correction learning like 
            backpropagation & gradient descent)
          - Trained SOMs classify inputs by finding the node with the closest distance (smallest metric) to the input"  
                          

                 ) #end tabPanel
               ) #end tabsetPanel
             ) #end main panel
    ) #end SOMs panel

             )#end navbarpage
)#end Fluid page
  
  
  
  
  
  
  
  
  
  
  
  