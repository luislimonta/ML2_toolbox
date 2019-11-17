#Import packages
library(shiny)
library(Rtsne)
#to install factoextra
# install.packages ("processx")
# library("devtools")
# install_github("kassambara/factoextra")
# library("factoextra")
library("factoextra")
library(kohonen)
library(ggplot2)
library(plotly)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
# Create preprocessing functions                                                      #######----
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# make built in function to reduce the number of factots in a dataset to a threshold 

reduceThFactors <- function(dataset, yvar ="", threshold = 10, varTypes = c("integer", "double", "logical", "numeric")){
  
  # reduce number of predictors, remove factors with more unique observations then threshold (th)
  # transform characters to numeric
  dim(dataset)
  red <- c()
  for(colnr in 1:ncol(dataset)){
    if(!class(dataset[,colnr]) %in% varTypes && length(unique(dataset[,colnr]))> threshold && colnames(dataset)[colnr] != as.character(yvar)){
      print(paste("Is", colnames(dataset)[colnr],"in varType list '", class(dataset[,colnr]), "' and has more than ", threshold, "uniques: ", length(unique(dataset[,colnr]))))
      red <- append(red,c(-1*colnr))
    }
  }
  
  print(red)
  if(!is.null(red)){dataset <- dataset[,red]}
  print(paste("Col Nr: ",colnr, "     -      reduced by: ", length(red), "                - dims: ",dim(dataset)))
  dim(dataset)
  return(dataset)
  
}

# make built in function to reduce null containing columns in a dataset to a threshold
# threshold set based on plot_missing
reduceThNull <- function(dataset, yvar="", threshold = 40, omitRest = FALSE){
  
  # reduce number of predictors, remove factors with more percentage of null observations then threshold (th)
  # transform characters to numeric
  dim(dataset)
  red <- c()
  for(colnr in 1:ncol(dataset)){
    perNull <- 100 / length(dataset[,colnr]) * sum(is.na(dataset[,colnr]))
    if(perNull > threshold && colnames(dataset)[colnr] != as.character(yvar)){
      print(paste("Has", colnames(dataset)[colnr]," more Nulls '(", sum(is.na(dataset[,colnr])), " -> ", perNull, "%" ,")' than threshold: ", threshold))
      red <- append(red,c(-1*colnr))
    }
  }
  print(red)
  if(!is.null(red)){dataset <- dataset[,red]}
  print(paste("Col Nr: ",colnr, "     -      reduced by: ", length(red), "                - dims: ",dim(dataset)))
  dim(dataset)
  
  if(omitRest){
    dataset <- na.omit(dataset)
    print("Nulls omitted")
  }
  
  return(dataset)
  
}


# make built in function to replace null containing columns in a dataset
replaceNull <- function(dataset, yvar = "", replaceText = "unknown", numericMethod = "median"){
  # Exception handling & preprocessing missing
  #
  #dataset: numeric matrix of data
  #yvar: dependent variable
  #replaceText: text which is the replacement for NA in text/factor classes 
  #numericMethod: method to apply for replacing numeric predictors; allowed values: median, average, zero
  
  for(colnr in 1:ncol(dataset)){
    if(colnames(dataset)[colnr] != yvar){
      
      # mutate missing values
      
      # numeric classes
      if(class(dataset[,colnr]) %in% c("integer", "double","numeric")){
        print("replace numeric")
        if(numericMethod == "median"){
          dataset[,colnr] <- replace(dataset[,colnr], is.na(dataset[,colnr]), median(dataset[,colnr], na.rm = TRUE))
        }else if(numericMethod == "mean"){
          dataset[,colnr] <- replace(dataset[,colnr], is.na(dataset[,colnr]), mean(dataset[,colnr], na.rm = TRUE))
        }else if(numericMethod == "zero"){
          dataset[,colnr] <- replace(dataset[,colnr], is.na(dataset[,colnr]), 0)
        }else{
          print(paste("Method ",numericMethod, " invalid. Valid values for numeric replacment are: median, mean, zero"))
        }
      }
      
      # logical classes
      if(class(dataset[,colnr]) %in% c("logical")){
        print("replace logical")
        dataset[,colnr] <- replace(dataset[,colnr], is.na(dataset[,colnr]), median(dataset[,colnr], na.rm = TRUE))
      }
      
      # text classes
      if(class(dataset[,colnr]) %in% c("character")){
        print("replace text")
        dataset[,colnr] <- replace(dataset[,colnr], is.na(dataset[,colnr]), replaceText)
      }
      
      # factor classes
      if(class(dataset[,colnr]) %in% c("factor")){
        print("replace factor")
        dataset[,colnr] <- as.character(dataset[,colnr])
        dataset[,colnr] <- replace(dataset[,colnr], is.na(dataset[,colnr]), replaceText)
        dataset[,colnr] <- as.factor(dataset[,colnr])
      }
      
      
      
    }
  }
  
  
  return(dataset)
  
}



# make built in function to convert factors to numeric variables
convertFactorToNumeric <- function(dataset, yvar=""){
  
  for(colnr in 1:ncol(dataset)){
    if(class(dataset[,colnr]) %in% c('factor') && colnames(dataset)[colnr] != as.character(yvar)){
      print(paste("IS factor and gets converted to numeric", colnames(dataset)[colnr]))
      dataset[,colnr] <- as.numeric(dataset[,colnr])
    }
  }
  
  return(dataset)
  
}

#end Data processing ----


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
# Start Server function                                                  ####### ----
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

function(input, output,session) {
  
#Read the dataset  ----
  readData <- reactive({
    
    #TODO: function data cleasing?
    
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'category.tSNE', label = 'Select a category',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'category.SOMs', label = 'Delete the features that should not be classified',
                      choices = names(df), selected = names(df))   
    observe({
      updateSelectInput(session, inputId = 'property.SOMs',
                        choices = names(df[, c(input$category.SOMs)]) ,selected = names(df)) 
    })
    
    
 return(df)
    
  })
  
# Create table to visualize the data set
  output$contents <- renderTable({
    #df <- readData()
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    #DT::datatable(df) #this one or the one below
    
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    } #end else
    
  }) #end output contents
  

# K-MEANS TAB ----
  
#Select the data for the plots K-meas ----
  selectedData <- reactive({
    df <- readData()
    return (df[, c(input$xcol, input$ycol)])
  })
  

#calculate kmeans to the selected dataset, cluster and nstart  ----
  clusters <- reactive({
    set.seed(3)
    kmeans(readData(), input$clusters, input$slider_nstart)
  })
  
#Plot the results of the calculated kmeans ----
  output$dfClusterPlot <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3, main="K-mean Clustering Plot")
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
#To analyse how many cluster do we want ----
  wss <- reactive({
    set.seed(3)
    sapply(1:input$slider_k, function(i){return(kmeans(readData(),centers = i,nstart=input$slider_nstart)$tot.withinss)})
  })
  
  
# Scree Plot   ----
  output$distPlot <- renderPlot({
    plot(1:input$slider_k, wss(), type="b", xlab="Number of Clusters",ylab="Total within-cluster sum of squares",main="Scree Plot")
    text(1:input$clusters, wss()[1:input$clusters], round(wss(),digits=2)[1:input$clusters], cex=1, pos=4, col="blue")
    #text(1:3, km.out$withinss[1:3], round(km.out$withinss,digits=2)[1:3], cex=0.6, pos=1, col="blue")
    
  })  
  
  
  

  
# HC ----  
  scaled_data <- reactive({
    # get all data
    data <- readData()
    data <- convertFactorToNumeric(data) #convert factor to numeric
    # scale data, with transpose or not
    if(input$transpose==TRUE) {
      return(t(as.matrix(scale(data))))
    }
    else {
      return (as.matrix(scale(data)))
    }
  })
  output$heatmap <- renderD3heatmap({
    scaled_data <- scaled_data()
    d3heatmap(scaled_data,
                     Rowv=NA, Colv=NA,
                     col=topo.colors(200, alpha=0.5),
                     scale="none",
                     xaxis_font_size=10,
                     yaxis_font_size=10)
    })
  
  hc <- reactive({
    scaled_data <- scaled_data()
    
    # calculate distances between data
    distances <- dist(scaled_data, method = input$method_dis)
    
    # compute hierarchical clustering based in distances calculated above:
    hc <- hclust(distances,method=input$method_link)
    
    # computes dendrogram graphical representation:
    # dend <- as.dendrogram(hc)
    return(hc)
  
  })
  
  
  #plot the output Dendogram
  output$dendPlot <- renderPlot({
    #TODO: Plot cutree and make button to cut tree
    plot(hc())
  })  
  
  #plot heatmap
  output$heatmatPlot <- renderPlot({
    scaled_data <- scaled_data()
    heatmap(scaled_data, scale="row", density.info="none", trace="none",
            col = topo.colors(200, alpha=0.5),
            Colv=F

            )
  })
  

  

  

  
  
  
# PCA ----
  # Compute PCA. ----
  pca.out <- function(){
    # scale=TRUE to scale the variables to have standard deviation = 1 
    data <- readData()
    data <- convertFactorToNumeric(data)
    pca.out=prcomp(data, scale=TRUE)
    return(pca.out)
  }
  
  # Scree plot
  output$scree.pca.Plot <- renderPlot({
    screeplot(pca.out())
  })
  
  # Plot biplot ----
  output$biPlot <- renderPlot({
    pr.out <- pca.out()
    #biplot(pr.out,scale=0)
    fviz_pca_biplot(pr.out, label ="var", col.ind="cos2") +
      
      scale_color_gradient2(low="blue",mid=' violet',
                            high ="red", midpoint=input$slider_midpoint)+
      theme_minimal()
  })
  

  # Plot biplot 2 ---- 
  output$circPlot <- renderPlot({   
    pr.out <- pca.out()
    #biplot without observations, circumference of the correlation circle ####
    fviz_pca_var(pr.out, col.var="contrib") +
      
    scale_color_gradient2(low="blue", mid="violet", high="red", midpoint=input$slider_midpoint) +
    
    theme_minimal()
    })
  
  # Plot stars ----
  output$starPlot <- renderPlot({
    df <- readData()
    stars(df)
    stars(df, key.loc = c(20,2)) 
  })
  
  # Compute the proportion of variance explained by each principal component (variance explained by each principal component / total variance explained by all four principal components) ----
  pve <- function(){
    pr.out <- pca.out()
    pr.var<- pr.out$sdev^2
    pve <- pr.var/sum(pr.var)
    return(pve)
  }
  
  # Proportion of Variance Explained ----
  output$pvePlot <- renderPlot({
    plot(pve() ,xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type='b') 
  })
 
  # Cumulative Proportion of Variance Explained ----
  output$cumulative.pvePlot <- renderPlot({
    plot(cumsum(pve()),xlab="PrincipalComponent",ylab="Cumulative Proportion of Variance Explained",ylim=c(0,1),type='b')
  })
  
  # TODO ? Rotation matrix provides the principal component of the loadings ---- 
  # rotation.matrix <- function(){
  #   pr.out <- pca.out()
  #   rot <- pr.out$rotation
  #   return(rot)
  # }
  # output$rotation.matrix <- renderPrint({
  #   pr.out <- pca.out()
  #   paste(format(pr.out$rotation))
  # })

  


# tSNE ----
  
  # Select matrix ----
  select.tsne.rest.data.matrix <- reactive({
    #data for tsne preparation
    data <- readData()
    
    # Remove duplicates in data:
    data_unique <- unique(data)
    
    #separate data set in cat. and rest
    category <- input$category.tSNE
    
    #rest data set
    rest.data <- data_unique[, ! names(data_unique) %in% category, drop = F]
    #rest.data <- convertFactorToNumeric(rest.data)
    rest.data.matrix <- as.matrix(scale(rest.data))
    
    return(rest.data.matrix)
    
  })

  # Select column to search categories within the matrix ----
  select.tsne.col.data <- reactive({
    #data for tsne preparation
    data <- readData()
    
    # Remove duplicates in data:
    data_unique <- unique(data.frame(data))
    
    #separate data set in cat. and rest
    category <- input$category.tSNE
    
    #cat data set
    cat.data <- data_unique[, c(category)]
    return(cat.data)
    
  })   

  
  # Plot output ----
  output$tSNEPlot <- renderPlotly({
    
    cat.data <- as.matrix (select.tsne.col.data())
    rest.data.matrix <- select.tsne.rest.data.matrix()
    
    dim <- input$slider_dim
    perplexity <- input$slider_perplexity
    max_iter <- input$slider_max_iter
    category <- input$category.tSNE
    
    #find index from duplicated
    index.duplicated<-which(duplicated(rest.data.matrix) | duplicated(rest.data.matrix[nrow(rest.data.matrix):1, ])[nrow(rest.data.matrix):1])
    
    if(length(index.duplicated) == 0) {
      unique.cat.data.unique <- cat.data
      unique.rest.data.matrix <- rest.data.matrix
    }else{
      #delete duplicated lines from category vector
      unique.cat.data.unique <-cat.data[-index.duplicated,]
      unique.rest.data.matrix <- rest.data.matrix[-index.duplicated,]
    }
    
    
    # For plotting evaluation against colorcode # category (~ classification solution) 
    row_label <- as.factor(rownames(unique.cat.data.unique)) #label from rows.....
    levels_category<- as.factor(unique.cat.data.unique)# convert the category to levels
    colors <- rainbow(nlevels(levels_category))#set color palete for the category
    colors <- colors[as.numeric(levels_category)] #set colors to the chosen category
    
    # Run tSNE:
    tsne <- Rtsne(unique.rest.data.matrix, dims = dim,
                  perplexity=perplexity, verbose=TRUE,
                  max_iter = max_iter)
    
    tsne.df <- data.frame(tsne$Y)
    
    # Plot data and labels:
    # plot(tsne$Y)
    # text(tsne$Y, labels=row_label,
    #      col=colors[row_label])
   
    p<- plot_ly()
    p <- add_trace(p, x = tsne.df$X1, y =tsne.df$X2, z=tsne.df$X3, color=as.factor(unique.cat.data.unique) )
    p
    
  })

  
  # SOMs ----
  
  # select the features that will be searched as a scaled matrix ----
  select.rest.soms <- reactive({
    
    #data for tsne preparation
    data <- readData()
    
    # Remove duplicates in data:
    data_unique <- unique(data)
    
    #separate data set in cat. and rest
    category <- input$category.SOMs
    
    #rest data set
    rest.data <- data_unique[, c(category)]
    rest.data <- convertFactorToNumeric(rest.data)
    
    if(input$check_scaled == TRUE) {
      rest.data.matrix <- as.matrix(scale(rest.data))
      return(rest.data.matrix)
    }
    else {
      rest.data.matrix <- as.matrix(rest.data)
      return(rest.data.matrix)
    } #end else

    
    return(rest.data.matrix)
  })
  
  # Select column to be labeled in some plots TODO----
  select.col.soms <- reactive({
    
    #data for soms preparation
    data <- readData()
    
    # Remove duplicates in data:
    data_unique <- unique(data)
    
    #separate data set in cat. and rest
    category <- input$category.SOMs
    
    #cat data set
    cat.data <- data_unique[, c(category)]
    return(cat.data)
    
  })
  
  # Train som model
  
  som.model <- reactive({
    
    rest.data.matrix.soms <- select.rest.soms()

    # Define the neuronal grid
    som_grid <- somgrid(xdim = input$slider_xdim, ydim = input$slider_ydim,
                        topo="hexagonal") #rondom choice, we can vary this
    
    
    # Train the model
    som_model <- som(rest.data.matrix.soms,
                     grid=som_grid,
                     rlen=1000,
                     alpha=c(0.05,0.01),
                     keep.data = TRUE)
    return(som_model)
    
  })
  
  # Plot training progress ----
  output$somsPlot.change <- renderPlot({

    # Check training progress
    plot(som.model(), type="changes")
    
    
  })
  
  # Plot count: how many samples are mapped to each node on the map. (5-10 samples per node) ----
  output$somsPlot.count <- renderPlot({
    
    # Check training progress
    plot(som.model(), type="count")
    
  })
  
  # Plot mapping ----
  output$somsPlot.mapping <- renderPlot({
    
    cat.data <- input$property.SOMs #variable to be classified
    rest.data.matrix <- select.rest.soms()
    
    categorie <- rest.data.matrix[, c(cat.data)]
    
    # For plotting evaluation against colorcode # category (~ classification solution) 
    row_label <- as.factor(rownames(rest.data.matrix)) # set the rownames as factor
    levels_category<-(as.factor(categorie))# convert the categorx to levels
    colors <- rainbow(nlevels(levels_category))#set color palete for the category 
    colors <- colors[as.numeric(levels_category)] #set colors to the chosen category

    plot(som.model(), type="mapping",
         col=colors[row_label])
    
  })
  
  # Plot dist.neighbours ----
  output$somsPlot.dist <- renderPlot({
    # U-Matrix: measure of distance between each node and its neighbours.
    # (Euclidean distance between weight vectors of neighboring neurons)
    # Can be used to identify clusters/boundaries within the SOM map.
    # Areas of low neighbour distance ~ groups of nodes that are similar.
    plot(som.model(), type="dist.neighbours")
    
  })
  
  # Plot Codes / Weight vectors ----
  output$somsPlot.codes <- renderPlot({
    
    # Codes / Weight vectors: representative of the samples mapped to a node.
    # highlights patterns in the distribution of samples and variables.
    plot(som.model(), type="codes")
  
  })
  
  # Color Pallet function ----
  coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
  
  
  # Plot Property: Heatmaps ----
  # Heatmaps: identify interesting areas on the map.
  # Visualise the distribution of a single variable (defined in [,x])
  # across the map
  # colnames(data) # to check index to put in [,x]
  output$somsPlot.property <- renderPlot({
    som_cluster <- som_cluster(input$soms.tree.h)
    rest.data.matrix.soms <- select.rest.soms()
    property <- grep(input$property.SOMs, colnames(rest.data.matrix.soms))
    plot(som.model(), type = "property", property = getCodes(som.model(), 1)[,property], main=colnames(getCodes(som.model()))[property], palette.name=coolBlueHotRed)
    # Visualize properties based on HC
    add.cluster.boundaries(som.model(),som_cluster)
  })
  
  # Clustering: isolate groups of samples with similar metrics ----
  output$somsPlot.tree <- renderPlot({
    som_model <- som.model()
    tree <- as.dendrogram(hclust(dist(as.numeric(unlist(som_model$codes)))))
    plot(tree, ylab = "Height (h)")
  })
  
  # Cut the tree somewhere based on the above tree ----
  som_cluster <- function(h){
    som.model <- som.model()
    cutree <- cutree(hclust(dist(as.numeric(unlist(som.model$codes)))),
                        h=h)
    return(cutree)
  }
  
  # Visualize mapping based on HC ----
  output$somsPlot.map.hc <- renderPlot({
    pretty_palette <- c("skyblue2", 'lightseagreen', 'mistyrose2', 'palevioletred2',
                        'lightsalmon3', 'tomato2', 'red4')
    
    rest.data.matrix <- select.rest.soms()

    cat.data <- input$property.SOMs #variable to be classified
    
    categorie <- rest.data.matrix[, c(cat.data)]
    
    row_label <- as.factor(rownames(rest.data.matrix)) # set the rownames as factor
    levels_category<-(as.factor(categorie))# convert the categorx to levels
    colors <- rainbow(nlevels(levels_category))#set color palete for the category 
    colors <- colors[as.numeric(levels_category)] #set colors to the chosen category
   
    som_cluster <- som_cluster(input$soms.tree.h)
    
    plot(som.model(), type="mapping",
         bgcol = pretty_palette[som_cluster], col=colors[row_label])
    add.cluster.boundaries(som.model(),som_cluster)

  })
  

  output$default <- renderText({ input$txt })
  output$placeholder <- renderText({ input$txt })
  

  
  
}# end server function
