

library(shiny)
#Import packages


function(input, output,session) {

#select the data for the plots K-meas
  selectedData <- reactive({
    df <- readData()
    return (df[, c(input$xcol, input$ycol)])
  })
  
  
  
#HC
  scaled_data <- reactive({
    # get all data
    data <- readData()
    # scale data, with transpose or not
    if(input$transpose==TRUE) {
      return(t(as.matrix(scale(data))))
    }
    else {
      return (as.matrix(scale(data)))
    }
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
  
  # output$heatmap <- renderD3heatmap({
  #   scaled_data <- scaled_data()
  #   d3heatmap(scaled_data, 
  #                    Rowv=NA, Colv=NA, 
  #                    col=topo.colors(200, alpha=0.5), 
  #                    scale="none", 
  #                    xaxis_font_size=10, 
  #                    yaxis_font_size=10)
  #   }) 
  
#calculate kmeans to the selected dataset, cluster and nstart  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters, input$slider_nstart)
  })

  
#Plot the results of the calculated kmeans
  output$dfClusterPlot <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    #par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3, main="K-mean Clustering Plot")
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  

# To analyse how many cluster do we want
  wss <- reactive({
    sapply(1:input$slider_k, function(i){return(kmeans(selectedData(),centers = i,nstart=input$slider_nstart)$tot.withinss)})
  })

  output$distPlot <- renderPlot({
    plot(1:input$slider_k, wss(), type="b", xlab="Number of Clusters",ylab="Total within-cluster sum of squares",main="Scree Plot")

  }) #end output distplo
  
  readData <- reactive({
    #TODO: function data cleasing
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    return(df)
    
  })
  
  output$contents <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    df <-readData() #acess the data
    
    # DT::datatable(df) #this one or the one below
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    } #end else
    
  }) #end output contents
  
}# end server function

