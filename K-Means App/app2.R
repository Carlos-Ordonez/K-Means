library(shiny)

ui<- fluidPage(
  theme = "bootstrap1.css",
  titlePanel("My K-Means"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(h2("Specs"),
      numericInput("k",
                  "Number of Clusters:",
                  min = 0,
                  max = 20,
                  value = 1),
      h3("Total Time:"),
      textOutput("totalTime"),
      h3("Itterations:"),
      textOutput("itterations_Text"),
      h3("Global Distance:"),
      textOutput("globalDistance"),
      width = 3
    ),
    # Show a plot of the generated distribution
    mainPanel(h2("Results"),
              tags$style(HTML("
                    .tabbable > .nav > li > a{background-color: green;  color: white}")),
              tabsetPanel(type = "tabs",
                          tabPanel("Data", plotOutput("data")),
                          tabPanel("Clusters", plotOutput("chart")),
                          tabPanel("Performance",plotOutput("avgDist"))
              ),
              
      width = 8
      
    )
  )
)

server <- function(input, output) {
  k <- reactive(input$k)
  my_K_Means<-function(myData,k){
    
    pause_time<-0
    start_time<-Sys.time()
    
    #Create list to hold component dataframes
    components<-list()
    for (i in 1:ncol(myData)){
      components[[i]]<-myData[i]
    }
    myDataComponents<-components
    
    #Creates index for the entire dataset and randomizes it
    index <- 1:length(myData[[1]])
    randomIndex<-sample(index,length(index),replace = FALSE)
    
    ##assign centroid index to first k randomized points
    int_Centroid_Index<-randomIndex[1:k]
    
    #Create list of centroid and turn into dataframe
    Centroids<-list()
    for(i in 1:length(myDataComponents)){
      Centroids[[i]]<-myDataComponents[[i]][int_Centroid_Index[1:k],]
    }
    centroids=as.data.frame(Centroids)
    
    #create list of clusters dataframes
    clusters<-list()
    for(i in 1:k){
      clusters[[i]]<-centroids[i,]
    }
    
    # creates list of distances to compute universal distance
    Cluster_Distance<-list()
    for (i in 1:k){
      Cluster_Distance[[i]]<-data.frame(Distance = 0)
    }
    
    #Calculate the distance from each point to the centroids
    #Returns a list of distances
    distance<-function(Centroids,Components,RI){
      distances<-list()
      sum = 0
      for (i in 1:nrow(Centroids)){
        for(j in 1:ncol(myData)){
          sum = sum + (Centroids[i,j]-Components[[j]][RI,])^2
        }
        distances[[i]]<- sqrt(sum)
        sum = 0
      }
      return (distances)
    }
    
    # Classify points in to respective clusters 
    # using the distance list to index which cluster to classify as
    # Computes new centroid and compares to current centroid
    # If equal it is done otherwise it continues untill equal centroids are produced
    new_centroids<-centroids
    current_centroids<-data.frame()
    itterations<-0
    
    while(isTRUE(all.equal(new_centroids,current_centroids))==FALSE){
      itterations<-itterations +1
      centroids<-new_centroids
      clusters<-list()
      
      #clear list of distances
      Cluster_Distance<-list()
      for (i in 1:k){
        Cluster_Distance[[i]]<-data.frame(Distance = 0)
      }
      for(i in 1:k){
        clusters[[i]]<-new_centroids[i,]
      }
      for(j in (k+1):length(randomIndex)){
        # Store distances to save for universal distance and classify point
        distances<-distance(centroids,myDataComponents,randomIndex[j])
        
        # Build Selected Point
        selected_Point<-c(myDataComponents[[1]][randomIndex[j],])
        for (s in 2:length(myDataComponents)){
          selected_Point[s]<-myDataComponents[[s]][randomIndex[j],]
        }
        
        # Builds list of smallest distances
        Cluster_Distance[[which.min(distances)]][nrow(Cluster_Distance[[which.min(distances)]])+1,]<-distances[[which.min(distances)]]
        
        # Builds Clusters
        clusters[[which.min(distances)]]<-rbind(clusters[[which.min(distances)]],selected_Point)
      }
      # Holds Current centroid to compare to once new centroid is calculated
      current_centroids<-centroids
      
      # Calculate New Centroid if equal to current centroid while loop breaks
      for (i in 1:nrow(centroids)){
        for (j in 1:length(components)){
          new_centroids[i,j]<-sum(clusters[[i]][,j]/length(clusters[[i]][,j]))
        }
      }
      # print(new_centroids)
      # Assigns cluster to each point
      for (i in 1:k){
        clusters[[i]]$CID<-i
      }
      # combines all cluster in to single dataframe 
      
      cluster_Itteration<-as.data.frame(rbindlist(clusters))
      par(bg = "blanchedalmond")
      plot(cluster_Itteration[,1]~cluster_Itteration[,2],
           main = paste("K = ", k),
           col=cluster_Itteration$CID + 3,
           cex = 1.5,
           pch = cluster_Itteration$CID + 5,
           xlab = colnames(myData[2]),
           ylab = colnames(myData[1]))
      
      for (i in 1:nrow(centroids)){
        points(centroids[i,2],
               centroids[i,1],
               col = "red",
               cex = 3,
               pch = 10)
      }
      
      Sys.sleep(pause_time)
      
    }
    end_time<-Sys.time()
    C_Distances<-Cluster_Distance
    
    avg_dis<-0
    for (i in 1:length(C_Distances)){
      avg_dis<-(avg_dis + (sum(C_Distances[[i]])/nrow(C_Distances[[i]])))
    }
    
    avg_Cluster_Distance<<-avg_dis/length(C_Distances)
    
    output$itterations_Text<-renderText(itterations)
    output$totalTime<-renderText((end_time-start_time)-(itterations*pause_time))
    output$globalDistance<-renderText(avg_Cluster_Distance)
    
    return(as.data.frame(rbindlist(clusters)))
  }
  
  
  output$data<-renderPlot(plot(myData[,1]~myData[,2],
                               main = "Dataset",
                               xlab = colnames(myData[2]),
                               ylab = colnames(myData[1])))
  output$chart<-renderPlot(my_K_Means(myData,k()))
  
  
  avgDistance<-data.frame(K = numeric(0), Distance = numeric(0))
  plot_Avg_Dist<-function(k){
    for (i in 1:k){
      Final_Cluster<-my_K_Means(myData,i)
      avgDistance<-rbind(avgDistance,data.frame(K = i,Distance = avg_Cluster_Distance))
    }
    plot(avgDistance,
         xlim<-10,
         type = "o",
         main = paste("Average Distance for k: 1-", k()))
  }

  output$avgDist<-renderPlot({
    plot_Avg_Dist(k())
    })
}
shinyApp(ui = ui, server = server)