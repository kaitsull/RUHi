#' Launches the Gone mFISHing shiny app
#'
#' @param table A table with values created via ruMake()
#' @return Launch Gone mFISHing shiny app
#'
#' @import shiny shinyWidgets umap ggplot2 shinydashboard dashboardthemes tidyr dplyr DT here shinythemes
#'
#' @export
goFISH <- function(table) {
  #Kaitlin Sullivan November 2020
  #This shiny app allows the naive scientist to browse and visualize analyzed
  #mFISH data in a user-friendly way


  # load the known quantified mFISH data
  table <- table
  #datafiles <- append(datafiles, "Upload New Dataset", after = 0)
  types <- c("Raw Expression", "Hierarchical Clustering")
  cellType <- c("All Cells", "Excitatory", "Inhibitory")

  #temp use for grey dot bgs
  forLayers <- table


  ui <- shinydashboard::dashboardPage(
    #dashboard header
    shinydashboard::dashboardHeader(title = "Gone mFISHing"),

    #Create dashboard sidebar
    shinydashboard::dashboardSidebar(
      sidebarMenu(
        #visualizing individual gene expression
        menuItem("Raw Gene Expression", tabName = "rgene", icon = icon("brain")),
        #visualizing hierarchical clustering of data
        menuItem("Hierarchical Clustering", tabName = "hclust", icon = icon("dna"))
      )
    ),

    #Create a dashboard
    shinydashboard::dashboardBody(

      # change theme
      dashboardthemes::shinyDashboardThemes(theme = "grey_dark"),

      #Create tabs for my content on the dashboard sidebar
      tabItems(
        # Visualizing individual gene expression
        tabItem(tabName = "rgene",
                  shiny::fluidPage(
                  theme = shinythemes::shinytheme("cyborg"),
                  # Title of this page
                  titlePanel("Raw Gene Expression - Gone mFISHing"),

                  # Sidebar with dropdown menu of genes to select
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      #sidebar heading number 2 with info on clustering
                      h4("Explore raw,individual gene expression:"),
                      br(),
                      #filter
                      selectInput(
                        "ei",
                        "Filter by cell types?",
                        choices = cellType,
                        selected = cellType[1]
                      ),
                      textInput("filter", "Input value to filter Slc17a7/Gad1 by:", value = "3"),
                      br(),
                      #creates a reactive dropdown menu contd in server
                      shiny::uiOutput("geneIn"),
                      br(),
                      shiny::checkboxInput("flipxS", "Flip X Axis",
                                    value = FALSE),
                      shiny::checkboxInput("flipyS", "Flip Y Axis",
                                    value = FALSE),
                      shiny::checkboxInput("rotateS", "Rotate 90 degrees",
                                    value = FALSE)
                    ),

                    # Visualization tabs
                    shiny::mainPanel(
                      shiny::tabsetPanel(
                        #view by spatial location
                        shiny::tabPanel("Spatial Location", plotOutput("spaceGenePlot"), downloadButton("downSpacegne")),
                        #view by umap coloured by quantity of gene expression
                        shiny::tabPanel("Dimensionality Reduction", plotOutput("umapGene"), downloadButton("downUgne")),
                        #expression of individual gene in clusters
                        shiny::tabPanel("Expression by Cluster", plotOutput("genevln"), downloadButton("downgVln")),
                        #plot the expression of all genes present
                        shiny::tabPanel("Expression of All Genes", plotOutput("plotall"), downloadButton("downAll"))
                      )
                    )
                  )
                )
        ),
        #tab for clustered data
        shinydashboard::tabItem(tabName = "hclust",
                  shiny::fluidPage(
                  theme = shinythemes::shinytheme("cyborg"),
                  # Title of current page
                  shiny::titlePanel("Clustered Gene Expression - Gone mFISHing"),

                  # Sidebar with a slider input for number of clusters to create
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      h4("Explore visualization of hierarchically clustered expression data:"),
                      br(),
                      #Select the threshold for number of clusters to divide data by using hierarchical clustering
                      shinyWidgets::sliderTextInput(
                        "clus",
                        "Select the number of clusters...",
                        choices = seq(from = 1, to = 15, by = 1),
                        selected = 4
                      ),
                      br(),
                      shiny::checkboxInput("flipxC", "Flip X Axis",
                                    value = FALSE, ),
                      shiny::checkboxInput("flipyC", "Flip Y Axis",
                                    value = FALSE, ),
                      shiny::checkboxInput("rotateC", "Rotate 90 degrees",
                                    value = FALSE)
                    ),
                    #tab panel of options for: xy space, dim reduction space, summary of mean expression per cluster
                    shiny::mainPanel(
                      shiny::tabsetPanel(
                        shiny::tabPanel("Spatial Location", plotOutput("spaceClusPlot"), downloadButton("downSpaceClus")),
                        shiny::tabPanel("Dimensionality Reduction", plotOutput("umapClus"), downloadButton("downUclus")),
                        shiny::tabPanel("Gene Expression per Cluster", plotOutput("vln"), downloadButton("downVln"))
                      )
                    )
                  )
                )
        )
      )
    )
  )






  ## Define server logic required to plot
  server <- function(input, output) {


    #reactive gene name list
    mygenes <- shiny::reactive({
      df <- table
      mygenes <- dplyr::select(df, -c(X,Y))
      if(input$ei == "Excitatory"){
        mygenes <- dplyr::select(mygenes, -Slc17a7)
      }
      if(input$ei == "Inhibitory"){
        mygenes <- dplyr::select(mygenes, Gad1)
      }
      mygenes <- names(mygenes)
      mygenes
    })

    #select genes from user id dataset
    output$geneIn <- shiny::renderUI({
      shiny::selectInput('geneNames', 'Select A Gene...', mygenes())
    })


    data.filter <- shiny::reactive({
      data.filter <- table
      if(input$ei == "Excitatory"){
        data.filter <- dplyr::filter(data.filter, Slc17a7 > input$filter)
        data.filter <- dplyr::select(data.filter, -Slc17a7)
      }
      if(input$ei == "Inhibitory"){
        data.filter <- dplyr::filter(data.filter, Gad1 > input$filter)
        data.filter <- dplyr::select(data.filter, -Gad1)
      }
      data.filter
    })

    #normalize data
    data.norm <- shiny::reactive({
      data.norm <- data.filter()
      #select only gene names
      data.norm <- dplyr::select(data.norm, mygenes())
      #normalize
      data.norm <- sweep(data.norm, 1,
                         apply(data.norm, 1, sum), "/")
      data.norm
    })

    #create a column of umap coords
    umap.data <- shiny::reactive({
      df <- data.filter()
      #umap normalized data
      coord <- umap::umap(data.norm())
      umap.data <- dplyr::mutate(data.norm(),
                                 umap1 = coord$layout[,1],
                                 umap2 = coord$layout[,2],
                                 X = df$X,
                                 Y = df$Y)
      umap.data
    })

    #create hierarchical clustering based on input
    clus.data <- shiny::reactive({
      #save data frame with only genes
      df <- dplyr::select(umap.data(), mygenes())
      #create hclust object
      clust <- hclust(d = dist(df), method = "ward.D2")
      #cut tree at inputted number of clusters
      newcol <- cutree(clust, k = input$clus)
      #create a new column called cluster with the values
      clus.data <- dplyr::mutate(umap.data(), cluster = as.factor(newcol))
      #return the reactive object
      clus.data
    })

    #choose the data to plot as grey points
    plotNum <- shiny::reactive({
      i <- 1
      while(data.file() != datafiles[i]){
        i <- i+1
      }
      i
    })

    #render table on main page
    output$table <- renderDataTable(
      datatable(
        clus.data(), options = list(scrollX = TRUE)
      ))

    #lengthen data
    long.data <- shiny::reactive({
      long.data <- clus.data()
      long.data <- tidyr::pivot_longer(long.data,
                                       cols = -c(X, Y, umap1, umap2, cluster),
                                       names_to = "Gene",
                                       values_to = "Quant")
      long.data
    })

    #filter data by gene input
    gene.data <- shiny::reactive({
      gene.data <- filter(long.data(), Gene == input$geneNames)
      gene.data
    })

    #plots in original X, Y space
    #by gene
    sgplot <- shiny::reactive({
      sgplot <- ggplot2::ggplot(gene.data(), aes(x=X, y=Y, colour = Quant))+
        geom_point()+
        theme_bw()+
        coord_fixed()+
        theme(plot.background = element_rect(fill = "transparent", color = NA))+
        scale_colour_gradient(low = "light blue", high = "navy blue")+
        labs(title = input$geneNames, colour=input$geneNames)
      if(input$flipxS){
        sgplot <- sgplot + scale_x_reverse()
      }
      if(input$flipyS){
        sgplot <- sgplot + scale_y_reverse()
      }
      if(input$rotateS){
        sgplot <- sgplot + coord_flip()
      }
      else{
        sgplot
      }
    })

    output$spaceGenePlot <- shiny::renderPlot({
      #colours graded amount of expression on dataset filtered to only contain inputted gene data
      sgp <- sgplot()
      sgp
    })

    #by cluster
    scplot <- shiny::reactive({
      cd <- forLayers[[plotNum()]]
      scplot <- ggplot2::ggplot(long.data(), aes(x=X, y=Y, colour = cluster))+
        geom_point(data = cd, colour = "grey")+
        geom_point()+
        theme_bw()+
        theme(plot.background = element_rect(fill = "transparent", color = NA))+
        coord_fixed()+
        facet_wrap(~cluster)
      if(input$flipxC){
        scplot <- scplot + scale_x_reverse()
      }
      if(input$flipyC){
        scplot <- scplot + scale_y_reverse()
      }
      if(input$rotateC){
        scplot <- scplot + coord_flip()
      }
      else{
        scplot
      }
    })

    output$spaceClusPlot <- shiny::renderPlot({
      #plot data in space, coloured by cluster in individual plots
      scp <- scplot()
      scp
    })

    #plot in dimensionally reduced space (via umap)
    ugplot <- shiny::reactive({
      #plot graded gene expression for "raw" gene data
      ugplot <- ggplot2::ggplot(gene.data(), aes(x=umap1, y=umap2, colour = Quant))+
        geom_point()+
        theme_bw()+
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              aspect.ratio=1)+
        scale_colour_gradient(low = "light blue", high = "navy blue")+
        labs(title = input$geneNames, colour=input$geneNames)
    })

    output$umapGene <- shiny::renderPlot({
      ugp <- ugplot()
      ugp
    })

    #plot in dimensionally reduced space (via umap)
    ucplot <- shiny::reactive({
      #plot umap coloured by cluster
      ucplot <- ggplot2::ggplot(clus.data(), aes(x=umap1, y=umap2, colour = cluster))+
        geom_point()+
        theme_bw()+
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              aspect.ratio=1)
    })

    output$umapClus <- shiny::renderPlot({
      ucp <- ucplot()
      ucp
    })

    allplot <- shiny::reactive({
      allplot <- ggplot(long.data(), aes(x=X, y=Y, colour = Quant))+
        geom_point(data = table, colour = "light blue")+
        geom_point(size = 0.5)+
        theme_bw()+
        theme(plot.background = element_rect(fill = "transparent", color = NA))+
        scale_colour_gradient(low = "light blue", high = "navy blue")+
        facet_wrap(~Gene)
      if(input$flipxS){
        allplot <- allplot + scale_x_reverse()
      }
      if(input$flipyS){
        allplot <- allplot + scale_y_reverse()
      }
      if(input$rotateS){
        allplot <- allplot + coord_flip()
      }
      else{
        allplot
      }
    })
    #plot all genes with their graded expression in space XY
    output$plotall <- renderPlot({
      ap <- allplot()
      ap

    })

    #violin plot by cluster
    # simplified violin plot code by Mark Cembrowski
    vplot <- shiny::reactive({
      vln.data <- long.data()
      vln.data <- group_by(vln.data, Gene)
      # plot
      vplot <- ggplot(vln.data,aes(x=cluster,y=Quant,fill=cluster)) +
        geom_violin(scale = "width")+
        #geom_jitter(alpha=0.5, size = 0.25) +
        theme_bw()+
        facet_wrap(~Gene)
    })

    output$vln <- shiny::renderPlot({
      vln <- vplot()
      vln
    })
    vgplot <- shiny::reactive({
      vln.data <- gene.data()
      # plot
      vgplot <- ggplot(vln.data,aes(x=cluster,y=Quant,fill=cluster)) +
        geom_violin(scale = "width")+
        geom_jitter(alpha=0.5, size = 0.25) +
        theme_bw()+
        labs(title = input$geneNames)
    })

    output$genevln <- shiny::renderPlot({
      vlnG <- vgplot()
      vlnG
    })



    #save plots
    #space gene plot
    output$downSpacegne<-downloadHandler(
      filename = function() {
        paste('plot', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, sgplot())
      },
      contentType = 'image/eps')

    #umap gene plot
    output$downUgne<-downloadHandler(
      filename = function() {
        paste('plot', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, ugplot())
      },
      contentType = 'image/eps')

    # all space gene plot
    output$downAll<-downloadHandler(
      filename = function() {
        paste('plot', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, allplot())
      },
      contentType = 'image/eps')

    # space cluster plot
    output$downSpaceClus<-downloadHandler(
      filename = function() {
        paste('plot', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, scplot())
      },
      contentType = 'image/eps')

    # umap cluster plot
    output$downUclus<-downloadHandler(
      filename = function() {
        paste('plot', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, ucplot())
      },
      contentType = 'image/eps')

    # table
    output$dtable<-downloadHandler(
      filename = function() {
        paste('table', '.cvs', sep='')
      },
      content=function(file){
        write.csv(clus.data(),file, row.names = FALSE)
      })
    # indi vln plot
    output$downgVln<-downloadHandler(
      filename = function() {
        paste('vlnplot', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, vgplot())
      },
      contentType = 'image/eps')

    # total vln plot
    output$downVln<-downloadHandler(
      filename = function() {
        paste('vlnplot', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, vplot())
      },
      contentType = 'image/eps')

  }

  # Run the application
  shinyApp(ui = ui, server = server)


}
