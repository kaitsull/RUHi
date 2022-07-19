#' Launches the Gone mFISHing shiny app
#'
#' @author Kaitlin E Sullivan
#'
#' @param mfish An mFISH object
#' @return Launch Gone mFISHing shiny app
#'
#' @import shiny shinyWidgets umap ggplot2 shinydashboard tidyr dplyr shinythemes shinybusy RUHi
#'
#' @export
newFISH <- function(mFISH){
  #Kaitlin Sullivan November 2020
  #This shiny app allows the naive scientist to browse and visualize analyzed
  #mFISH data in a user-friendly way


  # load the known quantified mFISH data
  mfish <- mFISH
  filgenes <- names(mFISH@rawData)

  #groupings
  grouping <- dplyr::select(mFISH@metaData, -c(X,Y,id))
  #throw error if numeric
  grouping <- names(grouping)
  if(length(grouping)==0){
    grouping <- NA
  }

  ui <- shinydashboard::dashboardPage(

    #Create a dashboard
    shiny::fluidPage(
      #theme
      theme = shinythemes::shinytheme("cyborg"),
        #progress bar :meowparty:
        add_busy_gif(
          src = "https://emojis.slackmojis.com/emojis/images/1563480763/5999/meow_party.gif?1563480763",
          height = 70, width = 70,
          timeout = 10,
          position = "full-page"
          ),

            # Title of this page
            titlePanel("Gone mFISHing"),
########## TOP BAR
            #Top bar with initial preprocessing
                  shiny::fluidRow(

                    #column 1 = filtering
                    shiny::column(
                      #sidebar heading number 2 with info on clustering
                      h4("Filtering:"),
                      br(),
    ###INPUTS
                   #filter by gene
                      selectInput(
                        "mg",
                        "Filter by:",
                        choices = filgenes,
                        selected = filgenes[1],
                        multiple = TRUE
                      ),

                    #threshold for filtering
                      sliderInput(
                        "filter",
                        "Filter Threshold:",
                        value = 0.1,
                        min = 0,
                        max = 2,
                        step = 0.1
                      ),
                      br()
                    ),

                    #column 2 = preprocessing
                    #remove outliers?
                    column(
                      h4("Preprocessing:"),
                      checkboxInput(
                        "out",
                        "Remove outliers?",
                        value = F
                      ),

                      #how many pcs?
                      sliderInput(
                        "npcs",
                        "Number of Principle Components to use:",
                        min = 3,
                        max = length(filgenes),
                        value = length(filgenes)
                      ),
                      br()
                  ),
                  #column 3 = umap settings
                  column(
                    h4("Dimensionality Reduction:"),
                    #NN
                    sliderInput(
                      "nn",
                      "UMAP Nearest Neighbours",
                      min = 4,
                      max = 40,
                      value = 15,
                      step = 1
                    ),

                    #mindist?
                    sliderInput(
                      "mindist",
                      "UMAP Minimum Distance:",
                      min = 0.05,
                      max = 0.5,
                      value = 0.1,
                      step = 0.05
                    ),
                    #metric
                    selectInput(
                      "metric",
                      "UMAP Distance Metric:",
                      choices = c('manhattan', 'euclidean'),
                      selected = 'manhattan'
                    ),
                    br()
                  ),
                ),
#### RAW GENE OUTPUTS
                #Raw gene data
                shiny::fluidRow(
                  shiny::sidebarLayout(
                    #editing gene selection
                    shiny::sidebarPanel(
                      h4("Raw Expression..."),
                      shiny::uiOutput("geneIn"),
                      br(),
                      shiny::checkboxInput("flipxS", "Flip X Axis",
                                           value = FALSE),
                      shiny::checkboxInput("flipyS", "Flip Y Axis",
                                           value = FALSE),
                      shiny::checkboxInput("rotateS", "Rotate 90 degrees",
                                           value = FALSE)
                    ),

                    #RAW DATA PANEL
                    shiny::mainPanel(
                      shiny::fluidRow(
                        column(
                          h3("Native Tissue"), br(),
                          shiny::plotOutput("rawSpace"),
                          br(),
                          shiny::downloadButton(
                            "rs",
                            "Download EPS"
                          )
                        ),
                        column(
                          h3("UMAP Space"), br(),
                          shiny::plotOutput("rawUMAP"),
                          br(),
                          shiny::downloadButton(
                            "ru",
                            "Download EPS"
                          )
                        ),
                        column(
                          h3("Gene Boxplot"), br(),
                          shiny::plotOutput("rawBoxPlot"),
                          br(),
                          shiny::downloadButton(
                            "rbp",
                            "Download EPS"
                          )
                        )
                      )
                    )
                  )
                ),
#CLUSTERED DATA PANEL
                shiny::fluidRow(
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      h4("Clustered Data:"),
                      #nclus
                      sliderInput(
                        "nclus",
                        "Number of Clusters",
                        min = 1,
                        max = 15,
                        value = 3,
                        step = 1
                      ), br(),
                      #NEW ADDITION
                      shiny::checkboxGroupInput("groups",
                                                "Group Plots by Metadata:",
                                                choices=grouping), br(),
                      shiny::downloadButton(
                        "obj",
                        "Download Current Analysis"
                      )
                    ),
                    shiny::mainPanel(
                      shiny::fluidRow(
                        column(
                          h3("Native Tissue"), br(),
                          shiny::plotOutput("clusSpace"),
                          br(),
                          shiny::downloadButton(
                            "cs",
                            "Download EPS"
                          )
                        ),
                        column(
                          h3("UMAP Space"), br(),
                          shiny::plotOutput("clusUMAP"),
                          br(),
                          shiny::downloadButton(
                            "cu",
                            "Download EPS"
                          )
                        ),
                        column(
                          h3("Cluster Boxplot"), br(),
                          shiny::plotOutput("clusBoxPlot"),
                          br(),
                          shiny::downloadButton(
                            "cbp",
                            "Download EPS"
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
      mygenes <- filgenes
      if(input$mg %in% selgenes){
        colNum <- match(input$mg, filgenes)
        mygenes <- fillgenes[-colNum]
      }
      mygenes
    })

#select genes from user id dataset
    output$geneIn <- shiny::renderUI({
      shiny::selectInput('geneNames', 'Select A Gene...', mygenes())
    })


###DATA FILTERING
    #filtering user input
    mf <- shiny::reactive({
      mf <- RUHi::ruFilter(mfish, threshold=input$filter,
                           filter.by = input$mg)
      mf
    })

###DATA PREPROCESSING
    #remove outliers
    mp <- shiny::reactive({
      prepro <- mf()
      mp <- RUHi::ruProcess(prepro, remove.outliers = input$out,
                             outlier.thresh = c(1, length(mygenes())))
      mp
    })

###DIM REDUCTION
    #run umap structure
    mu <- shiny::reactive({
      pca <- mp()
      mu <- RUHi::ruUMAP(pca, metric = input$metric, nn = input$nn,
                         min.dist = input$mindist, npc = input$npcs)
      mu
    })

###DATA CLUSTERING
    #run hclust
    mc <- shiny::reactive({
      umap <- mu()
      mc <- RUHi::ruCluster(umap, k=input$nclus)
      mc
    })


###PLOT OUTPUTS

#SPATIAL
  #RAW GENE
    rawSpace <- shiny::reactive({
      if(is.na(grouping)){
        sgplot <- RUHi::plotSpace(mc(), colour.by = input$geneIn)
      }
      else{
        sgplot <- RUHi::plotSpace(mc(), colour.by = input$geneIn,
                                  group.by = input$groups)
      }

      if(input$flipxS){
        sgplot <- sgplot + scale_x_reverse()
      }
      if(input$flipyS){
        sgplot <- sgplot + scale_y_reverse()
      }
      if(input$rotateS){
        sgplot <- sgplot + coord_flip()
      }
      sgplot <- rawSpace
      rawSpace
    })
#:meowparty:
    output$rs <- shiny::renderPlot({
      play_gif()
      rs <- rawSpace()
      update_busy_bar(100)
      stop_gif()
      rs
    })

##CLUSTERED
    clusSpace <- shiny::reactive({
      if(is.na(grouping)){
        sgplot <- RUHi::plotSpace(mc())
      }
      else{
        scplot <- RUHi::plotSpace(mc(),
                                  group.by = input$groups)
      }

      if(input$flipxS){
        scplot <- scplot + scale_x_reverse()
      }
      if(input$flipyS){
        scplot <- scplot + scale_y_reverse()
      }
      if(input$rotateS){
        scplot <- scplot + coord_flip()
      }
      scplot <- clusSpace
      clusSpace
    })
#:meowparty:
    output$cs <- shiny::renderPlot({
      play_gif()
      cs <- clusSpace()
      update_busy_bar(100)
      stop_gif()
      cs
    })

#DIM REDUCTION
    #RAW GENE
    rawUMAP <- shiny::reactive({
      rawUMAP <- RUHi::plotDim(mc(), colour.by = input$geneIn)
      rawUMAP
    })

    #:meowparty:
    output$ru <- shiny::renderPlot({
      play_gif()
      ru <- rawUMAP()
      update_busy_bar(100)
      stop_gif()
      ru
    })

  #CLUSTERED
    clusUMAP <- shiny::reactive({
      clusUMAP <- RUHi::plotDim(mc())
      clusUMAP
    })

    #:meowparty:
    output$cu <- shiny::renderPlot({
      play_gif()
      cu <- rclusUMAP()
      update_busy_bar(100)
      stop_gif()
      cu
    })

  #BOX PLOTS
    #RAW GENE
    rawBP <- shiny::reactive({
      rawBP <- RUHi::geneBoxPlot(mc(), input$geneIn)
      rawBP
    })

    #:meowparty:
    output$rbp <- shiny::renderPlot({
      play_gif()
      rbp <- rawBP()
      update_busy_bar(100)
      stop_gif()
      rbp
    })

    #CLUSTERED
    clusBP <- shiny::reactive({
      clusBP <- RUHi::plotDim(mc())+
        scale_colour_manual(values = rainbow(n=3))
      clusBP
    })

    #:meowparty:
    output$cbp <- shiny::renderPlot({
      play_gif()
      cbp <- clusBP()
      update_busy_bar(100)
      stop_gif()
      cbp
    })




#####DOWNLOAD BUTTONS

    #OBJECT
    output$obj<-downloadHandler(
      filename = function() {
        paste('goFISH', '.rds', sep='')
      },
      content=function(file){
        saveRDS(curObj(), file)
      })

    # raw space plot
    output$rs<-downloadHandler(
      filename = function() {
        paste(geneIn ,'Spatial', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, rawSpace())
      },
      contentType = 'image/eps')

    # raw umap plot
    output$ru<-downloadHandler(
      filename = function() {
        paste(geneIn, 'UMAP', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, rawUMAP())
      },
      contentType = 'image/eps')

    # raw box blot
    output$rbp<-downloadHandler(
      filename = function() {
        paste(geneIn, 'BoxPlot', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, rawBP())
      },
      contentType = 'image/eps')

    # cluster spatial
    output$cs<-downloadHandler(
      filename = function() {
        paste('clusterSpatial', '.cvs', sep='')
      },
      content=function(file){ggsave(file, clusSpace())
      },
      contentType = 'image/eps')

    # cluster umap
    output$cu<-downloadHandler(
      filename = function() {
        paste('clusterUMAP', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, clusUMAP())
      },
      contentType = 'image/eps')

    # total cluster bp
    output$cbp<-downloadHandler(
      filename = function() {
        paste('clusterBoxPlot', '.eps', sep='')
      },
      content=function(file){
        ggsave(file, clusBP())
      },
      contentType = 'image/eps')

  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
