#' Launches the Gone mFISHing shiny app
#'
#' @author Kaitlin E Sullivan
#'
#' @param mfish An mFISH object
#' @return Launch Gone mFISHing shiny app
#'
#' @import shiny shinyWidgets umap ggplot2 shinydashboard tidyr dplyr shinythemes shinybusy
#'
#' @export
newFISH <- function(mFISH, filter.by=NA){
  #Kaitlin Sullivan November 2020
  #This shiny app allows the naive scientist to browse and visualize analyzed
  #mFISH data in a user-friendly way


  # load the known quantified mFISH data
  mfish <- mFISH

  #genes for filtering
  filgenes <- dplyr::select(mFISH@rawData, -id)
  filgenes <- names(filgenes)

  #raw data
  raw <- mFISH@rawData
  #meta data
  meta <- mFISH@metaData

  if(is.na(filter.by)){
    filter.by <- filgenes[1]
  }

  if(!filter.by %in% filgenes){
    warning("Selected filter.by gene is not present in mFISH object.")
  }


  #groupings
  grouping <- dplyr::select(mFISH@metaData, -c(X,Y,id))
  #throw error if numeric
  grouping <- names(grouping)
  if(length(grouping)==0){
    grouping <- NA
  }

  ui <- shiny::fluidPage(
      #theme
      theme = shinythemes::shinytheme("cyborg"),
        #progress bar :meowparty:
        shinybusy::add_busy_gif(
          src = "https://emojis.slackmojis.com/emojis/images/1563480763/5999/meow_party.gif?1563480763",
          height = 200, width = 200,
          timeout = 10,
          position = "full-page"
          ),

            # Title of this page
            titlePanel("Gone mFISHing"),
      h5("1. Select analysis settings"),

########## BOTTOM BAR

            #Top bar with initial preprocessing

                  shiny::fluidRow(

                      #column 1 = filtering
                      shiny::column( width = 6,
                        wellPanel( style="padding: 5px;",
                        #sidebar heading number 2 with info on clustering
                        h6("Preprocess Data"),
                        ###INPUTS
                        #filter by gene
                        div(style="padding: 0px;",
                          selectInput(
                          "mg",
                          "Filter by:",
                          choices = filgenes,
                          selected = filter.by,
                          multiple = TRUE
                        )),

                        #threshold for filtering
                        div(style="padding: 0px;",
                          sliderInput(
                          "filter",
                          "Filter Threshold:",
                          value = 0.1,
                          min = 0,
                          max = 2,
                          step = 0.1
                        )),

                        #how many pcs?
                        div(style="padding: 0px;",
                            sliderInput(
                          "npcs",
                          "Number of Principle Components to use:",
                          min = 3,
                          max = length(filgenes),
                          value = length(filgenes),
                          step = 1
                        ))
                      ),
                      #row 3
                      checkboxInput(
                        "out",
                        "Remove outliers?",
                        value = T
                      )
                    ),

                    #column 3 = umap settings
                    column(width = 6,
                      wellPanel(style="padding: 5px;",
                      h6("Dimensionality Reduction"),
                      #metric
                      div(style="padding: 0px;",
                          selectInput(
                            "metric",
                            "UMAP Distance Metric:",
                            choices = c('manhattan', 'euclidean'),
                            selected = 'manhattan'
                      )),
                      #NN
                      div(style="padding: 0px;",
                        sliderInput(
                        "nn",
                        "UMAP Nearest Neighbours",
                        min = 4,
                        max = 40,
                        value = 15,
                        step = 1
                      )),

                      #mindist?
                      div(style="padding: 0px;",
                        sliderInput(
                        "mindist",
                        "UMAP Minimum Distance:",
                        min = 0.05,
                        max = 0.5,
                        value = 0.1,
                        step = 0.05
                      ))
                    )
                  )
                ),
                br(),
                # fluidRow(
                #   column(
                #   12,
                #   actionButton(
                #   "go",
                #   "Analyze"
                #   )
                # , align = "center"
                # , style = "margin-bottom: 10px;"
                # , style = "margin-top: -10px;"
                #   )
                # ),
#### RAW GENE OUTPUTS
                #Raw gene data
                shiny::fluidRow(
                  h5("2. Visualize raw gene expression"),
                  shiny::sidebarLayout(
                    #editing gene selection
                    shiny::sidebarPanel(
                      width = 2,
                      h6("Raw Expression"),
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
                      #shiny::fluidRow(
                        column(
                          width=4,
                          #h3("Native Tissue"), br(),
                          shiny::plotOutput("rawSpace"),
                          br(),
                          shiny::downloadButton(
                            "rs",
                            "Download EPS"
                          )
                        ),
                        column(
                          width=4,
                          #h3("UMAP Space"), br(),
                          shiny::plotOutput("rawUMAP"),
                          br(),
                          shiny::downloadButton(
                            "ru",
                            "Download EPS"
                          )
                        ),
                        column(
                          width=4,
                          #h3("Gene Boxplot"), br(),
                          shiny::plotOutput("rawBoxPlot"),
                          br(),
                          shiny::downloadButton(
                            "rbp",
                            "Download EPS"
                          )
                        )
                      )
                    )
                  ), br(),
#CLUSTERED DATA PANEL
                shiny::fluidRow(
                  h5("3. Visualize clustered data"),
                  shiny::sidebarLayout(
                    shiny::sidebarPanel(
                      width=2,
                      h6("Clustered Data"),
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
                        "Download Object"
                      )
                    ),
                    shiny::mainPanel(
                      #shiny::fluidRow(
                        column(
                          width=4,
                          #h3("Native Tissue"), br(),
                          shiny::plotOutput("clusSpace"),
                          br(),
                          shiny::downloadButton(
                            "cs",
                            "Download EPS"
                          )
                        ),
                        column(
                          width=4,
                          #h3("UMAP Space"), br(),
                          shiny::plotOutput("clusUMAP"),
                          br(),
                          shiny::downloadButton(
                            "cu",
                            "Download EPS"
                          )
                        ),
                        column(
                          width=4,
                          #h3("Cluster Boxplot"), br(),
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


########## SERVER ###########
  ## Define server logic required to plot
  server <- function(input, output){


#REACTIVE MISC
#reactive gene name list
    mygenes <- shiny::reactive({
      mygenes <- filgenes
      if(input$mg %in% filgenes){
        colNum <- match(input$mg, filgenes)
        mygenes <- filgenes[-colNum]
      }
      mygenes
    })

#select genes from user id dataset
    output$geneIn <- shiny::renderUI({
      shiny::selectInput('geneNames', 'Select A Gene...', mygenes())
    })

#reactive attributes list
     attribs <- shiny::reactive({
       attribs <- list(filter.by = input$mg, thresh = input$filter,
                      umap_nn = input$nn, umap_mindist = input$mindist,
                      umap_metric = input$metric, hclust_k = input$nclus,
                      hclust_metric = 'manhattan', pca = pca(),
                      npc = input$npcs, umap = mu())

       if(input$out){
         attribs$remove.outliers <- c(1, length(mygenes()))
       }

       attribs
     })

#reactive metadata for object building
     metaReact <- shiny::reactive({
       cc <- clusDat()
       metaReact <- dplyr::mutate(meta, cluster = clusDat$cluster)
       metaReact
     })


#####NEW: USER INPUT TO RUN
      ###DATA FILTERING
      #filtering user input
      mf <- shiny::reactive({
        print("begin filtering")
        mf <- dplyr::filter(raw, input$mg > input$filter)
        mf <- dplyr::select(mf, -(input$mg))
        #mf <- RUHi::ruFilter(mfish, threshold=input$filter,
                             #filter.by = input$mg)
        print("filtering completed")
        mf
      })

      ###DATA PREPROCESSING
      #remove outliers
      mp <- shiny::reactive({
        print("begin preprocessing")
        mp <- mf()

        if(input$out){
          #find cells with number of features outside of threshold
          prepro <- dplyr::mutate(mp, nfts =rowSums(df>0))

          #low/high
          prepro <- dplyr::filter(prepro, nfts>1)
          prepro <- dplyr::filter(prepro, nfts<length(mygenes()))

          #remove labs
          prepro <- dplyr::select(prepro, -nfts)
          mp <- prepro
        }
        print("preprocessing completed")

        #mp <- RUHi::ruProcess(prepro, remove.outliers = input$out,
                              #outlier.thresh = c(1, length(mygenes())))
        mp
      })

######DIM REDUCTION
      #RUN AND SAVE A PCA
      pca <- shiny::reactive({
        print("begin PCA")
        df <- dplyr::filter(mp(), -id)
        p <- prcomp(df, centre=T, scale=T)
        pca <- p$x
        print("PCA completed")

        pca
      })


      ###UMAP
      #run umap structure
      mu <- shiny::reactive({
        print("begin UMAP")
        df <- pca()
        df <- df[,1:input$npcs]

        config <- umap::umap.defaults
        config$n_neighbors <- input$nn
        config$min_dist <- input$mindist
        config$metric <- input$metric

        mu <- umap::umap(df, config = config)

        mu <- mu$layout

        #mu <- RUHi::ruUMAP(pca, metric = input$metric, nn = input$nn,
                           #min.dist = input$mindist, npc = input$npcs)
        print("UMAP completed")
        mu
      })

      ###DATA CLUSTERING
      #run hclust
      mc <- shiny::reactive({
        print("begin clustering")

        df <- pca()
        df <- df[,1:input$npcs]

        hc <- hclust(d=dist(df), method="ward.D2")
        clus <- cutree(hc, k=input$nclus)

        mc <- as.factor(clus)

        #mc <- RUHi::ruCluster(umap, k=input$nclus, npc = input$npcs)
        print("clustering completed")
        mc
      })

#########PLOT OUTPUT

      ###GRAPHING DATAFRAMES

      #SAVE FILTERED IDS
      filIds <- shiny::reactive({
        filIds <- dplyr::filter(mp(), id)
      })

      #SAVE CLUSTERS FOR METADATA
      clusDat <- shiny::reactive({
        #save filtered data
        inid <- filIds()
        inid <- dplyr::mutate(inid, cluster=mc())
        #save full data
        outid <- dplyr::filter(meta, !id %in% inid$id)
        outid <- dplyr::mutate(outid, cluster=paste(input$filter.by, "neg", sep="_"))
        outid <- dplyr::select(outid, c(id, cluster))

        #bind together
        clusDat <- rbind(inid, outid)
        clusDat <- dplyr::arrange(clustDat, id)

        #return
        clusDat
      })

      #SPATIAL COORDS FOR FILTERED DATA
      #wide
      gWide <- shiny::reactive({
        df <- mp()
        md <- dplyr::filter(meta, id %in% df$id)

        #add metadata to df
        df <- dplyr::mutate(df, UMAP_1 = attribs$umap[,1],
                            UMAP_2 = attribs$umap[,2], cluster = mc(),
                            X = md$X, Y = md$Y)

        #add other factorial gorupings
        if(!is.na(grouping)){
          for(i in 1:length(grouping)){
            #save the index of column
            ci <- grep(grouping[i], colnames(meta))
            #save varaible
            df <- dplyr::mutate(df, !!grouping[i] := meta[,ci])
          }
        }

        gWide <- df

        gWide

      })

      #long (for violins)
      gLong <- shiny::reactive({
        wide <- gWide()

        gLong <- tidyr::pivot_longer(wide, cols = mygenes(),
                                     names_to = "Gene",
                                     values_to = "Expression")

        gLong
      })


#SPATIAL
  #RAW GENE
    sgplot <- shiny::reactive({

      sgplot <- ggplot2::ggplot(gWide(), aes(x=X, y=Y, colour=input$geneIn))+
        geom_point()+
        theme_classic()+
        scale_fill_gradientn(values=c("cyan", "red"))+
        labs(title=paste("Spatial Location of ", input$geneIn, " Expression",
                         sep=""))

      #debug
      if(!is.na(grouping)){
        sgplot <- sgplot +
          facet_wrap(~input$groups)
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
      sgplot
    })

#:meowparty:
    output$rawSpace <- shiny::renderPlot({
      shinybusy::play_gif()
      rawSpace <- sgplot()
      shiny::busyupdate_busy_bar(100)
      shinybusy::stop_gif()
      rawSpace
    })

##CLUSTERED
    scplot <- shiny::reactive({

      scplot <- ggplot2::ggplot(gWide(), aes(x=X, y=Y, colour=cluster))+
        geom_point()+
        theme_classic()+
        labs(title="Spatial Location of Clusters")

      #debug
      if(!is.na(grouping)){
        scplot <- scplot +
          facet_wrap(~input$groups)
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
      scplot
    })
#:meowparty:
    output$clusSpace <- shiny::renderPlot({
      shinybusy::play_gif()
      clusSpace <- scplot()
      shinybusy::update_busy_bar(100)
      shinybusy::stop_gif()
      clusSpace
    })

#DIM REDUCTION
    #RAW GENE
    rumap <- shiny::reactive({
      rumap <- ggplot2::ggplot(gWide(), aes(x=UMAP_1, y=UMAP_2, colour=input$geneIn))+
        geom_point()+
        theme_classic()+
        scale_fill_gradientn(values=c("cyan", "red"))+
        labs(title=paste("UMAP with ", input$geneIn, " Expression",
                         sep=""))


      rumap <- RUHi::plotDim(mc(), colour.by = input$geneIn)+
        labs(title = paste(input$geneIn))
      rumap
    })

    #:meowparty:
    output$rawUMAP <- shiny::renderPlot({
      shinybusy::play_gif()
      rawUMAP <- rumap()
      shinybusy::update_busy_bar(100)
      shinybusy::stop_gif()
      rawUMAP
    })

  #CLUSTERED
    cumap <- shiny::reactive({
      cumap <- ggplot2::ggplot(gWide(), aes(x=UMAP_1, y=UMAP_2, colour=cluster))+
        geom_point()+
        theme_classic()+
        labs(title="UMAP with Clusters")
      cumap
    })

    #:meowparty:
    output$clusUMAP <- shiny::renderPlot({
      shinybusy::play_gif()
      clusUMAP <- cumap()
      shinybusy::update_busy_bar(100)
      shinybusy::stop_gif()
      clusUMAP
    })

  #BOX PLOTS
    #RAW GENE
    rawbox <- shiny::reactive({
      rawbox <- ggplot2::ggplot(gWide(), aes(x=cluster, y=input$geneIn, fill=cluster))+
        geom_boxplot(outlier.shape = NA)+
        theme_classic()+
        labs(y="Expression", title = input$geneIn)
      rawbox
    })

    #:meowparty:
    output$rawBP <- shiny::renderPlot({
      shinybusy::play_gif()
      rawBP <- rawbox()
      shinybusy::update_busy_bar(100)
      shinybusy::stop_gif()
      rawBP
    })

    #CLUSTERED
    clusbox <- shiny::reactive({
      clusbox <- ggplot2::ggplot(gLong(), aes(x=Gene, y=Expression, fill=Gene))+
        geom_boxplot(outlier.shape = NA)+
        theme_classic()+
        facet_wrap(~cluster)+
        scale_colour_manual(values = rainbow(n=length(mygenes())))
      clusbox
    })

    #:meowparty:
    output$clusBP <- shiny::renderPlot({
      shinybusy::play_gif()
      clusBP <- clusbox()
      shinybusy::update_busy_bar(100)
      shinybusy::stop_gif()
      clusBP
    })




#####DOWNLOAD BUTTONS

    #OBJECT
    output$obj<-downloadHandler(
      filename = function() {
        paste('goFISH', '.rds', sep='')
      },
      content=function(file){
        saveRDS(mc(), file)
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

