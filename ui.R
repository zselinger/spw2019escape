source('global.R')
ui <- fluidPage(
  useShinyjs(),
  navbarPage(title = "Escape Custom Tools", id = "title", windowTitle = "Escape Custom Tools",theme = shinytheme("spacelab"),
             tabPanel("Settings",
                      sidebarLayout(
                        sidebarPanel(
                          googleAuthUI("gaLogin"),
                          uiOutput("globalSettings")),
                        mainPanel()
                      )
             ),
             navbarMenu("Google AdWords",
                        tabPanel("N-Grams",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("grabData", "Grab Data"),
                                                hidden(
                                                  p(id = "getting", "Getting Data"),
                                                  p(id = "ready", "Data Ready")
                                                ),
                                                hr(),
                                                radioButtons("gramType", "Choose Type", 
                                                             choices = c("Unigrams", "Bigrams")),
                                                hr(),
                                                p("Filter by:"),
                                                textInput("campaign", "Campaign Name"),
                                                textInput("adGroup", "Ad Group Name"),
                                                textInput("keyword", "Keyword"),
                                                actionButton("plotTable", "Show Data"),
                                                hidden(actionButton("writeData", "Download Data")),
                                                hidden(
                                                  p(id = "gettingTable", "Generating Table"),
                                                  p(id = "readyTable", "Table Ready")),
                                                hr(),
                                                p("Plot"),
                                                selectInput("metric", "Metric:",
                                                            c("Clicks" = "Clicks",
                                                              "Impressions" = "Impressions",
                                                              "Conversions" = "Conversions",
                                                              "Cost" = "Cost",
                                                              "ConversionValue" = "ConversionValue",
                                                              "CTR" = "Ctr",
                                                              "Conversion Rate" = "ConversionRate",
                                                              "CPA" = "Cpa",
                                                              "CPC" = "AvgCpc",
                                                              "Frequency" = "Frequency"
                                                            )),
                                                radioButtons("facetType", "By:", 
                                                             choices = c("Total", "Campaign", "Adgroup", "Keyword")),
                                                actionButton("plotNgram", "Plot Data")
                                   ),
                                   mainPanel(
                                     DTOutput("plotTable"),
                                     plotOutput("nGramsPlot", height = "1500px")
                                   )
                                 )
                        )
             ),
             navbarMenu("Google Analytics",
                        tabPanel("Attribution Analysis",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                selectInput("sourceMetric", "Source Metric:",
                                                            c("Source/Medium" = "sourceMedium",
                                                              "Channel" = "channel")),
                                                actionButton("grabAttributionData", "Grab Data"),
                                                hidden(
                                                  p(id = "gettingAt", "Getting Data"),
                                                  p(id = "readyAt", "Data Ready")
                                                ),
                                                uiOutput("attributionFilters")),
                                   mainPanel(
                                     plotOutput("modelComparison"),
                                     DTOutput("attributionTable"),
                                     plotOutput("tsPlot")
                                   )
                                 )
                        )
                        
             )
  )
)