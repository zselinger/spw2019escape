source('global.R')
server <- function(input, output, session) {
  
  analyticsAccessToken  <- callModule(googleAuth, "gaLogin")
  adwordsAccessToken <- doAuth()
  
  globalSearchTermPerformance <- NULL
  globalNgram <- NULL
  globalPlot <- NULL
  gaProductData <- NULL
  gaMerged <- NULL
  gaMergeFiltered <- NULL
  gaAttribution <- NULL
  selectedFilters <- ""
  fullTitle <- ""
  
  
  account_list <- reactive({
    validate(
      need(analyticsAccessToken(), "Please log in to see settings")
    )
    with_shiny(ga_account_list,
               shiny_access_token = analyticsAccessToken())
  })
  
  selectedId <- callModule(authDropdown, "auth_menu", ga.table = account_list)
  
  output$globalSettings <- renderUI({
    accList <- account_list()
    tagList(
      authDropdownUI("auth_menu"),
      textInput("adwordsAccountId", "Account ID",
                placeholder = "XXX-XXX-XXXX",
                width = "35%", value = "111-111-1111"),
      dateRangeInput("dateRange","Date Range")
    )
  })
  
  graphTitle <- reactive({
    
    accList <- account_list()
    selectedAcc <- accList %>% filter(viewId == selectedId()) %>% select(accountName, webPropertyName, viewName) 
    paste(selectedAcc[1,1], selectedAcc[1,2], selectedAcc[1,3], sep = " > ")
    
  })
  
  observeEvent(input$grabData,{
    
    shinyjs::show("getting")
    shinyjs::hide("ready")
    
    searchTermPerformanceQuery <- statement(select=c("CampaignName","AdGroupName","KeywordId","Query","Clicks",
                                                     "Impressions","Conversions","Cost","ConversionValue"),
                                            report="SEARCH_QUERY_PERFORMANCE_REPORT",
                                            start=input$dateRange[1],
                                            end=input$dateRange[2])
    
    searchTermPerformance <- getData(clientCustomerId=input$adwordsAccountId,
                                     google_auth=adwordsAccessToken, statement=searchTermPerformanceQuery) 
    
    keywordPerformanceQuery <- statement(select=c("Id", "Criteria"),
                                         report="KEYWORDS_PERFORMANCE_REPORT",
                                         start=input$dateRange[1],
                                         end=input$dateRange[2])
    
    keywordPerformance <- getData(clientCustomerId=input$adwordsAccountId,
                                  google_auth=adwordsAccessToken, statement=keywordPerformanceQuery) 
    
    searchTermPerformance <- left_join(searchTermPerformance, keywordPerformance, by = "KeywordID")
    globalSearchTermPerformance <<- searchTermPerformance
    
    shinyjs::hide("getting")
    shinyjs::show("ready")
  })
  
  observeEvent(input$plotTable,{
    
    shinyjs::hide("writeData")
    shinyjs::show("gettingTable")
    
    if(input$campaign != "" && input$adGroup != "" && input$keyword != ""){
      filtered <- globalSearchTermPerformance %>% filter(Campaign == input$campaign, Adgroup == input$adGroup, 
                                                         Keyword == input$keyword)
    } else if(input$campaign != "" && input$adGroup != "") {
      filtered <- globalSearchTermPerformance %>% filter(Campaign == input$campaign, Adgroup == input$adGroup)
    } else if(input$campaign != "") {
      filtered <- globalSearchTermPerformance %>% filter(Campaign == input$campaign)
    } else{
      filtered <- globalSearchTermPerformance
    }
    
    if(input$gramType == "Unigrams"){
      unigrams <- filtered %>% unnest_tokens(word, Searchterm) %>% anti_join(stop_words) %>%
        group_by(Campaign, Adgroup, KeywordID, Keyword, word) %>%
        summarize(Frequency = n(), FrequencyPerc = n()/nrow(filtered), 
                  Clicks=sum(Clicks), ClickPerc = sum(Clicks)/sum(filtered$Clicks), 
                  Impressions=sum(Impressions), ImpressionPerc = sum(Impressions)/sum(filtered$Impressions), 
                  Conversions=sum(Conversions), ConversionPerc = sum(Conversions)/sum(filtered$Conversions),
                  ConversionValue = sum(ConversionValue), 
                  ConversionValuePerc = sum(ConversionValue)/sum(filtered$ConversionValue),
                  Cost=sum(Cost), CostPerc = sum(Cost)/sum(filtered$Cost),
                  Ctr=sum(Clicks)/sum(Impressions), ConversionRate=sum(Conversions)/sum(Clicks), 
                  Cpa=sum(Cost)/sum(Conversions),
                  AvgCpc = sum(Cost)/sum(Clicks))
      
      globalNgram <<- unigrams
    } else{
      bigrams <- filtered %>% unnest_tokens(word, Searchterm, token="ngrams", n=2) %>%
        group_by(Campaign, Adgroup, KeywordID, Keyword, word) %>%
        summarize(Frequency = n(), FrequencyPerc = n()/nrow(filtered), 
                  Clicks=sum(Clicks), ClickPerc = sum(Clicks)/sum(filtered$Clicks), 
                  Impressions=sum(Impressions), ImpressionPerc = sum(Impressions)/sum(filtered$Impressions), 
                  Conversions=sum(Conversions), ConversionPerc = sum(Conversions)/sum(filtered$Conversions),
                  ConversionValue = sum(ConversionValue), 
                  ConversionValuePerc = sum(ConversionValue)/sum(filtered$ConversionValue),
                  Cost=sum(Cost), CostPerc = sum(Cost)/sum(filtered$Cost),
                  Ctr=sum(Clicks)/sum(Impressions), ConversionRate=sum(Conversions)/sum(Clicks), 
                  Cpa=sum(Cost)/sum(Conversions),
                  AvgCpc = sum(Cost)/sum(Clicks))
      
      globalNgram <<- bigrams %>% ungroup()
      
      shinyjs::hide("gettingTable")
    }
    
    output$plotTable <- renderDT({
      datatable(globalNgram, filter = "top", options = list(scrollX = T)) %>% 
        formatCurrency(c("Cost", "AvgCpc","Cpa", "ConversionValue")) %>% 
        formatPercentage(c("Ctr", "ConversionRate", "FrequencyPerc", "ClickPerc", "ImpressionPerc",
                           "ConversionPerc", "ConversionValuePerc", "CostPerc"), 2)
    })
    
    shinyjs::show("writeData")
    
  })
  
  observeEvent(input$plotNgram,{
    
    
    if(input$facetType != "Total"){
      ds <- globalNgram %>% group_by(!! sym(input$facetType), word) %>% 
        summarise(metric = sum(!! sym(input$metric))) %>% 
        arrange(desc(metric)) %>% slice(1:10) %>% ungroup()
      
      ds$r <- nrow(ds):1
      
      p <- ds %>% ggplot(aes(x=r, y=metric)) + geom_col(aes(fill = !! sym(input$facetType))) + 
        facet_wrap(~!! sym(input$facetType), scales = "free") + coord_flip() +
        scale_x_continuous(breaks = ds$r, labels = ds$word) + theme(legend.position="none")
    } else{
      
      ds <- globalNgram %>% group_by(word) %>% summarise(metric = sum(!! sym(input$metric))) %>% 
        arrange(desc(metric)) %>% slice(1:10) %>% ungroup()
      
      ds$r <- nrow(ds):1
      
      p <- ds %>% ggplot(aes(x=r, y=metric)) + geom_col() + coord_flip() +
        scale_x_continuous(breaks = ds$r, labels = ds$word) + theme(legend.position="none")
      
    }
    
    p1 <- switch(input$facetType,
                 "Campaign" = p + facet_wrap(~Campaign, scales = "free", ncol = 3),
                 "Adgroup" = p + facet_wrap(~Adgroup, scales = "free", ncol = 3),
                 "Keyword" = p + facet_wrap(~Keyword, scales = "free", ncol = 3),
                 "Total" = p
    )
    
    output$nGramsPlot <- renderPlot({
      p1 
    })
    
  })
  
  observeEvent(input$writeData,{
    
    write.csv(globalNgram, "n-gram-data.csv")
    
  })
  
  observeEvent(input$grabAttributionData,{
    
    shinyjs::show("gettingAt")
    shinyjs::hide("readyAt")
    
    if(input$sourceMetric == "sourceMedium"){
      # Core reporting
      # Dimension download to merge with paths
      gaCoreData <- google_analytics(viewId = selectedId(),
                                     date_range = c(input$dateRange[1],input$dateRange[2]),
                                     metrics = c("transactions", "transactionRevenue"),
                                     dimensions = c("transactionId", "sessionCount","country", "region", "city", 
                                                    "language", "deviceCategory", "sourceMedium"),
                                     anti_sample = TRUE)
      
      # MCF Conversion Paths
      # Can download path for every transaction
      gaMcfData <- google_analytics_3(id = selectedId(),
                                      start = input$dateRange[1], end = input$dateRange[2],
                                      metrics = c("totalConversions", "totalConversionValue"),
                                      dimensions = c("sourceMediumPath", "transactionId", "conversionDate"),
                                      max_results = 10000,
                                      samplingLevel = "WALK",
                                      type = "mcf")
    } else{
      
      gaCoreData <- google_analytics(viewId = selectedId(),
                                     date_range = c(input$dateRange[1],input$dateRange[2]),
                                     metrics = c("transactions", "transactionRevenue"),
                                     dimensions = c("transactionId", "sessionCount","country", "region", "city", 
                                                    "language", "deviceCategory", "channelGrouping"),
                                     anti_sample = TRUE)
      
      # MCF Conversion Paths
      # Can download path for every transaction
      gaMcfData <- google_analytics_3(id = selectedId(),
                                      start = input$dateRange[1], end = input$dateRange[2],
                                      metrics = c("totalConversions", "totalConversionValue"),
                                      dimensions = c("basicChannelGroupingPath", "transactionId", "conversionDate"),
                                      max_results = 10000,
                                      samplingLevel = "WALK",
                                      type = "mcf")
    }
    
    # Clean conversion path column
    gaMcfData[,1] <- gaMcfData[,1] %>% str_replace_all(" / ", "/")
    gaMcfData[,1] <- gaMcfData[,1] %>% str_replace_all(":?(NA|CLICK|IMPRESSION|RICH_MEDIA):?", "")
    gaMcfData[,1] <- gaMcfData[,1] %>% str_replace_all("\\s", "")
    gaMcfData[,1] <- gaMcfData[,1] %>% str_replace_all(">", " > ")
    
    # Fix column types
    gaMcfData$totalConversions <- as.numeric(gaMcfData$totalConversions)
    gaMcfData$totalConversionValue <- as.numeric(gaMcfData$totalConversionValue)
    gaMcfData$conversionDate <- ymd(gaMcfData$conversionDate)
    
    # Grabbing product data for each transaction
    # Assume standard e-commerce, swhich to enhanced if result is NULL
    gaProductData <<- google_analytics(viewId = selectedId(),
                                       date_range = c(input$dateRange[1],input$dateRange[2]),
                                       metrics = c("itemQuantity"),
                                       dimensions = c("transactionId", "productName", "productCategory"),
                                       anti_sample = TRUE)
    
    if(is.null(gaProductData)){
      
      gaProductData <<- google_analytics(viewId = selectedId(),
                                         date_range = c(input$dateRange[1],input$dateRange[2]),
                                         metrics = c("itemQuantity"),
                                         dimensions = c("transactionId", "productName", "productCategoryHierarchy"),
                                         anti_sample = TRUE)
    }
    
    # Merging by transactionId
    gaMerged <<- left_join(gaMcfData, gaCoreData, by = "transactionId")
    
    # Remove totalConversions, totalConversionValue and transactions as they are redundant
    gaMerged <<- gaMerged[,-c(4,5,12)]
    
    # Naming path column
    colnames(gaMerged)[1] <<- "path"
    
    # Grab unique values for filtering
    uniqueTransactionIds <- c("all", unique(gaMerged$transactionId))
    uniqueConversionDate <- c("all", as.character(unique(gaMerged$conversionDate)))
    uniqueSessionCount <- c("all", sort(as.numeric(unique(gaMerged$sessionCount))))
    uniqueCountry <- c("all", unique(gaMerged$country))
    uniqueRegion <- c("all", unique(gaMerged$region))
    uniqueCity <- c("all", unique(gaMerged$city))
    uniqueLanguage <- c("all", unique(gaMerged$language))
    uniqueDeviceCategory <- c("all", unique(gaMerged$deviceCategory))
    uniqueProduct <- c("all", unique(gaProductData$productName))
    uniqueProductCategory <- c("all", unique(gaProductData[,3]))
    uniqueChannels <- unique(gaCoreData[,8]) 
    
    output$attributionFilters <- renderUI({
      tagList(
        hr(),
        selectInput("gaMetric", "Metric:",
                    choices = c("Transactions" = "conversions",
                                "Value" = "value")),
        selectInput("sessionCount", "Session Count:",
                    choices = uniqueSessionCount, selectize = T),
        selectInput("country", "Country:",
                    choices = uniqueCountry, selectize = T),
        selectInput("region", "Region:",
                    choices = uniqueRegion, selectize = T),
        selectInput("city", "City:",
                    choices = uniqueCity, selectize = T),
        selectInput("language", "Language:",
                    choices = uniqueLanguage, selectize = T),
        selectInput("deviceCategory", "Device Category:",
                    choices = uniqueDeviceCategory, selectize = T),
        selectInput("conversionDate", "Conversion Date:",
                    choices = uniqueConversionDate, selectize = T, selected = "all"),
        selectInput("productSelection", "Product:",
                    choices = uniqueProduct, selectize = T),
        selectInput("productCategory", "Product Category:",
                    choices = uniqueProductCategory, selectize = T),
        selectInput("transactionId", "Transaction Id:",
                    choices = uniqueTransactionIds, selectize = T),
        actionButton("visualise", "See Channel Comparison"),
        hidden(
          div(id = "timeSeriesInputs",
              hr(),
              actionButton("showTable", "Show Table"),
              hr(),
              selectInput("channelSelection", "Source",
                          choices = uniqueChannels, selectize = T),
              actionButton("visualiseTs", "Plot Time Series")
          )))
      
    })
    
    shinyjs::hide("gettingAt")
    
  })
  
  observeEvent(input$visualise,{
    
    selectedFilters <<- ""
    
    if(input$productSelection != "all"){
      
      gaProductfilter <- gaProductData %>% filter(productName == input$productSelection) %>% pull(transactionId)
      
      transactionIdF <- paste0(gaProductfilter, collapse = "|")
      
      selectedFilters <<- paste0(selectedFilters, "Product Filter: ", input$productSelection, " | ")
      
    } else if(input$productCategory != "all"){
      
      gaProductfilter <- subset(gaProductData, gaProductData[,3] == input$productCategory) %>% pull(transactionId)
      
      transactionIdF <- paste0(gaProductfilter, collapse = "|")
      
      selectedFilters <<- paste0(selectedFilters, "Product Category Filter: ", input$productCategory, " | ")
      
    } else if(input$transactionId == "all"){
      
      transactionIdF <- ".*"
      
    } else {
      
      transactionIdF <- input$transactionId
      
      selectedFilters <<- paste0(selectedFilters, "Transaction ID: ", input$transactionId, " | ")
    
    }
    
    if(input$sessionCount == "all"){
      
      sessionCountF <- ".*"
      
    } else {
      
      sessionCountF <- input$sessionCount
      
      selectedFilters <<- paste0(selectedFilters, "Session Count:  ", input$sessionCount, " | ")
      
    }
    
    if(input$country == "all"){
      
      countryF <- ".*"
      
    } else {
      
      countryF <- input$country
      
      selectedFilters <<- paste0(selectedFilters, "Country: ", input$country, " | ")
      
    }
    
    if(input$region == "all"){
      
      regionF <- ".*"
      
    } else {
      
      regionF <- input$region
      
      selectedFilters <<- paste0(selectedFilters, "Region: ", input$region, " | ")
      
    }
    
    if(input$city == "all"){
      
      cityF <- ".*"
      
    } else {
      
      cityF <- input$city
      
      selectedFilters <<- paste0(selectedFilters, "City:", input$city, " | ")
      
    }
    
    if(input$language == "all"){
      
      languageF <- ".*"
      
    } else {
      
      languageF <- input$language
      
      selectedFilters <<- paste0(selectedFilters, "Language: ", input$language, " | ")
      
    }
    
    if(input$deviceCategory == "all"){
      
      deviceCategoryF <- ".*"
      
    } else {
      
      deviceCategoryF <- input$deviceCategory
      
      selectedFilters <<- paste0(selectedFilters, "Device Category: ", input$deviceCategory, " | ")
      
    }
    
    if(input$conversionDate == "all"){
      
      conversionDateF <- ".*"
      
    } else {
      
      conversionDateF <- input$conversionDate
      
      selectedFilters <<- paste0(selectedFilters, "Conversion Date: ", input$conversionDate, " | ")
      
    }
    
    fullTitle <<- ""
    fullTitle <<- paste0(graphTitle(), " - ", selectedFilters)
    
    gaMergeFiltered <<- gaMerged %>% filter(grepl(transactionIdF, transactionId),
                                            grepl(sessionCountF, sessionCount),
                                            grepl(countryF, country),
                                            grepl(regionF, region),
                                            grepl(cityF, city),
                                            grepl(languageF, language),
                                            grepl(deviceCategoryF, deviceCategory),
                                            grepl(conversionDateF, conversionDate))
    
    gaTopConversionPathsFiltered <- gaMergeFiltered %>% 
      mutate(transactions = 1, value = transactionRevenue) %>% group_by(path) %>% 
      summarise(transactions = n(), value = sum(transactionRevenue))
    
    # Running heuristic and markov models
    hm <- heuristic_models(Data = gaTopConversionPathsFiltered, var_path = "path", var_conv = "transactions", 
                           var_value = "value")
    mm <- markov_model(Data = gaTopConversionPathsFiltered, var_path = "path", var_conv = "transactions", 
                       var_value = "value",
                       order = 1)
    
    # Merge models
    gaAttribution <<- left_join(hm, mm, by = "channel_name")
    
    # Rename columns
    colnames(gaAttribution)[8] <<- "markov_conversions"
    colnames(gaAttribution)[9] <<- "markov_value"
    
    # Create tidy data set for ggplot2
    # Gather all metrics
    gaAttributionTidy <- gaAttribution %>% gather(metric, value, first_touch_conversions:markov_value)
    
    # Separate into model and metric
    gaAttributionTidy$metric <- gaAttributionTidy$metric %>% str_replace("_conversions", "-conversions")
    gaAttributionTidy$metric <- gaAttributionTidy$metric %>% str_replace("_value", "-value")
    gaAttributionTidy <- gaAttributionTidy %>% separate(metric, c("model", "metric"), sep = "-")
    gaAttributionTidy <- gaAttributionTidy %>% spread(metric, value)
    
    shinyjs::show("timeSeriesInputs")
    output$modelComparison <- renderPlot({
      
      # Visualising model comparisons
      # Want to show top 5 + other
      
      # Find top 5 channels by conversions
      channelFilter <- gaAttributionTidy %>% group_by(channel_name) %>% summarise(conversions = sum(conversions)) %>% 
        top_n(5, conversions) %>% arrange(-conversions) %>% select(channel_name) %>% pull(channel_name)
      
      topChannels <- gaAttributionTidy %>% filter(channel_name %in% channelFilter)
      
      # Summarise all other channel stats and prepare for rbind
      otherChannels <- gaAttributionTidy %>% filter(!channel_name %in% channelFilter) %>% 
        group_by (model) %>% summarise(conversions = sum(conversions), value = sum(value)) %>% 
        mutate(channel_name = "Other") %>% select(channel_name, model, conversions, value)
      
      gData <<- rbind(topChannels, otherChannels)
      
      gData %>% ggplot(aes_string(x = "channel_name", y = input$gaMetric, fill = "model")) +
        geom_bar(stat = "identity", position = "dodge") +
        ggtitle(fullTitle)
      
    })
    
    
  })
  
  observeEvent(input$showTable,{
    
    output$attributionTable <- renderDT({
      
      datatable(gaAttribution, options = list(scrollX = TRUE)) %>%
        formatCurrency(c(3,5,7,9), currency = "") %>% 
        formatRound(c(2,4,6,8))
      
    })
    
  })
  
  observeEvent(input$visualiseTs,{
    # Create summarise function to use in nested data frame by date
    sumStats <- function(df){
      df %>% group_by(path) %>% summarise(transactions = n(), value = sum(transactionRevenue))
    }
    
    # Create a modeling function to use in nested summaries by date
    modelingAttrib <- function(df){
      
      hm <- heuristic_models(Data = df, var_path = "path", var_conv = "transactions", var_value = "value")
      mm <- markov_model(Data = df, var_path = "path", var_conv = "transactions", var_value = "value",
                         order = 1)
      
      # Merge models
      
      gaAttribution <- left_join(hm, mm, by = "channel_name")
      
      # Rename columns
      
      colnames(gaAttribution)[8] <- "markov_conversions"
      colnames(gaAttribution)[9] <- "markov_value"
      
      gaAttribution
      
    }
    
    # Create nested data frame with summaries and models
    gaTopConversionPathsDaily <- gaMergeFiltered %>% group_by(conversionDate) %>% nest() %>% 
      mutate(summaries = map(data, sumStats), models = map(summaries, modelingAttrib))
    
    # Unnest models column
    gaTopConversionPathsDaily <- gaTopConversionPathsDaily %>% unnest(... = models)
    
    # Create tidy data set for ggplot2
    # Gather all metrics
    gaAttributionTidyDaily <- gaTopConversionPathsDaily %>% gather(metric, value, first_touch_conversions:markov_value)
    
    # Separate into model and metric
    gaAttributionTidyDaily$metric <- gaAttributionTidyDaily$metric %>% str_replace("_conversions", "-conversions")
    gaAttributionTidyDaily$metric <- gaAttributionTidyDaily$metric %>% str_replace("_value", "-value")
    gaAttributionTidyDaily <- gaAttributionTidyDaily %>% separate(metric, c("model", "metric"), sep = "-")
    gaAttributionTidyDaily <- gaAttributionTidyDaily %>% spread(metric, value)  
    
    fullTitle <<- ""
    fullTitle <<- paste0(graphTitle(), " - ", selectedFilters)
    
    output$tsPlot <- renderPlot({
      
      channel <- str_replace_all(input$channelSelection, " ", "")
      
      gaAttributionTidyDaily %>% filter(channel_name == channel) %>% 
        ggplot(aes_string(x = "conversionDate", y = input$gaMetric, color = "model")) +
        geom_line() + ggtitle(fullTitle)
      
    })
    
  })
}