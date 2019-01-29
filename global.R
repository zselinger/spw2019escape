clientId <- ''
clientSecret <- ''
devToken <- ''

library(shiny)
library(shinyjs)
library(googleAuthR)
library(googleAnalyticsR)
library(RAdwords)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(DT)
library(rlang)
library(lubridate)
library(ChannelAttribution)

options(googleAuthR.webapp.client_id = clientId)
options(googleAuthR.webapp.client_secret = clientSecret)
ga_cache_call("cache")
data(stop_words)