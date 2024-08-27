library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(shinyjs)
library(tidyverse)
library(shinycssloaders)
library(DT)
library(xlsx)
library(RColorBrewer)
library(maptools)     
library(spdep)          
library(cartography)    
library(tmap)           
library(leaflet)
library(rgdal)
library(mailR)
library(shinyalert)
library(htmltools)
library(httr)
library(jsonlite)

#Base de dados------------------------------------------------------------------
base_dados <- readRDS("base_final.rds")

#Módulos------------------------------------------------------------------------

#UI do captcha
recaptchaUI <- function(id, sitekey = Sys.getenv("recaptcha_sitekey"), ...) {
  ns <- NS(id)
  
  tagList(tags$div(
    shiny::tags$script(
      src = "https://www.google.com/recaptcha/api.js",
      async = NA,
      defer = NA
    ),
    tags$script(
      paste0("shinyCaptcha = function(response) {
          Shiny.onInputChange('", ns("recaptcha_response"),"', response);
      }"
      )),
    tags$form(
      class = "shinyCAPTCHA-form",
      action = "?",
      method = "POST",
      tags$div(class = "g-recaptcha", `data-sitekey` = sitekey, `data-callback` = I("shinyCaptcha")),
      tags$br()
    )
  ))
}

#Server do CAPTCHA
recaptcha <- function(input, output, session, secret = Sys.getenv("recaptcha_secret")) {
  
  status <- reactive({
    if (isTruthy(input$recaptcha_response)) {
      url <- "https://www.google.com/recaptcha/api/siteverify"
      
      resp <- POST(url, body = list(
        secret = secret,
        response = input$recaptcha_response
      ))
      
      fromJSON(content(resp, "text"))
    } else {
      list(success = FALSE)
    }
  })
  
  return(status)
  
}

input_forms_ui <- function(id, box_label){
  ns <- NS(id)
  
  #Formulário da pesquisa por ano
  year_input <- selectizeInput(
    inputId = (NS(id, "year_input")),
    label = "Ano do início de vigência do contrato: ",
    choices = c("TODOS OS ANOS", sort(unique(base_dados$ano), decreasing = FALSE)),
    multiple = TRUE,
    options = list(maxItems = 1)
  )
  
  #Formulário de pesquisa por UF
  uf_input <- selectizeInput(
    inputId = (NS(id, "uf_input")),
    label = "UF do estabelecimento: ",
    choices = NULL,
    multiple = TRUE,
    options = list(maxItems = 1)
  )
  
  #Formulário de pesquisa por município
  city_input <- selectizeInput(
    inputId = (NS(id, "city_input")),
    label = "Município do estabelecimento: ",
    choices = NULL,
    multiple = TRUE,
    options = list(maxItems = 1)
  )
  
  #Formulário de pesquisa por OSS (nível UF)
  organization_state_input <- selectizeInput(
    inputId = (NS(id, "organization_state_input")),
    label = "Organizações Sociais: ",
    choices = NULL,
    multiple = TRUE,
    options = list(maxItems = 1)
  )
  
  #Formulário de pesquisa por OSS (nível municipal)
  organization_city_input <- selectizeInput(
    inputId = (NS(id, "organization_city_input")),
    label = "Organizações Sociais: ",
    choices = NULL,
    multiple = TRUE,
    options = list(maxItems = 1)
  )
  
  #Formulário de pesquisa por tipo de estabelecimento (nível UF)
  type_state_input <- selectizeInput(
    inputId = (NS(id, "type_state_input")),
    label = "Tipo do estabelecimento: ",
    choices = NULL,
    multiple = TRUE,
    options = list(maxItems = 1)
  )
  
  #Formulário de pesquisa por tipo de estabelecimento (nível municipal)
  type_city_input <- selectizeInput(
    inputId = (NS(id, "type_city_input")),
    label = "Tipo do estabelecimento: ",
    choices = NULL,
    multiple = TRUE,
    options = list(maxItems = 1)
  )
  
    box(
      title = box_label,
      solidHeader = TRUE,
      status = "primary",
      year_input,
      conditionalPanel(
        condition = "input.year_input.length > 0", ns = ns,
        uf_input
      ),
      conditionalPanel(
        "input.sidebar_menu == 'search_tab'",
        conditionalPanel(
          condition = "input.uf_input.length >0 && input.uf_input != 'TODOS OS ESTADOS'",ns =ns,
          city_input
        ),
        conditionalPanel(
          condition = "input.uf_input.length >0 && input.uf_input == 'TODOS OS ESTADOS'",ns = ns,
          organization_state_input
        ),
        conditionalPanel(
          condition = "input.uf_input.length >0 && input.uf_input != 'TODOS OS ESTADOS' && input.city_input.length >0",ns = ns,
          organization_city_input
        ),
        conditionalPanel(
          condition = "input.uf_input.length >0 && input.uf_input == 'TODOS OS ESTADOS' && input.organization_state_input.length >0",ns = ns,
          type_state_input
        ),
        conditionalPanel(
          condition = "input.uf_input.length >0 && input.uf_input != 'TODOS OS ESTADOS' && input.city_input.length >0 && input.organization_city_input.length >0", ns = ns,
          type_city_input
        )
        
      ),
      width = 12,
    )
  
  
}

input_forms_server <- function(id){
  moduleServer(id, function(input, output, session){
    #Inicialização da memória das variáveis
    selected_values <- reactiveValues(year = "", uf = "", city = "", organization_state = "", organization_city = "", type_state = "", type_city = "")
    
    
    #Atualização da memória
    observe({
      req(input$year_input, input$uf_input, input$city_input, input$organization_state_input, input$organization_city_input, input$type_state_input, input$type_city_input)
      selected_values$year <- input$year_input
      selected_values$uf <- input$uf_input
      selected_values$city <- input$city_input
      selected_values$organization_state <- input$organization_state_input
      selected_values$organization_city <- input$organization_city_input
      selected_values$type_state <- input$type_state_input
      selected_values$type_city <- input$type_city_input
    })
    
    #Verificação da memória
    memory_check <- function(input_value, vector){
      if(input_value %in% vector){
        displayed_value <- input_value
      } else{
        displayed_value <- NULL
      }
      
      return(displayed_value)
    }
    
    #Atribuição da alternativa "TODOS..."
    length_check <- function(vector, phrase_option){
      if(length(vector)>1){
        vector <- c(phrase_option, vector)
      }
      return(vector)
    }
    
    #Carregamento dinâmico do formulário de pesquisa por UF
    observeEvent(input$year_input,
                 {
                   if(input$year_input == "TODOS OS ANOS"){
                     uf <- c("TODOS OS ESTADOS", sort(unique(base_dados$UF), decreasing = FALSE))
                     
                     displayed_value <- memory_check(selected_values$uf, uf)
                     
                     updateSelectizeInput(
                       session,
                       inputId = "uf_input",
                       label = "UF do estabelecimento: ",
                       choices = uf,
                       selected = displayed_value
                     )
                   }
                   if(input$year_input != "TODOS OS ANOS"){
                     uf <- sort(unique(base_dados$UF[base_dados$ano == input$year_input]), decreasing = FALSE)
                     
                     
                     uf <- length_check(uf, "TODOS OS ESTADOS")
                     displayed_value <- memory_check(selected_values$uf, uf)
                     
                     updateSelectizeInput(
                       session,
                       inputId = "uf_input",
                       label = "UF do estabelecimento: ",
                       choices = uf,
                       selected = displayed_value
                     )
                   }
                   
                   
                 })
    
    #Carregamento dinâmico do formulário de pesquisa por município
    observeEvent({input$year_input
      input$uf_input},
      {
        if(input$year_input == "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS"){
          city <- sort(unique(base_dados$nome_municipio[base_dados$UF == input$uf_input]), decreasing = FALSE)
          
          city <- length_check(city, "TODOS OS MUNICÍPIOS")
          displayed_value <- memory_check(selected_values$city, city)
          
          updateSelectizeInput(
            session,
            inputId = "city_input",
            label = "Município do estabelecimento: ",
            choices = city,
            selected = displayed_value
          )
        }
        if(input$year_input != "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS"){
          city <- sort(unique(base_dados$nome_municipio[base_dados$ano == input$year_input & base_dados$UF == input$uf_input]), decreasing = FALSE)
          
          city <- length_check(city, "TODOS OS MUNICÍPIOS")
          displayed_value <- memory_check(selected_values$city, city)
          
          updateSelectizeInput(
            session,
            inputId = "city_input",
            label = "Município do estabelecimento: ",
            choices = city,
            selected = displayed_value
          )                  
        }
      })
    
    #Carregamento dinâmico do formulário de pesquisa por OSS (nível UF)
    observeEvent({
      input$year_input
      input$uf_input
    },
    {
      if(input$year_input == "TODOS OS ANOS" && input$uf_input == "TODOS OS ESTADOS"){
        organization <- c("TODAS AS ORGANIZAÇÕES", sort(unique(base_dados$nome_entidade), decreasing = FALSE))
        
        displayed_value <- memory_check(selected_values$organization_state, organization)
        
        updateSelectizeInput(
          session,
          inputId = "organization_state_input",
          label = "Organizações Sociais: ",
          choices = organization,
          selected = displayed_value
        )                  
      }
      if(input$year_input != "TODOS OS ANOS" && input$uf_input == "TODOS OS ESTADOS"){
        organization <- sort(unique(base_dados$nome_entidade[base_dados$ano == input$year_input], decreasing = FALSE))
        
        organization <- length_check(organization, "TODAS AS ORGANIZAÇÕES")
        displayed_value <- memory_check(selected_values$organization_state, organization)
        
        updateSelectizeInput(
          session,
          inputId = "organization_state_input",
          label = "Organizações Sociais: ",
          choices = organization,
          selected = displayed_value
        )                     
      }
    })
    
    #Carregamento dinâmico do formulário de pesquisa por OSS (nível municipal)
    observeEvent({input$year_input
      input$uf_input
      input$city_input},
      {
        if(input$year_input == "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input == "TODOS OS MUNICÍPIOS"){
          organization <- sort(unique(base_dados$nome_entidade[base_dados$UF == input$uf_input]), decreasing = FALSE)
          
          organization <- length_check(organization, "TODAS AS ORGANIZAÇÕES")
          displayed_value <- memory_check(selected_values$organization_city, organization)
          
          updateSelectizeInput(
            session,
            inputId = "organization_city_input",
            label = "Organizações Sociais: ",
            choices = organization,
            selected = displayed_value
          )
          
        }
        if(input$year_input != "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input == "TODOS OS MUNICÍPIOS"){
          organization <- sort(unique(base_dados$nome_entidade[base_dados$ano == input$year_input & base_dados$UF == input$uf_input]), decreasing = FALSE)
          
          organization <- length_check(organization, "TODAS AS ORGANIZAÇÕES")
          displayed_value <- memory_check(selected_values$organization_city, organization)
          
          updateSelectizeInput(
            session,
            inputId = "organization_city_input",
            label = "Organizações Sociais: ",
            choices = organization,
            selected = displayed_value
          )                   
        }
        if(input$year_input == "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input != "TODOS OS MUNICÍPIOS"){
          organization <- sort(unique(base_dados$nome_entidade[base_dados$UF == input$uf_input & base_dados$nome_municipio == input$city_input]), decreasing = FALSE)
          
          organization <- length_check(organization, "TODAS AS ORGANIZAÇÕES")
          displayed_value <- memory_check(selected_values$organization_city, organization)
          
          updateSelectizeInput(
            session,
            inputId = "organization_city_input",
            label = "Organizações Sociais: ",
            choices = organization,
            selected = displayed_value
          )                    
        }
        if(input$year_input != "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input != "TODOS OS MUNICÍPIOS"){
          organization <- sort(unique(base_dados$nome_entidade[base_dados$ano == input$year_input & base_dados$UF == input$uf_input & base_dados$nome_municipio == input$city_input]), decreasing = FALSE)
          
          organization <- length_check(organization, "TODAS AS ORGANIZAÇÕES")
          displayed_value <- memory_check(selected_values$organization_city, organization)
          
          updateSelectizeInput(
            session,
            inputId = "organization_city_input",
            label = "Organizações Sociais: ",
            choices = organization,
            selected = displayed_value
          )                   
        }
      })
    
    # Carregamento dinâmico do formulário de pesquisa por tipo de estabelecimento (nível UF)
    observeEvent({
      input$year_input
      input$uf_input
      input$organization_state_input
    },
    {
      if(input$year_input == "TODOS OS ANOS" && input$uf_input == "TODOS OS ESTADOS" && input$organization_state_input == "TODAS AS ORGANIZAÇÕES"){
        type <- c("TODOS OS TIPOS", sort(unique(base_dados$tp_estabelecimento),decreasing = FALSE))
        
        displayed_value <- memory_check(selected_values$type_state, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_state_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        ) 
      }
      if(input$year_input == "TODOS OS ANOS" && input$uf_input == "TODOS OS ESTADOS" && input$organization_state_input != "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$nome_entidade == input$organization_state_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_state, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_state_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                    
      }
      if(input$year_input != "TODOS OS ANOS" && input$uf_input == "TODOS OS ESTADOS" && input$organization_state_input == "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$ano == input$year_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_state, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_state_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                     
      }
      if(input$year_input != "TODOS OS ANOS" && input$uf_input == "TODOS OS ESTADOS" && input$organization_state_input != "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$ano == input$year_input & base_dados$nome_entidade == input$organization_state_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_state, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_state_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                    
      }
    })
    
    
    # Carregamento dinâmico do formulário de pesquisa por tipo de estabelecimento (nível municipal)
    observeEvent({
      input$year_input
      input$uf_input
      input$city_input
      input$organization_city_input    
    },
    {
      if(input$year_input == "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input == "TODOS OS MUNICÍPIOS" && input$organization_city_input == "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$UF == input$uf_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_city, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_city_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                  
      }
      if(input$year_input != "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input == "TODOS OS MUNICÍPIOS" && input$organization_city_input == "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$ano == input$year_input & base_dados$UF == input$uf_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_city, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_city_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                      
      }
      if(input$year_input == "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input != "TODOS OS MUNICÍPIOS" && input$organization_city_input == "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$UF == input$uf_input & base_dados$nome_municipio == input$city_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_city, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_city_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                    
      }
      if(input$year_input == "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input == "TODOS OS MUNICÍPIOS" && input$organization_city_input != "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$UF == input$uf_input & base_dados$nome_entidade == input$organization_city_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_city, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_city_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                    
      }
      if(input$year_input != "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input != "TODOS OS MUNICÍPIOS" && input$organization_city_input == "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$ano == input$year_input & base_dados$UF == input$uf_input & base_dados$nome_municipio == input$city_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_city, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_city_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                    
      }
      if(input$year_input != "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input == "TODOS OS MUNICÍPIOS" && input$organization_city_input != "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$ano == input$year_input & base_dados$UF == input$uf_input & base_dados$nome_entidade == input$organization_city_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_city, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_city_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                      
      }
      if(input$year_input == "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input != "TODOS OS MUNICÍPIOS" && input$organization_city_input != "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$UF == input$uf_input & base_dados$nome_municipio == input$city_input & base_dados$nome_entidade == input$organization_city_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_city, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_city_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                                         
      }
      if(input$year_input != "TODOS OS ANOS" && input$uf_input != "TODOS OS ESTADOS" && input$city_input != "TODOS OS MUNICÍPIOS" && input$organization_city_input != "TODAS AS ORGANIZAÇÕES"){
        type <- sort(unique(base_dados$tp_estabelecimento[base_dados$ano == input$year_input & base_dados$UF == input$uf_input & base_dados$nome_municipio == input$city_input & base_dados$nome_entidade == input$organization_city_input]),decreasing = FALSE)
        
        type <- length_check(type, "TODOS OS TIPOS")
        displayed_value <- memory_check(selected_values$type_city, type)
        
        updateSelectizeInput(
          session,
          inputId = "type_city_input",
          label = "Tipo do estabelecimento: ",
          choices = type,
          selected = displayed_value
        )                    
      }
    })
    
    year <- reactive({input$year_input})
    uf <- reactive({input$uf_input})
    city <- reactive({input$city_input})
    organization_state <- reactive({input$organization_state_input})
    organization_city <- reactive({input$organization_city_input})
    type_state <- reactive({input$type_state_input})
    type_city <- reactive({input$type_city_input})
    
    return(list(
      year = year,
      uf = uf,
      city = city,
      organization_state = organization_state,
      organization_city = organization_city,
      type_state = type_state,
      type_city = type_city))
  }
    )
}

#Cabeçalho da plataforma--------------------------------------------------------
header <- dashboardHeader(
  title = tags$image(src = 'img1.png', height = '100%', width = '100%'))

#Menu lateral da plataforma-----------------------------------------------------

#Botão para refazer pesquisa
reset_button <- actionButton(
  inputId = "reset_button",
  label = "Reiniciar"
)

#Botão para download da pesquisa em formato .xlsx
download_button <- downloadButton(
  outputId = "download_button",
  label = "Download",
  style = "color: #fff; background-color: #0275d8; border-color: #fff;"
)

#Botão apra limpar estatísticas
clear_button <- actionButton(
  inputId = "clear_button",
  label = "Reiniciar"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar_menu",
    menuItem("Apresentação", tabName = "intro_tab", icon = icon("dashboard")),
    menuItem("Pesquisa de estabelecimentos", tabName = "search_tab", icon = icon("dashboard")),
    menuItem("Estatísticas descritivas", tabName = "statistics_tab", icon = icon("dashboard")),
    menuItem("Contato", tabName = "contact_tab", icon = icon("dashboard")),
    conditionalPanel(
      condition = "input.sidebar_menu == 'search_tab' && input.search_button > 0",
      div(style="display:inline-block;width:50%;text-align: center;", download_button),
      div(style="display:inline-block;width:50%;text-align: center;", reset_button)
    ),
    conditionalPanel(
      condition = "input.sidebar_menu == 'statistics_tab' && input.display_button > 0",
      div(style="text-align: center;", clear_button)
    )
)
)

#Corpo da plataforma------------------------------------------------------------
#--Aba-"Apresentação"

#Download da nota metodológica
note_download_button <- downloadButton(
  outputId = "note_download_button",
  label = "Nota metodológica",
  style = "color: #fff; background-color: #0275d8; border-color: #fff;"  
)

#Caixa de texto
text_box <- box(
  title = "Portal da Base de Dados das Organizações Sociais de Saúde",
  solidHeader = TRUE,
  status = "primary",
  p("Bem-vindo ao Portal BDOSS!", style ="text-align: justify;", style = "font-size:18px;"),
  p("Este portal abriga uma base de dados nacional dos estabelecimentos de saúde geridos por OSS. A BDOSS - Base de Dados das Organizações Sociais de Saúde foi desenvolvida pelo Grupo de Estudos em Economia da Saúde e Criminalidade (GEESC), do Cedeplar/UFMG, em parceria com o Instituto Brasileiro das Organizações Sociais de Saúde (IBROSS).", style ="text-align: justify;", style = "font-size:18px;"),
  p("No nosso portal você pode navegar e conhecer o universo desses estabelecimentos, por meio de pesquisa direta e análise de indicadores.", style ="text-align: justify;", style = "font-size:18px;"),
  p("Atualmente, contamos com", strong("1874 estabelecimentos"), "geridos por", strong("158 OS"), "distintas.", style ="text-align: justify;", style = "font-size:18px;"),
  p("Caso seja representante de alguma OSS com estabelecimentos ausentes da BDOSS, envie seus dados por meio da aba", strong("Contato"), "para que possamos adicioná-los.", style ="text-align: justify;", style = "font-size:18px;"),
  p(strong("Pesquisadores envolvidos"), style ="text-align: right;", style = "font-size:18px;"),
  p("Mônica Viegas Andrade", style ="text-align: right;", style = "font-size:18px;"),
  p("Kenya Noronha", style ="text-align: right;", style = "font-size:18px;"),
  p("Catarina Barcelos", style ="text-align: right;", style = "font-size:18px;"),
  p("Henrique Bracarense", style ="text-align: right;", style = "font-size:18px;"),
  p(strong("Última atualização: "), "22 de agosto de 2023", style ="text-align: left;", style = "font-size:18px;"),
  fluidRow(
    column(width = 3, align = 'center',
           img(src = 'Imagem1.jpeg', width = '70%')),
    column(width = 3, align = 'center',
           img(src = 'Imagem2.png', width = '70%')),
    column(width = 3, align = 'center',
           img(src = 'Imagem3.png', width = '70%')),
    column(width = 3, align = 'center',
           img(src = 'Imagem4.png', width = '70%'))
  ),
  width = 12
)

intro_tab <- tabItem(
  tabName = "intro_tab",
  text_box,
  div(style="display:inline-block;width:100%;text-align: center;", note_download_button)
)

#--Aba-"Pesquisa de estabelecimentos"------------------------------
#Botão de pesquisa
search_button <- actionButton(
  inputId = "search_button",
  label = "Pesquisar"
)


search_tab <- tabItem(
  tabName = "search_tab",
  useShinyjs(),
  fluidRow(
    div(
      id = "search_form",
      input_forms_ui("search_tab", "Pesquisa dos estabelecimentos geridos por Organizações Sociais de Saúde no Brasil"),
      column(12, search_button, align = "center", style = "margin-botton: 10px;", style = "margin-top: -10px;")
    ),
    conditionalPanel(
      condition = "input.search_button > 0",
      div(
        id = "result_panel",
        tabsetPanel(
          id = "tabset_display",
          tabPanel(
            DT::dataTableOutput("data_display", width = '95%')
          )
        )
      )
    )
  )
)

#--Aba-"Estatísticas descritivas"
#Botão de exibição
display_button <- actionButton(
  inputId = "display_button",
  label = "Pesquisar"
)

#Exibição do mapa
map_box <- box(
  title = "Distribuição geográfica dos estabelecimentos geridos por OS",
  solidHeader = TRUE,
  status = "primary",
  withSpinner(leafletOutput("map_plot"), type = '8'),
  width = 12
)

#Exibição do gráfico de estabelecimentos
unities_box <- box(
  title = "Evolução anual dos estabelecimentos gerenciados por OS",
  solidHeader = TRUE,
  status = "primary",
  withSpinner(plotlyOutput("unities_plot"), type = '8'),
  uiOutput("ibross_input"),
  width = 12
)

#Exibição da tabela de OSS
table_box <- box(
  title = "Total de estabelecimentos por OS",
  solidHeader = TRUE,
  status = "primary",
  height = "598",
  withSpinner(DT::dataTableOutput("os_table"), type = '8'),style = "height:500px; overflow-y: scroll",
  width = 12
)

#Exibição do gráfico do número de estabelecimentos por OS
os_unities_plot_box <- box(
  title = "Distribuição das OSS segundo o número de estabelecimentos administrados",
  solidHeader = TRUE,
  status = "primary",
  withSpinner(plotlyOutput("os_unities_plot"), type = '8'),
  width = 12
)

#Exibição da tabela de estabelecimentos
type_unity_box <- box(
  title = "Tipos de estabelecimentos geridos por OSS",
  solidHeader = TRUE,
  status = "primary",
  height = "598",
  withSpinner(DT::dataTableOutput("type_unity_table"), type = '8'),style = "height:500px; overflow-y: scroll",
  width = 12
)

#Exibição do gráfico de estabelecimentos
type_unity_graph_box <- box(
  title = "Perfil dos tipos de estabelecimentos geridos por OSS",
  solidHeader = TRUE,
  status = "primary",
  height = "598",
  withSpinner(plotlyOutput("type_unity_graph"), type = '8'),
  width = 5
)

#Exibição do gráfico da gestão
state_city_box <- box(
  title = "Gestão dos estabelecimentos administrados por OSS",
  solidHeader = TRUE,
  status = "primary",
  withSpinner(plotlyOutput("state_city_plot"), type = '8'),
  width = 6
)

#Exibição do gráfico da gestão
ibross_box <- box(
  title = "Estabelecimentos geridos por OSS filiadas ao IBROSS",
  solidHeader = TRUE,
  status = "primary",
  withSpinner(plotlyOutput("ibross_plot"), type = '8'),
  width = 6
)

statistics_tab <- tabItem(
  tabName = "statistics_tab",
  useShinyjs(),
  div(
    id = "selection_form",
    fluidRow(
      input_forms_ui("statistics_tab", "Estatísticas dos estabelecimentos geridos por Organizações Sociais de Saúde no Brasil"),
      column(12, display_button, align = "center", style = "margin-botton: 10px;", style = "margin-top: -10px;")
    )
  ),
  conditionalPanel(
    condition = "input.display_button > 0",
    div(
      id = "plots_tab",
      fluidRow(
        map_box
      ),
      fluidRow(
        column(
          valueBoxOutput("number_os", width = 12),
          os_unities_plot_box,
          width = 5
        ),
        column(
          table_box,
          width = 7
        )
      ),
      fluidRow(
        type_unity_graph_box,
        column(
          type_unity_box,
          width = 7
        )
      ),
      fluidRow(
        state_city_box,
        ibross_box
      ),
      conditionalPanel(
        condition = "input['statistics_tab-year_input'] == 'TODOS OS ANOS'",
        fluidRow(
          unities_box
        )
      )
    )
  )

)

#--Aba-"Contato"

#Entrada do nome
name_form <- textInput(
  inputId = "name_contact",
  label = "Nome:*",
  value = ""
)

#Entrada da organização/estabelecimento
organization_form <- textInput(
  inputId = "organization_contact",
  label = "Organização/Estabelecimento:",
  value = ""
)

#Entrada do e-mail de contato
email_form <- textInput(
  inputId = "email_contact",
  label = "Endereço de e-mail:*",
  value = ""
)

#Entrada da mensagem
message_form <- textAreaInput(
  inputId = "message_contact",
  label = "Mensagem:*",
  width = "65.7%",
  height = "150px",
  value = ""
)

#Caixa de contato
contact_form_box <- box(
  title = "Formulário de contato",
  solidHeader = TRUE,
  status = "primary",
  column(
    name_form,
    width = 8
  ),
  column(
    organization_form,
    width = 8
  ),
  column(
    email_form,
    width = 8
  ),
  column(
    message_form,
    width = 12
  ),
  column(
    p("*Campos obrigatórios", style ="text-align: left;", style = "font-size:12px;"),
    width = 12
  ),
  column(
    recaptchaUI("test_captcha", sitekey = "chave"),
    width = 12
  ),
  div(style="display:inline-block;width:100%;text-align: center;", uiOutput("send_mail_button")),
  width = 12
)

contact_tab <- tabItem(
  tabName = "contact_tab",
  useShinyjs(),
  fluidRow(
    contact_form_box,
    )
)

#Criação do corpo da plataforma-------------------------------------------------
body <- dashboardBody(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
                              background-color: #2d3a4d;
                              }
                              .skin-blue .main-header .logo:hover {
                              background-color: #2d3a4d;
                              }
                              .skin-blue .main-header .navbar {
                              background-color: #2d3a4d;
                              }    
                              '))),
  tabItems(
    intro_tab,
    search_tab,
    statistics_tab,
    contact_tab
  )
)

#UI da plataforma---------------------------------------------------------------
ui <- dashboardPage(
  title = "Portal BDOSS",
  header,
  sidebar,
  body
)

#Server da plataforma-----------------------------------------------------------
server <- function(input, output, session){
  #Atribuição dos valores de entrada do usuário
  search_values <- input_forms_server("search_tab")
  statistics_values <- input_forms_server("statistics_tab")
  
  #Exibição dos resultados da pesquisa - aba "Pesquisa de estabelecimentos"
  observeEvent(input$search_button,
               {
                 shinyjs::hide("search_form")
                 shinyjs::show("result_panel")
                 shinyjs::show("reset_button")
                 shinyjs::show("download_button")
               }
               
  )
  
  #Reset da página - aba "Pesquisa de estabelecimentos"
  observeEvent(input$reset_button,
               {
                 shinyjs::hide("result_panel")
                 shinyjs::hide("reset_button")
                 shinyjs::hide("download_button")
                 shinyjs::reset("search_form")
                 shinyjs::show("search_form")
               })
  
  #Exibição dos resultados - aba "Estatísticas descritivas"
  observeEvent(input$display_button,
               {
                 shinyjs::hide("selection_form")
                 shinyjs::show("plots_tab")
                 shinyjs::show("clear_button")
               })
  
  #Reset da página - aba "Estatísticas descritivas"
  observeEvent(input$clear_button,
               {
                 shinyjs::hide("plots_tab")
                 shinyjs::hide("clear_button") 
                 shinyjs::reset("selection_form")
                 shinyjs::show("selection_form")                
               })
  
  #Habilitação dos botões de pesquisa e exibição
  input_search_tab <- reactive({
    if(!is.null(search_values$uf())){
      if(search_values$uf() == "TODOS OS ESTADOS"){
        length(search_values$type_state())
      } else{
        length(search_values$type_city())
      }
    } else{
      0
    }
    
  })
  
  input_statistics_tab <- reactive({
    if(!is.null(statistics_values$uf())){
      length(statistics_values$uf())
    } else{
      0
    }
    
  })
  
  observe({
    useShinyjs()
    if(input_search_tab()==0){
      disable("search_button")
    }
    if(input_search_tab() > 0){
      enable("search_button")
    }
  })
  
  observe({
    useShinyjs()
    if(input_statistics_tab()==0){
      disable("display_button")
    }
    if(input_statistics_tab() > 0){
      enable("display_button")
    }
  })
  
  #Configuração do CAPTCHA
  result <- callModule(recaptcha,
                               "test_captcha",
                               secret = "chave")
  
  #Habilitação do botão "Enviar" da aba "Contato"
  output$send_mail_button <- renderUI({
    req(result()$success)
    conditionalPanel(
      'input.name_contact != "" && input.email_contact != "" && (/@/).test(input.email_contact) && input.message_contact != ""',
      actionButton(
        inputId = "send_button",
        label = "Enviar mensagem",
        style = "color: #fff; background-color: #0275d8; border-color: #fff;"
      )
    )
  })
  
  
  #Envio do e-mail
  observeEvent(input$send_button,
      {
        body_message = paste("Nome: ", input$name_contact, "Organização/Estabelecimento: ", input$organization_contact, "E-mail de contato: ", input$email_contact, "Mensagem: ", input$message_contact, sep = "\n")
        
        send.mail(from = "geesc.shinyios@gmail.com",
                  to = "hbracarense@hotmail.com",
                  subject = "Contato - Portal BDOSS",
                  body = body_message,
                  smtp = list(
                    host.name = "smtp.gmail.com",
                    port = 465,
                    user.name = "geesc.shinyios@gmail.com",
                    passwd = "senha",
                    ssl = TRUE),
                  authenticate = TRUE,
                  html = TRUE,
                  send = TRUE
        )
        
        shinyalert("Mensagem enviada com sucesso!", type = "success")
        
        updateTextInput(session, "name_contact", value = "")
        updateTextInput(session, "organization_contact", value = "")
        updateTextInput(session, "email_contact", value = "")
        updateTextInput(session, "message_contact", value = "")
    })
  
  #Tratamento da base para exibição na plataforma
  exibition_data <- function(df){
    df$CNPJ <- lapply(df$CNPJ, function(x){
      if(x != '-') {
        cnpj <- x
        cnpj_tratado <- substring(cnpj,c(1,3,6,9,13),c(2,5,8,12,nchar(cnpj)))
        x <- paste0(cnpj_tratado[1],".",cnpj_tratado[2],".",cnpj_tratado[3],"/",cnpj_tratado[4],"-",cnpj_tratado[5])
      } else{
        x= "-"
      }
    })
    
    df$ibross <- lapply(df$ibross, function(x){
      ifelse(x == 0, x <- "NÃO", x <- "SIM")
    })
    
    df$recurso_federal <- lapply(df$recurso_federal, function(x){
      if(x == 1){
        x <- "SIM"
      } else{
        if(x == 0){
          x <- "NÃO"
        } else{
          x <- "-"
        }
      }
    })
    
    df$TPGESTAO <- lapply(df$TPGESTAO, function(x){
      ifelse(x == "M", x<- "MUNICIPAL",
             ifelse(x == "E", x<- "ESTADUAL",
                    ifelse(x == "D", x<- "DUPLA", NA)))
    })
    
    return(df)
  }
  
  #Filtragem dos dados de acrdo com os parâmetros da pesquisa
  filtering_data <- eventReactive(input$search_button,
                                  {
                                    if(search_values$year() == "TODOS OS ANOS"){
                                      if(search_values$uf() == "TODOS OS ESTADOS"){
                                        if(search_values$organization_state() == "TODAS AS ORGANIZAÇÕES"){
                                          if(search_values$type_state() == "TODOS OS TIPOS"){
                                            filtered_data <- base_dados
                                          } else{
                                            filtered_data <- base_dados %>% filter(tp_estabelecimento == search_values$type_state())
                                          }
                                        }else{
                                          if(search_values$type_state() == "TODOS OS TIPOS"){
                                            filtered_data <- base_dados %>% filter(nome_entidade == search_values$organization_state())
                                          } else{
                                            filtered_data <- base_dados %>% filter(nome_entidade == search_values$organization_state(),
                                                                                   tp_estabelecimento == search_values$type_state())
                                          }                        
                                        }
                                        
                                      } else{
                                        if(search_values$city() == "TODOS OS MUNICÍPIOS"){
                                          if(search_values$organization_city() == "TODAS AS ORGANIZAÇÕES"){
                                            if(search_values$type_city() == "TODOS OS TIPOS"){
                                              filtered_data <- base_dados %>% filter(UF == search_values$uf())
                                            } else{
                                              filtered_data <- base_dados %>% filter(UF == search_values$uf(),
                                                                                     tp_estabelecimento == search_values$type_city())
                                            }
                                          } else{
                                            if(search_values$type_city() == "TODOS OS TIPOS"){
                                              filtered_data <- base_dados %>% filter(UF == search_values$uf(),
                                                                                     nome_entidade == search_values$organization_city())                          
                                            } else{
                                              filtered_data <- base_dados %>% filter(UF == search_values$uf(),
                                                                                     nome_entidade == search_values$organization_city(),
                                                                                     tp_estabelecimento == search_values$type_city())   
                                            }                          
                                          }
                                        } else{
                                          if(search_values$organization_city() == "TODAS AS ORGANIZAÇÕES"){
                                            if(search_values$type_city() == "TODOS OS TIPOS"){
                                              filtered_data <- base_dados %>% filter(UF == search_values$uf(),
                                                                                     nome_municipio == search_values$city())                            
                                            } else{
                                              filtered_data <- base_dados %>% filter(UF == search_values$uf(),
                                                                                     nome_municipio == search_values$city(),
                                                                                     tp_estabelecimento == search_values$type_city())                              
                                            }                          
                                          } else{
                                            if(search_values$type_city() == "TODOS OS TIPOS"){
                                              filtered_data <- base_dados %>% filter(UF == search_values$uf(),
                                                                                     nome_entidade == search_values$organization_city(),
                                                                                     nome_municipio == search_values$city())                             
                                            } else{
                                              filtered_data <- base_dados %>% filter(UF == search_values$uf(),
                                                                                     nome_entidade == search_values$organization_city(),
                                                                                     nome_municipio == search_values$city(),
                                                                                     tp_estabelecimento == search_values$type_city())                            
                                            }                          
                                          }                        
                                        }
                                      }
                                    } else{
                                      if(search_values$uf() == "TODOS OS ESTADOS"){
                                        if(search_values$organization_state() == "TODAS AS ORGANIZAÇÕES"){
                                          if(search_values$type_state() == "TODOS OS TIPOS"){
                                            filtered_data <- base_dados %>% filter (ano == search_values$year())
                                          } else{
                                            filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                   tp_estabelecimento == search_values$type_state())
                                          }
                                        }else{
                                          if(search_values$type_state() == "TODOS OS TIPOS"){
                                            filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                   nome_entidade == search_values$organization_state())
                                          } else{
                                            filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                   nome_entidade == search_values$organization_state(),
                                                                                   tp_estabelecimento == search_values$type_state())
                                          }                        
                                        }
                                        
                                      } else{
                                        if(search_values$city() == "TODOS OS MUNICÍPIOS"){
                                          if(search_values$organization_city() == "TODAS AS ORGANIZAÇÕES"){
                                            if(search_values$type_city() == "TODOS OS TIPOS"){
                                              filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                     UF == search_values$uf())
                                            } else{
                                              filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                     UF == search_values$uf(),
                                                                                     tp_estabelecimento == search_values$type_city())
                                            }
                                          } else{
                                            if(search_values$type_city() == "TODOS OS TIPOS"){
                                              filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                     UF == search_values$uf(),
                                                                                     nome_entidade == search_values$organization_city())                          
                                            } else{
                                              filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                     UF == search_values$uf(),
                                                                                     nome_entidade == search_values$organization_city(),
                                                                                     tp_estabelecimento == search_values$type_city())   
                                            }                          
                                          }
                                        } else{
                                          if(search_values$organization_city() == "TODAS AS ORGANIZAÇÕES"){
                                            if(search_values$type_city() == "TODOS OS TIPOS"){
                                              filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                     UF == search_values$uf(),
                                                                                     nome_municipio == search_values$city())                            
                                            } else{
                                              filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                     UF == search_values$uf(),
                                                                                     nome_municipio == search_values$city(),
                                                                                     tp_estabelecimento == search_values$type_city())                              
                                            }                          
                                          } else{
                                            if(search_values$type_city() == "TODOS OS TIPOS"){
                                              filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                     UF == search_values$uf(),
                                                                                     nome_entidade == search_values$organization_city(),
                                                                                     nome_municipio == search_values$city())                             
                                            } else{
                                              filtered_data <- base_dados %>% filter(ano == search_values$year(),
                                                                                     UF == search_values$uf(),
                                                                                     nome_entidade == search_values$organization_city(),
                                                                                     nome_municipio == search_values$city(),
                                                                                     tp_estabelecimento == search_values$type_city())                            
                                            }                          
                                          }                        
                                        }
                                      }
                                    }
                                    filtered_data <- exibition_data(filtered_data)
                                    colnames(filtered_data) <- c("CNES", "UF", "codigo_municipio", "sigla", "Nome da OS", "Município", "Nome da unidade", "Ano de início da vigência", "campanha", "CNPJ", "no_contrato", "Valor anual (R$)", "Recurso federal", "Gestão", "servico", "NAT_JUR", "Tipo", "codigo_uf", "unidade_oss", "unidade_total_oss", "Membro do IBROSS")
                                    filtered_data <- filtered_data[,c(2,6,7,1,17,14,5,10,21,8,12,13)]
                                  })
  
  
  #Exibição os dados
  output$data_display <- DT::renderDataTable(
    filtering_data(),
    options = list(scrollX = '400px',
                   language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )


  #Download da nota metodológica
  output$note_download_button <- downloadHandler(
    filename = "nota_metodologica.pdf",
    content = function(file) {
      file.copy("www/nota_metodologica.pdf", file)
    }
  )
  
  #Criação dos arquivos para download em formato .csv
  output$download_button <- downloadHandler(
    filename = function(){
      paste('pesquisa_estabelecimentos_oss_', Sys.Date(),'.xlsx',sep='')
    },
    content = function(file){
      write.xlsx2(filtering_data(), file)
    }
  )
  
  #Plotagem do mapa
  unity_check <- function(unities){
    if(statistics_values$uf() == "DISTRITO FEDERAL"||statistics_values$uf() == "RONDÔNIA"||statistics_values$uf() == "MATO GROSSO DO SUL"||(statistics_values$year() != "TODOS OS ANOS" && statistics_values$uf() != "TODOS OS ESTADOS")){
      n <- 3
    } else{
      if (unities >= 1 && unities<6){
        n <- 2
      }
      if (unities >=6 && unities <15){
        n <- 3
      }
      if (unities >= 15 && unities <30){
        n <-4
      }
      if (unities >=30 && unities <50){
        n <- 5
      }
      if (unities >=50 && unities <80){
        n <- 6
      }
      if (unities >= 80 && unities <100){
        n <- 7
      }
      if (unities >= 100 && unities < 160){
        n <- 8
      }
      if (unities >= 160){
        n <- 9
      }
      
    }
    
    return(n)
  }
  
  
  data_map_formation <- function(year,uf){
    if(uf == "TODOS OS ESTADOS"){
      if(year == "TODOS OS ANOS"){
        if(!exists('brasil_shp')){
          brasil_shp <- readOGR("www", "br", stringsAsFactors=FALSE, encoding="UTF-8")
        }
        
        unities <- base_dados %>% group_by(codigo_uf) %>% summarise(Estabelecimentos = sum(!is.na(nome_unidade)))
        data_map <- merge(brasil_shp, unities, by.x = "CD_UF", by.y = "codigo_uf")
        proj4string(data_map) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
        Encoding(data_map$NM_UF) <- "UTF-8"
        data_map$Estabelecimentos[is.na(data_map$Estabelecimentos)] <- 0
      } else{
        if(!exists('brasil_shp')){
          brasil_shp <- readOGR("www", "br", stringsAsFactors=FALSE, encoding="UTF-8")
        }
        
        unities <- base_dados %>% filter(ano == year) %>% group_by(codigo_uf) %>% summarise(Estabelecimentos = sum(!is.na(nome_unidade)))
        data_map <- merge(brasil_shp, unities, by.x = "CD_UF", by.y = "codigo_uf")
        proj4string(data_map) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
        Encoding(data_map$NM_UF) <- "UTF-8"
        data_map$Estabelecimentos[is.na(data_map$Estabelecimentos)] <- 0        
      }
    } else{
      switch(uf,
             "ALAGOAS" = {
               if(!exists('al_shp')){
                 al_shp <- readOGR("www", "al", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               
               shp <- al_shp
             },
             "AMAZONAS" = {
               if(!exists('am_shp')){
                 am_shp <- readOGR("www", "am", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- am_shp
             },
             "AMAPÁ" = {
               if(!exists('ap_shp')){
                 ap_shp <- readOGR("www", "ap", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- ap_shp
             },
             "BAHIA" = {
               if(!exists('ba_shp')){
                 ba_shp <- readOGR("www", "ba", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- ba_shp
             },
             "CEARÁ" = {
               if(!exists('ce_shp')){
                 ce_shp <- readOGR("www", "ce", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- ce_shp
             },
             "DISTRITO FEDERAL" = {
               if(!exists('df_shp')){
                 df_shp <- readOGR("www", "df", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- df_shp
             },
             "ESPÍRITO SANTO" = {
               if(!exists('es_shp')){
                 es_shp <- readOGR("www", "es", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- es_shp
             },
             "GOIÁS" = {
               if(!exists('go_shp')){
                 go_shp <- readOGR("www", "go", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- go_shp
             },
             "MARANHÃO" = {
               if(!exists('ma_shp')){
                 ma_shp <- readOGR("www", "ma", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- ma_shp
             },
             "MINAS GERAIS" = {
               if(!exists('mg_shp')){
                 mg_shp <- readOGR("www", "mg", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- mg_shp
             },
             "MATO GROSSO DO SUL" = {
               if(!exists('ms_shp')){
                 ms_shp <- readOGR("www", "ms", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- ms_shp
             },
             "MATO GROSSO" = {
               if(!exists('mt_shp')){
                 mt_shp <- readOGR("www", "mt", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- mt_shp
             },
             "PARÁ" = {
               if(!exists('pa_shp')){
                 pa_shp <- readOGR("www", "pa", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- pa_shp
             },
             "PARAÍBA" = {
               if(!exists('pb_shp')){
                 pb_shp <- readOGR("www", "pb", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- pb_shp
             },
             "PERNAMBUCO" = {
               if(!exists('pe_shp')){
                 pe_shp <- readOGR("www", "pe", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- pe_shp
             },
             "PARANÁ" = {
               if(!exists('pr_shp')){
                 pr_shp <- readOGR("www", "pr", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- pr_shp
             },
             "RIO DE JANEIRO" = {
               if(!exists('rj_shp')){
                 rj_shp <- readOGR("www", "rj", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- rj_shp
             },
             "RONDÔNIA" = {
               if(!exists('ro_shp')){
                 ro_shp <- readOGR("www", "ro", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- ro_shp
             },
             "RIO GRANDE DO SUL" = {
               if(!exists('rs_shp')){
                 rs_shp <- readOGR("www", "rs", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- rs_shp
             },
             "SANTA CATARINA" = {
               if(!exists('sc_shp')){
                 sc_shp <- readOGR("www", "sc", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- sc_shp
             },
             "SÃO PAULO" = {
               if(!exists('sp_shp')){
                 sp_shp <- readOGR("www", "sp", stringsAsFactors=FALSE, encoding="UTF-8")
               }
               shp <- sp_shp
             }
      )
      if(year == "TODOS OS ANOS"){
        unities <- base_dados %>% filter(UF == uf) %>% group_by(codigo_municipio) %>% summarise(Estabelecimentos = sum(!is.na(nome_unidade)))
        shp$CD_MUN <- substring(shp$CD_MUN, 1, nchar(shp$CD_MUN)-1)
        data_map <- merge(shp, unities, by.x = "CD_MUN", by.y = "codigo_municipio")
        proj4string(data_map) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
        Encoding(data_map$NM_MUN) <- "UTF-8"
        data_map$Estabelecimentos[is.na(data_map$Estabelecimentos)] <- 0
      } else{
        unities <- base_dados %>% filter(ano == year & UF == uf) %>% group_by(codigo_municipio) %>% summarise(Estabelecimentos = sum(!is.na(nome_unidade)))
        shp$CD_MUN <- substring(shp$CD_MUN, 1, nchar(shp$CD_MUN)-1)
        data_map <- merge(shp, unities, by.x = "CD_MUN", by.y = "codigo_municipio")
        proj4string(data_map) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
        Encoding(data_map$NM_MUN) <- "UTF-8"
        data_map$Estabelecimentos[is.na(data_map$Estabelecimentos)] <- 0        
      }      
    }
    
    return(data_map)
  }
  
  map_plotting <- function(data_map, number_bins, location){
    pal <- colorBin("YlOrRd", log10(data_map$Estabelecimentos), number_bins, pretty = TRUE)
    
    state_popup <- paste0(location, 
                          "<br><strong>Estabelecimentos: </strong>", 
                          data_map$Estabelecimentos)
    
    leaflet(data = data_map) %>%
      addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(opacity = 1), group = "ESRI World Imagery") %>%
      addPolygons(fillColor = ~pal(log10(data_map$Estabelecimentos)), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = state_popup) %>%
      addLegend("bottomright", pal = pal, values = ~data_map$Estabelecimentos,
                labFormat = labelFormat(prefix = '', suffix = '', between = ' &ndash; ', digits = 0, big.mark = '.',
                                        transform = function(x) 10^x),
                title = "Estabelecimentos",
                opacity = 1)
  }
  
  map_compiled <- eventReactive(input$display_button,
                                {
                                  isolate({
                                    data_map <- data_map_formation(statistics_values$year(), statistics_values$uf())
                                    number_bins <- unity_check(sum(data_map$Estabelecimentos))
                                    
                                    if(statistics_values$uf() == "TODOS OS ESTADOS"){
                                      location <- data_map$NM_UF
                                    } else{
                                      location <- data_map$NM_MUN
                                    }
                                    
                                    map_plotting(data_map, number_bins, location)
                                  })
                                }
  )
  
  output$map_plot <- renderLeaflet({
      map_compiled()
  })
  
  #Card do total de estabelecimentos
  os_counting <- function(year, uf){
    if(year == "TODOS OS ANOS"){
      if(uf == "TODOS OS ESTADOS"){
        organizations <- base_dados$nome_entidade
      } else{
        organizations <- base_dados %>% filter(UF == uf) %>% pull(nome_entidade)
      }
    } else{
      if(uf == "TODOS OS ESTADOS"){
        organizations <- base_dados %>% filter(ano == year) %>% pull(nome_entidade)
      } else{
        organizations <- base_dados %>% filter(ano == year & UF == uf) %>% pull(nome_entidade)       
      }
    }
    return(organizations)
  }
  
  output$number_os <- renderValueBox({
    organizations_number <- sum(!is.na(unique(os_counting(statistics_values$year(), statistics_values$uf())))) 
    
    valueBox(
      organizations_number,
      ifelse(organizations_number <= 1, "Organização Social de Saúde atuante", "Organizações Sociais de Saúde atuantes"),
      color = "aqua"
    )
  })
  
  #Tabela com quantidade de estabelecimentos por OS
  os_table_filtering <- function(){
    total_organizations <- data.frame(os_counting(statistics_values$year(), statistics_values$uf()))
    colnames(total_organizations) <- "OSS"
    table_organizations <- total_organizations %>% group_by(OSS) %>% summarise('Quantidade de estabelecimentos' = n())

  }
  
  output$os_table <- DT::renderDataTable(
    os_table_filtering(),
    options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE

  )
  
  #Gráfico com distribuição dos estabelecimentos por OS
  os_unities_calculation <- function(){
    total_organizations <- data.frame(os_counting(statistics_values$year(), statistics_values$uf()))
    colnames(total_organizations) <- "nome_entidades"
    table_organizations <- total_organizations %>% group_by(nome_entidades) %>% summarise(counting = n())
    
    if(max(table_organizations$counting) >= 8){
      plot_table_min <- table_organizations %>%
        filter(counting < 8) %>%
        group_by(counting) %>%
        summarise(oss = n())
      
      plot_table_max <- table_organizations %>%
        filter(counting >= 8) %>%
        summarise(oss = sum(n()))
      
      plot_table_max <- cbind(8, plot_table_max)
      colnames(plot_table_max) <- c("counting", "oss")
      
      plot_table <- data.frame(rbind(plot_table_min, plot_table_max))
    } else{
      plot_table <- table_organizations %>%
        group_by(counting) %>%
        summarise(oss = n())
      
      plot_table <- data.frame(plot_table)
    }
    
    if(1 %in% plot_table$counting){
      NA
    } else{
      plot_table[nrow(plot_table)+1,] <- c(1,0)
    }
    
    if(2 %in% plot_table$counting){
      NA
    } else{
      plot_table[nrow(plot_table)+1,] <- c(2,0)
    }
    
    if(3 %in% plot_table$counting){
      NA
    } else{
      plot_table[nrow(plot_table)+1,] <- c(3,0)
    }
    
    if(4 %in% plot_table$counting){
      NA
    } else{
      plot_table[nrow(plot_table)+1,] <- c(4,0)
    }
    
    if(5 %in% plot_table$counting){
      NA
    } else{
      plot_table[nrow(plot_table)+1,] <- c(5,0)
    }
    
    if(6 %in% plot_table$counting){
      NA
    } else{
      plot_table[nrow(plot_table)+1,] <- c(6,0)
    }
    
    if(7 %in% plot_table$counting){
      NA
    } else{
      plot_table[nrow(plot_table)+1,] <- c(7,0)
    }
    
    if(8 %in% plot_table$counting){
      NA
    } else{
      plot_table[nrow(plot_table)+1,] <- c(8,0)
    }
    
    plot_table <- plot_table %>% arrange(counting)
    
    plot_table$counting[plot_table$counting == 8] <- "8+"
    
    return(plot_table)
  }
  
  output$os_unities_plot <- renderPlotly({
    plot_table <- os_unities_calculation()
    
    plot_ly(plot_table, x= ~counting, y = ~oss, type = 'bar', name = "Organizações Sociais de Saúde", hovertemplate = ~paste("Quantidade: ", format(as.numeric(oss), nsmall = 0, big.mark = ".", decimal.mark = ",", preserve.width = "none")), marker = list(color = "rgb(0,0,139)")) %>% 
      layout(yaxis = list(title = "Quantidade de OSS"), xaxis = list(title = "Estabelecimentos geridos")) %>%
      toWebGL()
  })
  
  #Tabela do tipo de estabelecimentos
  type_unities_counting <- function(year, uf){
    if(year == "TODOS OS ANOS"){
      if(uf == "TODOS OS ESTADOS"){
        type_unities <- base_dados$tp_estabelecimento
      } else{
        type_unities <- base_dados %>% filter(UF == uf) %>% pull(tp_estabelecimento)
      }
    } else{
      if(uf == "TODOS OS ESTADOS"){
        type_unities <- base_dados %>% filter(ano == year) %>% pull(tp_estabelecimento)
      } else{
        type_unities <- base_dados %>% filter(ano == year & UF == uf) %>% pull(tp_estabelecimento)       
      }
    }
    return(type_unities)
  }
  
  type_unities_filtering <- function(){
    total_types <- data.frame(type_unities_counting(statistics_values$year(), statistics_values$uf()))
    colnames(total_types) <- "Tipo"
    table_types <- total_types %>% group_by(Tipo) %>% summarise('Quantidade de estabelecimentos' = n())
    
  }
  
  output$type_unity_table <- DT::renderDataTable(
    type_unities_filtering(),
    options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.12.1/i18n/pt-BR.json')),
    rownames = FALSE
  )
  
  #Gráfico do tipo de estabelecimentos
  type_unity_graph_treatment <- function(){
    type_plot <- type_unities_filtering()
    colnames(type_plot) <- c('tipo', 'estabelecimentos')
    
    if(nrow(type_plot > 7)){
      type_plot <- type_plot %>%
        mutate(rank = rank(-estabelecimentos), 
               tipo = ifelse(rank <= 7, tipo, 'OUTROS'))
    }
    
    return(type_plot)
  }
  
  output$type_unity_graph <- renderPlotly({
    type_plot <- type_unity_graph_treatment()
    
    plot_ly(type_plot, labels = ~tipo, values = ~estabelecimentos, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ifelse(type_plot$tipo == "OUTROS", paste(sum(type_plot$estabelecimentos[type_plot$tipo == "OUTROS"]), ifelse(sum(type_plot$estabelecimentos[type_plot$tipo == "OUTROS"])>1,'unidades','unidade')),paste(type_plot$estabelecimentos, ifelse(type_plot$estabelecimentos>1,'unidades','unidade'))),
            showlegend = FALSE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      toWebGL()
  })

  #Gráfico da gestão dos estabelecimentos
  state_city_filtering <- function(year, uf){
    if(year == "TODOS OS ANOS"){
      if(uf == "TODOS OS ESTADOS"){
        type_administration <- base_dados$TPGESTAO
      } else{
        type_administration <- base_dados %>% filter(UF == uf) %>% pull(TPGESTAO)
      }
    } else{
      if(uf == "TODOS OS ESTADOS"){
        type_administration <- base_dados %>% filter(ano == year) %>% pull(TPGESTAO)
      } else{
        type_administration <- base_dados %>% filter(ano == year & UF == uf) %>% pull(TPGESTAO)       
      }
    }
    
    for(i in 1:length(type_administration)){
      ifelse(type_administration[i] == "M", type_administration[i] <- "Municipal",
             ifelse(type_administration[i] == "E", type_administration[i] <- "Estadual",
                    ifelse(type_administration[i] == "D", type_administration[i] <- "Dupla", NA)))
    }
    
    return(type_administration)
  }

  state_city_treatment <- function(){
    type_administration <- data.frame(state_city_filtering(statistics_values$year(), statistics_values$uf()))
    
    colnames(type_administration) <- "gestao"
    administration_graph <- type_administration %>% group_by(gestao) %>% summarise(estabelecimentos = n())
  }
  
  output$state_city_plot <- renderPlotly({
    administration_plot <- state_city_treatment()
    
    plot_ly(administration_plot, labels = ~gestao, values = ~estabelecimentos, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(estabelecimentos, ifelse(estabelecimentos>1,'unidades','unidade')),
            showlegend = TRUE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      toWebGL()
  })

  #Gráfico de filiação ao IBROSS
  ibross_filtering <- function(year, uf){
    if(year == "TODOS OS ANOS"){
      if(uf == "TODOS OS ESTADOS"){
        ibross_member <- base_dados$ibross
      } else{
        ibross_member <- base_dados %>% filter(UF == uf) %>% pull(ibross)
      }
    } else{
      if(uf == "TODOS OS ESTADOS"){
        ibross_member <- base_dados %>% filter(ano == year) %>% pull(ibross)
      } else{
        ibross_member <- base_dados %>% filter(ano == year & UF == uf) %>% pull(ibross)       
      }
    }
    
    for(i in 1:length(ibross_member)){
      ifelse(ibross_member[i] == 0, ibross_member[i] <- "Não-membro",
             ifelse(ibross_member[i] == 1, ibross_member[i] <- "Filiada ao IBROSS", NA))
    }
    
    return(ibross_member)
  }
  
  ibross_treatment <- function(){
    ibross_member <- data.frame(ibross_filtering(statistics_values$year(), statistics_values$uf()))
    
    colnames(ibross_member) <- "ibross"
    ibross_graph <- ibross_member %>% group_by(ibross) %>% summarise(estabelecimentos = n())
  }
  
  output$ibross_plot <- renderPlotly({
    ibross_plot <- ibross_treatment()
    
    plot_ly(ibross_plot, labels = ~ibross, values = ~estabelecimentos, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(estabelecimentos, ifelse(estabelecimentos>1,'unidades','unidade')),
            showlegend = TRUE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
      toWebGL()
  })
  
  #Plotagem da evolução do total de estabelecimentos
  unities_formation <- function(uf){
    if(uf == "TODOS OS ESTADOS"){
      data_unities <- data.frame(base_dados %>% filter(ano != "SEM ANO REGISTRADO") %>% group_by(ano) %>% summarise(estabelecimentos = sum(!is.na(nome_unidade))))

    } else{
      data_unities <- data.frame(base_dados %>% filter(ano != "SEM ANO REGISTRADO" & uf == base_dados$UF) %>% group_by(ano) %>% summarise(estabelecimentos = sum(!is.na(nome_unidade))))
    }
    
    if(1994 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(1994,0)
    }
    
    if(1996 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(1996,0)
    }
    
    if(1997 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(1997,0)
    }
    
    if(1998 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(1998,0)
    }
    
    if(1999 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(1999,0)
    }
    
    if(2000 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2000,0)
    }
    
    if(2002 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2002,0)
    }
    
    if(2005 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2005,0)
    }
    
    if(2006 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2006,0)
    }
    
    if(2007 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2007,0)
    }
    
    if(2008 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2008,0)
    }
    
    if(2009 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2009,0)
    }
    
    if(2010 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2010,0)
    }
    
    if(2011 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2011,0)
    }
    
    if(2012 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2012,0)
    }
    
    if(2013 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2013,0)
    }
    
    if(2014 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2014,0)
    }
    
    if(2015 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2015,0)
    }
    
    if(2016 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2016,0)
    }
    
    if(2017 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2017,0)
    }
    
    if(2018 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2018,0)
    }
    
    if(2019 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2019,0)
    }
    
    if(2020 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2020,0)
    }
    
    if(2021 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2021,0)
    }
    
    if(2022 %in% data_unities$ano){
      NA
    } else{
      data_unities[nrow(data_unities)+1,] <- c(2022,0)
    }
    
    data_unities <- data_unities %>% arrange(ano)
    
    return(data_unities)
  }
  
  unities_plotting <- function(data_unities){
    plot_ly(data_unities, x= ~ano, y = ~estabelecimentos, type = 'bar', name = "Total", hovertemplate = ~paste("Estabelecimentos: ", format(as.numeric(estabelecimentos), nsmall = 0, big.mark = ".", decimal.mark = ",", preserve.width = "none")), marker = list(color = "rgb(0,0,139)")) %>%
      add_trace(y = ~estabelecimentos, name = "Total", type = "scatter", mode = "lines+markers", hovertemplate = ~paste("Estabelecimentos: ", format(as.numeric(estabelecimentos), nsmall = 0, big.mark = ".", decimal.mark = ",", preserve.width = "none")), line = list(color = "rgb(139,0,0)"), marker = list(color = "rgb(139,0,0)")) %>% 
      layout(yaxis = list(title = "Estabelecimentos geridos por OS"), xaxis = list(title = "Ano", rangeselector = list(buttons = list(list(count = 1, label = "Ano", step = "ano", stepmode = "backward"), list(count = 1, label = "Ano", step = "ano", stepmode = "todate"))), rangeslider= list(type = "date")), barmode = 'stack', autosize = TRUE) %>% 
      toWebGL()
    
  }
  
  unities_compiled <- eventReactive(input$display_button,
                                    {
                                      isolate({
                                        data_unities <- unities_formation(statistics_values$uf())
                                        unities_plotting(data_unities)
                                        }) 
                                    }
  )
  
  output$unities_plot <- renderPlotly({
    unities_compiled()
  })
  
}

#Execução da plataforma---------------------------------------------------------
shinyApp(ui, server)
