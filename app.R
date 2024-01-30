library(shiny)
library(bslib)
library(datasets)

# script global.R

pacman::p_load("tidyverse", "lubridate", "shiny")

# lectura de datos
malaria_data <- rio::import(here::here("Documents", "_2023", "om", "rData", "pensa", "pensa-20231205-shiny", "data", "malaria_facility_count_data.csv")) %>% 
  as_tibble()
# lectura de datos vs02
malaria_data_vs02 <- rio::import(here::here("Documents", "_2023", "om", "rData", "pensa", "pensa-20231205-shiny", "data", "malaria_facility_count_data.csv")) %>% 
  as_tibble()

# limpiar datos y pivotar largo
malaria_data <- malaria_data %>%
  select(-newid) %>%
  pivot_longer(cols = starts_with("malaria_"), names_to = "age_group", values_to = "cases_reported")
# limpiar datos y pivotar largo vs02
malaria_data_vs02 <- malaria_data_vs02 %>%
  select(-newid) %>%
  pivot_longer(cols = starts_with("malaria_"), names_to = "age_group", values_to = "cases_reported")


# define la función de gráficos
plot_epicurve <- function(data, district = "All", agegroup = "malaria_tot") {
  
  if (!("All" %in% district)) {
    data <- data %>%
      filter(District %in% district)
    
    plot_title_district <- stringr::str_glue("{paste0(district, collapse = ', ')} districts")
    
  } else {
    
    plot_title_district <- "all districts"
    
  }
  
  # si no quedan datos, devuelve NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  data <- data %>%
    filter(age_group == agegroup)
  
  
  # si no quedan datos, devuelve NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  if (agegroup == "malaria_tot") {
    agegroup_title <- "All ages"
  } else {
    agegroup_title <- stringr::str_glue("{str_remove(agegroup, 'malaria_rdt')} years")
  }
  
  
  ggplot(data, aes(x = data_date, y = cases_reported)) +
    geom_col(width = 1, fill = "#0000FF") +
    theme_minimal() +
    labs(
      x = "date",
      y = "number of cases",
      title = stringr::str_glue("Malaria cases - {plot_title_district}"),
      subtitle = agegroup_title
    )
}
# define la función de gráficos vs02
plot_epicurve_vs02 <- function(data, district = "All", agegroup = "malaria_tot") {
  
  if (!("All" %in% district)) {
    data <- data %>%
      filter(District %in% district)
    
    plot_title_district <- stringr::str_glue("{paste0(district, collapse = ', ')} districts")
    
  } else {
    
    plot_title_district <- "all districts"
    
  }
  
  # si no quedan datos, devuelve NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  data <- data %>%
    filter(age_group == agegroup)
  
  
  # si no quedan datos, devuelve NULL
  if (nrow(data) == 0) {
    
    return(NULL)
  }
  
  if (agegroup == "malaria_tot") {
    agegroup_title <- "All ages"
  } else {
    agegroup_title <- stringr::str_glue("{str_remove(agegroup, 'malaria_rdt')} years")
  }
  
  
  ggplot(data, aes(x = data_date, y = cases_reported)) +
    geom_col(width = 1, fill = "#F100CB") +
    theme_minimal() +
    labs(
      x = "date",
      y = "number of cases",
      title = stringr::str_glue("Malaria cases - {plot_title_district}"),
      subtitle = agegroup_title
    )
}
# Fin script global.R

# Define UI for application that draws a histogram
ui <- fluidPage(
  ## Google Fonts 'Roboto' _____________________________________________________________________________________________________________________________________ ##
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href="https://fonts.gstatic.com"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400;500;700;900&display=swap", rel="stylesheet")
  ),
  ## Google Fonts 'Roboto Slab' _____________________________________________________________________________________________________________________________________ ##
  tags$head(
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com"),
    tags$link(href="https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@100;200;300;400;500;600;700;800;900&family=Roboto:wght@100&display=swap", rel="stylesheet")
  ),
  ## css _____________________________________________________________________________________________________________________________________ ##
  tags$head(
    tags$link(rel="stylesheet", href="sccs/custom.css"),
  ),
  
  tags$style("
    html {
      font-size: 14px !important;
    }
    @media (min-width: 768px)
    .navbar>.container-fluid .navbar-brand {
      margin-left: 0 !important;
    }
    .navbar-default .navbar-nav .active a {
      color: #000 !important;
      background-color: #fff !important;
    }
    .navbar-default .navbar-nav .open a {
      color: #000 !important;
      background-color: #fff !important;
    }
    a {
      color: #333 !important;
      text-decoration: none;
      background-color: transparent !important;
      display: flex !important;
    }
    .nav-pills li.active a {
      color: #000 !important;
      background-color: transparent !important;
    }
    .nav-pills li.hover a {
      color: #000 !important;
    }
    .dropdown-toggle::after {
      display: none;
    }
    .navbar-nav {
      display: flex;
      flex-direction: row;
    }
    .nav-pills {
      display: flex;
      flex-direction: column;
    }
    .navbar {
      margin-bottom: 0 !important;
    }
    .navbar-default {
      background-color: #fff;
    }
    .navbar-header {
      display: none;
    }
    .navbar-brand {
      height: auto;
      padding-top: calc(0.3125rem + 5px);
      border-bottom: 1px solid #e3e3e3;
      color: #777 !important;
      margin-bottom: 1rem;
      font-size: .9rem !important;
    }
    .navbar-brand-aspb {
      left: 15px;
      position: absolute;
    }
    .d-inline-block align-text-top {
      border: none !important;
    }
    .nav-pills {
      line-height: .5rem;
    }
    .navbar-h4 {
      padding-top: calc(0.3125rem + 5px);
    }
    .container-fluid {
      overflow: hidden;
      padding: 0 !important;
    }
    .dropdown-menu {
      box-shadow: none;
      border: none;
    }
    .well {
      height: calc(100dvh - (120px + 1rem)) !important;
      border-radius: 0;
      margin-bottom: 0 !important;
      border-right: solid 1px #e7e7e7;
      border-top: 0 !important;
      overflow: hidden;
      padding: 19px !important;
      max-width: 310px;
    }
    .col-sm-4 {
      max-width: 310px;
    }
    .col-sm-8 {
      /*margin-left: calc((33.3333333333% + 310px) /2);*/
      position: absolute;
      width: calc((100% /12) *9);
      margin-left: calc((25% + 310px) /2);
    }
    #tab-pensa-1,
    #tab-pensa-2A {
      background-image: linear-gradient(180deg, #f5f5f5 0%, #fff 100%);
    }
    article {
      padding-top: 3rem;
      overflow: scroll;
      height: calc(100dvh - (120px + 1rem)) !important;
    }
    .accordion-item {
      background-color: transparent !important;
      border: solid 1px #707070;
    }
    .accordion-button {
      font-size: 1.75rem;
      background-color: transparent !important;
      padding: calc(var(--bs-accordion-btn-padding-y) /2) var(--bs-accordion-btn-padding-x) !important;
    }
    .accordion-flush .accordion-item:last-child {
      border-bottom: solid 1px #707070;
    }
    .accordion {
      border-top: solid 1px #707070;
    }
    .accordion-button:not(.collapsed) {
      box-shadow: none !important;
    }
    .accordion-button::after {
      background-image: var(--aspb-accordion-btn-icon);
    }
    .accordion-button::after {
      background-image: url('img/ASPBaccordion-btn-icon.svg') !important;
      background-size: contain;
      background-position: center;
      background-repeat: no-repeat;
      height: 40px;
      width: 40px;
    }
    .accordion-button:not(.collapsed)::after {
      background-image: url('img/ASPBaccordion-btn-active-icon.svg') !important;
    }
    .accordion-button {
      font-family: 'Roboto Slab', serif;
      font-weight: 400;
    }
    .article-content {
      margin-bottom: 3rem;
    }
    .collaborators-logos {
      display: flex;
      align-items: center;
      justify-content: center;
    }
    .collaborator-logo {
      margin: 0 1.5rem;
      mix-blend-mode: multiply;
    }
    li {
      margin-bottom: 0.25rem;
    }
    .shiny-plot-output {
      background-color: #fff;
      display: flex;
      align-items: center;
      justify-content: center;
    }
    img {
      mix-blend-mode: multiply;
    }
    #title_malaria_epicurve {
      background-color: #fff;
      padding: 1.5rem 0;
      border-radius: 1.5rem 1.5rem 0 0;
      text-align: center;
      margin-top: 3rem;
      margin-bottom: 0;
      background-color: rgb(190 243 254 / 25%);
      font-weight: 400 !important;
    }
    #title_malaria_epicurve_vs02 {
      background-color: #fff;
      padding: 1.5rem 0;
      border-radius: 1.5rem 1.5rem 0 0;
      text-align: center;
      margin-top: 3rem;
      margin-bottom: 0;
      background-color: rgb(202 186 242 / 10%);
      font-weight: 400 !important;
    }
    .selectize-input {
      border: none;
      border-radius: 1.5rem;
      display: flex;
      align-items: center;
      min-height: 44px;
    }
    label {
      font-weight: 400 !important;
    }
    .selectize-dropdown .selected {
      background-color: #f5f5f5 !important;
      color: #000 !important;
    }
    .selectize-control.single .selectize-input:not(.no-arrow):after {
      border: solid black;
      border-width: 0 1.5px 1.5px 0;
      display: inline-block;
      padding: 5px;
      transform: rotate(45deg);
      -webkit-transform: rotate(45deg);
      margin-top: -7.5px !important;
    }
    .btn {
      border: solid 2px #333 !important;
      border-radius: 25px;
      margin: 0 auto;
      font-weight: 500 !important;
      display: flex !important;
      align-items: center;
    }
    #aquest_indicador_identifica {
      display: flex;
      justify-content: center;
    }
    .fas {
      background-image: url(img/ASPBarrow-bottom-icon.svg) !important;
      height: 25px;
      width: 25px;
      color: rgb(245 245 245 / 0%);
      transform: translateX(-5px);
    }
    .selectize-input.focus {
      border-color: rgb(91 225 255);
      outline: 0;
      box-shadow: inset 0 1px 1px rgba(0,0,0,0.075), 0 0 8px rgba(91,225,255,0.6);
    }
    .selectize-input.dropdown-active {
      border-radius: 1.5rem;
      display: flex;
      align-items: center;
      min-height: 44px;
    }
    .selectize-control.multi .selectize-input div {
      padding: 2px 10px !important;
      border-radius: 1rem !important;
    }
    .caret {
      display: none !important;
    }
    #title_sidebar_panel {
      font-family: 'Roboto Slab', serif !important;
    }
    .alert-pensa-dones {
      background-image: linear-gradient(180deg, #fff 0%, rgb(190 243 254 /50%) 100%);
      padding: 3rem 1.5rem 1.5rem 1.5rem;
      border-radius: 0 0 1.5rem 1.5rem;
      border: none;
    }
    .alert-pensa-homes {
      background-image: linear-gradient(180deg, #fff 0%, rgb(202 186 242 /50%) 100%);
      padding: 3rem 1.5rem 1.5rem 1.5rem;
      border-radius: 0 0 1.5rem 1.5rem;
      border: none;
    }
    .pensa-dones-circle-icon {
      height: 53px;
      width: 53px;
      background-color: #BEF3FE;
	    background-image: conic-gradient(from 90deg, #BEF3FE, #0000FF);
      border-radius: 50%;
      margin-right: 15px;
    }
    .pensa-homes-circle-icon {
      height: 53px;
      width: 53px;
      background-color: #BEF3FE;
	    background-image: conic-gradient(from 90deg, #BEF3FE 0%, #F100CB 50%);
      border-radius: 50%;
      margin-right: 15px;
    }
    .alert-pensa {
      padding: 1.5rem;
      border: solid 0.125rem #FF0534;
      border-radius: 1.5rem;
      margin: 0 15px;
    }
    #alert-pensa {
      padding: 0 15px;
    }
    .bi-exclamation-triangle-fill {
      width: 47px;
      height: auto;
      margin-right: 15px !important;
    }
    .hr-alert {
      border-bottom: solid 0.5rem #FF0534 !important;
      width: 100px;
    }
    #tab-pensa-2AOutput {
      padding-bottom: 3rem;
      overflow: scroll;
      height: calc(100dvh - (120px + 1rem)) !important;
    }
  "),
  ## Favicon _____________________________________________________________________________________________________________________________________ ##
  tags$head(
    tags$link(rel="icon", href="http://webs.aspb.cat/guia-comunicacio-digital/guia/img/favicon.ico"),
  ),
  ## js _____________________________________________________________________________________________________________________________________ ##
  tags$head(
    tags$script(src="bootstrap/dist/js/bootstrap.bundle.min.js"),
  ),
  
  ## ui.R ___________________________________________________________________________________________________________________________________ ##
  ## html content-navbar ___ ##
  bootstrapPage(
    htmlTemplate("www/components/navbar.html", name = "navbar-html")
  ),
  
  navbarPage("",
             tabPanel("Sobre aquesta web",
                      fluidRow(id = "tab-pensa-1",
                                        navlistPanel(
                                          "Contingut de la pàgina",
                                          tabPanel("Presentació",
                                                   bootstrapPage(
                                                     htmlTemplate("www/components/nav-home.html", name = "nav-home")
                                                   ),
                                          ),
                                          tabPanel("Metodologia",
                                                   bootstrapPage(
                                                     htmlTemplate("www/components/nav-profile.html", name = "nav-profile")
                                                   ),
                                          ),
                                          tabPanel("Crèdits i autories",
                                                   bootstrapPage(
                                                     htmlTemplate("www/components/nav-contact.html", name = "nav-contact")
                                                   ),
                                          )
                                        )
                        ),
             ),
             navbarMenu("Pobresa energètica",
                        tabPanel("En números",
                          fluidRow(id = "tab-pensa-2A",
                                   sidebarLayout(
                                     
                                     sidebarPanel(
                                       h5(id = "title_sidebar_panel", "Persones en situació de pobresa energètica"),
                                       hr(),
                                       # selector para el distrito
                                       selectInput(
                                         inputId = "select_district",
                                         label = "Select district",
                                         choices = c(
                                           "All",
                                           "Spring",
                                           "Bolo",
                                           "Dingo",
                                           "Barnard"
                                         ),
                                         selected = "All",
                                         multiple = TRUE
                                       ),
                                       hr(),
                                       # selector para el grupo de edad
                                       selectInput(
                                         inputId = "select_agegroup",
                                         label = "Select age group",
                                         choices = c(
                                           "All ages" = "malaria_tot",
                                           "0-4 yrs" = "malaria_rdt_0-4",
                                           "5-14 yrs" = "malaria_rdt_5-14",
                                           "15+ yrs" = "malaria_rdt_15"
                                         ), 
                                         selected = "All",
                                         multiple = FALSE
                                       ),
                                        p("Aquest indicador identifica a les persones que no es poden permetre mantenir el seu habitatge a una temperatura adequada durant els mesos freds."),
                                       # línea horizontal
                                       hr(),
                                       div(id = "aquest_indicador_identifica",
                                         downloadButton(
                                           outputId = "download_epicurve",
                                           label = "Descarregar"
                                         )
                                       )
                                       
                                     ),
                                     
                                     mainPanel(
                                       column(12, id = "tab-pensa-2AOutput",
                                         column(6,
                                            h6("Percentatge de dones amb temperatura inadequada als mesos freds", id = "title_malaria_epicurve"),
                                            # La curva epidemiológica va aquí
                                            plotOutput("malaria_epicurve"),
                                            bootstrapPage(
                                              htmlTemplate("www/components/alert-pensa-dones.html", name = "alert-pensa-dones")
                                            ),
                                         ),
                                         column(6,
                                            h6("Percentatge de homes amb temperatura inadequada als mesos freds", id = "title_malaria_epicurve_vs02"),
                                            # La curva epidemiológica va aquí
                                            plotOutput("malaria_epicurve_vs02"),
                                            bootstrapPage(
                                              htmlTemplate("www/components/alert-pensa-homes.html", name = "alert-pensa-homes")
                                            ),
                                         ),
                                         
                                           bootstrapPage(
                                             htmlTemplate("www/components/alert-pensa.html", name = "alert-pensa", id ="alert-pensa")
                                           ),
                                       ),
                                     ),
                                     
                                   )
                          )
                                 
                        ),
                        tabPanel("Per barris")),
             
             navbarMenu("Pobresa energètica i salut",
                        tabPanel("Estat de salut"),
                        tabPanel("Ús de serveis de salut"),
                        tabPanel("Ús de medicaments"))
  ),
  
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$malaria_epicurve <- renderPlot(
    plot_epicurve(malaria_data, district = input$select_district, agegroup = input$select_agegroup)
  )
  
  # vs02
  output$malaria_epicurve_vs02 <- renderPlot(
    plot_epicurve_vs02(malaria_data_vs02, district = input$select_district, agegroup = input$select_agegroup)
  )
  
  output$download_epicurve <- downloadHandler(
    filename = function() {
      stringr::str_glue("malaria_epicurve_{input$select_district}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             plot_epicurve(malaria_data, district = input$select_district, agegroup = input$select_agegroup),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
  # vs02
  output$download_epicurve <- downloadHandler(
    filename = function() {
      stringr::str_glue("malaria_epicurve_vs02_{input$select_district}.png")
    },
    
    content = function(file) {
      ggsave(file, 
             plot_epicurve_vs02(malaria_data_vs02, district = input$select_district, agegroup = input$select_agegroup),
             width = 8, height = 5, dpi = 300)
    }
    
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
