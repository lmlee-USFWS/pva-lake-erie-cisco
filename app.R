library(compositions)
library(dplyr)
library(EnvStats)
library(ggplot2)
library(mpmsim)
library(popbio)
library(readxl)
library(reshape2)
library(rhandsontable)
library(shiny)
library(shinyalert)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(utils)

##########################################################################################################
##  PREP  ################################################################################################
###########################################################################################################---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
#-SOURCE EXTERNAL FUNCTIONS-------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
source("utils.R",local=TRUE)


#---------------------------------------------------------------------------------------------------------
#-READ IN DATA--------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
# length-weight
lw.cisco <- read_excel("cisco.xlsx",sheet="lenwt")
lw <- read_excel("cisco.xlsx",sheet="lw")
n.lw <- nrow(lw)

# age-length
agelen.cisco <- read_excel("cisco.xlsx",sheet="agelen")
vonB <- read_excel("cisco.xlsx",sheet="vonB")
n.vonB <- nrow(vonB)

# maturity
mat.cisco <- read_excel("cisco.xlsx",sheet="maturity")

# fecundity
fwt.cisco <- read_excel("cisco.xlsx",sheet="fecundweight")
flen.cisco <- read_excel("cisco.xlsx",sheet="fecundlength")
fecund <- read_excel("cisco.xlsx", sheet ="fecundity")
n.fecund <- nrow(fecund)

# early life survival
earlyS <- read_excel("cisco.xlsx",sheet="earlyS")


#---------------------------------------------------------------------------------------------------------
#-POPULATE LISTS------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
#stat.dist <- c("Fix","Uniform","Normal","Triangle")
stock.area <- c("Lakewide","Eastern Unit","Western Unit")
threat.apply <- c("No","Yes")
opts.adv <- c("Default","Custom")


##########################################################################################################
##  SHINY  ###############################################################################################
##########################################################################################################
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Lake Erie Cisco Population Viability Analysis",titleWidth = 450),
  dashboardSidebar(id="",
             width = 450,
             sidebarMenu(id = "tabs",
                    menuItem("Welcome",tabName = "menuWelcome", icon = shiny::icon("face-smile")),
                    menuItem("Assumptions",tabName = "menuAssumptions", icon = shiny::icon("eye"),startExpanded = TRUE,
                        menuSubItem("Survival",tabName = "menuSurvival", icon = shiny::icon("skull-crossbones")),
                        menuSubItem("Reproduction",tabName = "menuReproduction", icon = shiny::icon("egg"))),
                    menuItem("Model Set Up",tabName = "menuSetup", icon = shiny::icon("wrench"),startExpanded = TRUE,
                        menuSubItem("Dimensions",tabName = "menuDim", icon = shiny::icon("list-ol")),
                        menuSubItem("Proposed Stocking",tabName = "menuStocking", icon = shiny::icon("fish")),
                        menuSubItem("Allee Effect",tabName = "menuAllee", icon = shiny::icon("xmarks-lines")),
                        menuSubItem("Perturbations",tabName = "menuPerturbations", icon = shiny::icon("radiation")),
                        menuSubItem("Advanced",tabName = "menuAdvanced", icon = shiny::icon("star"))),
                menuItem("Run Model",tabName = "menuRun", icon = shiny::icon("person-running")),
                menuItemOutput("menuResults"),
                menuItem("Additional Info",tabName = "menuInfo", icon = shiny::icon("info")),
                actionButton("btnQuit","Quit",icon = shiny::icon("xmark"),class="btn-lg btn-danger")
                )),
  dashboardBody(

    useShinyjs(),

      tags$head(
        tags$style(HTML("
      .box {
        overflow: scroll;
      }"))
      ),

            tabItems(
            ##################################################
            ##  MENU: WELCOME  ###############################
            ##################################################
            tabItem(tabName = "menuWelcome",
                                fluidRow(valueBox("Welcome","Population viability analysis of Cisco in Lake Erie",icon = shiny::icon("face-smile"),width=9)),
                                fluidRow(box(title="About the Model",status="primary",solidHeader=TRUE,width=9,
                                             "This app allows users to run a population viability analysis for Cisco in Lake Erie.",br(),
                                             "The analysis is based on a stochastic Leslie matrix projection model (female-only).",br(),
                                             "The model computes population persistence for user-specified scenarios.")),
                                fluidRow(box(title="Steps",status="primary",solidHeader=TRUE,width=9,
                                             "(1) Review model assumptions",br(),
                                             "(2) Set model dimensions",br(),
                                             "(3) Prepare and enter stocking data",br(),
                                             "(4) Configure Allee effect and population perturbations if desired",br(),
                                             "(5) Run model",br(),
                                             "(6) Examine results"))),


            ##################################################
            ##  MENU: SURVIVAL  ##############################
            ##################################################
            tabItem(tabName = "menuSurvival",
                            fluidRow(valueBox("Survival","deriving age-specific survival",icon = shiny::icon("skull-crossbones"),width=9)),
                            fluidRow(box(title="Early Life Survival",status="primary",solidHeader=TRUE,width=9,
                                         "Early life survival for wild fish is the product of survival at the egg, fry, and age-0 stages.",br(),
                                         "Survival for each of these stages is drawn from a beta distribution.",br(),
                                         "Survival for stocked early life fish is one half the rate for wild fish.",br(),
                                         "All together, early life survival represents survival from the egg stage to age 1.")),
                            fluidRow(column(4,style="width:25%;",
                                        box(title="Egg Survival",status="primary",solidHeader=TRUE,
                                            plotOutput("p.eggS"),width=NULL)),
                                    column(4,style="width:25%",
                                        box(title="Fry Survival",status="primary",solidHeader=TRUE,
                                            plotOutput("p.fryS"),width=NULL)),
                                    column(4,style="width:25%",
                                           box(title="Age-0 Survival",status="primary",solidHeader=TRUE,
                                               plotOutput("p.age0S"),width=NULL))),
                            fluidRow(box(title="Age-1+ Survival",status="primary",solidHeader=TRUE,width=9,
                                         "Survival for wild fish age 1 and older is estimated based on the assumed relationship between natural mortality and weight.",br(),
                                         "This requires parameters characterizing both the age-length and length-weight relationships to derive weight at age.",br(),
                                         "The plots below show the age-length and length-weight curves available to the model.",br(),
                                         "In each simulation, the model randomly selects one age-length curve to compute length at age and one length-weight curve to compute weight at length.",br(),
                                         "The estimated weights at age are used to calculate natural mortality to get estimates of natural mortality at age.",br(),
                                         "Finally, the estimates of natural mortality at age are converted to survival at age.",br(),
                                         "In order to add additional realism to the models, the vector of age-specific survival rates varies within each year of each simulation.",br(),
                                         "This is accomplished by randomly selecting an error (currently, +/- 30%) and adding that value to the entire vector of survival at age for the given year.",br(),
                                         "In their first year in the model, stocked individuals are subject to age-specific survival rates that are one half the rates computed for wild fish.")),
                            fluidRow(box(title="Age-Length Graph",status="primary",solidHeader=TRUE,plotOutput("p.agelen"),width=9)),
                            fluidRow(box(title="Length-Weight Graph",status="primary",solidHeader=TRUE,plotOutput("p.lenwt"),width=9))),


            ##################################################
            ##  MENU: REPRODUCTION  ##########################
            ##################################################
            tabItem(tabName = "menuReproduction",
                            fluidRow(valueBox("Reproduction","maturity and fecundity",icon = shiny::icon("hourglass-half"),width=9)),
                            fluidRow(box(title="Maturity",status="primary",solidHeader=TRUE,width=9,
                                         "The maturity ogive is used in the calculation of fertility at age.",br(),
                                         "The plot below represents the proportion of females mature at each age.",br(),
                                         "The sex ratio is assumed to be 1:1 at all ages.")),
                            fluidRow(box(title="Maturity Graph",status="primary",solidHeader=TRUE,plotOutput("p.maturity"),width=9)),
                            fluidRow(box(title="Fecundity",status="primary",solidHeader=TRUE,width=9,
                                          "Fecundity data are also used in the calculation of fertility at age.",br(),
                                          "The plots below show the fecundity-weight and fecundity-length relationships available to the model.",br(),
                                          "In each simulation, the model randomly selects one of these relationships to derive fecundity at age.",br(),
                                          "Note that stocked fish cannot produce offspring in their first year in the model, even if mature; however,
                                          after this first year, stocked fish reproduce at the same rate as wild fish.")),
                            fluidRow(box(title="Fecundity-Weight Graph",status="primary",solidHeader=TRUE,plotOutput("p.fecundwt"),width=9)),
                            fluidRow(box(title="Fecundity-Length Graph",status="primary",solidHeader=TRUE,plotOutput("p.fecundlen"),width=9))

            ),


            ##################################################
            ##  MENU: DIMENSIONS  ############################
            ##################################################
            tabItem(tabName = "menuDim",
                                fluidRow(valueBox("Dimensions","set size of model",icon = shiny::icon("list-ol"),width=9)),
                                fluidRow(box(title="About Model Dimensions",status="primary",solidHeader=TRUE,width=9,
                                             "The model dimensions control how many years into the future the population is projected and how many populations are simulated.",br(),
                                             "The user is asked to select the dimensions of the model below.",br(),
                                             "Note that the model run time increases with increasing numbers of simulations.")),
                                fluidRow(box(sliderInput("n.projections","Number projection years",value=50,min=0,max=100,step=5),width=9)),
                                fluidRow(box(sliderInput("n.simulations","Number simulations",value=100,min=0,max=10000,step=500),width=9))),


            ##################################################
            ##  MENU: PROPOSED STOCKING  #####################
            ##################################################
            tabItem(tabName = "menuStocking",
                                fluidRow(valueBox("Proposed Stocking","introduction of hatchery fish",icon = shiny::icon("fish"),width=9)),
                                fluidRow(box(title="About Stocking",status="primary",solidHeader=TRUE,width=9,
                                             "The model allows for stocking of multiple ages over multiple years.",br(),
                                             "The user is asked to select the number of years stocking will occur and the number of ages to stock.",br(),
                                             "The user should then fill in the table below with the desired stocking data.",br(),
                                             "To stock eggs, enter -2 for age. Eggs must be stocked in month 12.",br(),
                                             "To stock fry, enter -1 for age. Fry can only be stocked in months 1 through 5.",br(),
                                             "Age-0 fish can only be stocked in months 6 through 12.",br(),
                                             "Fish age 1 and older can be stocked in any month.",br(),br(),
                                             "While this is a female-only model, the user should enter the total number of fish stocked; the model will divide
                                             this number in half to derive the number of stocked females.")),
                                fluidRow(box(sliderInput("n.stockyrs","Number years to stock",value=10,min=1,max=25,step=1),width=9)),
                                fluidRow(box(sliderInput("n.stockage","Number ages to stock",value=1,min=1,max=5,step=1),width=9)),
                                fluidRow(box(title="Enter Data (must be numeric)",status="primary",solidHeader=TRUE,width=9,rHandsontableOutput("table")))),


            ##################################################
            ##  MENU: ALLEE EFFECT  ##########################
            ##################################################
            tabItem(tabName = "menuAllee",
                        fluidRow(valueBox("Allee Effect","set a population threshold",icon = shiny::icon("xmarks-lines"),width=9)),
                        fluidRow(box(title="About the Allee Effect",status="primary",solidHeader=TRUE,width=9,
                                     "In many populations, reproductive success can be reduced at low population densities; this phenomenom is known as the demographic Allee effect.",br(),
                                     "This effect can create a threshold below which the population cannot sustain itself.",br(),
                                     "If an Allee effect is desired, the user selects the stock unit of interest and sets the Allee threshold at a value greater than 0.",br(),
                                     "If used, the model will predict 0 recruits when the size of the spawning stock falls below the user-defined threshold.")),
                        fluidRow(box(title="Stock Areas",status="primary",solidHeader=TRUE,
                                     radioButtons("stock.sel","Select unit stock",stock.area),width=4)),
                        fluidRow(box(title="Population Threshold",status="primary",solidHeader=TRUE,
                                     numericInput("thresh.sel", "Set Allee threshold (number/hectares)",0,min=0,max=5,step=0.01),width=4))
            ),


            ##################################################
            ##  MENU: PERTURBATIONS  #########################
            ##################################################
            tabItem(tabName = "menuPerturbations",
                        fluidRow(valueBox("Perturbations","perturbations to the population",icon = shiny::icon("radiation"),width=9)),
                        fluidRow(box(title="About Commercial Fishing",status="primary",solidHeader=TRUE,width=9,
                                     "The user may choose to apply a commercial fishery to the population.",br(),
                                     "If applied, the user provides the instantaneouse F rate and first age at full selection.")),
                        fluidRow(column(6,style="width:30%;",
                                    box(title="Commercial Fishing",status="primary",solidHeader=TRUE,width=NULL,
                                        radioButtons("fishery.sel","Apply commercial fishery",threat.apply),
                                        numericInput("F.sel","Instantaneous F",0.1,min=0,max=2,step=0.01),                                        
                                        numericInput("Fage.sel","First age at full selection",3,min=1,max=15,step=1))),
                                column(6,style="width:45%",
                                    box(title="Commercial Fishery Selectivity",status="primary",solidHeader=TRUE,
                                        plotOutput("p.select"),width=NULL))),
                        fluidRow(box(title="About Recruitment Booms",status="primary",solidHeader=TRUE,width=9,
                                    "The user may choose to apply intermittent years of strong year classes, or recruitment booms.",br(),
                                    "If used, the user supplies the frequency of boom years and the percent increase in fry survival, which
                                    is how the recruitment booms are defined.")),
                        fluidRow(column(6,style="width:30%;",
                                        box(title="Recruitment Dynamics",status="primary",solidHeader=TRUE,width=NULL,
                                            radioButtons("boom.sel","Apply boom years",threat.apply),
                                            numericInput("YC.period","Boom year frequency (every n years)",4,min=1,max=20,step=1),
                                            numericInput("pct.boom","Percent increase fry survival",600,min=150,max=1000,step=50)
                                            )))
            ),

            
            ##################################################
            ##  MENU: ADVANCED  ##############################
            ##################################################            
            tabItem(tabName = "menuAdvanced",
                    fluidRow(valueBox("Advanced","custom options",icon = shiny::icon("star"),width=9)),
                    fluidRow(box(title="Caution",status="primary",solidHeader=TRUE,width=9,
                                 "Use extreme caution when applying custom options.",br(),
                                 "Only advanced users should apply these options.")),
                    fluidRow(column(4,style="width:25%;",
                                    box(title="Egg Survival",status="primary",solidHeader=TRUE,width=NULL,
                                        radioButtons("adv.eggS.sel","Egg survival options",opts.adv),
                                        numericInput("eggS.mu.custom","Mean (default = 0.00276)",0.00276,min=0,max=0.5,step=0.0001),
                                        numericInput("eggS.var.custom","Variance (default = 0.00000527)",0.00000527,min=0,max=0.5,step=0.00000001))),
                             column(4,style="width:25%",
                                    box(title="Fry Survival",status="primary",solidHeader=TRUE,width=NULL,
                                        radioButtons("adv.fryS.sel","Fry survival options",opts.adv),
                                        numericInput("fryS.mu.custom","Mean (default = 0.0611)",0.0611,min=0,max=0.5,step=0.0001),
                                        numericInput("fryS.var.custom","Variance (default = 0.000149)",0.000149,min=0,max=0.5,step=0.000001))),
                             column(4,style="width:25%",
                                    box(title="Age-0 Survival",status="primary",solidHeader=TRUE,width=NULL,
                                        radioButtons("adv.age0S.sel","Age-0 survival options",opts.adv),
                                        numericInput("age0S.mu.custom","Mean (default = 0.0556)",0.0556,min=0,max=0.5,step=0.0001),
                                        numericInput("age0S.var.custom","Variance (default = 0.000124)",0.000124,min=0,max=0.5,step=0.000001)))),
                    fluidRow(box(title="Age-1+ Survival",status="primary",solidHeader=TRUE,width=4,
                                 radioButtons("adv.adultS.sel","Age-1+ survival options",opts.adv))),
                    fluidRow(box(title="Custom Age-1+ Survival",status="primary",solidHeader=TRUE,width=9,rHandsontableOutput("hot")))
                    
            ),
            

            ##################################################
            ##  MENU: RUN MODEL  #############################
            ##################################################
            tabItem(tabName = "menuRun",
                            fluidRow(valueBox("Run Model","review and run",icon = shiny::icon("person-running"),width=9)),
                            fluidRow(box(title="About Running the Model",status="primary",solidHeader=TRUE,width=9,
                                         "Before running the model, please review input stocking data and other settings.",br(),
                                         "If a change is needed, return to the associated menu to make the change.",br(),
                                         "Once the user is satisfied with all inputs, click the button to run the model.")),
                            fluidRow(box(title="Stocking Data",status="primary",solidHeader=TRUE,width=9,rHandsontableOutput("review.stock"))),
                            fluidRow(box(title="Review Inputs",status="primary",solidHeader=TRUE,tableOutput('review.input'),width=4),
                                            actionButton("btnRunModel","Run Model",icon = shiny::icon("person-running"),class="btn-lg btn-primary"))),


            ##################################################
            ##  MENU: RESULTS  ###############################
            ##################################################
            tabItem(tabName = "menuResults",
                    fluidRow(valueBox("Results","output",icon = shiny::icon("magnifying-glass-chart"),width=9)),
                    fluidRow(box(title="Model Summary",status="primary",solidHeader=TRUE,
                                 textOutput("out.summary1"),textOutput("out.summary5"),textOutput("out.summary2"),
                                 textOutput("out.summary3"),textOutput("out.summary4"),textOutput("out.summary6"),
                                 textOutput("out.summary8"),width=9)),
                    fluidRow(column(6,style="width:45%;",
                                    box(title="Distribution for Post-Stocking Lambda (wild fish)",status="primary",solidHeader=TRUE,
                                        withSpinner(plotOutput("p.lambda")),width=NULL)),
                             column(6,style="width:30%",
                                    box(title="Statistics for Post-Stocking Lambda (wild fish)",status="primary",solidHeader=TRUE,
                                        withSpinner(tableOutput("tbl.lambda")),width=NULL))),
                    fluidRow(column(6,style="width:45%;",
                                    box(title="Median Population Size (females)",status="primary",solidHeader=TRUE,
                                        withSpinner(plotOutput("p.popmedian")),width=NULL)),
                             column(6,style="width:30%",
                                    box(title="Statistics for Terminal Year Population (females)",status="primary",solidHeader=TRUE,
                                        withSpinner(tableOutput("tbl.end")),width=NULL))),
                    fluidRow(column(6,style="width:45%;",
                                    box(title="Median Mature Population Size (females)",status="primary",solidHeader=TRUE,
                                        withSpinner(plotOutput("p.matmedian")),width=NULL)),
                             column(6,style="width:30%",
                                    box(title="Statistics for Terminal Year Mature Population (females)",status="primary",solidHeader=TRUE,
                                        withSpinner(tableOutput("tbl.endmat")),width=NULL))),
                    fluidRow(column(6,style="width:45%;",
                                    box(title="Median Mature Population Size by Origin (females)",status="primary",solidHeader=TRUE,
                                        withSpinner(plotOutput("p.matorigin")),width=NULL)),
                             column(6,style="width:30%",
                                    box(title="Statistics for Terminal Year Mature Population by Origin (females)",status="primary",solidHeader=TRUE,
                                        withSpinner(tableOutput("tbl.endmat.orig")),width=NULL)))
            ),
            

            ##################################################
            ##  MENU: ADDITIONAL INFO  #######################
            ##################################################
            tabItem(tabName = "menuInfo",
                            fluidRow(valueBox("Additional Info","Check here for latest update information",icon = shiny::icon("info"),width=9)),
                            fluidRow("Last update: 5 February 2025"),
                            fluidRow("Issue tracking: laura_lee@fws.gov"))

            )
  )
)


##########################################################################################################

server <- function(input, output, session) {


  ##################################################
  ##  REVIEW INPUT  ################################
  ##################################################
  output$review.input <- renderTable({
    df.input <- data.frame(matrix(ncol=1, nrow=10))
       colnames(df.input) <- c("Value")
    rownames(df.input) <- c("N projection years","N simulations","Unit stock","Allee threshold",
                            "Commercial fishing","Boom dynamics","Egg survival","Fry survival",
                            "Age-0 survival","Age-1+ survival")
    df.input[1,1] <- input$n.projections
    df.input[2,1] <- input$n.simulations
    df.input[3,1] <- input$stock.sel
    df.input[4,1] <- input$thresh.sel
    df.input[5,1] <- input$fishery.sel
    df.input[6,1] <- input$boom.sel
    df.input[7,1] <- input$adv.eggS.sel
    df.input[8,1] <- input$adv.fryS.sel
    df.input[9,1] <- input$adv.age0S.sel
    df.input[10,1] <- input$adv.adultS.sel
      return(df.input)
  },rownames = TRUE)
  ##################################################


  ##################################################
  ##  PROPOSED STOCKING INPUT  #####################
  ##################################################
  values <- reactiveValues(data = as.data.frame(matrix(NA, nrow = 1, ncol = 7)))

  # Observe changes in num_cols or num_rows to adjust the data structure
  observeEvent(list(input$n.stockyrs, input$n.stockage), {

    n_cols <- input$n.stockyrs
    n_rows <- input$n.stockage

    # Create or adjust the data frame to have the correct number of rows and columns
    new_data <- as.data.frame(matrix(NA, nrow = n_rows, ncol = n_cols+2))
    yrnum <- sprintf("yr%d",seq(1:n_cols))
    colnames(new_data) <- c("age","month",yrnum)

    # Copy existing data into the new data frame, if it exists
    if (!is.null(values$data)) {
      common_cols <- min(ncol(values$data), n_cols)
      common_rows <- min(nrow(values$data), n_rows)
      new_data[1:common_rows, 1:common_cols] <- values$data[1:common_rows, 1:common_cols]
    }

    # Update the reactive data with the new structure
    values$data <- new_data

  })

  # Render the handsontable
  output$table <- renderRHandsontable({

    # Render the table, ensuring all columns are numeric
    rhandsontable(values$data) %>%
      hot_col(col = seq_len(ncol(values$data)), type = "numeric")  # Set all columns to numeric type

  })

  # Update the reactive data when the user edits the table
  observe({
    if (!is.null(input$table)) {
      values$data <- hot_to_r(input$table)  # Save the table data to the reactive value
    }
  })

  # Display the results in another output
  output$review.stock <- renderRHandsontable({
    rhandsontable(values$data,readOnly=TRUE)
  })
  ##################################################


  ##################################################
  ##  PLOT EGG SURVIVAL  ###########################
  ##################################################
  output$p.eggS <- renderPlot({

    # Generate distribution
    dist.eggS <- rbeta(10000,1.438715,519.835)
    dist.eggS <- as.data.frame(dist.eggS)
    colnames(dist.eggS) <- c("eggS")

    ggplot(data = dist.eggS,aes(x = eggS)) +
      geom_histogram(color="lightskyblue4", fill="lightskyblue2", bins = 30) +
      labs(x="egg survival",y="frequency") +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))

  })
  ##################################################


  ##################################################
  ##  PLOT FRY SURVIVAL  ###########################
  ##################################################
  output$p.fryS <- renderPlot({

    # Generate distribution
    dist.fryS <- rbeta(10000,23.46313,360.5489)
    dist.fryS <- as.data.frame(dist.fryS)
    colnames(dist.fryS) <- c("fryS")

    ggplot(data = dist.fryS,aes(x = fryS)) +
      geom_histogram(color="darkseagreen4", fill="darkseagreen2", bins = 30) +
      labs(x="fry survival",y="frequency") +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))

  })
  ##################################################


  ##################################################
  ##  PLOT AGE-0 SURVIVAL  #########################
  ##################################################
  output$p.age0S <- renderPlot({

    # Generate distribution
    dist.age0S <- rbeta(10000,23.46313,360.5489)
    dist.age0S <- as.data.frame(dist.age0S)
    colnames(dist.age0S) <- c("age0S")

    ggplot(data = dist.age0S,aes(x = age0S)) +
      geom_histogram(color="lightgoldenrod4", fill="lightgoldenrod2", bins = 30) +
      labs(x="age-0 survival",y="frequency") +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))

  })
  ##################################################


  ##################################################
  ##  PLOT AGE-LENGTH  #############################
  ##################################################
  output$p.agelen <- renderPlot({
    ggplot(data = agelen.cisco, aes(x=age, y = length, group=water)) +
        geom_line(linewidth=1.25,aes(color=water)) +
        labs(x="Age",
                 y="Length (mm)") +
        theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))
  })
  ##################################################


  ##################################################
  ##  PLOT LENGTH-WEIGHT  ##########################
  ##################################################
  output$p.lenwt <- renderPlot({
    ggplot(data = lw.cisco, aes(x=length, y = weight, group=water)) +
        geom_line(linewidth=1.25,aes(color=water)) +
        labs(x="Length (mm)",
                 y="Weight (g)") +
        theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))
  })
  ##################################################


  ##################################################
  ##  PLOT MATURITY  ###############################
  ##################################################
  output$p.maturity <- renderPlot({
    ggplot(data = mat.cisco, aes(x=age, y = maturity)) +
        geom_line(color="blue",linewidth=1.25) +
        labs(x="Age",
                 y="Proportion Mature") +
        theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))
  })
  ##################################################


  ##################################################
  ##  PLOT FECUNDITY-WEIGHT  #######################
  ##################################################
  output$p.fecundwt <- renderPlot({
    ggplot(data = fwt.cisco, aes(x=weight, y=fecundity, group=water)) +
        geom_line(linewidth=1.25,aes(color=water)) +
        labs(x="Weight (g)",
                 y="Number of Eggs") +
        scale_color_manual(values=c("green3","darkblue")) +
        theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))

  })
  ##################################################


  ##################################################
  ##  PLOT FECUNDITY-LENGTH  #######################
  ##################################################
  output$p.fecundlen <- renderPlot({
    ggplot(data = flen.cisco, aes(x=length, y=fecundity, group=water)) +
        geom_line(linewidth=1.25,aes(color=water)) +
        labs(x="Length (mm)",
                 y="Number of Eggs") +
        scale_color_manual(values=c("darkgreen","mediumpurple3")) +
        theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))

  })
  ##################################################
  
  
  ##################################################
  ##  ENABLE/DISABLE COMM FISHERY OPTIONS  #########
  ##################################################
  observeEvent(input$fishery.sel, {
    if (input$fishery.sel == "No") {
      shinyjs::disable(id = "F.sel")
      shinyjs::disable(id = "Fage.sel")
    } else {
      shinyjs::enable(id = "F.sel")
      shinyjs::enable(id = "Fage.sel")
    }
  })
  ##################################################  


  ##################################################
  ##  PLOT COMMERCIAL FISHERY SELECTIVITY  #########
  ##################################################
  output$p.select <- renderPlot({

    age.max <- 15
    age <- 1:age.max

    sel.not <- as.data.frame(rep(0,input$Fage.sel-1))
    colnames(sel.not) <- c('Selectivity')
    sel.full <- as.data.frame(rep(1,15-input$Fage.sel+1))
    colnames(sel.full) <- c('Selectivity')

    select.gear <- rbind(sel.not,sel.full)
    select.gear <- cbind(age,select.gear)

    ggplot(data = select.gear,aes(x=age,y=Selectivity)) +
      geom_line(linewidth=1.25) +
      labs(x="Age",
           y="Selectivity") +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))
  })
  ##################################################
  
  
  ##################################################
  ##  ENABLE/DISABLE BOOM YEAR-CLASS OPTIONS  ######
  ##################################################
  observeEvent(input$boom.sel, {
    if (input$boom.sel == "No") {
      shinyjs::disable(id = "YC.period")
      shinyjs::disable(id = "pct.boom")
    } else {
      shinyjs::enable(id = "YC.period")
      shinyjs::enable(id = "pct.boom")
    }
  })  ##################################################
  
  
  ##################################################
  ##  ENABLE/DISABLE ADVANCED OPTIONS  #############
  ################################################## 
  observeEvent(input$adv.eggS.sel, {
    if (input$adv.eggS.sel == "Default") {
      shinyjs::disable(id = "eggS.mu.custom")
      shinyjs::disable(id = "eggS.var.custom")
    } else {
      shinyjs::enable(id = "eggS.mu.custom")
      shinyjs::enable(id = "eggS.var.custom")
    }
  })
  
  observeEvent(input$adv.fryS.sel, {
    if (input$adv.fryS.sel == "Default") {
      shinyjs::disable(id = "fryS.mu.custom")
      shinyjs::disable(id = "fryS.var.custom")
    } else {
      shinyjs::enable(id = "fryS.mu.custom")
      shinyjs::enable(id = "fryS.var.custom")
    }
  })
  
  observeEvent(input$adv.age0S.sel, {
    if (input$adv.age0S.sel == "Default") {
      shinyjs::disable(id = "age0S.mu.custom")
      shinyjs::disable(id = "age0S.var.custom")
    } else {
      shinyjs::enable(id = "age0S.mu.custom")
      shinyjs::enable(id = "age0S.var.custom")
    }
  })  
  
  ##################################################

  
  ##################################################
  ##  CUSTOM AGE-1+ SURVIVAL INPUT  ################
  ################################################## 
  # Create a reactive value to hold the data
  customS <- reactiveValues(data = NULL)
  
  # Initialize the table with sequential column names and no initial data
  observe({
    if (is.null(customS$data)) {
      agenum <- sprintf("age%d",seq(1:15))
      # Initialize an empty data frame with sequential column names
      customS$data <- setNames(data.frame(matrix(ncol = 15, nrow = 1)), agenum)
    }
  })
  
  observe({
    if (!is.null(input$hot))
      customS$data <- hot_to_r(input$hot)
  })
  
  # Render the rhandsontable
  output$hot <- renderRHandsontable({
    if (!is.null(customS$data)) {
      rhandsontable(customS$data, readOnly = (input$adv.adultS.sel == "Default")) %>%
        hot_col(col = seq_len(ncol(customS$data)), type = "numeric")  # Ensure numeric columns
    }
  })
  
  # Enable or disable the table based on selection
  observeEvent(input$adv.adultS.sel, {
    if (input$adv.adultS.sel == "Default") {
      shinyjs::disable("hot")
    } else {
      shinyjs::enable("hot")
    }
  }) 
  ##################################################  


  ##################################################
  ##  RUN MODEL  ###################################
  ##################################################
  pva <- eventReactive(input$btnRunModel, {

    validate(
      need(values$data != "", label = "stocking data")
    )

    n.projections <- input$n.projections
    n.simulations <- input$n.simulations
    age.max <- 15
    age <- 1:age.max
    sexratio <- 0.5

    stocking.dat <- values$data
    stocking.pro <- as.data.frame(stocking.dat)
    n.stockpro <- ncol(stocking.pro) - 2
    n.stockage <- input$n.stockage

   validate(
      need(n.projections >= n.stockpro, "n projection years must exceed n stocking years")
     )
   
   if (any(stocking.pro$age < -3)) {
     validate("stocking age(s) must fall within -2 to 15")
   }
   if (any(stocking.pro$age > age.max)) {
     validate("stocking age(s) must fall within -2 to 15")
   }
   
   if (any(stocking.pro$month < 1)) {
     validate("stocking month(s) must fall within 1 to 12")
   }
   if (any(stocking.pro$month > 12)) {
     validate("stocking month(s) must fall within 1 to 12")
   }   
   
    if (any(stocking.pro$age == -2)) {
      check1 <- subset(stocking.pro,stocking.pro$age == -2)
      if (any(check1$month != 12)) {
        validate("eggs must be stocked in month 12")
      }
    }
   
   if (any(stocking.pro$age == -1)) {
     check2 <- subset(stocking.pro,stocking.pro$age == -1)
     if (any(check2$month > 6)) {
       validate("fry can only be stocked in months 1 through 5")
     }
   }
   
   if (any(stocking.pro$age == 0)) {
     check3 <- subset(stocking.pro,stocking.pro$age == 0)
     if (any(check3$month < 6)) {
       validate("age 0 can only be stocked in months 6 through 12")
     }
   }   
   

    stock.unit <- input$stock.sel
    area.lake <- switch(stock.unit,"Lakewide" = 2597665,"Eastern Unit" = 1411713,"Western Unit" = 1185952)
    thresh.allee <- input$thresh.sel

    fishery.use <- input$fishery.sel
    F.rate <- input$F.sel

    sel.not <- rep(0,input$Fage.sel-1)
    sel.full <- rep(1,age.max-input$Fage.sel+1)
    select.gear <- c(sel.not,sel.full)
    F.partial <- F.rate * select.gear
    
    boom.use <- input$boom.sel
    int.boom <- input$YC.period
    boom.inc <- input$pct.boom
    
    if (input$adv.eggS.sel == "Custom") {
      earlyS[1,2] = input$eggS.mu.custom
      earlyS[1,3] = input$eggS.var.custom
    }
    
    if (input$adv.fryS.sel == "Custom") {
      earlyS[2,2] = input$fryS.mu.custom
      earlyS[2,3] = input$fryS.var.custom
    }    
    
    if (input$adv.age0S.sel == "Custom") {
      earlyS[3,2] = input$age0S.mu.custom
      earlyS[3,3] = input$age0S.var.custom
    }    
  
    customS.use <- input$adv.adultS.sel
    S.custom <- customS$data

    # generate life history parameters
    message("generating life history parameters...")
    lh <- lifehistory.sims(n.simulations,age,vonB,n.vonB,lw,n.lw,fecund,n.fecund,earlyS)
    #print(lh)  # Debugging to check the output
   # if (is.null(lh$sim.bio) || nrow(lh$sim.bio) == 0) {
   #     stop("Simulation result is empty or NULL.")
   # }
    message("...life history parameters done")


    # run simulations & population projections
    message("running simulations and population projections...")
      withProgress(message="Running simulations", value = 0,{
      simpro <- sims.run(n.projections,n.simulations,age,sexratio,mat.cisco$maturity,
                         lh$sim.bio,lh$sim.natmort,fishery.use,F.partial,
                         customS.use,S.custom,lh$sim.weight,lh$sim.fecundity,
                         lh$sim.eggS,lh$sim.fryS,lh$sim.age0S,stocking.pro,
                         area.lake,thresh.allee,boom.use,int.boom,boom.inc)
      message("...simulations and population projections done")
    })


    # compute summary statistics
    message("computing summary statistics...")
    calcs <- stats.calc(n.projections,n.simulations,n.stockpro,simpro$lambda.wild,simpro$changes.all,simpro$sim.pop,simpro$sim.mature.pop,
                    simpro$sim.hatchery,simpro$sim.other,simpro$sim.mature.hatchery,simpro$sim.mature.other,simpro$sim.popwt)
    message("...done computing summary statistics")

    return(list(lh=lh,simpro=simpro,calcs=calcs))

  })
  ##################################################


#  disable Run button
#  observeEvent(input$btnRunModel, {
#    shinyjs::disable("btnRunModel")
#  })


  ##################################################
  ##  SHOW RESULTS MENU ############################
  ##################################################
  output$menuResults <- renderMenu({

    if(input$btnRunModel != 0)
      menuItem("Results", tabName = "menuResults", icon = icon("magnifying-glass-chart")
      )

  })
  ##################################################


  ##################################################
  ##  OPEN RESULTS MENU  ###########################
  ##################################################
  output$loading_message <- renderText("")

  observeEvent(input$btnRunModel, {
    updateTabItems(session, "tabs", "menuResults")
  })
  ##################################################


  ##################################################
  ##  MODEL SUMMARY TEXT  ##########################
  ##################################################
  output$out.summary1 <- renderText({
    paste("For each of",input$n.simulations,"simulations, the", input$stock.sel,"population was projected forward",input$n.projections,"years.")
  })
  
  output$out.summary2 <- renderText({
    if (input$thresh.sel > 0) {
      paste("An Allee effect was assumed with a threshold of", input$thresh.sel, "number/hectare.")
    } else {
      paste("No Allee effect was assumed.")
    }
  })
  
  output$out.summary3 <- renderText({
    if (input$fishery.sel == "Yes") {
      paste("A commercial fishery was applied with a F rate of", input$F.sel, "and full selection at age", input$Fage.sel, ".")
    } else {
      paste("No commercial fishery applied.")
    }
  })
  
  output$out.summary4 <- renderText({
    if (input$boom.sel == "Yes") {
      paste("Boom dynamics were applied by increasing fry survival",input$pct.boom,"% every",input$YC.period,"years, on average.")
    } else {
      paste("Boom dynamics were not applied.")
    }
  })  
  
  output$out.summary5 <- renderText({
    paste("Proposed stocking occured from years 1 to",ncol(values$data)-2,".")
  })
  
  output$out.summary6 <- renderText({
    e <- pva()[[3]]$extinct
    p.e <- pva()[[3]]$p.extinct
    paste("A total of",e,"simulations resulted in extinction (<1 fish in final year); this equates to an extinction
          probability of",p.e,".")
  })
  
  #  output$out.summary7 <- renderText({
  #    p.e <- pva()[[3]]$p.extinct
  #    paste("This equates to an extinction probability of",p.e,".")
  #  })
  
  output$out.summary8 <- renderText({
    pct.exceed <- pva()[[3]]$percent.exceed
    paste("A total of",pct.exceed,"% of the simulations resulted in a lambda greater than or equal to 1.0.")
  })
  ##################################################


  ##################################################
  ##  PLOT WILD LAMBDA POST-STOCKING  ##############
  ##################################################
  output$p.lambda <- renderPlot({

    # Store the reactive output in a variable
    sim_lambda <- as.data.frame(pva()[[3]]$lambdasim.ps)
    colnames(sim_lambda) <- c("lambda")

    ggplot(data = sim_lambda,aes(x = lambda)) +
      geom_histogram(color="darkolivegreen4", fill="darkolivegreen3", bins = 30) +
      labs(x="post-stocking lambda (wild fish)",y="frequency") +
      geom_vline(xintercept=1,linewidth=1) +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))

  })
  ##################################################


  ##################################################
  ##  PLOT MEDIAN POPN SIZE OVER TIME ##############
  ##################################################
  output$p.popmedian <- renderPlot({

    # Store the reactive output in a variable
    sim_popquantile <- as.data.frame(pva()[[3]]$tpop.quantile)

    ggplot(data = sim_popquantile) +
      geom_ribbon(aes(x=Year, ymin=P25/1000, ymax=P75/1000), fill="lightblue1", alpha=0.5) +
      geom_line(aes(x=Year, y=Median/1000), color="cornflowerblue", linewidth=1) +
      labs(x="Year",y="000s of fish") +
      scale_x_continuous(breaks=seq(1,input$n.projections,5)) +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))

  })
  ##################################################


  ##################################################
  ##  PLOT MEDIAN MATURE POPN SIZE OVER TIME #######
  ##################################################
  output$p.matmedian <- renderPlot({

    # Store the reactive output in a variable
    sim_matquantile <- as.data.frame(pva()[[3]]$tmature.quantile)

    ggplot(data = sim_matquantile) +
      geom_ribbon(aes(x=Year, ymin=P25/1000, ymax=P75/1000), fill="rosybrown2", alpha=0.5) +
      geom_line(aes(x=Year, y=Median/1000), color="rosybrown4", linewidth=1) +
      labs(x="Year",y="000s of fish") +
      scale_x_continuous(breaks=seq(1,input$n.projections,5)) +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))

  })
  ##################################################


  ##################################################
  ##  PLOT MEDIAN MATURE POPN SIZE BY ORIGIN #######
  ##################################################
  output$p.matorigin <- renderPlot({

    # Store the reactive output in a variable
    sim_matorigquantile <- as.data.frame(pva()[[3]]$mature.origin.quantile)

    ggplot(data = sim_matorigquantile, aes(x=Year,y=Median/1000,group=Origin,color=Origin)) +
      geom_line() +
      labs(x="Year",y="000s of fish") +
      scale_x_continuous(breaks=seq(1,input$n.projections,5)) +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold",size = 12, hjust= 0.5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white", color="grey"),
            axis.line = element_line(colour = "black"))

  })
  ##################################################


  ##################################################
  ##  TABLES FOR POPN PARAMETERS ###################
  ##################################################
  # wild lambda post-stocking
  output$tbl.lambda <- renderTable({
    stats_lambda <- as.data.frame(pva()[[3]]$tlambda.stats)
    stats_lambda
  })

  # terminal year population size
  output$tbl.end <- renderTable({
    stats_end <- as.data.frame(pva()[[3]]$tend.stats)
    stats_end
  })

  # terminal year mature population size
  output$tbl.endmat <- renderTable({
    stats_endmat <- as.data.frame(pva()[[3]]$tendmat.stats)
    stats_endmat
  })

  # terminal year mature population size by origin
  output$tbl.endmat.orig <- renderTable({
    stats_endmat.orig <- as.data.frame(pva()[[3]]$end.origin.stats)
    stats_endmat.orig
  })
  ##################################################


  ##################################################
  ##  QUIT APP  ####################################
  ##################################################
  observeEvent(input$btnQuit, {
    stopApp()
  })
  ##################################################

}

shinyApp(ui, server)


