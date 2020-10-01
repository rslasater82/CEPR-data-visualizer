#Version Control: Version 3.0 Distributed Version
#In this version:
#Moved data manipulation outside of file
#Title changed to CEPR Visualization App
#MAD removed
#Added log transormation
#troubleshooing boxplots
#Variable: incp_wag, Sex, educ92, occly2_03, age converted to: Earnings, Sex, Education, Occupation, Age, Race
#Plots: histograms, scatterplots, boxplots, line, stacked bar
#Tables: Histograms, Scatterplots
#Separable by gender? Yes
#Average values: Mean and Median
#Seperable by career? Yes
#Separable by marriage status? Yes
#Seperable by race? Yes
#Can be filtered by unemployment? Yes
#Education binned to levels
#Tables for:
#Factor Histograms: Yes
#Continuous Histograms: Yes
#Scatterplots: Yes
#Boxplots: Yes
#Line: Not sure what would be applicable?

#Remove comment when running locally, needs to be on when publishing.
library(shiny)
library(tidyverse)
library(haven)
library(broom)


#load in data
CEPRdata <- readRDS(file = "CEPR_reduced.rds")

#set factor variables to remove errors when plotting

error = paste("\nYou have made an invalid selection.\n",
              "Try selecting a different plot type \n",
              "or deselect mean or median ")

#Create a vector with variable names in it to reduce coding
keyvariables <- c('Earnings', 'Sex', 'Education', 'Occupation', 'Age', 'MaritalStatus', 'Race', 'FamilySize', 'FamilyMakeup')

#Create a vector for career fields
careers <- c('All Careers'=0,
             '1: Executive level management' = 1, 
             '2: Human Resources, Transportation, Construction, and Agricultural management' = 2,
             '3: Education, Engineering, Gaming, Medical, Real Estate, Funeral, Postal, and Natural Sciences management' = 3,
             '4: Talent and Sports Agents' = 4, 
             '5: Business Consultants' = 5, 
             '6: Accountants' = 6, 
             '7: Financial Specialists' = 7, 
             '8: Computer/IT' = 8, 
             '9: Statisticians' = 9, 
             '10: Architects'=10, 
             '11: Surveyors'=11, 
             '12: Engineers'=12, 
             '13: Physical Sciences'=13, 
             '14: Economists'=14, 
             '15: Psychologists, Sociologists, and Urban Planners'=15, 
             '16: Scientific Technicians'=16, 
             '17: Social Services'=17, 
             '18: Lawyers, Judges, and Magistrates' =18, 
             '19: Paralegals and legal staff'=19, 
             '20: Postsecondary Teachers'=20, 
             '21: Preschool through 12th Grade Teachers'=21, 
             '22: Historians'=22, 
             '23: Arts, Design, Sports and Media'=23,
             '24: Medical Doctors and related specialties'=24,
             '25: Nurses and medical specialists'=25,
             '26: Veterinarians'=26,
             '27: Health Technicians'=27,
             '28: Therapists, dental assistants, and other medical fields'=28,
             '29: Public safety, police, fire, supervisory'=29,
             '30: Public safety, police, fire, non-supervisory' =30,
             '31: Animal control, security guard, lifeguard and other non-uniformed public safety'=31,
             '32: Chefs and head cooks'=32,
             '33: Food service workers and bartenders' =33,
             '34: Housekeeping, lawn service, pest control, supervisory'=34,
             '35: Housekeeping, lawn service, pest control, non-supervisory'=35,
             '36: Supervisors of gaming and personal services'=36,
             '37: Personal care and services workers'=37,
             '38: Retail and non retail sales, supervisory'=38,
             '39: Retail and non retail sales, non-supervisory'=39,
             '40: Office and administrative support'=40,
             '41: Farming, fishing & forestry'=41,
             '42: Construction and masonry, supervisory'=42,
             '43: Carpenters'=43,
             '44: Construction and masonry, non-supervisory'=44,
             '45: Electricians'=45,
             '46: Skilled construction specialties'=46,
             '47: Extraction workers'=47,
             '48: Installation, maintenance and repair'=48,
             '49: Production occupations'=49,
             '50: Transportation supervisors, Pilots, Air Traffic Controllers and airfield operations'=50,
             '51: Transportation, non-supervisory'=51,
             '52: Armed Forces'=52,
             '53: Never Worked'=53)
  

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("CEPR Visualization"),

    # Set Sidebar 
 sidebarPanel(   
    
    #First Input, to select response variable for plots
    selectInput('y', 
                'Select your response (y) variable', 
                keyvariables),
    
    #Second input, select explanatory variable
    selectInput('x', 
                'Select your explanatory (x) variable', 
                keyvariables),
    
    #Change plot type, currently just histogram(or bar if discrete), scatterplot, boxplot, and line(useless on this tab)
    selectInput('plot_type', 'Select Plot Type',  c('Histogram (x variable)', 
                                                    'Scatterplot (x and y variables)', 
                                                    'Boxplot (x and y variables)', 
                                                    'Line Plot (x and y variables)',
                                                    'Stacked Bar (x and y variable)'
    )),
    #Checkbox to subset by sex
    radioButtons('color', 
                       'Separate by:', 
                       choiceNames = c("No separation",
                          "Sex",
                         "Marital Status",
                         "Race"),
                 choiceValues = c('NULL',
                          'Sex',
                          'MaritalStatus',
                          'Race')
                      ),
    
    #Checkbox to use median wages
   
    radioButtons('scaled', 
                       'Modify y variable?', 
                       choiceNames = c("No",
                                       "Median",
                                       "Mean"),
                      choiceValues= c('FALSE',
                                      'median',
                                      'mean'
                                      ), 
                 inline = TRUE),
    #Checkbox Buttons to deselect unemployed
    radioButtons('unemployed', 'Remove non-earners?', 
                 choiceNames = c("None", "Unemployed", 
                                 "Never Worked",
                                 "Minors",
                                 "All"), 
                 choiceValues = c("FALSE" , 
                                  "empl == 1",
                                  "Occupation != 53",
                                  "Age >= 18",
                                  "empl == 1 & Occupation != 53"),
                 inline=T),
    
    #Allow for selection of career fields
    selectInput('career', "Select a specific career field", careers),
    

    
    #Slider filters max wage in dataset, meant to allow for "zoom" on the plots
    sliderInput('filtermaxwage',
                    'Filter max wage?',
                    min = 0, max = 1000000, value = 1000000),
    
    #Select to use log wages
    checkboxInput('log','Use log wages?',value = FALSE),
    
    shiny::conditionalPanel(
      condition = "input.plot_type == 'Scatterplot (x and y variables)' & input.x == 'Age'",
      checkboxInput('agesq', "Add age squared?", value = FALSE)
    )
    
 ),
 
 #Set Main Panel, will only be used for plot output
 mainPanel(
            plotOutput('draw_plots'),
            tableOutput('dynamicTable')
                 
              ),
 tags$div(class="header", checked=NA,
                    tags$p("Data: Center for Economic and Policy Research. 2019. March CPS Uniform Extracts, Version 1.1. Washington, DC"),
                    tags$a(href="http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/", "Retrieved from ceprdata.org"))
)
    
# Define server logic required to draw a plots
server <- function(input, output, session) {
  
  
  

  #mediany <- reactiveValues(data = NULL)
  
    #function to set the plots to the output
    output$draw_plots <- renderPlot({
      
      if(input$career != 0 & input$unemployed == FALSE){ #broken
      CEPRdata <- CEPRdata %>%
        filter(Occupation == input$career)
      }
      else if(input$career == 0 & input$unemployed != FALSE){
        CEPRdata <- CEPRdata %>%
          filter_(input$unemployed)
        }
      else if(input$career != 0 & input$unemployed != FALSE){
        CEPRdata <- CEPRdata %>%
          filter(Occupation == input$career) %>%
          filter_(input$unemployed)
      }
      CEPRdata <- CEPRdata %>%
        filter(Earnings <= input$filtermaxwage)
      
      if(input$log == TRUE){
        CEPRdata <- CEPRdata %>%
          mutate(Earnings = log(Earnings + 1))
      }
      
      
    #Control sequence to set the plot type based off inputted plot type and allow for filtering    
    plot_selected <- 
    
      
      if(input$plot_type == 'Scatterplot (x and y variables)'){  
        CEPRdata %>% 
        
        #filter allows for the filtering of max wage based off slider value
        #filter(Earnings <= input$filtermaxwage) %>%
        
        
        #creates the plot, x and y are set by inputs, color subsets by gender
        ggplot(aes_string(x = input$x, 
                          y = input$y, 
                         color = input$color)) + 
                          geom_point() +
                          geom_smooth(method=lm)
      }
        #boxplot        
        else if(input$plot_type == 'Boxplot (x and y variables)' & input$scaled == FALSE){
               plot_selected <- CEPRdata %>% 
                   #filter(Earnings <= input$filtermaxwage) %>%
                   ggplot(aes_string(x = input$x, 
                                     y = input$y, 
                                 color = input$color)) +  
                    geom_boxplot() 
        }
        #Line plot
        else if(input$plot_type == 'Line Plot (x and y variables)'){ 
           plot_selected <- CEPRdata %>% 
               #filter(Earnings <= input$filtermaxwage) %>%
               ggplot(aes_string(x = input$x, 
                                 y = input$y, 
                             color = input$color)) +  
                    geom_line()
            }
        #Histograms, has two controls first will set the plots to a bar type for factor variables    
        else if(input$plot_type == 'Histogram (x variable)' & 
                input$x != 'Age' &
                input$x != 'Earnings' &
                input$scaled == FALSE){
            plot_selected <- CEPRdata %>% 
                #filter(Earnings <= input$filtermaxwage) %>%
                ggplot(aes_string(x = input$x, 
                color = input$color)) +  
                    geom_bar() 
                    
        }
        #Second histogram type for continuous variables
        else if(input$plot_type == 'Histogram (x variable)' & 
                class(input$x) != 'factor' &
                input$scaled == FALSE){
           plot_selected <- CEPRdata %>% 
               #filter(Earnings <= input$filtermaxwage) %>%
               ggplot(aes_string(x = input$x, 
                              color = input$color)) +  
               geom_histogram() +
             geom_vline(aes(xintercept = mean(get(input$x))), color = "red") +
             geom_vline(aes(xintercept = median(get(input$x))), color = "blue", linetype = "dashed")
             
             
           
        }
    if(input$plot_type == 'Boxplot (x and y variables)' & input$scaled == FALSE){
      plot_selected <- CEPRdata %>% 
        #filter(Earnings <= input$filtermaxwage) %>%
        ggplot(aes_string(x = input$x, 
                          y = input$y, 
                          color = input$color)) +  
        geom_boxplot() 
    }
    #Line plot Medians, using controls to separate out subsetting by gender
    
    else if(input$plot_type == 'Line Plot (x and y variables)' & input$scaled != FALSE & input$color != "NULL"){
      plot_selected <- CEPRdata %>% 
        #filter(Earnings <= input$filtermaxwage) %>%
        group_by_(input$x, input$color) %>%
        summarise_(Mean_or_Median = paste(input$scaled,"(",input$y,")")) %>%
        ggplot(aes_string(x = input$x, 
                          y = "Mean_or_Median", 
                          color = input$color)) +  
        geom_line()
    
    }
    else if(input$plot_type == 'Line Plot (x and y variables)' & input$scaled != FALSE & input$color == "NULL"){
      plot_selected <- CEPRdata %>% 
        #filter(Earnings <= input$filtermaxwage) %>%
        group_by_(input$x) %>%
        summarise_(Mean_or_Median = paste(input$scaled,"(",input$y,")")) %>%
        ggplot(aes_string(x = input$x, 
                          y = "Mean_or_Median")) +  
        geom_line()
    }
    else if(input$plot_type == 'Scatterplot (x and y variables)' & input$scaled != FALSE & input$color == "NULL"){
      plot_selected <- CEPRdata %>% 
        #filter(Earnings <= input$filtermaxwage) %>%
        group_by_(input$x) %>%
        summarise_(Mean_or_Median = paste(input$scaled,"(",input$y,")")) %>%
        ggplot(aes_string(x = input$x, 
                          y = "Mean_or_Median")) +  
        geom_point()+
        geom_smooth(method=lm, se=FALSE)
    }
    else if(input$plot_type == 'Scatterplot (x and y variables)' & input$scaled != FALSE & !is.null(input$color)){
      plot_selected <- CEPRdata %>% 
        #filter(Earnings <= input$filtermaxwage) %>%
        group_by_(input$x, input$color) %>%
        summarise_(Mean_or_Median = paste(input$scaled,"(",input$y,")")) %>%
        ggplot(aes_string(x = input$x, 
                          y = "Mean_or_Median",
                          color = input$color)) +  
        geom_point()+
        geom_smooth(method=lm, se=FALSE)
    }
    else if(input$plot_type == 'Stacked Bar (x and y variable)'){
      plot_selected <- CEPRdata %>%
        ggplot(aes_string(x = input$x,
                          fill = input$y,
                          color = input$color)) +
        geom_bar(position="fill")
    }
#Remainder plots don't make sense, but going to allow them to be user created anyways  
    
    else if(input$plot_type == 'Histogram (x variable)' & input$scaled != FALSE & input$color != "NULL"){
      plot_selected <- CEPRdata %>% 
        group_by_(input$x, input$color) %>%
        summarise_(Mean_or_Median = paste(input$scaled,"(",input$y,")")) %>%
        ggplot(aes_string(x = input$x, 
                          y = "Mean_or_Median", 
                          color = input$color)) +  
        geom_histogram()
      
    }
    else if(input$plot_type == 'Line Plot (x and y variables)' & input$scaled != FALSE & input$color == "NULL"){
      plot_selected <- CEPRdata %>% 
        #filter(Earnings <= input$filtermaxwage) %>%
        group_by_(input$x) %>%
        summarise_(Mean_or_Median = paste(input$scaled,"(",input$y,")")) %>%
        ggplot(aes_string(x = input$x, 
                          y = "Mean_or_Median")) +  
        geom_line()
    }
    else if(input$plot_type == 'Scatterplot (x and y variables)' & input$scaled != FALSE & input$color == "NULL"){
      plot_selected <- CEPRdata %>% 
        #filter(Earnings <= input$filtermaxwage) %>%
        group_by_(input$x) %>%
        summarise_(Mean_or_Median = paste(input$scaled,"(",input$y,")")) %>%
        ggplot(aes_string(x = input$x, 
                          y = "Mean_or_Median")) +  
        geom_point()+
        geom_smooth(method=lm, se=FALSE)
    }
    else if(input$plot_type == 'Scatterplot (x and y variables)' & input$scaled != FALSE & !is.null(input$color)){
      plot_selected <- CEPRdata %>% 
        #filter(Earnings <= input$filtermaxwage) %>%
        group_by_(input$x, input$color) %>%
        summarise_(Mean_or_Median = paste(input$scaled,"(",input$y,")")) %>%
        ggplot(aes_string(x = input$x, 
                          y = "Mean_or_Median",
                          color = input$color)) +  
        geom_point()+
        geom_smooth(method=lm, se=FALSE)
    }
    else if(input$plot_type == 'Stacked Bar (x and y variable)'){
      plot_selected <- CEPRdata %>%
        ggplot(aes_string(x = input$x,
                          fill = input$y,
                          color = input$color)) +
        geom_bar(position="fill")
    }
    else {
    #Default plot, will display an error on the screen  
    ggplot() +
      annotate("text", x = 4, y =25, size = 8, label = error) +
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
    }
      plot_selected
    })
    
    output$dynamicTable <- renderTable({
      if(input$career != 0 & input$unemployed == FALSE){#broken
        CEPRdata <- CEPRdata %>%
          filter(Occupation == input$career) 
      }
      else if(input$career == 0 & input$unemployed != FALSE){#works
        CEPRdata <- CEPRdata %>%
          filter_(input$unemployed)
      }
      else if(input$career != 0 & input$unemployed != FALSE){#works for graph, not for table
        CEPRdata <- CEPRdata %>%
          filter(Occupation == input$career) %>%
          filter_(input$unemployed)
      }
      CEPRdata <- CEPRdata %>%
        filter(Earnings <= input$filtermaxwage)
      
      if(input$log == TRUE){
        CEPRdata <- CEPRdata %>%
          mutate(Earnings = log(Earnings + 1))
      }
      
      #Control sequence to set the plot type based off inputted plot type and allow for filtering    
      tabulate <- 
        if(input$color == "NULL"){
      #Histogram for continuous variables
          if(input$plot_type == 'Histogram (x variable)' & input$x != "Age" & input$x != "Earnings"){
            CEPRdata %>%
              group_by_(input$x) %>%
              summarise(Count = n()) %>%
              mutate(Proportion = Count / sum(Count))
          
          }
          #Histogram table for factor variables
          else if(input$plot_type == 'Histogram (x variable)' & class(input$x) != 'factor'){  
            CEPRdata %>% 
              summarise(Min = min(get(input$x)),
                        Mean = mean(get(input$x)), 
                        Median = median(get(input$x)),
                        Max = max(get(input$x)),
                        SD = sd(get(input$x)))
          }
      #Scatterplot
          else if(input$plot_type == 'Scatterplot (x and y variables)'){
            if(input$agesq == FALSE){
            reg <- lm(reformulate(input$x, input$y), data = CEPRdata)
            }
            else{
              regterms <- c(input$x, 'agesq')
              reg <- lm(reformulate(regterms, input$y), data = CEPRdata)
              }
              tidy(reg)
          }
        
      #Boxplots, display means and difference of means
          else if(input$plot_type == 'Boxplot (x and y variables)'){
            CEPRdata %>%
              group_by_(input$x) %>%
              summarise(SampleSize = n(), 
                        Mean = mean(get(input$y)),
                        Median = median(get(input$y)),
                        SD = sd(get(input$y))) 
                        
          }
      #Stacked bar graphs    
          else if(input$plot_type == 'Stacked Bar (x and y variable)'){
            CEPRdata %>%
            group_by_(input$x, input$y) %>%
              summarise(Count = n()) %>%
              mutate(Proportion = Count / sum(Count))
          }
          
          }
      #Below tables are for when the data is separated by a factor
      #Histogram separated by factor and factor variables
      else if(input$plot_type == 'Histogram (x variable)' & input$x != "Age" & input$x != "Earnings"){  
          CEPRdata %>% 
            group_by_(input$x,input$color) %>%
            summarise(Count = n()) %>%
            mutate(Proportion = Count / sum(Count))
      }
      #Histogram table separated by factor and continuous variables
      else if(input$plot_type == 'Histogram (x variable)' & class(input$x) != 'factor'){  
        CEPRdata %>% 
          group_by_(input$color) %>%
          summarise(Min = min(get(input$x), na.rm = T),
                    Mean = mean(get(input$x)), 
                    Median = median(get(input$x)),
                    Max = max(get(input$x)),
                    SD = sd(get(input$x)))
      }
      #Scatterplot separated by factor
      else if(input$plot_type == 'Scatterplot (x and y variables)'){
        if(input$agesq == TRUE){
          regterms <- c(input$x, input$color, 'agesq')
          reg <- lm(reformulate(regterms, input$y), data = CEPRdata)}
        else{
          regterms <- c(input$x, input$color)
          reg <- lm(reformulate(regterms, input$y), data = CEPRdata)}
        tidy(reg)
      }
      #Boxplots, display means and difference of means
      else if(input$plot_type == 'Boxplot (x and y variables)'){
        CEPRdata %>%
          group_by_(input$x, input$color) %>%
          summarise(SampleSize = n(), 
                    Mean = mean(get(input$y)),
                    Median = median(get(input$y)),
                    SD = sd(get(input$y))) 
        
      }
      #Stacked bar graphs, separable   
      else if(input$plot_type == 'Stacked Bar (x and y variable)'){
        CEPRdata %>%
          group_by_(input$x, input$y, input$color) %>%
          summarise(Count = n()) %>%
          mutate(Proportion = Count / sum(Count))
      }
      tabulate
    })
}    


# Run the application 
shinyApp(ui = ui, server = server)
