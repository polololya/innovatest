#     InnovaTest. Web-based software to process data from ELISA and calculate ED50/LD50/IC50
#     based on 4PL sigmoid curve. 
#     Copyright (C) 2018  Olga Poleshchuk, Ruslan Al-Shehadat
#     
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation version 3 of the License, or
#     any later version.
#     Contacts: poleshchukolala@gmail.com 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#install.packages('conflicted')
if (!"shiny" %in% installed.packages()) install.packages('shiny')
if (!"readxl" %in% installed.packages()) install.packages('readxl')
if (!"drc" %in% installed.packages()) install.packages('drc')
if (!"ggplot2" %in% installed.packages()) install.packages('ggplot2')
if (!"dplyr" %in% installed.packages()) install.packages('dplyr')
library(shiny)
library(readxl)
library(drc)
library(ggplot2)
library(dplyr)


remove_out = function(x){
  s = median(x)
  ds = abs(median(x) - x)
  x[ds == max(ds)] = NA
  return(x)
}
a = c(1:26)
b = letters
set = data.frame(a,b)

ui = navbarPage(title = " InnovaTest ELISA Curve Fitting ",
           tabPanel("Table Mode", 
                    sidebarLayout(
                          sidebarPanel(
                            fileInput(inputId='inputfile',label='Upload your file',
                                      buttonLabel = "Choose",
                                      placeholder = "File's not chosen"),
                            sliderInput(inputId = 'Replicates', label = 'Maximal number of replicates', min = 1, max = 20, value = 2),
                            radioButtons(inputId = "Filetype",label = h4("Choose File type"),
                                         choices = list(".csv" = 1, ".xlsx" = 2), selected = 1,inline = TRUE),
                            textInput(inputId = 'Sheet', label = 'Excel sheet number', value = 1),
                            #radioButtons(inputId = 'Separator', label = 'Columns separator (CSV only)', choices = c('Tab'='\t', 'Comma'=',', 'Space'=' ')),
                            #radioButtons(inputId = 'Decimal', 'Decimal separator (CSV only)', c('Comma'=',', 'Dot'='.')),
                            #checkboxInput(inputId = 'Remove_out', label = 'Remove fuckin outliers', value = F),
                            tags$hr(),
                            actionButton(inputId = 'Final', label = 'Calculate'),
                            tags$hr(),
                            textInput(inputId = 'results', label = 'Output file name', value = Sys.Date()),
                            downloadButton("downloadsData", "Download results")
                          ),

                          mainPanel(
                            tags$h3('Initial data'),
                            tags$strong('Calibrators'),
                            tableOutput('Calibration_table'),
                            tags$strong('Tests'),
                            tableOutput('Test_initial'),
                            tags$h3('Results'),
                            tags$strong('Calibrators'),
                            tableOutput('cal'),
                            tags$strong('Tests'),
                            tableOutput('Test_final'),
                            tags$h4('Curve plot'),
                            plotOutput('model', height = "600px", width = "100%")
                          )
                        )),
           tabPanel("Plate Mode",
                    sidebarLayout(
                      sidebarPanel(
                        fileInput(inputId='inputfile_plate',label='Upload your file (.xls only)',
                                  buttonLabel = "Choose",
                                  placeholder = "File's not chosen"),
                        textInput(inputId = 'Concentrations', label = 'Enter concentrations here:', width = '100%'),
                        actionButton(inputId = 'Final_plate', label = 'Calculate'),
                        tags$hr(),
                        textInput(inputId = 'results_plate', label = 'Output file name', value = Sys.Date()),
                        downloadButton("downloadsData_plate", "Download results")
                      ),
                      mainPanel(tags$h3('Results'),
                        tags$strong('Calibrators'),
                        tableOutput('cal_plate'),
                        tags$strong('Tests'),
                        tableOutput('Test_final_plate'),
                        tags$h4('Curve plot'),
                        plotOutput('model_plate', height = "600px", width = "100%")
                      ))),
           tabPanel('Multiple Plates Mode'),
           tabPanel("Instructions",includeHTML('instructions.html'))
           
)

server = function(input, output) {

    #col_range
    a = reactive({paste('A',':',set$b[input$Replicates+1], sep = '')})
    ODs = reactive({paste('OD', c(1:input$Replicates), collapse=NULL)})
    sheet = reactive({as.numeric(input$Sheet)})
    
    #download file
    data <- reactive({
      print(input$inputfile$type)
      if (is.null(input$inputfile)) {
        return(NULL) }
      if (input$Filetype == '1') {
        read.csv(input$inputfile$datapath, stringsAsFactors = FALSE)
      } else {
        read_excel(input$inputfile$datapath, col_names = F, col_types = 'numeric', range = cell_cols(a()), sheet = sheet())
      }
    })
    
    
    
    
    #create calibration table.
    Calibration_table = reactive({
      a = data()
      colnames(a) = c('Concentration', ODs())
      a = a[!is.na(a$Concentration),]
      a
    })
    
    Test_table = reactive({
      q = data()
      q = q[is.na(q[,1]),-1]
      colnames(q) = ODs()
      q
    })

    #Render tables for view
    observeEvent(input$inputfile, {
      output$Calibration_table = renderTable({Calibration_table()}, hover = T,digits = 3, bordered = T)
      output$Test_initial = renderTable({Test_table()}, hover = T,digits = 3, bordered = T) 
    })
    
  #})
  
  
###count
  
  observeEvent(input$Final, {
    #Process calibration table
    cal = Calibration_table()
    cal$ODmean = dplyr::select(cal, ODs()) %>% apply(1, FUN = mean, na.rm = T)
    cal$CV = dplyr::select(cal, ODs()) %>% apply(1, FUN = sd, na.rm = T)/cal$ODmean * 100
    cal$CV[is.na(cal$CV)] = 0
   
    
    # if (sum(cal$CV > 8, na.rm = T) > 0 & input$Remove_out) {
    #   r = cal[cal$CV > 8, ODs()]
    #   r = apply(r, MARGIN = 1, FUN = remove_out)
    #   r = t(r)
    #   cal[cal$CV > 8, ODs()] = r[,ODs()]
    # }
    
    cal$ODmean = dplyr::select(cal, ODs()) %>% apply(1, FUN = mean, na.rm = T)
    cal$CV = dplyr::select(cal, ODs()) %>% apply(1, FUN = sd, na.rm = T)/cal$ODmean * 100
    cal$CV[is.na(cal$CV)] = 0
    
    #Process test table
    test = Test_table()
    
    test$ODmean = dplyr::select(test, ODs()) %>% apply(1, FUN = mean, na.rm = T)
    test$CV = dplyr::select(test, ODs()) %>% apply(1, FUN = sd, na.rm = T)/test$ODmean * 100
    test$CV[is.na(test$CV)] = 0
    
    # if (sum(test$CV > 8, na.rm = T) > 0 & input$Remove_out) {
    #   r = test[test$CV > 8, ODs()]
    #   r = apply(r, MARGIN = 1, FUN = remove_out)
    #   r = t(r)
    #   test[test$CV > 8, ODs()] = r[,ODs()]
    # }
    
    test$ODmean = dplyr::select(test, ODs()) %>% apply(1, FUN = mean, na.rm = T)
    test$CV = dplyr::select(test, ODs()) %>% apply(1, FUN = sd, na.rm = T)/test$ODmean * 100
    test$CV[is.na(test$CV)] = 0
    
    #Fit model
    fit = drm(ODmean ~ Concentration, data=cal, fct=LL.4(names=c('Slope','Lower Limit','Upper Limit','ED50')))
    
    #Predict concentrations
    cal$Result = fit$coefficients[[4]]*(((-1*fit$coefficients[[2]]+cal$ODmean)/(fit$coefficients[[3]]-cal$ODmean))^(1/-fit$coefficients[[1]]))
    cal$Log10Conc = log10(cal$Result)
    output$cal = renderTable(cal, hover = T,digits = 3, bordered = T)
    
    test$Result = fit$coefficients[[4]]*(((-1*fit$coefficients[[2]]+test$ODmean)/(fit$coefficients[[3]]-test$ODmean))^(1/-fit$coefficients[[1]]))
    test$Log10Conc = log10(test$Result)
    output$Test_final = renderTable(test, hover = T,digits = 3, bordered = T)
    
    
    
    #calcuate model and plot
    xmin = ifelse(min(cal$Concentration) == 0, 0.01, min(cal$Concentration))
    x = seq(xmin, max(cal$Concentration), length=100)
    y = (fit$coefficients[[2]]+(fit$coefficients[[3]]-fit$coefficients[[2]])/(1+(x/fit$coefficients[[4]])^fit$coefficients[[1]]))
    model = data.frame(x,y)
    
    # output$model = renderPlot({
    #   ggplot(cal, aes(log10(Concentration), ODmean)) +
    #     geom_line(color = 'black') + geom_point(color = 'black') +
    #     xlab('Log10(Concentration)') +
    #     ylab('Optical Density') +
    #     geom_line(data = model, aes(x = log10(x), y = y, color = 'red'), inherit.aes = FALSE) +
    #     geom_point(data = test, aes(x = log10(test$Result), y = test$ODmean, color = 'blue'), inherit.aes = FALSE) +
    #     theme_bw() + guides(color = 'none')})
    
    output$model = renderPlot({
      ggplot(cal, aes(log10(Concentration), ODmean)) +
        geom_line(aes(color = 'Calibration')) + geom_point(aes(color = 'Calibration')) +
        xlab('Log10(Concentration)') +
        ylab('Optical Density') +
        geom_line(data = model, aes(x = log10(x), y = y, color = '4PL model'), inherit.aes = FALSE) +
        geom_point(data = test, aes(x = log10(test$Result), y = test$ODmean, color = 'Test Results'), inherit.aes = FALSE) +
        geom_point(data = cal, aes(x = log10(cal$Result), y = cal$ODmean, color = 'Calibration Results'), inherit.aes = FALSE) +
        theme_bw() + 
        scale_color_manual(name = ' ',values = c('4PL model' = '#0FAFA7', 'Test Results' = '#F7B538', 'Calibration Results' = '#C84C09', 'Calibration' = '#444444'))})
    
######## Save results ##########
    output$downloadsData <- downloadHandler(
      filename = function() {paste(input$results, '.csv', sep = "")},
      content = function(filename) {
        test$Concentration = NA
        final = rbind(cal, test)
        final = apply(final, MARGIN = 2, FUN = round, digits = 3)
        write.csv2(final, filename, row.names = FALSE)
      }
    )
  })
  
######## Plate mode ##########  
  data_plate = reactive({
    if(is.null(input$inputfile_plate))
      return(NULL)
    read_excel(input$inputfile_plate$datapath, col_names = F, range = 'A1:L16')
  })

  
  observeEvent(input$Final_plate, {
      data_values = as.numeric(unlist(data_plate()[1:8,]))
      data_mapping = unlist(data_plate()[9:16,])
      data_table = data.frame(values = data_values, map = data_mapping, stringsAsFactors = F)
      
      data_table = data_table[!is.na(data_table$values),]
      
      aggregated = aggregate(x = data_values, by = list(data_mapping), FUN = mean, na.rm = T)
      
      Calibration = data.frame(Means = aggregated$x[grepl('C', aggregated$Group.1)], Concentrations = as.numeric(strsplit(input$Concentrations, ",")[[1]]))
      Test = data.frame(Means = aggregated$x[grepl('T', aggregated$Group.1)])
      
      aggregated2 = aggregate(x = data_values, by = list(data_mapping), FUN = sd, na.rm = T)
      
      Calibration$CV = aggregated2$x[grepl('C', aggregated2$Group.1)] / Calibration$Means * 100
      Calibration$CV[is.na(Calibration$CV)] = 0
      Calibration$Labels = aggregated2$Group.1[grepl('C', aggregated2$Group.1)]
      
      
      Test$CV = aggregated2$x[grepl('T', aggregated2$Group.1)] / Test$Means * 100
      Test$CV[is.na(Test$CV)] = 0
      Test$Labels = aggregated2$Group.1[grepl('T', aggregated2$Group.1)]
      
      
      fit = drm(Means ~ Concentrations, data=Calibration, fct=LL.4(names=c('Slope','Lower Limit','Upper Limit','ED50')))
      
      Calibration$Result = fit$coefficients[[4]]*(((-1*fit$coefficients[[2]]+Calibration$Means)/(fit$coefficients[[3]]-Calibration$Means))^(1/-fit$coefficients[[1]]))
      Calibration$Log10Conc = log10(Calibration$Result)
      
      Test$Result = fit$coefficients[[4]]*(((-1*fit$coefficients[[2]]+Test$Means)/(fit$coefficients[[3]]-Test$Means))^(1/-fit$coefficients[[1]]))
      Test$Log10Conc = log10(Test$Result)
      
      xmin = ifelse(min(Calibration$Concentrations) == 0, 0.01, min(Calibration$Concentrations))
      x = seq(xmin, max(Calibration$Concentrations), length=100)
      y = (fit$coefficients[[2]]+(fit$coefficients[[3]]-fit$coefficients[[2]])/(1+(x/fit$coefficients[[4]])^fit$coefficients[[1]]))
      model = data.frame(x,y)
      
      output$model_plate = renderPlot({
        ggplot(Calibration, aes(log10(Concentrations), Means)) +
          geom_line(aes(color = 'Calibration')) + geom_point(aes(color = 'Calibration')) +
          xlab('Log10(Concentration)') +
          ylab('Optical Density') +
          geom_line(data = model, aes(x = log10(x), y = y, color = '4PL model'), inherit.aes = FALSE) +
          geom_point(data = Test, aes(x = log10(Test$Result), y = Test$Means, color = 'Test Results'), inherit.aes = FALSE) +
          geom_point(data = Calibration, aes(x = log10(Calibration$Result), y = Calibration$Means, color = 'Calibration Results'), inherit.aes = FALSE) +
          theme_bw() + 
          scale_color_manual(name = ' ',values = c('4PL model' = '#0FAFA7', 'Test Results' = '#F7B538', 'Calibration Results' = '#C84C09', 'Calibration' = '#444444'))})
      
      output$cal_plate = renderTable(Calibration)
      output$Test_final_plate = renderTable(Test)
      
      output$downloadsData_plate <- downloadHandler(
        filename = function() {paste(input$results_plate, '.csv', sep = "")},
        content = function(filename) {
          Test$Concentrations = NA
          final = rbind(Calibration, Test)
          final = apply(final, MARGIN = 2, FUN = round, digits = 3)
          write.csv2(final, filename, row.names = FALSE)
        }
      )
      
  })  
}


shinyApp(ui = ui, server = server)
