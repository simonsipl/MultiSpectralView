##TODO LIST
# Zapisywanie plików csv po akceptacji !! -> DONE
# ulepszenie wyświetlania plotów -> DONE
# auto dodawanie nowych presetów -> DONE
# Tabel obliczen jakości filtra  -> GÓWNO ?! ASAP
# znaleść preset lampy rtęciowej i xenonowej -> JEST RTĘCIOWA
# iść po nowym roku do dr. Studenta i mu pokazać jak to wygląda BYŁEM ZROBIĆ OBLICZANIE JAKOŚCI FILTRA ASAP
# 

library(data.table)
library(shiny)
library(ggplot2)
library(flux)
#hmm ? sprawdzić to na dole
#library(ggvis)





shinyServer(function(input, output) {
  
  
  ##Function Section
  
  #Fluorochromes function taker
  outFluoro <- reactive({
    fluoro <- read.csv('./Fluorochromes/fluo_list.csv',header = TRUE,sep = "")$Name
    fluoro <- as.character(fluoro)
    return(fluoro)
  })
  
  #Excitation filter function taker
  outFilterex <- reactive({
    filterex <- read.csv('./Filters/filter_listex.csv',header = TRUE,sep = "")$Name
    filterex <- as.character(filterex)
    return(filterex)
  })
  
  #Emission filter function taker
  outFilterem <- reactive({
    filterem <- read.csv('./Filters/filter_listem.csv',header = TRUE,sep = "")$Name
    filterem <- as.character(filterem)
    return(filterem)
  })
  
  #Lamp function taker
  outLamp <- reactive({
    lamp <- read.csv('./Lamps/lamp_list.csv',header = TRUE,sep = "")$Name
    lamp <- as.character(lamp)
    return(lamp)
  })
  
  ## Reactive Events Funtions
  
  
  #Saving Event
  #While u choose what to upload and it will add it to list,and save
  saveing <- eventReactive(input$saveButton,
                           {
                             newCSV <- input$file1
                             data <- read.csv(newCSV$datapath)
                             name <- input$file1[['name']]
                             if(input$wave == 'lamp')
                             {
                               path = paste('./Lamps/' , name, sep = "")
                               write.csv(data,file = path )
                               lamp <- read.table('./Lamps/lamp_list.csv',header = T,check.names = FALSE)
                               newLamp <- data.table(Name = name)
                               combineLamp = list(lamp,newLamp)
                               lamp <-rbindlitst(combineLamp)
                               write.table(lamp,'./Lamps/lamp_list.csv',append = F,row.names = TRUE)
                               
                               return("Added new Lamp!")
                               
                             }
                             if(input$wave == 'Fluorochromes')
                             {
                               
                               path = paste('./Fluorochromes/' , name, sep = "")
                               write.csv(data,file = path )
                               fluo <- read.table('./Fluorochromes/fluo_list.csv',header = T, check.names = FALSE)
                               newFluo = data.table(Name = name)
                               combineFluo = list(fluo,newFluo)
                               fluo <- rbindlist(combineFluo)
                               write.table(fluo,'./Fluorochromes/fluo_list.csv',append = F,row.names = TRUE)
                               
                               return("Added new Fluorochrome!")
                               
                             }
                             if(input$wave =="Filter")
                             {
                               if(input$filtertype == 'filterem')
                               {
                                 path = paste('./Filters/' , name, sep = "")
                                 write.csv(data,file = path )
                                 filtem <- read.table('./Filters/filter_listem.csv',header = T,check.names = FALSE)
                                 newFiltem = data.table(Name = name)
                                 combineFiltem = list(filtem,newFiltem)
                                 filtem <- rbindlist(combineFiltem)
                                 write.table(filtem,'./Filters/filter_listem.csv',append = F,row.names = TRUE)
                                 
                                 
                                 return("Added new Filter of Emission!")
                                 
                               }
                               if(input$filtertype == 'filterex')
                               {
                                 path = paste('./Filters/' , name, sep = "")
                                 write.csv(data,file = path )
                                 filtex <- read.table('./Filters/filter_listex.csv',header = T,check.names = FALSE)
                                 newFiltex = data.table(Name = name)
                                 combineFiltex = list(filtex,newFiltex)
                                 filtex <- rbindlist(combineFiltex)
                                 write.table(filtex,'./Filters/filter_listex.csv',append = F,row.names = TRUE)
                                 
                                 
                                 return("Added new Filter of Excitation!")
                                 
                               }
                             }
                             
                             else
                             {
                               return("File filed to save!")
                             }
                             #runif(NULL)
                           })
  
  afterFilter <- reactive({
    fluorochrome_name <- input$fluor
    filter_ex_name <- input$filterex
    filter_em_name <- input$filterem
    lamp_name <- input$lamp
    generate_table_allow = FALSE
    k_ex = "Brak"
    k_em = "Brak"
    resultTable = c()
    fluoro_ex = NULL
    fluoro_em = NULL
    
    if(is.null(fluorochrome_name) || fluorochrome_name == "Choose")
    {
      #return(NULL)
    }
    else
    {
      if(is.null(filter_ex_name) || filter_ex_name == "Choose")
      {
        
      }
      else
      {
        path_fluorochrome = paste('./Fluorochromes/',fluorochrome_name , sep = "")
        data_plot_f <- read.csv(path_fluorochrome)
        path_filter_ex = paste('./Filters/',filter_ex_name , sep = "")
        data_plot_filter_ex <- read.csv(path_filter_ex)
        fluoro_ex = data_plot_f
        
        
        for(i in 1:length(data_plot_f$Wavelength))
        {
          if(length(data_plot_filter_ex$Excitation[which(data_plot_filter_ex$Wavelength == data_plot_f$Wavelength[i])])==0)
          {
            fluoro_ex$Excitation[i] = 0
          }
          else
          {
            fluoro_ex$Excitation[i] = data_plot_f$Excitation[i] * (100-sum(data_plot_filter_ex$Excitation[which(data_plot_filter_ex$Wavelength==data_plot_f$Wavelength[i])]))/100
          }
        }
        #fluoro_ex$Excitation = dnorm(fluoro_ex$Excitation)
        k_ex=sum(fluoro_ex$Excitation)/sum(data_plot_f$Excitation) 
        data_plot_f$Excitation <- fluoro_ex$Excitation
        #data_plot_f$Excitation = fluoro_ex
      }
    }
    
    if(is.null(fluorochrome_name) || fluorochrome_name == "Choose")
    {
      
    }
    else
    {
      if(is.null(filter_em_name) || filter_em_name == "Choose")
      {
        
      }
      else
      {
        path_fluorochrome = paste('./Fluorochromes/',fluorochrome_name , sep = "")
        data_plot_f <- read.csv(path_fluorochrome)
        path_filter_em = paste('./Filters/',filter_em_name , sep = "")
        data_plot_filter_em <- read.csv(path_filter_em)
        fluoro_em = data_plot_f
        
        for(i in 1:length(data_plot_f$Wavelength))
        {
          if(length(data_plot_filter_em$Emission[which(data_plot_filter_em$Emission == data_plot_f$Emission[i])])==0)
          {
            
            fluoro_em$Emission[i] = data_plot_f$Emission[i] * (100-sum(data_plot_filter_em$Emission[which(data_plot_filter_em$Wavelength==data_plot_f$Wavelength[i])])) /100
          }
          else
          {
            fluoro_em$Emission[i] = 0
          }
        }
        #fluoro_em$Emission = dnorm(fluoro_em$Emission)
        k_em=sum(fluoro_em$Emission)/sum(data_plot_f$Emission) 
        data_plot_f$Emission <- fluoro_em$Emission
        #  fluoro_em$Excitation = rnorm(fluoro_em$Excitation,mean=0)
        #  k_em=sum(fluoro_em$Emission)/sum(data_plot_f$Emission) * 100
        
      }
      p1 <- NULL
      p2 <- NULL
      #,fill = Emission
      
      
      #Wczytane dane przedstawione jako Emisja i Ekscytacja
      
      if(!is.null(data_plot_f$Excitation))
      {
        p1 <- geom_ribbon(data=data_plot_f,aes(x=Wavelength,ymin=0,ymax=Excitation,colour="Filter Excitation"),fill ='red' ,alpha=0.1)
      }
      if(!is.null(data_plot_f$Emission))
      {
        p2 <- geom_ribbon(data=data_plot_f,aes(x=Wavelength,ymin=0,ymax=Emission,colour="Filter Emission"),fill ='blue' ,alpha=0.1)
      }
      #p1 <-  geom_line(aes(x=Wavelength,y=Excitation),color ='red',fill="red" ,alpha=0.5) 
      #p2 <-  geom_line(aes(x=Wavelength,y=Emission),color ='blue',fill="blue" ,alpha=0.5)
      p <- ggplot() +p1 +p2
      print(p)
    }
  }
  
  )
  
  ##Algorithms Functions
  
  
  
  
  ##Section of UI
  #Render Reactive SelectOptions
  
  output$fluoro <- renderUI({
    
    selectInput("fluor", 
                label = "Choose a variable to display",
                choices = outFluoro(),
                selected = 1)
  })
  
  output$filterem <- renderUI({
    
    selectInput("filterem", 
                label = "Choose a variable to display",
                choices = outFilterem(),
                selected = 1)
  })
  
  output$filterex <- renderUI({
    
    selectInput("filterex", 
                label = "Choose a variable to display",
                choices = outFilterex(),
                selected = 1)
  })
  
  output$lamp <- renderUI({
    
    selectInput("lamp", 
                label = "Choose a variable to display",
                choices = outLamp(),
                selected = 1)
  })
  output$filtertype <- renderUI({
    if(input$wave == 'Filter')
    {
      p("Choose Type of a filter")
      radioButtons("filtertype","Filter Type",
                   c(Emission = "filterem",  Excitation = "filterex")
      )
      
    }
    else
    {
      return(NULL)
    }
  })
  
  #Test Rendering of path
  output$nText <- renderText({
    saveing()
  })
  
  
  ##Table Render Section with logic
  
  output$contents <- renderTable({
    inFile <- input$file1
    
    
    if(is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote  = input$quote)
  })
  
  #Tablica filtrów
  
  #Algoritms of tables
  # TU JESZCZE POKOPAC MUSZE !!!
  
  output$filterTable <- renderTable({
    fluorochrome_name <- input$fluor
    filter_ex_name <- input$filterex
    filter_em_name <- input$filterem
    lamp_name <- input$lamp
    generate_table_allow = FALSE
    k_ex = "Brak"
    k_em = "Brak"
    resultTable = c()
    data_plot_f <- NULL
    
    if(is.null(fluorochrome_name) || fluorochrome_name == "Choose")
    {
      #return(NULL)
    }
    else
    {
      if(is.null(filter_ex_name) || filter_ex_name == "Choose")
      {
        
      }
      else
      {
        path_fluorochrome = paste('./Fluorochromes/',fluorochrome_name , sep = "")
        data_plot_f <- read.csv(path_fluorochrome)
        path_filter_ex = paste('./Filters/',filter_ex_name , sep = "")
        data_plot_filter_ex <- read.csv(path_filter_ex)
        fluoro_ex = data_plot_f
        
        
        
        
        
        for(i in 1:length(data_plot_f$Wavelength))
        {
          if(length(data_plot_filter_ex$Excitation[which(data_plot_filter_ex$Wavelength == data_plot_f$Wavelength[i])])==0)
          {
            fluoro_ex$Excitation[i] = 0
          }
          else
          {
            fluoro_ex$Excitation[i] = data_plot_f$Excitation[i] * (100-sum(data_plot_filter_ex$Excitation[which(data_plot_filter_ex$Wavelength==data_plot_f$Wavelength[i])]))
          }
        }
        #fluoro_ex$Excitation = dnorm(fluoro_ex$Excitation)
        k_ex=sum(fluoro_ex$Excitation)/sum(data_plot_f$Excitation) 
        p_ex=auc(fluoro_ex$Wavelength,fluoro_ex$Excitation)/auc(data_plot_f$Wavelength,data_plot_f$Excitation)
        #data_plot_f$Excitation = fluoro_ex
      }
    }
    
    if(is.null(fluorochrome_name) || fluorochrome_name == "Choose")
    {
      
    }
    else
    {
      if(is.null(filter_em_name) || filter_em_name == "Choose")
      {
        
      }
      else
      {
        if(is.null(data_plot_f))
        {
          path_fluorochrome = paste('./Fluorochromes/',fluorochrome_name , sep = "")
          data_plot_f <- read.csv(path_fluorochrome)
        }
        
        path_filter_em = paste('./Filters/',filter_em_name , sep = "")
        data_plot_filter_em <- read.csv(path_filter_em)
        fluoro_em = data_plot_f
        
        for(i in 1:length(fluoro_em$Wavelength))
        {
          if(length(data_plot_filter_em$Emission[which(data_plot_filter_em$Emission == data_plot_f$Emission[i])])==0)
          {
            
            fluoro_em$Emission[i] = data_plot_f$Emission[i] * (100-sum(data_plot_filter_em$Emission[which(data_plot_filter_em$Wavelength==data_plot_f$Wavelength[i])]))/100
          }
          else
          {
            fluoro_em$Emission[i] = 0
          }
        }
        #fluoro_em$Emission = dnorm(fluoro_em$Emission)
        k_em=sum(fluoro_em$Emission)/sum(data_plot_f$Emission) 
        p_em=auc(fluoro_em$Wavelength,fluoro_em$Emission)/auc(data_plot_f$Wavelength,data_plot_f$Emission)*100
        #  fluoro_em$Excitation = rnorm(fluoro_em$Excitation,mean=0)
        #  k_em=sum(fluoro_em$Emission)/sum(data_plot_f$Emission) * 100
        
      }
      
    }
    
    
    if(k_ex != "Brak")
    {
      resultTable = c(k_ex,p_ex,resultTable)
    }
    else
    {
      resultTable = c('','',resultTable)
    }
    if(k_em != "Brak")
    {
      resultTable = c(k_em,p_em,resultTable)
    }
    else
    {
      resultTable = c('','',resultTable)
    }
    
    
    t=matrix(resultTable,ncol=2)
    t[]=ifelse(!is.na(as.numeric(t)),format(as.numeric(t),digits=3),t)
    rownames(t)=c("Quality index","Filter [%]")
    colnames(t)=c("emmission","excitation")
    t=as.table(t)
    
    
  })
  
  
  ##Plot Section
  
  #Main Plot 
  output$plot <- renderPlot({
    # plot musi ładować wybrane przezemnie filtry lampy etc. do plota z presetów
    fluorochrome_name <- input$fluor
    filter_ex_name <- input$filterex
    filter_em_name <- input$filterem
    lamp_name <- input$lamp
    generate_plot_allow = FALSE
    
    
    if(is.null(fluorochrome_name) || fluorochrome_name == "Choose")
    {
      p1 <- NULL
      p2 <- NULL
    }
    else
    {
      path = paste('./Fluorochromes/',fluorochrome_name , sep = "")
      data_plot_f <- read.csv(path)
      p1 <- geom_ribbon(data=data_plot_f,aes(x=Wavelength,ymin=0,ymax=Excitation,colour="Fluorochromes Excitation"),fill ='red' ,alpha=0.5)
      p2 <- geom_ribbon(data=data_plot_f,aes(x=Wavelength,ymin=0,ymax=Emission,colour="Fluorochromes Emission"),fill ='blue' ,alpha=0.5)
      #p1 <- geom_line(data=data_plot_f,aes(x=Wavelength,y=Excitation,colour="Fluorochromes Excitation"),color ='red' ,alpha=0.5) #+ geom_ribbon()#fill="red",alpha = 0.2)
      #p2 <- geom_line(data=data_plot_f,aes(x=Wavelength,y=Emission,colour="Fluorochromes Emission"),color ='blue' ,alpha=0.5)# + geom_ribbon()#fill="blue", alpha = 0.2)
      generate_plot_allow = TRUE
      
    }
    if(is.null(filter_em_name) || filter_em_name == "Choose")
    {
      p3 <- NULL
      p4 <- NULL
    }
    else
    {
      path = paste('./Filters/',filter_em_name , sep = "")
      data_plot_em <- read.csv(path)
      p3 <- geom_ribbon(data=data_plot_em,aes(x=Wavelength,ymin=0,ymax=Excitation,colour="Filter Excitation"),fill ='green' ,alpha=0.1)
      p4 <- geom_ribbon(data=data_plot_em,aes(x=Wavelength,ymin=0,ymax=Emission,colour="Filter Emission"),fill ='darkolivegreen1' ,alpha=0.1)
      #p3 <-  geom_line(data=data_plot_em,aes(x=Wavelength,y=Excitation),color ='green' ,alpha=0.5) 
      #p4 <-  geom_line(data=data_plot_em,aes(x=Wavelength,y=Emission),color ='black' ,alpha=0.5)
      generate_plot_allow = TRUE
    }
    if(is.null(filter_ex_name) || filter_ex_name == "Choose")
    {
      p5 <- NULL
      p6 <- NULL
    }
    else
    {
      path = paste('./Filters/',filter_ex_name , sep = "")
      data_plot_ex <- read.csv(path)
      #p5 <-  geom_line(data=data_plot_ex,aes(x=Wavelength,y=Excitation),color ='yellow' ,alpha=0.5) 
      #p6 <-  geom_line(data=data_plot_ex,aes(x=Wavelength,y=Emission),color ='violet' ,alpha=0.5)
      p5 <- geom_ribbon(data=data_plot_ex,aes(x=Wavelength,ymin=0,ymax=Excitation,colour="Filter Excitation"),fill ='yellow' ,alpha=0.1)
      p6 <- geom_ribbon(data=data_plot_ex,aes(x=Wavelength,ymin=0,ymax=Emission,colour="Filter Emission"),fill ='violet' ,alpha=0.1)
      generate_plot_allow = TRUE
    }
    if(is.null(lamp_name) || lamp_name == "Choose")
    {
      p7 <- NULL
      
    }
    else
    {
      path = paste('./Lamps/',lamp_name , sep = "")
      data_plot_l <- read.csv(path)
      p7 <-  geom_line(data=data_plot_l,aes(x=Wavelength,y=Emission),color ='black' ,alpha=0.5)
      generate_plot_allow = TRUE
    }
    #plot_main 
    
    if(generate_plot_allow)
    {
      p<-ggplot() + p1 + p2 + p3 + p4 + p5 + p6 + p7 
      print(p)
    }
    else
    {
      return(NULL) 
    }
    
    
  })
  
  output$newPlot <- renderPlot({
    afterFilter()
  })
  
  
  #Upload Plot to check upload Values are right
  output$spectra <- renderPlot({
    inPlot <- input$file1
    if(is.null(inPlot))
    {
      return(NULL)
    }
    else
    {
      p1 <- NULL
      p2 <- NULL
      #,fill = Emission
      data_plot <- read.csv(inPlot$datapath)
      #Wczytane dane przedstawione jako Emisja i Ekscytacja
      if(!is.null(data_plot$Emission))
      {
        p2 <-  geom_line(aes(x=Wavelength,y=Emission),color ='blue' ,alpha=0.5)
      }
      if(!is.null(data_plot$Excitation))
      {
        p1 <-  geom_line(aes(x=Wavelength,y=Excitation),color ='red' ,alpha=0.5) 
      }
      #p1 <-  geom_line(aes(x=Wavelength,y=Excitation),color ='red',fill="red" ,alpha=0.5) 
      #p2 <-  geom_line(aes(x=Wavelength,y=Emission),color ='blue',fill="blue" ,alpha=0.5)
      p <- ggplot(data_plot) +p1 +p2
      #p3 <- p1+p2
      #działa ale jeszcze nie to
      #p1 <- ggplot(data_plot, aes(x=Wavelength,y=Excitation,fill = Emission)) + geom_bar(stat = "identity")
      
      #ciekawy efekt wycięcia w zakresie filtra na histogramie
      #p1 <- ggplot(data_plot,aes(x=Wavelength,fill = as.factor(Emission))) + geom_histogram(position = "identity")
      
      #nope to nie to
      #p1 <-ggplot(data_plot,aes(x=Wavelength,fill = as.factor(Emission))) + geom_density(position = "identity")
      
      
      #wczytanie funkcji do obsługi multi grafów testing
      #multiplot <-dget("multiplot.R")
      
      #p<-multiplot(p1,p2, cols=2)
    }
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
})
