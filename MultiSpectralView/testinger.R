output$filterTable <- renderTable({
  fluorochrome_name <- input$fluor
  filter_ex_name <- input$filterex
  filter_em_name <- input$filterem
  lamp_name <- input$lamp
  generate_table_allow = FALSE
  
  
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
      path_filter = paste('./Filters/',filter_ex_name , sep = "")
      data_plot_filter <- read.csv(path_filter)
      fluoro_ex = data_plot_f$Excitation
      
    
      
      for(i in length(fluoro_ex))
      {
        if(length(data_plot_filter$Excitation[which(data_plot_filter$Excitation == data_plot_f$Excitation[i])])==0)
        {
          fluoro_ex[i] = 0
        }
        else
        {
          fluoro_ex[i] = data_plot_f$Excitation[i] * data_plot_filter$Excitation[which(data_plot_filter$Wavelength==data_plot_f$Wavelength[i])]
        }
      }
      data_plot_f$Excitation = fluoro_ex
    }
  }
    
  
  
})