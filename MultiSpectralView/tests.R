##współczynnik dobierania filtra
#Excitation
k_ex=sum(fluoro_ex)/sum(fluoro_ex)*100 

##Poprawka na lustro dichroiczne
#dichro_ex = fluoro_ex
#for (i in 1:length(fluoro_ex)){
#  if (length(dc$V2[which(dc$V1==n_ex$V1[i])])==0) {
#    d_ex$V2[i]=n_ex$V2[i]
#  }
#  else {
#    d_ex$V2[i]=n_ex$V2[i]*(1-dc$V2[which(dc$V1==n_ex$V1[i])]) 
#  }      
#}


#Emission
#k_em=sum(fluoro_em)/sum(fluoro_em)*100 
k_em = NULL

#Lamp
if(is.null(fluorochrome_name) || fluorochrome_name == "Choose" & is.null(lamp_name) || lamp_name == "Choose")
{
  #return(NULL)
}
else
{
  k_lamp=NULL
  
  lamp = paste('./Lamps/',lamp_name , sep = "")
  data_plot_lamp <- read.csv(lamp)
  path_fluorochrome = paste('./Fluorochromes/',fluorochrome_name , sep = "")
  data_plot_f <- read.csv(path_fluorochrome)
  for (i in 1:length(fluoro_ex))
  {
    k_lamp$V1[i]=data_plot_f$Wavelength[i]
    k_lamp$V2[i]=data_plot_lamp$Emission[which(data_plot_lamp$Wavelength==data_plot_f$Wavelength[i])]
  }
  
  
  k_lamp$V2[is.na(k_lamp$V2)]=0
  k_l=sum(k_lamp$V2)/sum(k_lamp$V2)*100 # jaka czesc calosci swiecenia lampy swieci w zakresie wzbudzenia
  
  t=matrix(c(k_lamp,k_l,' '),ncol=2)
  #t[]=ifelse(!is.na(as.numeric(t)),format(as.numeric(t),digits=3),t)
  #colnames(t)=c("filter [%]","d.mirror [%]")
  rownames(t)=c("excitation","emmission")
  t=as.table(t)
}
#t=as.table(fluoro_ex)