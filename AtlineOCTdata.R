
# The purpose of this script is to load a set of OCT data files, collate them into an easy to use format, and save this to a .xlsx file and plot a few summary plots.


# The only rule for this is that the word before the last space should be a number you are looking at as a process variable.
# E.g. my filename is "At-Line_Report_20mg 130 %coated.csv", the script will use the 130 number as a comparison aainst the other files.

######################################################################################################################################################################################################################
rm(list = ls())
graphics.off()

library(ggplot2)
library(plotly)
library(stringr)
library(readr)
library(mclust)
library(xlsx)

loc_of_ProcPar = 1

pth = choose.dir(default="", caption = "Select location of files")    # Prompt to select the folder of OCT CSV files to read 
filelist = list.files(path = pth, pattern = ".csv",full.names=TRUE)   


######################################################################################################################################################################################################################
## -- File reading and data sorting -- ##
#Generate a set of variables for data storing
filedata = data.frame()
data_placeholder=data.frame()
sumry=data.frame()
data = data.frame()
gmm_data=data.frame()
gmm_sumry=data.frame()
rwnam = list()
process_details = list()
gmm=list()
pred=list()

## need to read each file and build a new array/list/df of each item
# To do this the script loops through all of the files in the filelist for the folder
for(i in 1:length(filelist)){
  #Read the current csv file and split the bulk table by tabs (its the delimiter the phyllon program writes files in) 
  data_placeholder=read.csv(filelist[i],header=FALSE)
  data = strsplit(data_placeholder[13:nrow(data_placeholder),],split="\t") #splits the data based on its tabs (specific to how the offline data is stored)
  
  # Try and split the  
  gmm <- Mclust(as.numeric(unlist(data)[seq(10,length(unlist(data)),12)]),2)
  pred = predict(gmm)$classification
  
  
  df_raw = data.frame(as.numeric(unlist(data)[seq(7,length(unlist(data)),12)]),unlist(data)[seq(8,length(unlist(data)),12)],
                      rep(as.numeric(unlist(strsplit(filelist[i],split=" "))[length(unlist(strsplit(filelist[i],split=" ")))-loc_of_ProcPar]),length(unlist(data)[seq(7,length(unlist(data)),12)])),
                      as.numeric(unlist(data)[seq(10,length(unlist(data)),12)]),as.numeric(unlist(data)[seq(15,length(unlist(data)),12)]),
                      as.numeric(unlist(data)[seq(18,length(unlist(data)),12)]),pred)#,as.numeric(unlist(strsplit(data_placeholder[13,]," "))[[1]]))
  filedata =rbind(filedata,df_raw)
  
  # area for summary statistics capture from each report
  df_sum=data.frame(substr(data_placeholder[1,],9,str_length(data_placeholder[1,])),substr(data_placeholder[3,],6,str_length(data_placeholder[3,])),
                    as.numeric(unlist(strsplit(filelist[i],split=" "))[length(unlist(strsplit(filelist[i],split=" ")))-1]),
                    parse_number(data_placeholder[5,]),parse_number(data_placeholder[6,]),parse_number(data_placeholder[7,]),parse_number(data_placeholder[8,]),
                    parse_number(data_placeholder[9,]),parse_number(data_placeholder[10,]))#,as.numeric(unlist(strsplit(data_placeholder[13,]," "))[[1]]))
  sumry=rbind(sumry,df_sum)
  
  #gmm removing unnecessary
  df_gmm =df_raw[which(df_raw[,7]==which.max(table(unlist(df_raw[,7])))),]
  gmm_data=rbind(gmm_data,df_gmm)
  
  df_gmmsumry = data.frame(substr(data_placeholder[1,],9,str_length(data_placeholder[1,])),substr(data_placeholder[3,],6,str_length(data_placeholder[3,])),
                           as.numeric(unlist(strsplit(filelist[i],split=" "))[length(unlist(strsplit(filelist[i],split=" ")))-1]),
                           mean(df_gmm[,4]),(sd(df_gmm[,4])/mean(df_gmm[,4]))*100,
                           mean(df_gmm[,5]),(sd(df_gmm[,5])/mean(df_gmm[,5]))*100,
                           mean(df_gmm[,6]),(sd(df_gmm[,6])/mean(df_gmm[,6]))*100)
  gmm_sumry=rbind(gmm_sumry,df_gmmsumry)
}

######################################################################################################################################################################################################################
## -- File Writing -- ##

#Set Column names
rwnam = unlist(data)[1:6]
rwnam[3]="Percent_Coat_Added"
rwnam[4]="Coating_Thickness"
rwnam[5]=unlist(strsplit(rwnam[5],split=" "))[1]
rwnam[6]=unlist(strsplit(rwnam[6],split=" "))[1]
#rwnam[7]="Mass_Coat_Added"
colnames(filedata) <- c(rwnam[1:6],"Gmm_Classification")
colnames(sumry) <- c("Sample_name",rwnam[2:4],"Thickness_RSD",rwnam[5],"Roughness_RSD",rwnam[6],"Homogeneity_RSD")#"Mass_Coat_Added")
colnames(gmm_data) <- c(rwnam[1:6],"Gmm_Classification")
colnames(gmm_sumry) <- c("Sample_name",rwnam[2:4],"Thickness_RSD",rwnam[5],"Roughness_RSD",rwnam[6],"Homogeneity_RSD")

#Save the files as a summary XLSX file
filename = paste0(pth,"/OCT data ",mapply(function(x, y) paste0(intersect(x, y),collapse = " "), strsplit(sumry[1,1], ' '), strsplit(sumry[2,1], ' ')),".xlsx")
write.xlsx(sumry,file=filename,sheetName="Summary data")
write.xlsx(filedata,file=filename,sheetName="All data",append=TRUE)
write.xlsx(gmm_sumry,file=filename,sheetName="Summary of cleaned Data",append=TRUE)
write.xlsx(gmm_data,file=filename,sheetName="All cleaned Data",append=TRUE)

######################################################################################################################################################################################################################
## -- Plotting Section -- ##
#Plot for % Coating Added

## Comparison plot for the raw and gmm data
dev.new(width = 12, height = 9, unit = "in")
plt <- ggplot() + geom_point(data = filedata,aes(x=Percent_Coat_Added,y=Coating_Thickness, color=Gmm_Classification))+
  geom_point(data=sumry,aes(x=Percent_Coat_Added,y=Coating_Thickness),size=4,color='coral4')+geom_point(data=gmm_sumry,aes(x=Percent_Coat_Added,y=Coating_Thickness),size=4,color='coral2')+
  theme_bw()+xlab("Coating Solution Added (%)")+ylab("Film Thickness (microns)")+scale_y_continuous(breaks=seq(0,140,10))+ scale_x_continuous(breaks=seq(0,140,10))+theme(legend.position="none")
plt

## Histogram for a single process point OCT readings for the gmm split
x=130
A=filedata[which(filedata["Percent_Coat_Added"]==x & filedata["Gmm_Classification"]==1),]
B=filedata[which(filedata["Percent_Coat_Added"]==x & filedata["Gmm_Classification"]==2),]
dev.new(width = 12, height = 9, unit = "in")
ggplot()+geom_histogram(data=A, aes(x=Coating_Thickness), fill="Coral1",color="Coral1",alpha=0.6)+geom_histogram(data=B, aes(x=Coating_Thickness), fill="aquamarine2",color="aquamarine2",alpha=0.6)+xlab(paste0("Histogram at ",x,"% Coating Solution Added"))+ylab("Frequency")


## -- Plots for the raw files as a process variable changes -- ##
# Film thickness
dev.new(width = 12, height = 9, unit = "in")
plt1 <- ggplot() + geom_point(data = filedata,aes(x=Percent_Coat_Added,y=Coating_Thickness),alpha=0.5,size=2.5,color='darkgray')+
  geom_point(data=sumry,aes(x=Percent_Coat_Added,y=Coating_Thickness),size=4,color='coral2')+theme_bw()+
  geom_text(data=sumry,aes(x=Percent_Coat_Added,y=Coating_Thickness,label=Coating_Thickness),nudge_x=2.5,check_overlap=T,size=3,nudge_y=1.5)+
  xlab("Coating Solution Added (%)")+ylab("Film Thickness (microns)")+scale_y_continuous(breaks=seq(0,round(max(filedata["Coating_Thickness"])+5,-1),10))+ scale_x_continuous(breaks=seq(0,max(filedata["Percent_Coat_Added"]),10))
plt1

# Roughness
dev.new(width = 12, height = 9, unit = "in")
plt2 <- ggplot() + geom_point(data = filedata,aes(x=Percent_Coat_Added,y=Roughness),alpha=0.5,size=2.5,color='darkgray')+
  geom_point(data=sumry,aes(x=Percent_Coat_Added,y=Roughness),size=4,color='coral2')+theme_bw()+
  geom_text(data=sumry,aes(x=Percent_Coat_Added,y=Roughness,label=Roughness),nudge_x=2.5,check_overlap=T,size=3,nudge_y=0.15)+
  xlab("Coating Solution Added (%)")+ylab("Roughness (microns)")+scale_y_continuous(breaks=seq(0,round(max(filedata["Coating_Thickness"])+5,-1),2))+ scale_x_continuous(breaks=seq(0,max(filedata["Percent_Coat_Added"]),10))
plt2

# Homogeneity
dev.new(width = 12, height = 9, unit = "in")
plt3 <- ggplot() + geom_point(data = filedata,aes(x=Percent_Coat_Added,y=Homogeneity),alpha=0.5,size=2.5,color='darkgray')+
  geom_point(data=sumry,aes(x=Percent_Coat_Added,y=Homogeneity),size=4,color='coral2')+theme_bw()+
  geom_text(data=sumry,aes(x=Percent_Coat_Added,y=Homogeneity,label=Homogeneity),nudge_x=3,check_overlap=T,size=3,nudge_y=2.5)+ylim(c(65,100))+
  xlab("Coating Solution Added (%)")+ylab("Homogeneity (%)")+scale_y_continuous(breaks=seq(0,round(max(filedata["Coating_Thickness"])+5,-1),10))+ scale_x_continuous(breaks=seq(0,max(filedata["Percent_Coat_Added"]),10))
plt3


## -- Plots for gmm summary data -- ##
# Film thickness
dev.new(width = 12, height = 9, unit = "in")
plt1 <- ggplot() + geom_point(data = gmm_data,aes(x=Percent_Coat_Added,y=Coating_Thickness),alpha=0.5,size=2.5,color='darkgray')+
  geom_point(data=gmm_sumry,aes(x=Percent_Coat_Added,y=Coating_Thickness),size=4,color='coral2')+theme_bw()+
  geom_text(data=gmm_sumry,aes(x=Percent_Coat_Added,y=Coating_Thickness,label=round(Coating_Thickness,digits=3)),nudge_x=2.5,check_overlap=T,size=3,nudge_y=1.5)+
  xlab("Coating Solution Added (%)")+ylab("Film Thickness (microns)")+scale_y_continuous( breaks=seq(0,140,10))+ scale_x_continuous(breaks=seq(0,140,10))
plt1

# Roughness
dev.new(width = 12, height = 9, unit = "in")
plt2 <- ggplot() + geom_point(data = gmm_data,aes(x=Percent_Coat_Added,y=Roughness),alpha=0.5,size=2.5,color='darkgray')+
  geom_point(data=gmm_sumry,aes(x=Percent_Coat_Added,y=Roughness),size=4,color='coral2')+theme_bw()+
  geom_text(data=gmm_sumry,aes(x=Percent_Coat_Added,y=Roughness,label=round(Roughness,digits=3)),nudge_x=2.5,check_overlap=T,size=3,nudge_y=0.15)+
  xlab("Coating Solution Added (%)")+ylab("Roughness (microns)")+scale_y_continuous( breaks=seq(0,20,1))+ scale_x_continuous(breaks=seq(0,140,10))
plt2

# Homogeneity
dev.new(width = 12, height = 9, unit = "in")
plt3 <- ggplot() + geom_point(data = gmm_data,aes(x=Percent_Coat_Added,y=Homogeneity),alpha=0.5,size=2.5,color='darkgray')+
  geom_point(data=gmm_sumry,aes(x=Percent_Coat_Added,y=Homogeneity),size=4,color='coral2')+theme_bw()+
  geom_text(data=gmm_sumry,aes(x=Percent_Coat_Added,y=Homogeneity,label=round(Homogeneity,digits=3)),nudge_x=3,check_overlap=T,size=3,nudge_y=2.5)+ylim(c(65,100))+
  xlab("Coating Solution Added (%)")+ylab("Homogeneity (%)")+scale_y_continuous( breaks=seq(0,100,10))+ scale_x_continuous(breaks=seq(0,140,10))
plt3
