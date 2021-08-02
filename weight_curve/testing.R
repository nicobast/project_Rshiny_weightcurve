
age_month<-round(as.numeric(as.Date('2000-11-11')-as.Date('1986-11-11'))/30)
underweight_bmi<-norm_data[norm_data$Month==age_month,'P15']
targetweight_bmi<-norm_data[norm_data$Month==age_month,'P25']

height<-157/100

underweight_weight<-ceiling(underweight_bmi*(height^2))
targetweigth_weight<-ceiling(targetweight_bmi*(height^2))

paste('bla',as.Date('2000-11-11'),sep='_')
setwd('C:/Users/Nico/PowerFolders/project_Rshiny_weightcurve/weight_curve/')
norm_data<-read.delim('bmi_girls_perc_WHO2007.txt',header=T)
