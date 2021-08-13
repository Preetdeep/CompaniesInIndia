
library(dplyr)
library(readr)
library(ggplot2)
library(stringr) 

setwd("~/OneDrive - Invest India/ForData/Other New/MCA Company Details/CompaniesInIndia")
companies <- read_csv("registered_companies.csv")
## This data is only till March 2020 (at least not till December 2020)
View(companies)
companies$regd<-as.Date(companies$DATE_OF_REGISTRATION,format="%d-%m-%Y")
companies$year<-format(companies$regd,format="%Y")
companies$monthyear<-format(companies$regd,format="%m-%Y")
companies$month<-format(companies$regd,format="%m")
companies$day<-format(companies$regd,format="%d")
companies$weekday<-weekdays(companies$regd)

companies$activity<-ifelse(companies$PRINCIPAL_BUSINESS_ACTIVITY_AS_PER_CIN=="Activities of private households as employers and undifferentiated production activities of private households", "Activities of private households",companies$PRINCIPAL_BUSINESS_ACTIVITY_AS_PER_CIN)
companies$activity<-ifelse(companies$PRINCIPAL_BUSINESS_ACTIVITY_AS_PER_CIN=="Wholesale and retail trade repair of motor vehicles motorcycles and personal and household goods"," Trade and repair of motor vehicles and goods",companies$PRINCIPAL_BUSINESS_ACTIVITY_AS_PER_CIN)

datewise<-companies%>%group_by(regd)%>%summarise(totalcos=length(regd))
yearwise<-companies%>%group_by(year)%>%summarise(totalcos=length(year))
monthwise<-companies%>%group_by(month)%>%summarise(totalcos=length(month))
statewise<-companies%>%group_by(REGISTERED_STATE)%>%summarise(totalcos=length(REGISTERED_STATE))

library(SmartEDA)
ExpData(data=companies,type=2)
plot2 <- ExpCatViz(companies,target=NULL,col ="slateblue4",clim=10,margin=2,Page = c(2,2),sample=4)
plot2[[1]]
##Let us look at Break up of these companies for UP.

state<-"Uttar Pradesh"
for (i in 1:length(unique(companies$REGISTERED_STATE))){

  state<-unique(companies$REGISTERED_STATE)[i]
upcompanies<-filter(companies,REGISTERED_STATE==state)
upcompanies<-filter(upcompanies,COMPANY_STATUS=="ACTV")
upcompanies<-filter(upcompanies,COMPANY_CLASS=="Private"|COMPANY_CLASS=="Private(One Person Company)")
upcosbytype<-upcompanies%>%group_by(activity)%>%summarise(totalcos=length(activity))
colnames(upcosbytype)<-c("activity","totalcos")
upcosbytype$activity <- str_wrap(upcosbytype$activity, width = 20)

a<-ggplot(upcosbytype,aes(x=activity,y=totalcos))  
a<-a+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) +
  labs(title = paste("Activity wise break up of active Pvt companies in",state)) +xlab("Activity") +ylab("Total Companies")
print(a)
setwd("~/OneDrive - Invest India/ForData/Other New/MCA Company Details/Output")
ggsave(paste0(state,"ActivityTypeActivePvtCos.png"),plot=last_plot(), width = 11, height =7)

}

for (i in 1:length(unique(companies$REGISTERED_STATE))){
  
  state<-unique(companies$REGISTERED_STATE)[i]
  upcompanies<-filter(companies,REGISTERED_STATE==state)
  upcompanies<-filter(upcompanies,COMPANY_STATUS=="ACTV")
  upcompanies<-filter(upcompanies,COMPANY_CLASS=="Private(One Person Company)")
  upcosbytype<-upcompanies%>%group_by(activity)%>%summarise(totalcos=length(activity))
  colnames(upcosbytype)<-c("activity","totalcos")
  upcosbytype$activity <- str_wrap(upcosbytype$activity, width = 20)
  
  a<-ggplot(upcosbytype,aes(x=activity,y=totalcos))  
  a<-a+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) +
    labs(title = paste("Activity wise break up of active OPC companies in",state)) +xlab("Activity") +ylab("Total Companies")
  print(a)
  print(i)
  setwd("~/OneDrive - Invest India/ForData/Other New/MCA Company Details/Output")
  ggsave(paste0(state,"ActivityTypeActiveOPCCos.png"),plot=last_plot(), width = 11, height =7)
}


upcompanies<-filter(companies,REGISTERED_STATE==state)
someupcompanies<-filter(upcompanies,grepl("Agra",REGISTERED_OFFICE_ADDRESS))
agracompanies<-filter(companies,grepl("Agra",REGISTERED_OFFICE_ADDRESS))

foreigncompanies<-filter(companies,companies$COMPANY_SUB_CATEGORY=="Subsidiary of Foreign Company")

upcosbytype<-foreigncompanies%>%group_by(activity)%>%summarise(totalcos=length(activity),totalauthcap=sum(AUTHORIZED_CAP),totalpaidcap=sum(PAIDUP_CAPITAL))
#colnames(upcosbytype)<-c("activity","totalcos")
#upcosbytype$activity <- str_wrap(upcosbytype$activity, width = 20)

a<-ggplot(upcosbytype,aes(x=activity,y=totalcos))  
a<-a+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Activity wise break up of active Foreign Companies") +
  xlab("Activity") +ylab("Total Companies")
print(a)

##To get foreign companies in each state as per category
for (i in 1:length(unique(companies$REGISTERED_STATE))){
  
  state<-unique(companies$REGISTERED_STATE)[i]
  upcompanies<-filter(companies,REGISTERED_STATE==state)
  upcompanies<-filter(upcompanies,COMPANY_STATUS=="ACTV")
  #upcompanies<-filter(upcompanies,COMPANY_CLASS=="Private"|COMPANY_CLASS=="Private(One Person Company)")
  upcompanies<-filter(upcompanies,upcompanies$COMPANY_SUB_CATEGORY=="Subsidiary of Foreign Company")
  upcosbytype<-upcompanies%>%group_by(activity)%>%summarise(totalcos=length(activity),totalauthcap=sum(AUTHORIZED_CAP),totalpaidcap=sum(PAIDUP_CAPITAL))
  #colnames(upcosbytype)<-c("activity","totalcos")
  upcosbytype$activity <- str_wrap(upcosbytype$activity, width = 20)
  
  a<-ggplot(upcosbytype,aes(x=activity,y=totalcos))  
  a<-a+geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90)) +
    labs(title = paste("Activity wise break up of active foreign companies in",state)) +xlab("Activity") +ylab("Total Companies")
  print(a)
  setwd("~/OneDrive - Invest India/ForData/Other New/MCA Company Details/Output")
  ggsave(paste0(state,"ActivityTypeActiveForeignCos.png"),plot=last_plot(), width = 11, height =7)
  
}

#Where have foreign companies exited the most?
#Get total number of foreign cos, get total number of active foreign cos, subtract, use %, and full.
activeforeigncos<-companies%>%group_by(REGISTERED_STATE)%>%filter(COMPANY_STATUS=="ACTV")%>%filter(COMPANY_SUB_CATEGORY=="Subsidiary of Foreign Company")%>%summarise(activeforeigncos=length(COMPANY_SUB_CATEGORY))
totalforeigncos<-companies%>%group_by(REGISTERED_STATE)%>%filter(COMPANY_SUB_CATEGORY=="Subsidiary of Foreign Company")%>%summarise(totalforeigncos=length(COMPANY_SUB_CATEGORY))
foregincosbystate<-merge(totalforeigncos,activeforeigncos,by="REGISTERED_STATE")
foregincosbystate$diff<-foregincosbystate$totalforeigncos-foregincosbystate$activeforeigncos
foregincosbystate$diffpercent<-(foregincosbystate$totalforeigncos-foregincosbystate$activeforeigncos)/foregincosbystate$totalforeigncos
write.csv(foregincosbystate,"ForeignCompanyExitsByState.csv")


# How Manufacturing Companies have grown ----------------------------------


##See how manufacturing companies have increased
#Filter Manufacturing
manufaccos<-filter(companies, activity=="Manufacturing")
#See year trend
manufaccostrend<-manufaccos%>%group_by(year)%>%summarise(manufaccos=length(year))
a<-ggplot(manufaccostrend,aes(x=year,y=manufaccos,group=1))
a<-a+geom_line()+geom_point()  + labs(title = "Year Wise New Manufacturing Companies 1850-2020") +
  xlab("Year") +ylab("Total Companies")
print(a)
setwd("~/OneDrive - Invest India/ForData/Other New/MCA Company Details/Output")
ggsave("ManufacturingCosRegd1857onwards.png",plot=last_plot(), width = 11, height =7)
#ggsave(paste0(state,"ActivityTypeActivePvtCos.png"),plot=last_plot(), width = 11, height =7)
#See trend for last 30 years
manufaccostrendrecent<-filter(manufaccos,year>1990)
manufaccostrendrecentsum<-manufaccostrendrecent%>%group_by(year)%>%summarise(manufaccos=length(year))
a<-ggplot(manufaccostrendrecentsum,aes(x=year,y=manufaccos,group=1))
a<-a+geom_line()+geom_point() + #theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Year Wise New Manufacturing Companies 1990-2020",caption="@DrPreetDeep @InvestIndia") +
  xlab("Year") +ylab("Total Companies")

print(a)
ggsave("NewManufacCos1990on.png",plot=last_plot(), width =10, height =9)
#See for Foreign Companies

foreignmanufaccostrendrecent<-filter(manufaccostrendrecent,COMPANY_SUB_CATEGORY=="Subsidiary of Foreign Company")
foreignmanufaccostrendrecentsum<-foreignmanufaccostrendrecent%>%group_by(year)%>%summarise(manufaccos=length(year))
a<-ggplot(foreignmanufaccostrendrecentsum,aes(x=year,y=manufaccos,group=1))
a<-a+geom_line()+geom_point() + #theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Year Wise New Foreign Manufacturing Companies 1990-2020",caption="@DrPreetDeep @InvestIndia") +
  xlab("Year") +ylab("Total Companies")
print(a)
ggsave("NewForeignManufacCos1990on.png",plot=last_plot(), width =10, height =9)
##See it with the total number of foreign companies

foreigncompaniesyearwise<-foreigncompanies%>%group_by(year)%>%summarise(totalcos=length(year))
foreigncompaniesyearwise$type<-"Total"
foreignmanufaccostrendrecentsum$type<-"Manufacturing"
#foreignandmanufaccos<-merge(foreigncompaniesyearwise,foreignmanufaccostrendrecentsum,by="year",all.y = TRUE)
foreignandmanufaccos<-rbind(foreigncompaniesyearwise,foreignmanufaccostrendrecentsum)
foreignandmanufaccos<-filter(foreignandmanufaccos,year>2000)
a<-ggplot(foreignandmanufaccos,aes(x=year,y=cos,group=type))#+ theme_classic()
a<-a+geom_line(aes(color=type))+geom_point(aes(color=type)) + #theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Year Wise New Foreign Companies",caption="@DrPreetDeep @InvestIndia")+#scale_color_manual(name = "Y series", values = c("Y1" = "darkblue", "Y2" = "red"))+
  xlab("Year") +ylab("Total Companies")

print(a)

ggsave("NewForeignCosAndManufac2000on.png",plot=last_plot(), width =10, height =9)


a<-ggplot()+
  geom_line(data=foreigncompaniesyearwise,aes(y=cos,x= year,colour="blue"))+#+geom_point()
  geom_line(data=foreignmanufaccostrendrecentsum,aes(y=cos,x= year,colour="red"))#,size=1) +geom_point()#+
scale_color_manual(name = "Y series", values = c("Y1" = "darkblue", "Y2" = "red"))







