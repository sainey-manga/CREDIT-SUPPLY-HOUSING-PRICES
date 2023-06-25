library("dplyr")
library("ggplot2")
library("plotly")
library("tidyr")
library("vars")
library("tseries")
library("VIM")
library("mice")
library("corrplot")
#library(BMR)
## LOADING THE DATA##
df<-read.csv("/Users/saineymanga/Desktop/Housing.csv", header = TRUE)
attach(df)
head(df)
#colnames(df)
names(df)
## change some variable names for conviniences##
df_1 <- df %>% 
  rename("Housing_cpi" = "Housing.cpi" ,
         "Spread" = "Spread..risky...safe.",
         "Loan_growth" = "total.loans.growth.rate",
         "Mortgage" = "X30.Year.Fixed.Rate.Mortgage.Average",
         "Output_growth"= "Growth.rate.of.output")
head(df_1)
## select the needed variables
df_2<- df_1%>% dplyr::select('Date','Housing_cpi','Spread', 'Loan_growth',
                             'Mortgage','Output_growth')


head(df_2)
class(df_2$Date)
class(df_2$Loan_growth)
class(df_2$Mortgage)
df_2$Date <- as.Date(df_2$Date,
                     format = "%m/%d/%y")
#as.numeric(df_2$Loan_growth)
df_2$Loan_growth<- as.numeric(as.character(df_2$Loan_growth))
#rlang::last_error()
names(df_2)
summary(df_2)
## MISSING DATA ##
any(is.na(df_2))

missin_plot <- aggr(df_2, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df_2), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
#mice_df<- mice(df_2, m=5, maxit = 5, method = 'pmm', seed = 500)
#head(mice_df)
#summary(mice_df)
#any(is.na(mice_df))

##### DROP NA'S
final_df<- df_2%>% drop_na()
any(is.na(final_df))
head(final_df, 10)
summary(final_df)

#final_df$Output_growth<- final_df$Output_growth*100
#final_df$Loan_growth<-final_df$Loan_growth*100
####PLOTS 

x1<- plot_ly(df_2, x =~Date, y =~Housing_cpi , name = 'Housing_cpi',type='scatter',mode='lines')%>%
  layout(showlegend=FALSE)
x2<- plot_ly(df_2, x =~Date, y =~Mortgage , name = 'Mortgage',type='scatter',mode='lines')%>%
  layout(showlegend=FALSE)
x3<- plot_ly(df_2, x =~Date, y =~Spread , name = 'Spread',type='scatter',mode='lines')%>%
  layout(showlegend=TRUE)
x4<- plot_ly(df_2, x =~Date, y =~Loan_growth , name = 'Loan_growth',type='scatter',mode='lines')%>%
  layout(showlegend=TRUE)
x5<- plot_ly(df_2, x =~Date, y =~Output_growth , name = 'Output_growth',type='scatter',mode='lines')%>%
  layout(showlegend=TRUE)
graphs<-subplot(x1,x2,x3,x4,x5)
graphs
Mult_graphs<-plot_ly(final_df, x =~Date, y =~Housing_cpi , name = 'Housing_cpi',type='scatter',mode='lines')%>%
  layout(showlegend=TRUE)%>% 
  add_trace(y =~Mortgage , name = 'Mortgage',type='scatter',mode='lines')%>%
  layout(showlegend=TRUE)%>% 
  add_trace(y =~Spread , name = 'Spread',type='scatter',mode='lines')%>%
  layout(showlegend=TRUE)%>% 
  add_trace(y =~Loan_growth , name = 'Loan_growth',type='scatter',mode='lines')%>%
  layout(showlegend=TRUE)%>% 
  add_trace(y =~Output_growth , name = 'Output_growth',type='scatter',mode='lines')%>%
  layout(showlegend=TRUE)
Mult_graphs


### CORRELATION
matrix_df<- final_df %>% dplyr::select('Housing_cpi', 'Spread','Loan_growth','Mortgage', 
                                       'Output_growth')
corr_matrix<-cor(matrix_df)
corrplot(corr_matrix, type="upper")

### CHECK FOR STATIONARITY
PP.test(final_df$Housing_cpi) #Not Stationary
PP.test(final_df$Mortgage)
PP.test(final_df$Spread)
PP.test(final_df$Loan_growth)
PP.test(final_df$Output_growth)
# adf test
adf.test(final_df$Housing_cpi)
adf.test(final_df$Mortgage)
### FIRST DIFFERENCE
dif_Housing_cpi<-diff(final_df$Housing_cpi, differences = 1)
PP.test(dif_Housing_cpi)
##here we multiply the first difference of housing price by 100 to inflate the values
dif_Housing_cpi_1<-dif_Housing_cpi*100
PP.test(dif_Housing_cpi_1)
# turn loan growth rate to percentage
Loan_growth<-final_df$Loan_growth*100
# turn output growth to percentage
Output_growth<-final_df$Output_growth*100
#dif_Output<-diff(final_df$Output, differences = 1)
#PP.test(dif_Output)



acf(dif_Housing_cpi, lag =30 )
pacf(dif_Housing_cpi, lag =30)


######BUILDING THE VAR MODEL###### with output growth
var_df<- cbind( final_df$Output_growth,final_df$Loan_growth,dif_Housing_cpi, final_df$Mortgage,   
                final_df$Spread)
# with inflated first difference of housing price
var_df_1<- cbind(Output_growth,Loan_growth,dif_Housing_cpi_1, final_df$Mortgage,    
                 final_df$Spread)
var_df<- var_df[-1,]
var_df_1<- var_df_1[-1,]

colnames(var_df)<- cbind("Output_growth","Loan_growth","dif_Housing_cpi", "Mortgage",  
                         "Spread")
colnames(var_df_1)<- cbind("Output_growth","Loan_growth","dif_Housing_cpi_1", "Mortgage",  
                           "Spread")
head(var_df, 10)
head(var_df_1, 10)#inflated
##selecting lag
lag_select<- VARselect(var_df, lag.max = 10, type = "const")
lag_select$selection
var_model<- VAR(var_df, p=3, type = "const", season = NULL, exog = NULL)
summary(var_model)
var_model_1<- VAR(var_df_1, p=3, type = "const", season = NULL, exog= NULL)
summary(var_model_1)

##GRANGER CAUSALITY
cause.x <- causality(var_model, cause = "dif_Housing_cpi") 
cause.x
cause.x1 <- causality(var_model, cause = "Mortgage") 
cause.x1
cause.x2 <- causality(var_model, cause = "Loan_growth") 
cause.x2
cause.x3 <- causality(var_model, cause = "Spread") 
cause.x3
cause.x4 <- causality(var_model, cause = "Output_growth") 
cause.x4
## IMPULSE RESPONSE FUNCTION
# Check how a shock in mortgage will affect Housing
Output_irf <- vars:::irf(var_model_1, impulse = "Output_growth", response = "dif_Housing_cpi_1", 
                         n.ahead = 8, boot = TRUE)
plot(Output_irf, ylab = "Housing_cpi", main = "Output_growth shock to Housing")

# Check how a shock in Loan growth will affect Housing
Loan_growth_irf <- vars:::irf(var_model_1, impulse = "Loan_growth", 
                              response = "dif_Housing_cpi_1", n.ahead = 8, boot = TRUE)
plot(Loan_growth_irf, ylab = "Housing_cpi", main = "Loan_growth shock to Housing")

# Check how a shock in mortgage will affect Housing
Mortgage_irf <- vars:::irf(var_model_1, impulse = "Mortgage", response = "dif_Housing_cpi_1", 
                    n.ahead = 8, boot = TRUE)
plot(Mortgage_irf, ylab = "Housing_cpi", main = "Mortgage shock to Housing")
# Check how a shock in Spread will affect Housing
Spread_irf <- vars:::irf(var_model_1, impulse = "Spread", response = "dif_Housing_cpi_1", 
                    n.ahead = 8, boot = TRUE)
plot(Spread_irf, ylab = "Housing_cpi", main = "Spread shock to Housing")


## TRY ORDERING
order_irf<- vars:::irf(var_model_1, impulse =  "Output_growth", 
                       response = c("Loan_growth","dif_Housing_cpi_1", "Mortgage","Spread"),
                       ortho = TRUE, boot = TRUE)
plot(order_irf)


###### VAR without Output####
var_df_2<- cbind(Loan_growth,dif_Housing_cpi_1,final_df$Mortgage,
                 final_df$Spread)
var_df_2<- var_df_2[-1,]
colnames(var_df_2)<- cbind("Loan_growth","dif_Housing_cpi_1", "Mortgage",  
                           "Spread")
var_model_2<- VAR(var_df_2, p=3, type = "const", season = NULL, exog= NULL)
summary(var_model_2)
Loan_growth_irf_1 <- vars:::irf(var_model_2, impulse = "Loan_growth", 
                              response = "dif_Housing_cpi_1", n.ahead = 8, boot = TRUE)
plot(Loan_growth_irf_1, ylab = "dif_Housing_cpi_1", main = "Loan_growth shock to Housing")

Mortgage_irf_1 <- vars:::irf(var_model_2, impulse = "Mortgage", 
                             response = "dif_Housing_cpi_1", n.ahead = 8, boot = TRUE)
plot(Mortgage_irf_1, ylab = "dif_Housing_cpi_1", main = "Mortgage shock to Housing")
Spread_irf_1 <- vars:::irf(var_model_2, impulse = "Spread",
                           response = "dif_Housing_cpi_1", n.ahead = 8, boot = TRUE)
plot(Spread_irf_1, ylab = "dif_Housing_cpi_1", main = "Spread shock to Housing")

order_irf_2<- vars:::irf(var_model_2, impulse = "Loan_growth",
                         response = c("dif_Housing_cpi_1", "Mortgage",
                                      "Spread"),
                         
                         ortho = TRUE, boot = TRUE)
plot(order_irf_2)

ir_f<- vars:::irf(var_model_1)
plot(ir_f)
