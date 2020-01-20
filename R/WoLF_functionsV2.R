
#install.packages(c("naniar","readxl","dplyr","nls2","proto","ggplot2","ggpubr","minpack.lm"))
#library(naniar)
#library(readxl)
#library(dplyr)
#library(nls2)
#library(proto)
#library(ggplot2)
#library(ggpubr)
#library(minpack.lm)

####################################################################################################################
#' ReadIn of Spiral Measurements
#'
#' Reads in the spiral measurements from an excel template with multiple sheets necessary for the calculation of the
#' growth-invariant and growth-independent meristic characters used for analysis of morphology in nummulitid foraminifera.
#'
#'
#' @param Inputpath source path for the excel sheet
#' @param Inputsheets vector containing names of existing sheets in the template; sheet names may be changed, but the
#' order of the sheets must remain the same Summary -> marginal radius -> chamber base length -> backbend angle ->
#' septal angle -> chamber area -> chamber perimeter
#' @param sheetlength max. number of rows to read from excel
#' @param rad_degree TRUE or FALSE statement if radians and degree columns are present in the marginal radius sheet
#'
#' @return the function returns the dataframes Sum (Summary table), MR (marginal radius), CBL (Chamber base length),
#'  BBA (Backbend angle), SA (Septal angle), CA (chamber area), CP (chamber perimeter), CL (chamber length) and PR (Perimeter ratio)
#'
#' @examples
#' system.file("extdata","input_data.xlsx", package = "WoLF")
#' sheets<-c("summary","marginal radius","chamber base length","backbend angle","septal angle", "chamber area", "chamber perimeter")
#' ReadInSpiral(Inputpath = "C:/Users/user/Desktop/Input_data.xlsx",Inputsheets = sheets,sheetlength = 50,rad_degree=TRUE)
#'
#' @export

ReadInSpiral<- function(Inputpath,Inputsheets,sheetlength,rad_degree){
  #Inputpath - path where file is
  #Inputsheets - vector with the sheet names in the template
  #sheetlength - number of lines for read-in
  #rad_degree - T/F template contains radians and degrees
  #dummy variables are defined
  y<-Inputpath
  z<-sheetlength

  #dummy variable for sheet (from vector Inputsheets) is defined
  x<-Inputsheets[1]

  #summary table Sum is created in global environment
  Sum<- read_excel(path=y, sheet=x)
  #dummy variable for sheet is defined
  x<-Inputsheets[2]

  #marginal radius table is created globaly and cut according to sheetlength
  MR <- read_excel(path=y, sheet=x)
  MR<-MR[1:sheetlength,]

  #if radians and degrees are given as x-axis, degrees are left out
   if(rad_degree==T){
     MR<-select(MR,-degree)
   } else {
     MR<-MR
   }
  #dummy varibale for the sheet is defined
  x<-Inputsheets[3]

  #chamber base length table is created globaly and cut according to sheetlength
  CBL <- read_excel(path=y, sheet=x)
  CBL<-CBL[1:sheetlength,]

  #dummy varibale for the sheet is defined
  x<-Inputsheets[4]

  #backbend angle table is created globaly and cut according to sheetlength
  BBA <- read_excel(path=y, sheet=x)
  BBA<-BBA[1:sheetlength,]

  #dummy varibale for the sheet is defined
  x<-Inputsheets[5]

  #septal angle table is created globaly and cut according to sheetlength
  SA <- read_excel(path=y, sheet=x)
  SA<-SA[1:sheetlength,]

  #dummy varibale for the sheet is defined
  x<-Inputsheets[6]

  #chamber area table is created globaly and cut according to sheetlength
  CA <- read_excel(path=y, sheet=x)
  CA<-CA[1:sheetlength,]

  #dummy varibale for the sheet is defined
  x<-Inputsheets[7]

  #chamber perimeter table is created globaly and cut according to sheetlength
  CP <- read_excel(path=y, sheet=x)
  CP<-CP[1:sheetlength,]

  #chamber length is calculated based from chamber area and chamber base length
  CL<-CA/CBL

  #perimeter ratio of the chamber is calculated
  PR<-CP/(4*sqrt(CA))

  #vector of specimen names is created globally
  specimens<-Sum$code


  assign("Sum",Sum,envir = .GlobalEnv)
  assign("MR",MR,envir = .GlobalEnv)
  assign("CBL",CBL,envir = .GlobalEnv)
  assign("BBA",BBA,envir = .GlobalEnv)
  assign("SA",SA,envir = .GlobalEnv)
  assign("CA",CA,envir = .GlobalEnv)
  assign("CP",CP,envir = .GlobalEnv)
  assign("CL",CL,envir = .GlobalEnv)
  assign("PR",PR,envir = .GlobalEnv)
  assign("specimens",specimens,envir = .GlobalEnv)

}
####################################################################################################################
#' ReadIn of chamberlet measurements
#'
#' Reads in the chamberlet measurements from an excel template with multiple sheets necessary for the calculation of the
#' growth-invariant and growth-independent meristic characters, which are used for analysis of morphology in nummulitid genera
#' with subdivided chambers.
#'
#' @param Inputpath source path for the excel sheet
#' @param Inputsheet string input of sheet name
#' @param v.length max. number of rows allocated per specimen
#' @param from string input of first excel column to be read in
#' @param to string input of last excel column to be read in
#'
#' @return the function returns the lists CLT_A (chamberlet area), CLT_P (chamberlet perimeter), CLT_L (chamberlet length)
#'
#' @examples
#' system.file("extdata","input_data.xlsx", package = "WoLF")
#' ReadInChamberlets(Inputpath ="C:/Users/user/Desktop/Input_data.xlsx",Inputsheet = "chamberlets",v.length = 52,from="A",to="ET")
#'
#' @export

#
ReadInChamberlets<-function(Inputpath,Inputsheet,v.length,from,to) {
  #Inputpath - path where file is
  #Inputsheets - vector with the sheet names in the template
  #from - first excel column
  #to - last excel column
  #dummy variable for Inputpath
  a<-Inputpath
  #dummy variable for Inputsheet
  b<-Inputsheet
  #dummy variable for vertical length allocated per sepcimen in the excel sheet
  c<-v.length
  #dummy variable for horizontal length allocated per specimen / currently unused
  #d<-h.length
  #starting line number for the readin
  n<-1
  #running variable to extend the line number
  m<-c
  #localy renaming Summary
  Summary<-Sum
  #creating a local vector for specimen names used in the for loop
  specimens <- Summary$code
  #creating empty dummy lists for chamberlet area (A), chamberlet perimeter (P), chamberlet length (L)
  temp3A<-list()
  temp3P<-list()
  temp3L<-list()

  #looping the readin for each specimen
  for(f in specimens) {
    #create and print the excel frame which is readin
    frame<-paste(from,(n+1),":",to,m,sep="")
    print(frame)
    #readin of data
    temp2<-read_excel(a, sheet = b,range=frame,col_names = F,trim_ws=T, col_types="numeric")
    #omitting empty columns
    temp2 <- temp2[,colSums(is.na(temp2))<nrow(temp2)]
    #tranforming to datatable
    temp2<-as_tibble(temp2)

    #selecting only the first (area) column of each chamber
    temp2A<-temp2[, seq(1,ncol(temp2),3)]
    #renaming columns
    names(temp2A)<-as.character(seq(1,ncol(temp2A),1))
    #creating a list including all specimens
    temp3A<-append(temp3A,list(temp2A))
    CLT_A<-temp3A

    #selecting only the second (perimeter) column of each chamber
    temp2P<-temp2[, seq(2,ncol(temp2),3)]
    #renaming columns
    names(temp2P)<-as.character(seq(1,ncol(temp2P),1))
    #creating a list including all specimens
    temp3P<-append(temp3P,list(temp2P))
    CLT_P<-temp3P

    #selecting only the third (legnth) column of each chamber
    temp2L<-temp2[, seq(3,ncol(temp2),3)]
    #renaming columns
    names(temp2L)<-as.character(seq(1,ncol(temp2L),1))
    #creating a list including all specimens
    temp3L<-append(temp3L,list(temp2L))
    CLT_L<-temp3L

    #shifting the frame to the next specimen
    n<-n+c
    m<-m+c



  }
  #reanming the datatables in the list according to specimens
  names(CLT_A)<-specimens
  names(CLT_P)<-specimens
  names(CLT_L)<-specimens

  #creating the lists globaly
  assign("CLT_A",CLT_A,envir = .GlobalEnv)
  assign("CLT_P",CLT_P,envir = .GlobalEnv)
  assign("CLT_L",CLT_L,envir = .GlobalEnv)
}
####################################################################################################################
#' Parameter Average
#'
#' Calculates the mean of the given biometric parameter along the mesaured chamber series per specimen.
#' Intended to be used for the parameters backbend angle, septal angle and perimeter ratio.
#'
#' @param param name of the parameter to be processed
#'
#' @return the function returns the mean parameter for each specimens as a new column in the summary table
#'
#' @examples
#'
#'
#' ParamMean()
#'
#' @export
ParamMean<-function(param){
    # param - measured parameter to be processed
    #calculates the average per column
    temp<-colMeans(param[2:ncol(param)], na.rm=TRUE)
    #creates dummy df
    temp<-as.data.frame(temp)
    #extracts string name of the parameter
    z<-deparse(substitute(param))
    #the string name is assigned as a column name
    colnames(temp)<-z
    #the average of the parameter is column bound to the summary table
    Sum<-cbind(Sum, temp)
    assign("Sum",Sum,envir = .GlobalEnv)
}
####################################################################################################################
#' Estimation of chamber length parameters ICL (Initial Chamber length) and CLex (chamber length expansions)
#'
#' Fitting the chamber number vs. chamber length series with a power function and extracting the biometric parameters.
#'
#' @param print.mod input if the model should be printed; TRUE or FALSE
#' @param path_plot path where graphs should be saved; string
#' @param dev_plot giving which extension should be used; string - if = NULL then graphs are not saved
#'
#' @return the function returns the estimated parameters ICL and CLex for each specimens as a new column in the summary table
#'
#' @examples
#'
#'
#' EstParamCL(print.mod=FALSE,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot = NULL )
#' EstParamCL(print.mod=TRUE,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot = "jpeg")
#'
#' @export
EstParamCL<-function(print.mod,path_plot,dev_plot){
  #print.mod - TRUE or FALSE if model should be printed
  #path_plot - a string giving the path where graphs should be saved
  #dev_plot - giving the extension for the exported graphs
  #if dev_plot = null then no graphs are exported

  #dummy variable inheriting the argument print model TRUE or FALSE
  y<-print.mod
  #dummy dataframe is created
  CL_param<- data.frame()
  #first chamber length is omitted in the fitting
  temp1<- CL[,2:ncol(CL)]
  #creating a local vector for specimen names used in the for loop
  specimens <- Sum$code
  #for loop using the vector of strings specimens created by the function ReadInSpiral()
  for(i in specimens){
    #only specimen i is selected
    temp2<-select(temp1,i)
    #creates a seq of chamber number
    ch<-seq(2,(nrow(temp1)+1),1)
    #column binding chamber number and chamber length
    CL<-cbind(ch,temp2)
    #dropping missing chamber values
    CL<-na.exclude(CL)
    #cutting df to maximum rows
    CL<-CL[2:nrow(CL),]
    #modelling of power function
    mod1<-lm(log(CL[,2])~CL$ch)
    #extracting parameters of power function
    CL_param<-rbind(CL_param, coef(mod1))
    #renaming the parameters
    names(CL_param)<-c("ICL","CLex")
    #predicting the theoretical fit
    th<-exp(predict(mod1,CL))
    #create a temp df for plotting
    temp_plot<-cbind(CL,th)
    #renaming temp df variables
    names(temp_plot)<-c("chambers",i,"model")
    #plotting raw data vs fit
    plot1<-(ggplot(temp_plot, aes(x=temp_plot$chambers, y = chamber_length, color = variable)) +
              geom_point(aes(y = temp_plot[,2], col = "raw")) +
              geom_line(aes(y = temp_plot[,3], col = "fit"))+
              labs(title = i,x="chambers", y= "chamber_length"))
    #if statement to save or not to save the graph
    if(is.character(dev_plot)){

      plotname<-paste("CL",i,".",dev_plot,sep="")
      ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
    } else {
      print(plot1)
    }
    #if statement to print or omit the model
    if(y==T){
      print("------------------")
      print(i)
      print("------------------")
      print(summary(mod1))
    } else {
      print(i)
    }

  }
  #transforming the parameter ICL back to its original scale
  CL_param$ICL<-exp(CL_param$ICL)
  #columnbinding the parameters to the summary table
  Sum<-cbind(Sum, CL_param)
  assign("Sum",Sum,envir = .GlobalEnv)
  }
###################################################################################################################
#' Estimation of chamber length parameters IMR (initial marginal radius) and MRe (marginal radius expansions)
#'
#' Fitting a series of rotational steps (in radians) vs. marginal radius with a power function and extracting the biometric parameters.
#'
#' @param print.mod input if the model should be printed; TRUE or FALSE
#' @param path_plot path where graphs should be saved; string
#' @param dev_plot giving which extension should be used; string - if = NULL then graphs are not saved
#'
#' @return the function returns the estimated parameters IMR and MRe for each specimens as a new column in the summary table
#'
#' @examples
#'
#'
#' EstParamMR(print.mod=FALSE,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot = NULL )
#' EstParamMR(print.mod=FALSE,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg" )
#'
#' @export
EstParamMR<-function(print.mod,path_plot,dev_plot){
  # creates dummy dataframe
  MR_param<- data.frame()
  # seperates x-values (radians)
  temp1<-select(MR,rad)
  # seperates MR values
  temp2<-select(MR,-c(rad))
  # #dummy variable inheriting the argument print model TRUE or FALSE
  y<-print.mod
  #string vector with specimen code for the for loop
  specimens<-Sum$code
  #for loop using the vector of strings specimens created by the function ReadInSpiral()
  for(i in specimens){
    # selects marginal radius of specimen i
    temp3<-select(temp2,i)
    # column binding radians and marginal radius values
    MR<-cbind(temp1,temp3)
    # exclude missing values
    MR<-na.exclude(MR)
    # modelling of exponential function
    mod1<-lm(log(MR[,2])~MR$rad)
    #extracting parameters of power function
    MR_param<-rbind(MR_param, coef(mod1))
    #renaming the parameters
    names(MR_param)<-c("IMR","MRe")
    #predicting the theoretical fit
    th<-exp(predict(mod1,MR))
    #create a temp df for plotting
    temp_plot<-cbind(MR,th)
    #renaming temp df variables
    names(temp_plot)<-c("radians",i,"model")
    #plotting raw data vs fit
    plot1<-(ggplot(temp_plot, aes(x=temp_plot$radians, y = marginal_radius, color = variable)) +
              geom_point(aes(y = temp_plot[,2], col = "raw")) +
              geom_line(aes(y = temp_plot[,3], col = "fit"))+
              labs(title = i,x="radians", y= "marginal radius"))
    #if statement to save or not to save the graph
    if(is.character(dev_plot)){

      plotname<-paste("MR",i,".",dev_plot,sep="")
      ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
    } else {
      print(plot1)
    }
    #if statement to print or omit the model
    if(y==T){
      print("------------------")
      print(i)
      print("------------------")
      print(summary(mod1))
    } else {
      print(i)
    }
  }
  #transforming the parameter IMR back to its original scale
  MR_param$IMR<-exp(MR_param$IMR)
  #columnbinding the parameters to the summary table
  Sum <-cbind(Sum, MR_param)
  assign("Sum",Sum,envir = .GlobalEnv)
}
####################################################################################################################
#' Estimation of chamber length parameters CBI (chamber base increase) and ICBL (initial chamber base length)
#'
#' Fitting the chamber number vs. chamber base length series with a power function and extracting the biometric parameters.
#'
#' @param print.mod input if the model should be printed; TRUE or FALSE
#' @param path_plot path where graphs should be saved; string
#' @param dev_plot giving which extension should be used; string - if = NULL then graphs are not saved
#'
#' @return the function returns the estimated parameters CBI and ICBL for each specimens as a new column in the summary table
#'
#' @examples
#'
#'
#' EstParamCBL(print.mod=FALSE,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot = NULL )
#' EstParamCBL(print.mod=FALSE,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg" )
#'
#' @export
EstParamCBL<-function(print.mod,path_plot,dev_plot){
    # creates dummy dataframe
    CBL_param<- data.frame()
    # seperates x-values (chamber number)
    temp1<-select(CBL,ch)
    # seperates CBL values
    temp2<-select(CBL,-ch)
    # dummy variable inheriting the argument print model TRUE or FALSE
    y<-print.mod
    # for loop using the vector of strings specimens created by the function ReadInSpiral()
    plots_CBL<-list()
    #string vector with specimen code for the for loop
    specimens<-Sum$code
    for(i in specimens){
      # selects chamber base length of specimen i
      temp3<-select(temp2,i)
      # column binding chamber number and chamber base length values
      CBL<-cbind(temp1,temp3)
      # exclude missing values
      CBL<-na.exclude(CBL)
      # formular linear function
      fo_CBL<- CBL[,2] ~ ch
      # fitting the linear function
      mod1<-lm(fo_CBL,CBL,model = T)
      # bind function parameter to dummy df
      CBL_param<-rbind(CBL_param, coef(mod1))
      # predicting the theoretical fit
      th<-predict(mod1,CBL)
      #create a temp df for plotting
      temp_plot<-cbind(CBL,th)
      #renaming temp df variables
      names(temp_plot)<-c("ch",i,"model")
      #plotting raw data vs fit
      plot1<-(ggplot(temp_plot, aes(x=temp_plot$ch, y = chamber_base_length, color = variable)) +
                geom_point(aes(y = temp_plot[,2], col = "raw")) +
                geom_line(aes(y = temp_plot[,3], col = "fit"))+
                labs(title = i,x="chambers", y= "chamber base length"))
      #if statement to save or not to save the graph
      if(is.character(dev_plot)){
        plotname<-paste("CBL",i,".",dev_plot,sep="")
        ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
      } else {
        print(plot1)
      }
      #if statement to print or omit the model
      if(y==T){
        print("------------------")
        print(i)
        print("------------------")
        print(summary(mod1))
      } else {
        print(i)
      }
    }
    #columnbinding the parameters to the summary table
    names(CBL_param)<-c("CBI","ICBL")

    Sum <-cbind(Sum, CBL_param)
    assign("Sum",Sum,envir = .GlobalEnv)
  }
#####################################################################################################################
#' Estimation of the parameter PRCt (perimeter ratio of chamberlets)
#'
#' Calculating the mean PRCtL for each specimens using the formula described in Hohenegger and Torres (2017).
#' It uses the lists read in by ReadInChamberlets().
#'
#'
#' @return the function returns the estimated parameter PRCt for each specimens as a new column in the summary table
#'
#' @examples
#'
#'
#' ParamPRCtL()
#'
#' @export
ParamPRCtL<-function(){
  #creates dummy df
  PRdf<-data.frame()
  #string vector with specimen code for the for loop
  specimens<-Sum$code
  #for loop per specimen
  for (f in specimens){
    #extract df of the specimen to be analyzed for Area and Perimeter
    tempA<-CLT_A[[f]]
    tempP<-CLT_P[[f]]
    #leaves out empty columns
    tempA <- tempA[,colSums(is.na(tempA))<nrow(tempA)]
    tempP <- tempP[,colSums(is.na(tempP))<nrow(tempP)]
    #extract namnes of tempA
    chambers<-names(tempA)
    #creates dummy vector
    PR<-c()
    #for loop per chamber
    for (i in chambers)
    {
      #cuts Area df to chamber i
      tempA1<-(tempA[,i])
      #omits NA
      tempA1<-na.omit(tempA1)
      #cut Perimeter df to chamber i
      tempP1<-(tempP[,i])
      #omits NA
      tempP1<-na.omit(tempP1)
      #calculates perimeter ratio of all chamberlets
      tempPR<- tempP1/(4*sqrt(tempA1))
      #calculates ratio between the first chamberlet and the mean value of the remaining chamberlets
      tempPR1<-tempPR[1,]/mean(tempPR[2:nrow(tempPR),])
      #creates a vector with the perimeter ratio sequence of the specimen
      PR<-c(PR,tempPR1)


    }
    # calculates the mean of PR of each specimen
    PR<-mean(PR,na.rm = T)
    #bind the result of each specimen into a df
    PRdf<-rbind(PRdf,PR)
    #removes object PR
    rm(PR)
  }
  #names the column
  names(PRdf)<-c("PRCt")
  #columnbinding the parameters to the summary table
  Sum<-cbind(Sum, PRdf)
  assign("Sum",Sum,envir = .GlobalEnv)
}
#################################################################################################################
#' Estimation of chamberlet number parameters ICtN (initial chamberlet number), CtNI (initial chamber base length) and NOC
#' (Number of operculinid chambers).
#'
#' Fitting the chamber number vs. chamberlet number series with a power function and extracting the biometric parameters. The NOC
#' of operculinid chambers is directly counted from the input data.
#'
#' @param print.mod input if the model should be printed; TRUE or FALSE
#' @param path_plot path where graphs should be saved; string
#' @param dev_plot giving which extension should be used; string - if = NULL then graphs are not saved
#'
#' @return the function returns the estimated parameters ICtN and CtNI for each specimens as a new column in the summary table
#'
#' @examples
#'
#'
#' EstParamCtN(print.mod=FALSE, path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg" )
#' EstParamCtN(print.mod=TRUE, path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg" )
#'
#' @export
EstParamCtN<-function(print.mod,path_plot,dev_plot){
  # creates dummy df to inherit parameters
  CtNdf_param<-data.frame()
  # dummy variable inheriting print.mod
  z<-print.mod
  #string vector with specimen code for the for loop
  specimens<-Sum$code
  # for loop per specimen
  for (f in specimens){
    #cuts area df to specimen
    tempA<-CLT_A[[f]]
    #creates vector with names
    chambers<-names(tempA)
    #converts vector to numeric series
    ch<-as.numeric(chambers)
    #dummy vector for CtN
    CtN<-c()
    #dummy vector for NOC
    NOC<-c()
    #for loop per chamber
    for (i in chambers)
    {
      #cuts area df to chambers
      tempA1<-(tempA[,i])
      #assings tempA1 globally - not needed
      tempA1<<-tempA1
      #removes na
      tempA1<-na.omit(tempA1)
      # index is defined as the first column of the Area dataset
      index<-na.omit(tempA1[,1])
      # index is summed up
      index<-sum(index)
      # if index is smaller than one then there are no chamberlets
      if(index<1){
        #thus the chamber is assigend zero
        tempCtN<-0
      } else{
        #or else 1
        tempCtN<-nrow(tempA1[,1])
      }
      #if the temp value is 1
      if(tempCtN==1){
        # NOC value is 1
        tempNOC<-1
      } else {
        # NOC value is 0
        tempNOC<-0
      }
      #the NOC values of chambers are bound together
      NOC<-c(NOC,tempNOC)
      # the CtN value of chambers are bount together
      CtN<-c(CtN,tempCtN)

    }
    #creates a data frame with the amount of chamberlets per chamber
    CtNdf<-data.frame(ch,CtN)
    #assigns CtNdf globally
    CtNdf<<-CtNdf
    #replace with CtN values 0 (chambers not measured) and (chambers with no chamberlets)
    #with NA
    CtNdf<-CtNdf %>% replace_with_na(replace = list(CtN = 0))
    #and exclude this NA
    CtNdf<-na.exclude(CtNdf)
    #fit exponential model
    mod1.1<-lm(log(CtNdf$CtN)~CtNdf$ch)
    #create a vector with the function coefs (parameters ICtN and CtNI) and the sum of NOC
    x<-c(coef(mod1.1),NOCact<-sum(NOC))
    #create a matrix that is filled with the coefs from above
    x<-as.data.frame(matrix(data = x,nrow = 1,ncol = 3,byrow = T))
    #if statement to print model summary or just specimen code
    if(z==T){
      print("------------------")
      print(i)
      print("------------------")
      print(summary(mod1.1))
    } else {
      print(f)
    }
    #rbinds the current parameters to dummy df
    CtNdf_param<-rbind(CtNdf_param,x )
    #predicts the fitted model
    th<-exp(predict(mod1.1,CtNdf))
    #binds them to the actual data
    temp_plot<-cbind(CtNdf,th)
    #renames the df for the plot
    names(temp_plot)<-c("chambers",f,"model")
    #plots actual vs fitted data
    plot1<-(ggplot(temp_plot, aes(x=temp_plot$chambers, y = chamberlet_number, color = variable)) +
              geom_point(aes(y = temp_plot[,2], col = "raw")) +
              geom_line(aes(y = temp_plot[,3], col = "fit"))+
              labs(title = f,x="chambers", y= "chamberlet number"))
    #if statement to save or not to save the graph
    if(is.character(dev_plot)){

      plotname<-paste("CtN",f,".",dev_plot,sep="")
      ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
    } else {
      print(plot1)
    }
  }
  #renames the df variables according to parameter names
  names(CtNdf_param)<-c("ICtN","CtNI","NOC")
  #ICtN is dropped in favour of NOC
  CtNdf_param<-select(CtNdf_param,-ICtN)
  #cbinds all parameters to the summary table
  Sum<-cbind(Sum, CtNdf_param)
  assign("Sum",Sum,envir = .GlobalEnv)
}
###################################################################################################
#' Estimation of chamberlet length parameters CtLFex (Initial Final Chamberlet Length), CtLFinc (increase of final chamberlet length)
#' and CtLD (Chamberlet Length Decrease)
#'
#' Fitting the chamberlet number vs. chamberlet length series of each chamber of a specimen with a Michaelis Menten function. The function
#' parameters of the Michealis Menten function correspond to the decrease of chamberlet length and the length of the final chamberlet within one chamber.
#' The mean of the decrease of chamberlet length for one specimen is calculated to get CtLD. The length of the final chamberlet vs. chamber series
#' is fitted by a linear function and the function parameters are extracted as the biometric parameters CtLFex and CtLFinc.
#'
#' @param startMM1 a vector with lower and upper limit for the starting values for the first Michaelis Menten parameter; default is between 100 and 200
#' @param startMM2 a vector with lower and upper limit for the starting values for the second Michaelis Menten parameter; default  is between-1 and 1
#' @param print.mod input if the model should be printed; TRUE or FALSE
#' @param path_plot path where graphs should be saved; string
#' @param dev_plot which extension should be used to save graphs of the exponential function; string - if = NULL then graphs are not saved
#' @param dev_plot_Ct which extension should be used to save graphs of the Michaelis Menten function; string - if = NULL then graphs are not saved
#' @return the function returns the estimated parameters CtLFex, CtLFinc  and CtLD for each specimens as new columns in the summary table
#'
#' @examples
#'
#'
#' EstParamCtL(startMM1=c(100,200),startMM2=c(-1,1),print.mod=F,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg",dev_plot_Ct = "jpg")
#' EstParamCtL(startMM1=c(100,200),startMM2=c(-1,1),print.mod=F,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg",dev_plot_Ct = "jpg")
#' EstParamCtL(print.mod=FALSE, path_plot ="C:/Users/user/Desktop/ggplots", dev_plot ="jpg", dev_plot_Ct = "jpg")
#' EstParamCtL(print.mod=TRUE, path_plot ="C:/Users/user/Desktop/ggplots", dev_plot =NULL, dev_plot_Ct = NULL)
#'
#' @export
EstParamCtL<-function(startMM1,startMM2,print.mod,path_plot,dev_plot,dev_plot_Ct){
#defines start parameter for the MM function
  # if statement to use default or input
  if(!exists("startMM1")){
  start1<-startMM1
  start2<-startMM2
  }  else {
  start1<-c(100,200)
  start2<-c(-1,1)
  }
#creates dummy df to inherit parameters
CtLdf_param<-data.frame()
#creates temp dummy df to inherit parameters
CtLdf_temp<-data.frame()
#dummy variable inheriting print.mod
z<-print.mod
#string vector with specimen code for the for loop
specimens<-Sum$code
#for loop per specimen
for (f in specimens){
  #cuts length df to specimen
  tempL<-CLT_L[[f]]
  #creates vector with names
  chambers<-names(tempL)
  #converts vector to numeric series
  ch<-as.numeric(chambers)
  #dummy vector for CtL
  CtL<-c()
  #creates dummy df2 for calculations per chamber
  CtLdf2<-data.frame()
  #empties temp df
  CtLdf_temp<-data.frame()
  #for loop per chamber
  for (i in chambers)
  {
    #cuts area df to chamber i
    tempL1<-(tempL[,i])
    #removes na
    tempL1<-na.omit(tempL1)
    #tempL1 is renamed
    CtL<-tempL1
    #creates a seq from 1 to number of chamberlets
    chl<-seq(1,nrow(tempL1),1)
    #creates a df from number of chamberlets and chamberlet length
    CtLdf<-data.frame(chl,CtL)
    #renames variables
    names(CtLdf)<-c("chl","CtL")
    #if statement - assingns default coef to a chamber if there are less than three chamberlets
    if (nrow(CtLdf)<3) {
      coef_temp<-data.frame(1,1)
    }
    # else the Michaelis Menten function is fitted
    else {
      fo_CtL<- CtLdf$CtL ~ (CtLF*chl)/(CtLD+chl)
      st1<-data.frame(CtLF=start1, CtLD =start2)
      mod1<-nls2(data = CtLdf,fo_CtL,start=st1,nls.control(warnOnly = T,maxiter = 100,minFactor = 0))
      #predicts the fitted model
      th<-predict(mod1,CtLdf)
      #binds them to the actual data
      temp_plot<-cbind(CtLdf,th)
      #renames the df for the plot
      names(temp_plot)<-c("chamberlets",i,"model")
      #plots actual vs fitted data
      plot1<-(ggplot(temp_plot, aes(x=temp_plot$chamberlets, y = chamberlet_length, color = variable)) +
                geom_point(aes(y = temp_plot[,2], col = "raw")) +
                geom_line(aes(y = temp_plot[,3], col = "fit"))+
                labs(title = paste(f,i,sep=""),x="chamberlets", y= "chamberlet length"))
      #if statement to save or not to save the graph
      if(is.character(dev_plot_Ct)){

        plotname<-paste("CtL",f,i,".",dev_plot,sep="")
        ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
      } else {
        print(plot1)
      }
      #if statement to print model summary or just specimen and chamber code
      if(z==T){
        print("------------------")
        print(f)
        print(i)
        print("------------------")
        print(summary(mod1))
      } else {
        print(f)
        print(i)
      }
      coef_temp<-coef(mod1)
    }
    #rbinds the current parameters to dummy df2
    CtLdf2<-rbind(CtLdf2, coef_temp)
  }
  #renames the df variables according to parameter names
  names(CtLdf2)<-c("CtLF","CtLD")
  #binds sequence of chamber numbers to the MM coefficient per chamber series
  CtLdf2<-data.frame(ch,CtLdf2)
  #default coeffs (1,1) from earlier are replace by NA
  CtLdf2<-CtLdf2 %>% replace_with_na(CtLdf2,replace = list(CtLF=1,CtLD=1))
  #the NAs are excluded
  CtLdf2<-na.exclude(CtLdf2)
  #linear formular
  fo_CtLF<- CtLdf2$CtLF ~ ch
  #fit linear model
  mod1<-lm(fo_CtLF,CtLdf2,model = T)
  #predicts the fitted model
  th<-predict(mod1,CtLdf2)
  #leaves out CtLD
  temp_plot<-CtLdf2[,1:2]
  #binds predicted data to the actual data
  temp_plot<-cbind(temp_plot,th)
  #renames the df for the plot
  names(temp_plot)<-c("chambers",f,"model")
  #plots actual vs fitted data
  plot1<-(ggplot(temp_plot, aes(x=temp_plot$chambers, y = final_chamberlet_length, color = variable)) +
            geom_point(aes(y = temp_plot[,2], col = "raw")) +
            geom_line(aes(y = temp_plot[,3], col = "fit"))+
            labs(title = f,x="chambers", y= "final chamberlet length"))
  #if statement to save or not to save the graph
  if(is.character(dev_plot)){
    plotname<-paste("CtFL",f,".",dev_plot,sep="")
    ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
  } else {
    print(plot1)
  }
  #if statement to print model summary or just specimen code
  if(z==T){
    print("------------------")
    print(f)
    print("------------------")
    print(summary(mod1))
  } else {
    print(f)
  }
  # calculates the mean of CTLD per specimen
  CtLD<-mean(CtLdf2$CtLD, na.rm = T)
  # coef from the linear models
  CtLdf_temp2<-rbind(CtLdf_temp, coef(mod1))
  # and CTLD per specimen are bound in a df
  CtLdf_temp2<-cbind(CtLdf_temp2,CtLD)
  # probably not necessary
  CtLdf_temp2<-CtLdf_temp2
  #colnames are assigned
  colnames(CtLdf_temp2)<-c("CtLFex","CtLFinc", "CtLD")
  #the biometric parameters of one specimen are rbound to the dummydf
  CtLdf_param<-rbind(CtLdf_param,CtLdf_temp2)
  #colnames are assigned
  colnames(CtLdf_param)<-c("CtLFex","CtLFinc", "CtLD")
}
  #the biometric parameters of all specimens are cbound to the summary df
Sum<-cbind(Sum, CtLdf_param)
assign("Sum",Sum,envir = .GlobalEnv)
}
#################################################################################################################
#' Estimation of chamberlet heigth parameters ImaxACtH (Initial Maximum Chamberlet Height), maxACtHinc (Maximum Chamberlet Heights Increase)
#' and posACtHinc (Increase in Position of Maximum Chamberlet Height)
#'
#' Fitting the chamberlet number vs. chamberlet height series of each chamber of a specimen with a polynomial function of 2nd order.
#' The parameters of this function are used to calculate the maximum chamberlet height and its position within a chamber. Two different
#' approaches can be chosen when calculating maximum chamberlet height. Either based solely on calculations or the theoretical maximum
#' chamberlet height and position are compared with the actual data and if they strongly deviate from each other the actual values are used.
#' The maximum chamberlet height is fitted by an exponential function and the extracted function parameters are the ImaxACtH and maxACtHinc.
#' The position of the maximum chamberlet height is fitted by an exponential function and were function parameter a assumes 1 and
#' function parameter b is extracted as posACtHinc.
#'
#'
#' @param approach_ACtH input which approach should be used TRUE or FALSe
#' @param print.mod input if the model should be printed; TRUE or FALSE
#' @param path_plot path where graphs should be saved; string
#' @param dev_plot which extension should be used to save graphs of the exponential function; string - if = NULL then graphs are not saved
#' @param dev_plot_Ct which extension should be used to save graphs of the Michaelis Menten function; string - if = NULL then graphs are not saved
#'
#'
#' @return the function returns the estimated parameters ImaxACtH, maxACtHinc and posACtHinc for each specimens as a new column in the summary table
#'
#' @examples
#'
#'
#' EstParamACtH(approach = TRUE, print.mod = FALSE, path_plot ="C:/Users/user/Desktop/ggplots", dev_plot ="jpg", dev_plot_Ct = "jpg")
#' EstParamACtH(approach = TRUE, print.mod = FALSE, path_plot ="C:/Users/user/Desktop/ggplots", dev_plot ="jpg", dev_plot_Ct = NULL)
#' EstParamACtH(approach = FALSE, print.mod = TRUE, path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg", dev_plot_Ct = NULL)
#' @export
EstParamACtH<-function(approach_ACtH,print.mod,path_plot,dev_plot,dev_plot_Ct){
#dummy variable that inherits print.mod
x<-print.mod
#dummy variable that inherits approach_ACtH value
z<-approach_ACtH
#dummy df to inherit biometric parameters
ACtHdf_param<-data.frame(1,1,1)
#name dummy df
names(ACtHdf_param)<-c("ImaxActH","maxACtHInc", "posACtHInc")
#creating a local vector for specimen names used in the for loop
specimens <- Sum$code
for (f in specimens){
  #extracts chamberlet length per specimen
  tempL<-CLT_L[[f]]
  #extracts chamberlet area per specimen
  tempA<-CLT_A[[f]]
  #calculates chamberlet height per specimen
  tempH<-tempA/tempL
  #extracts names of from tempL
  chambers<-names(tempL)
  #and converts them in to the chamber number sequence
  ch<-as.numeric(chambers)
  #dummy vector for the chamberlet height series
  ACtH<-c()
  #dummy df to keep values of maximum chamberlet height and position of maximum chamberlet height per chamber
  ACtHdf2<-data.frame()
  #dummy df to keep the biometric parameters per specimen
  ACtHdf_temp<-data.frame()
  #dummy df to create chamberlet number sequence
  CtN<-data.frame()
  #dummy df to store the maximum of the actual chamberlet height
  maxCtH<-data.frame()
  #dummy df to store the minimum of the actual chamberlet height
  minCtH<-data.frame()
  #dummy df to store the theoretical and actual position of maximum chamberlet height
  posACtH1<-data.frame()
  #for loop per chamber
  for (i in chambers)
  {
    #extract the chamber i from the chamberlet height df
    tempH1<-(tempH[,i])
    #assign it globally
    tempH1<<-(tempH1)
    #omit NA
    tempH1<-na.omit(tempH1)
    #rename it to ACtH
    ACtH<-tempH1
    #if chambers could not be measured they are assigned the default values c(1,1)
    if(sum(ACtH)==0){
      ACtH<-c(1,1)
    } else {
      ACtH<-ACtH
    }
    #chamberlet sequence is as long as needed
    chl<-seq(1,length(ACtH),1)
    #chamberlet height and chamberlet sequence are bound together as a df
    ACtHdf<-data.frame(chl,ACtH)
    #renames variables
    names(ACtHdf)<-c("chl","ACtH")
    #counts the number of chamberlets
    CtN<-rbind(CtN,nrow(ACtHdf))
    #finds the position of the maximum chamberlet heigth in the chamber
    posACtH1_temp<-ACtHdf %>% filter(ACtHdf$ACtH ==max(ACtHdf$ACtH))
    #extraxt the CtN of the maximum chamberlet heigth
    posACtH1_temp<-posACtH1_temp$chl
    #stores the actual MaxCtH value of the current chamber
    maxCtH<-rbind(maxCtH,max(ACtHdf$ACtH))
    #stores the actual MinCtH value of the current chamber
    minCtH<-rbind(minCtH,min(ACtHdf$ACtH))
    #stores the actual posACtH value of the current chamber
    posACtH1<-rbind(posACtH1,posACtH1_temp)
    #if chambers are smaller than three chamberlets no second order polynom is fitted and default values (1,1,1,) are used
    if (nrow(ACtHdf)<3) {
      coef_temp<-data.frame(1,1,1)
      names(coef_temp)<-c("b0","b1","b2")
      #if print.mod is T then a message will be given chamber i doesn't have enough chambers
      if(x==T){
        print(i)
        print((mod1<-"not enough chamberlets"))
      }
    } else
    {
      #fits the second order polynomial to chamberlet height
      mod1<-lm(ACtHdf$ACtH ~ ACtHdf$chl + I(ACtHdf$chl^2))
      #extracts the coefficients
      coef_temp<-coef(mod1)
      #predicts theoretical values
      th<-predict(mod1,ACtHdf)
      #binds together theoretical and actual values
      temp_plot<-cbind(ACtHdf,th)
      #names the variables
      names(temp_plot)<-c("chamberlets",i,"model")
      # plots chamberlet number versus chamberlet height
      plot1<-(ggplot(temp_plot, aes(x=temp_plot$chamberlets, y = chamberlet_heigth, color = variable)) +
                geom_point(aes(y = temp_plot[,2], col = "raw")) +
                geom_line(aes(y = temp_plot[,3], col = "fit"))+
                labs(title = paste(f,i,sep=""),x="chamberlets", y= "chamberlet heigth"))
      # if dev is given then plots are saved
      if(is.character(dev_plot_Ct)){

        plotname<-paste("CtH",f,i,".",dev_plot,sep="")
        ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
      } else {
        print(plot1)
      }
      # if print.mod = T then print specimen, chamber number and model, else only specimen number and chamber number
    }
    if(x==T){
      print("------------------")
      print(f)
      print(i)
      print("------------------")
      print(summary(mod1))
    } else {
      print(f)
      print(i)
    }
    #rbinds the coef of the 2nd order polynomial per chamber into a df
    ACtHdf2<-rbind(ACtHdf2, coef_temp)

  }
  # cbinds CtN, actual max, min chamberlet heigth and position of maximum chamberlet height to df
  ACtHdf2<-cbind(ACtHdf2, CtN)
  ACtHdf2<-cbind(ACtHdf2, maxCtH)
  ACtHdf2<-cbind(ACtHdf2, minCtH)
  ACtHdf2<-cbind(ACtHdf2, posACtH1)
  #renames all variables
  names(ACtHdf2)<-c("b0","b1","b2","CtN","maxCtH","minCtH","posACtH1")
  #calculates the position of maximum chamberlet heigtht based on the 2nd order polynom fit
  posACtH<- abs(ACtHdf2$b1)/(2*abs(ACtHdf2$b2))
  #rounds it up to integers
  posACtH<-round(posACtH, 0)
  #cbinds the position of maximum chamberlet height to df
  ACtHdf2<-cbind(ACtHdf2, posACtH)
  #calculates the maximum chamberlet height based on the 2nd order polynom fit
  maxACtH<- ACtHdf2$b2*ACtHdf2$posACtH**2 + ACtHdf2$b1 * ACtHdf2$posACtH + ACtHdf2$b0
  #cbinds maximum chamberlet height to df
  ACtHdf2<-cbind(ACtHdf2, maxACtH)
  #cbinds chamer number series to  df
  ACtHdf2<-cbind(ACtHdf2, ch)
  #if approach is T, calcualte position of maximum chamberlet height is ignored in favour of actual value
  #if maximum chamberlet height actual vs computed deviate too strongely
  if(z==T){
    ACtHdf2$posACtH <- ifelse(ACtHdf2$maxACtH < (ACtHdf2$maxCtH*0.5), NA, ACtHdf2$posACtH)
  } else {
    #else the calculated value is used
    ACtHdf2$posACtH <- ACtHdf2$posACtH
  }
  #values for missing chambers are set to NA
  ACtHdf2$posACtH <- ifelse(ACtHdf2$posACtH==0, NA, ACtHdf2$posACtH)
  ACtHdf2$maxACtH <- ifelse(ACtHdf2$maxACtH==1, NA, ACtHdf2$posACtH)
  #if approach is T, calculated maximum chamberlet height is ignored in favour of actual value if they deviate too strongely
  if(z==T){
    ACtHdf2$maxACtH <- ifelse(ACtHdf2$maxACtH < (ACtHdf2$maxCtH*0.5), ACtHdf2$maxCtH, ACtHdf2$maxACtH)
  } else {
    ACtHdf2$maxACtH <- ACtHdf2$maxACtH
  }
  #if chamber has no chamberlets position of maximum chamberlet height is set to 1
  ACtHdf2$posACtH <- ifelse(ACtHdf2$CtN==1, 1, ACtHdf2$posACtH)
  #if chamber has no chamberlets maximum chamberlet height is set to actual value
  ACtHdf2$maxACtH <- ifelse(ACtHdf2$CtN==1, ACtHdf2$maxCtH, ACtHdf2$maxACtH)
  #prepares df for fitting using chamber number and position of max chamberlet height
  ACtHdf3_1<-select(ACtHdf2,c(ch,posACtH))
  #renames variables
  names(ACtHdf3_1)<-c("ch","posACtH")
  #omits NA
  ACtHdf3_1<-na.omit(ACtHdf3_1)
  #prepares df for fitting using chamber numer and max chamberlet height
  ACtHdf3_2<-select(ACtHdf2,c(ch,maxACtH))
  #renames variables
  names(ACtHdf3_2)<-c("ch","maxACtH")
  #excludes na
  ACtHdf3_2<-na.exclude(ACtHdf3_2)
  #fits exponential model to max chamber height
  mod2<-lm(log(ACtHdf3_2$maxACtH)~ACtHdf3_2$ch)
  #predicts theoretical fit
  th<-exp(predict(mod2,ACtHdf3_2))
  #binds actual data and theoretical fit in a df
  temp_plot<-cbind(ACtHdf3_2,th)
  #renames variables
  names(temp_plot)<-c("chambers",f,"model")
  #plot actual vs fitted data
  plot1<-(ggplot(temp_plot, aes(x=temp_plot$chambers, y = max_chamberlet_heigth, color = variable)) +
            geom_point(aes(y = temp_plot[,2], col = "raw")) +
            geom_line(aes(y = temp_plot[,3], col = "fit"))+
            labs(title = f,x="chambers", y= "max chamberlet heigth"))
  #if extension is given plot is saved
  if(is.character(dev_plot)){

    plotname<-paste("maxACtH",f,".",dev_plot,sep="")
    ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
  } else {
    print(plot1)
  }
  #if print.mod is T models are printed else just specimen code
  if(x==T){
    print("------------------")
    print(f)
    print("maxACtH")
    print("------------------")
    print(summary(mod2))
  } else {
    print(f)
  }
  #function parameters for maxACtH are extracted
  ACtHdf_temp<-rbind( ACtHdf_temp,coef(mod2))
  #and named accordingly
  names(ACtHdf_temp)<-c("ImaxACtH","maxACtHInc")
  #ImaxACtH is transfered back to original scale
  ACtHdf_temp$ImaxACtH<-exp(ACtHdf_temp$ImaxACtH)
  #fits exponential model to position of maximum chamberlet height
  mod3<-lm(log(ACtHdf3_1$posACtH)~ACtHdf3_1$ch)
  #predicts theoretical fir for model
  th<-exp(predict(mod3,ACtHdf3_2))
  #binds actual and theoretical fit together
  temp_plot<-cbind(ACtHdf3_1,th)
  #renames variables
  names(temp_plot)<-c("chambers",f,"model")
  #creates plot actual vs fitted data
  plot1<-(ggplot(temp_plot, aes(x=temp_plot$chambers, y = pos_chamberlet_heigth, color = variable)) +
            geom_point(aes(y = temp_plot[,2], col = "raw")) +
            geom_line(aes(y = temp_plot[,3], col = "fit"))+
            labs(title = f,x="chambers", y= "pos chamberlet heigth"))
  #if extension is given then plots are saved
  if(is.character(dev_plot)){

    plotname<-paste("posACtH",f,".",dev_plot,sep="")
    ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
  } else {
    print(plot1)
  }
  #creates dummy dataframe
  coef_temp<-data.frame()
  #rbinds coef from pos mac chamberlet height model
  coef_temp<-rbind(coef_temp,coef(mod3))
  #renames variables
  names(coef_temp)<-c("XX","posACtHInc")
  #binds specimen results to temp df
  ACtHdf_temp<-cbind( ACtHdf_temp,coef_temp)
  #drops the first coeff of the exponential function fit to position of chamberlet height, because initial pos max chamberlet height is always 1
  ACtHdf_temp<-select(ACtHdf_temp,-XX)
  #renames variables according to character names
  names(ACtHdf_temp)<-c("ImaxActH","maxACtHInc", "posACtHInc")
  #rbind specimen results to the overall df
  ACtHdf_param<-rbind(ACtHdf_param, ACtHdf_temp)
  #if print.mod is T then pos max chamberlet height is printed
  if(x==T){
    print("------------------")
    print(f)
    print("posACtH")
    print("------------------")
    print(summary(mod3))
  } else {
    print(f)
  }
}
#results are bound to summary table
Sum<-cbind(Sum, ACtHdf_param[2:nrow(ACtHdf_param),])
assign("Sum",Sum,envir = .GlobalEnv)
}

####################################################################################################################
#' ReadIn of test thickness measurements
#'
#' Reads in the test thickness measurements from an excel template necessary for the calculation of the
#' growth-invariant and growth-independent meristic characters used for analysis of morphology of planispirally
#' coiling foraminifera in axial sections. Follows the approach by Eder et al. (2018)
#'
#'
#' @param Inputpath source path for the excel sheet
#' @param Inputsheet vector containing names of existing sheets in the template; sheet names may be changed, but the
#' order of the sheets must remain the same Summary -> marginal radius -> chamber base length -> backbend angle ->
#' septal angle -> chamber area -> chamber perimeter
#' @param from start column in excel
#' @param to end column in excel
#' @param nolines number of lines to read in
#' @param nopoints number of measurements per specimen
#'
#' @return the function returns the list TFdf, containing a dataframes with the varaiables MR (marginal radius), Th (Thickness),
#' Mlth (mediolateral thickness) per specimen, and a vector with all specimen codes.
#'
#' @examples
#' system.file("extdata","input_data.xlsx", package = "WoLF")
#' ReadInTF(Inputpath=C:/Users/user/Desktop/TemplateTF.xlsx",Inputsheet="Sheet1",from = "A",to="D",nolines = 101,nopoints = 5)
#'
#' @export
ReadInTF<-function(Inputpath,Inputsheet,from,to,nolines,nopoints){
  #dummy variable - number of first row is 2 since variable names exist
  n<-2
  #dummy varaible - number of last row for the first specimen
  m<-n+(nopoints-1)
  #dummy variable - iteration numbers
  it<-1
  #dummy list
  TFdf<-list()
  #repeat statment for read-in
  repeat {
    #creates the input frame to read in e.g., A2:B2
    frame<-paste(from,n,":",to,m,sep="")
    #reads data from excel
    temp1<- read_excel(Inputpath, Inputsheet, col_names = FALSE, range = frame)
    #renames variables
    names(temp1)<-c("specimen","r","Th", "MTh")
    #adds the data per specimen to the list - first column is omitted includes only specimen code
    temp2<-list(temp1[,2:ncol(temp1)])
    #name of df in the list is the specimen code
    names(temp2)<-temp1[1,1]
    #if it is the first iteration temp2 becomes the permanent list
    if(it==1){
      TFdf<-temp2
      #else temp2 is appended to the permanent list
    } else {
      TFdf<-append(TFdf,temp2)
    }
    #frame jumps to the next specimen
    n<-n+nopoints
    m<-m+nopoints
    #iteration +1
    it<-it+1
    #if the frame reaches the maximum amount of lines then stop
    if (m > nolines){
      break
    }
  }
  #assigns list globally
  TFdf<-TFdf
  assign("TFdf",TFdf,envir = .GlobalEnv)
  #assings vector with specimens names globally
  specimens<-names(TFdf)
  assign("specimens",specimens,envir = .GlobalEnv)
}

######################################################################################################
#' Estimation of morphometric characters the morphometric characters ThMR3 (total thickness at 3 mm marginal radius),
#' MaxMlTh(maximal mediolateral thickness), MRmax(marginal radius at MaxMlth) and F (the Flattening ratio).
#'
#' This function fits marginal radius versus thickness with a power function and marginal radius versus mediolateral thickness
#' with a composite function after Eder et al (2018). It extracts the function parameters to consecutively calculate the morphometric
#' character THMR3 based on the power function and MaxMlTh, MRmax and F based on the composite function.
#'
#' @param estMTH input if MlTh should be fitted or only Th; TRUE or FALSE value
#' @param para_hybrid vector containing start parameter for the composite function
#' @param para_power vector containing start parameter for the power function
#' @param print.mod input if the model should be printed; TRUE or FALSE
#' @param path_plot path where graphs should be saved; string
#' @param dev_plot which extension should be used to save graphs of the exponential function; string - if = NULL then graphs are not saved
#'
#' @return the function returns the list TFdf, containing a dataframes with the varaiables MR (marginal radius), Th (Thickness),
#' Mlth (mediolateral thickness) per specimen, and a vector with all specimen codes
#'
#' @examples
#'
#'
#' EstTF(estMTH = FALSE,print.mod=FALSE,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg" )
#' EstTF(estMTH = TRUE,print.mod=TRUE,path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg" )
#' EstTF(estMTH = TRUE,para_hybrid=c(b0=2e-18,b1=5,b2=-0.005,b3=500),para_power=c(a=300,b=0.05,c=-700),print.mod=TRUE, path_plot ="C:/Users/user/Desktop/ggplots",dev_plot ="jpg")
#'
#' @export
EstTF<-function(estMTH,para_hybrid,para_power,print.mod,path_plot,dev_plot){
  #dummy varaible inherits print.mod
  j<-print.mod
  #dummy df for end results
  TF_param<-data.frame()
  #if start parameter are given use them, else use default
  if(!exists("para_hybrid"))
  {
    start_h<-para_hybrid
  } else {
    start_h<-c(b0=2e-18,b1=5,b2=-0.005,b3=500)
  }
  #if start parameter are given use them, else use default
  if(!exists("para_power"))
  {
    start_p<-para_power
  } else {
    start_p<-c(a=300,b=0.05,c=-700)
  }
  #loops thorugh specimens
  for(i in specimens){
    #cuts input dataframe to specimen
    TFdf1<-TFdf[[i]]
    #if statement to fit mediolateral thickness
    if(estMTH==T){
    #composite function formula
    fo_MtH<-TFdf1$MTh~b0*(TFdf1$r+b3)**b1*exp(b2*(TFdf1$r+b3))
    #fits function to data
    mod1<-nlsLM(fo_MtH, start=start_h,TFdf1,control= nls.lm.control(maxiter=500, maxfev =1000))
    #if print.mod is on print specimen and model else only specimen
    if(j==T){
      print("------------------")
      print(i)
      print("------------------")
      print(summary(mod1))
    } else {
      print(i)
    }
    #predict theoretical fit
    theo<-predict(mod1,TFdf1)
    #cut plot df to radius and mediolateral thickness
    temp_plot<-select(TFdf1,c(r,MTh))
    #cbinds theoretical fit to actual data
    temp_plot<-cbind(temp_plot,theo)
    #renames variable
    names(temp_plot)<-c("radius",i,"model")
    #plots actual data vs fit
    plot1<-(ggplot(temp_plot, aes(x=temp_plot$radius, y = mediolateral_thickness, color = variable)) +
              geom_point(aes(y = temp_plot[,2], col = "raw")) +
              geom_line(aes(y = temp_plot[,3], col = "fit"))+
              labs(title = i,x="radius", y= "mediolateral thickness"))
    #if extension is not NULL then plot is saved
    if(is.character(dev_plot)){

      plotname<-paste("MTh",i,".",dev_plot,sep="")
      ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
    } else {
      print(plot1)
    }
    #predicts model for marginal radius values - not needed anymore
    predict(mod1,TFdf1$r)
    #extract model coef
    X<-coef(mod1)
    #makes a df with the model coef
    X<-matrix(X,nrow = 1,ncol = 4,byrow = T)
    X<-data.frame(X)
    #renames the coef
    names(X)<-c("b0","b1","b2","b3")
    #assigns the coeff to variables
    b0<-X$b0
    b1<-X$b1
    b2<-X$b2
    b3<-X$b3
    #calcualte the MRmax
    rm<- -((b2*b3+b1)/b2)
    #calculates the MaxMlTH
    MaxMTH<- b0*(rm+b3)**b1*exp(b2*(rm+b3))
    #calculates F
    F_<-MaxMTH/rm
    #if mediolateral thickness is not fitted the moprhometric parameters are set to 0
    } else {
      rm<-0
      MaxMTH<-0
      F_<-0
    }
    #formular for power function plus offset parameter c
    fo_Th<-TFdf1$Th~a*(TFdf1$r**b)+c
    #fit model to actual data
    mod2<-nlsLM(fo_Th, start=start_p,TFdf1,control=nls.lm.control(maxiter=500))
    #if print.mod is on print specimen and model else only specimen
    if(j==T){
      print(summary(mod1))
    } else {

      print("---------------")
    }
    #predict theoretical fit
    theo<-predict(mod2,TFdf1)
    #cut plot df to radius and thickness
    temp_plot<-select(TFdf1,c(r,Th))
    #cbinds theoretical fit to actual data
    temp_plot<-cbind(temp_plot,theo)
    #renames variable
    names(temp_plot)<-c("radius",i,"model")
    #plots actual data vs fit
    plot1<-(ggplot(temp_plot, aes(x=temp_plot$radius, y = mediolateral_thickness, color = variable)) +
              geom_point(aes(y = temp_plot[,2], col = "raw")) +
              geom_line(aes(y = temp_plot[,3], col = "fit"))+
              labs(title = i,x="radius", y= "thickness"))
    #if extension is not NULL then plot is saved
    if(is.character(dev_plot)){
      plotname<-paste("Th",i,".",dev_plot,sep="")
      ggsave(plotname,plot=plot1,device=dev_plot,path=path_plot,dpi=300)
    } else {
      print(plot1)
    }
    #extract model coef
    X<-coef(mod2)
    #makes a df with the model coef
    X<-matrix(X,nrow = 1,ncol = 3,byrow = T)
    X<-data.frame(X)
    #renames the coef
    names(X)<-c("a","b","c")
    #assigns the coeff to variables
    a<-X$a
    b<-X$b
    c<-X$c
    #calculates the morphometric character ThMR3
    Th3<-a*3000**b+c
    #binds characters for mediolateral thickness in a df
    TF_temp<-data.frame(rm,MaxMTH,F_)
    #attaches character for thickness to df
    TF_temp<-cbind(TF_temp,Th3)
    #binds the results per specimen to the overall df
    TF_param<-rbind(TF_param,TF_temp)
    #assigns the overall df globally
    TF_param<-TF_param
    assign("TF_param",TF_param,envir = .GlobalEnv)
  }
}
###############################################################################
