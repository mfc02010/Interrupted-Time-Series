library(shiny)
library(nlme)

#source("C:\\Users\\Sony\\Dropbox\\UCI\\RESEARCH\\PartialCoherence FB\\RCode\\functions.R")

# Define server logic required to summarize and view the selected
# dataset

month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

shinyServer(function(input, output,session) {
  
  ##Load data
  simulationData <- eventReactive(input$simulation, {
    inFile <- input$file1
    data= read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    unit.all = unique(data[,2])
    unit.all = as.array(unit.all)
    u = input$Hospital_num
    subdata = data[data$unit == unit.all[u],]
    T = dim(subdata)[1]
    Y = subdata[,3]
    originalTime = as.character(subdata[,1])
    # let user enter t0
    t0 = input$t0
    # let user enter L
    L1 = input$L1
    L2 = input$L2
    t.c = t0 + (-L1):L2
    
    # let user enter starting month: 1-12
    month.start = input$month
    # let user enter starting year: e.g 2008
    year.start = input$year
    
    year.total = floor(T/12)+1
    year = year.start + 0:year.total
    year = as.character(year)
    label.init = apply(expand.grid(month, year), 1, paste, collapse="-")
    label.lik = label.init[month.start-1+t.c]
    label = label.init[month.start + 6*(0:(floor(T/6)-1))]
    at.label = month.start + 6*(0:(floor(T/6)-1))
    
    return(list(originalTime=originalTime,Y=Y, T=T, t0 = t0, L1=L1, L2=L2 ,unit.all=unit.all,label.init=label.init,label.lik=label.lik, u=u, at.label=at.label, label=label))
    
  })
  
  ##PLot time series
  observe({
    inFile <- input$file1
    if (is.null(inFile)==FALSE){
      data= read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      unit.all = unique(data[,2])
      unit.all = as.array(unit.all)
      u = input$Hospital_num;
      updateTextInput(session, "nameHospital", value = unit.all[u])
    }
    #unit.all = simulationData()$unit.all;
    
  })
  
  plotExploratoryData <- eventReactive(input$plotData,{
    Y=simulationData()$Y; T=simulationData()$T; 
    t0 = simulationData()$t0; L1=simulationData()$L1;L2=simulationData()$L2;
    unit.all=simulationData()$unit.all; u=simulationData()$u; 
    at.label=simulationData()$at.label; label=simulationData()$label;
    
    plot(Y, main=unit.all[u],type="o",col="blue",lwd=2,xaxt = 'n',xlab="",ylab="")
    axis(1, at=at.label, labels=FALSE)
    text(x=at.label, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
         labels=label, srt=45, adj=1, xpd=TRUE,cex=1.1)
    abline(v=t0, col="grey", lwd=2)
  })
  
  output$plot <- renderPlot({
    plotExploratoryData()
  })
  
  
  ###Plot Log-Likelihood
  LogLikelihood <- eventReactive(input$analyze, {
    Y=simulationData()$Y; T=simulationData()$T; 
    t0 = simulationData()$t0; L1=simulationData()$L1;L2=simulationData()$L2;
    
    t.c = t0 + (-L1):L2
    
    loglik = rep(0,L1+L2+1)
    for(i in 1:(L1+L2+1)) {
      t.temp = t.c[i]
      t1 = 1:(t.temp-1)
      t2 = t.temp:T
      Y1.temp = Y[t1]
      Y2.temp = Y[t.temp:T]
      rslt1 = gls(Y1.temp ~ t1, correlation = corAR1(form=~1))
      rslt2 = gls(Y2.temp ~ t2, correlation = corAR1(form=~1))
      loglik[i] = rslt1$logLik + rslt2$logLik
    }
    
    
    t.est = t.c[which.max(loglik)]
    t1 = 1:(t.est-1)
    t2 = t.est:T
    Y1.est = Y[t1]
    Y2.est = Y[t.est:T]
    rslt1 = gls(Y1.est ~ t1, correlation = corAR1(form=~1))
    rslt2 = gls(Y2.est ~ t2, correlation = corAR1(form=~1))
    
    label.init=  simulationData()$label.init;
    t.est.name=label.init[t.est]
    
    tableResult <- rbind(as.data.frame(summary(rslt1)$tTable),as.data.frame(summary(rslt2)$tTable))
    
    tableResult=round(tableResult,2)
    tableResult=rbind(tableResult[1:2,],sigma_1=c(round(rslt1$sigma,2),"","",""),tableResult[-(1:2),])
    tableResult=rbind(tableResult,sigma_2=c(round(rslt2$sigma,2),"","",""),
                      t.estimate = c(paste(t.est," ( ",t.est.name," ) "),"","",""))
    x<-rslt1$modelStruct$corStruct 
    tableResult=rbind(tableResult[1:3,],phi_1=c(round(coef(x,unconstrained=FALSE),2),"","",""),tableResult[-(1:3),])
    x<-rslt2$modelStruct$corStruct 
    tableResult=rbind(tableResult[1:7,],phi_2=c(round(coef(x,unconstrained=FALSE),2),"","",""),tableResult[-(1:7),])
    
    customTable  = matrix(0,9,5)
    customTable[,1]=c("Intercept", "time", "std_dev_noise", "AR_coeff","Intercept", "time", "std_dev_noise", "AR_coeff","est_change_point")
    customTable[,2:5] = as.matrix(tableResult[,1:4])
    customTable = as.data.frame(customTable)
    colnames(customTable)=c("","Est","Std.Error","t-value","p-value")
    
    ##export the data
    originalTime=simulationData()$originalTime;
    exportTable = cbind(originalTime,Y,c(rslt1$fitted,rslt2$fitted))
    exportTable = as.data.frame(exportTable)
    colnames(exportTable) = c("Time", "Original Data", "Fitted Data")
    
    unit.all=simulationData()$unit.all; u=simulationData()$u;
    fileName = paste(unit.all[u], " Regression Results")  
    
    #tempTable = matrix(0,1,5)
    analysisTable = customTable
    analysisTable[,1]=c("Pre - Intercept", "Pre - time", "Pre - std_dev_noise", "Pre - AR_coeff","Post - Intercept", "Post -  time", "Post - std_dev_noise", "Post - AR_coeff","Est_change_point")
    
    
    
    return(list(Y=Y, fileName=fileName, exportTable=exportTable, t.est=t.est, t1=t1, t2=t2,rslt1 = rslt1,rslt2 = rslt2, Y1.est=Y1.est, Y2.est=Y2.est,loglik=loglik,tableResult=customTable,analysisTable=analysisTable))
  })
  
  ####### Export Regression Results.
  output$exportResults <- downloadHandler(
    filename = function() { 
      paste( LogLikelihood()$fileName, '.csv', sep='')
    },
    content = function(file) {
      write.csv(LogLikelihood()$exportTable, file)
    }
  )
  ####### Export Analysis Results.
  output$exportAnalysis <- downloadHandler(
    filename = function() { 
      paste( "Analysis Results", '.csv', sep='')
    },
    content = function(file) {
      write.csv(LogLikelihood()$analysisTable, file)
    }
  )
  ####### Export Inference Results.
  output$exportInference <- downloadHandler(
    filename = function() { 
      paste( "Inference Results", '.csv', sep='')
    },
    content = function(file) {
      tempTable= rbind(c("Change in Slope",""),as.matrix(ChangeSlope()),
                       c("Change in Noise Variance",""),as.matrix(ChangeWN()),
                       c("Change in Level",""),as.matrix(ChangeLevel()))
      tempTable = as.data.frame(tempTable)
      colnames(tempTable)=c("","Value")
      write.csv(tempTable, file)
    }
  )
  
  
  
  output$plotLogLikelihood <- renderPlot({
    loglik = LogLikelihood()$loglik
    Y=simulationData()$Y; T=simulationData()$T; 
    t0 = simulationData()$t0; L1=simulationData()$L1;L2=simulationData()$L2;
    label.lik=  simulationData()$label.lik;
    
    at.lik = 1:(L1+L2+1)
    
    par(mar=c(6,4,3,3),cex.lab=1.4,cex.axis=1.4,cex.main=1.4,las=1)
    plot(loglik,type="o",col="purple",lwd=2,xaxt = 'n',xlab="",ylab="",cex.lab=1.2,
         cex.axis=1.2, main="Log-likelihood of Changing Point Candidates")
    axis(1, at=at.lik, labels=FALSE)
    text(x=at.lik, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
         labels=label.lik, srt=45, adj=1, xpd=TRUE,cex=1.1)
    abline(v=which.max(loglik), col=2, lwd=2)
    
  })
  
  output$plotEstimateLines <- renderPlot({
    
    Y = LogLikelihood()$Y
    rslt1 = LogLikelihood()$rslt1
    rslt2 = LogLikelihood()$rslt2
    t.est =  LogLikelihood()$t.est
    unit.all=simulationData()$unit.all; u=simulationData()$u; t0 = simulationData()$t0;
    at.label=simulationData()$at.label; label=simulationData()$label;
    
    plot(Y, main=unit.all[u],type="o",col="blue",lwd=2,xaxt = 'n',xlab="",ylab="")
    axis(1, at=at.label, labels=FALSE)
    text(x=at.label, y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
         labels=label, srt=45, adj=1, xpd=TRUE,cex=1.1)
    lines(c(rslt1$fitted,rslt2$fitted),type="o", lwd=2, pch=18, col="purple")
    abline(v=t0, col="grey", lwd=2) # theoretical changing point
    abline(v=t.est, col=2, lwd=2) # estimated changing point
  })
  
  output$resid1 <- renderPlot({
    rslt1 = LogLikelihood()$rslt1; unit.all=simulationData()$unit.all; u=simulationData()$u;
    hist(rslt1$resid, breaks=20,ylab="" , main=unit.all[u], xlab = "Residuals for Pre", cex.lab=1.5)
  })
  
  output$resid2 <- renderPlot({
    rslt2 = LogLikelihood()$rslt2; unit.all=simulationData()$unit.all; u=simulationData()$u;
    hist(rslt2$resid, breaks=20, ylab="",  main=unit.all[u],xlab="Residuals for Post", cex.lab=1.5)
  })
  
  output$ACF1 <- renderPlot({
    rslt1 = LogLikelihood()$rslt1
    acf(rslt1$resid, ylim=c(-.5,1), main="Autocorrelation Function for Pre")
  })
  
  output$ACF2 <- renderPlot({
    rslt2 = LogLikelihood()$rslt2
    acf(rslt2$resid, ylim=c(-0.5,1), main="Autocorrelation Function for Post")
  })
  
  #Table of estimate
  
  output$table1 <- renderTable({
    LogLikelihood()$tableResult[c(1:4,9),]
  })
  
  output$table2 <- renderTable({
    LogLikelihood()$tableResult[c(5:8),]
  })
  
  
  ###########
  #Inference:
  ###########
  
  ###Change in slope
  ChangeSlope <- eventReactive(input$showChangeSlope, {
    rslt1=LogLikelihood()$rslt1
    rslt2=LogLikelihood()$rslt2
    
    beta11.est = rslt1$coef[2]
    beta12.est = rslt2$coef[2]
    var.est = rslt1$varB[2,2]+rslt2$varB[2,2]
    slope.stat = abs(beta11.est-beta12.est)/sqrt(var.est)
    pvalue1 = 2*(1-pnorm(slope.stat))
    ci.low = beta12.est - beta11.est - 1.96*sqrt(var.est)
    ci.up = beta12.est - beta11.est + 1.96*sqrt(var.est)
    
    tableResult = matrix(0,5,2)
    tableResult[,1] = c("Pre: ","Post: ","Diff = Post - Pre: ","95% CI for Diff: ","p-value: ")
    tableResult[,2] = c(round(beta11.est,2), round(beta12.est,2), round(beta12.est,2)-round(beta11.est,2) , paste("( ", as.character(round(ci.low,2))," , ", as.character(round(ci.up,2)), " )"), round(pvalue1,2))
    tableResult=as.data.frame(tableResult)
    colnames(tableResult)=c("","Value")
    return(tableResult)
  })
  
  output$tableChangeSlope <- renderTable({
    ChangeSlope()
  })
  
  ###Change in AR coefficients
  ChangeAR <- eventReactive(input$showChangeAR, {
    Y1.est=LogLikelihood()$Y1.est
    Y2.est=LogLikelihood()$Y2.est
    t1 = LogLikelihood()$t1
    t2 = LogLikelihood()$t2
    t.est = LogLikelihood()$t.est
    Y=simulationData()$Y; T=simulationData()$T;  
    t0 = simulationData()$t0; L=simulationData()$L;
    
    ols1 = lm(Y1.est~t1)
    ols2 = lm(Y2.est~t2)
    ROne = ols1$residuals
    RTwo = ols2$residuals
    RRed = c(ols1$residuals, ols2$residuals)
    
    modAR1 = lm(ROne[-1] ~ 0 + ROne[-length(ROne)])
    modAR2 = lm(RTwo[-1] ~ 0 + RTwo[-length(RTwo)])
    modRed = lm(RRed[-1] ~ 0 + RRed[-length(RRed)])
    
    RSSF = sum((modAR1$residuals)^2) + sum((modAR2$residuals)^2) 
    RSSR = sum((modRed$residuals)^2)
    
    FstatRF = ((RSSR - RSSF)/2)/(RSSF/(T-2))
    pvalueRF = 2*pf(FstatRF, 2, T-2, lower.tail=FALSE)
    
    diffAR = modAR1$coefficients - modAR2$coefficients
      
   
    tableResult = matrix(0,4,2)
    tableResult[,1] = c("Pre:", "Post:", "Diff = Post - Pre: ", "p-value: ")
    tableResult[,2] = c(round(modAR1$coefficients, 2), round(modAR2$coefficients, 2), round(diffAR,2) , as.character(round(pvalueRF,2)))
    tableResult= as.data.frame(tableResult)
    colnames(tableResult)=c("","Value")
    return(tableResult)
  })
  output$tableChangeAR<- renderTable({
    ChangeAR()
  })
  
  ###Change in noise variance
  ChangeWN <- eventReactive(input$showChangeWN, {
    Y1.est=LogLikelihood()$Y1.est
    Y2.est=LogLikelihood()$Y2.est
    t1 = LogLikelihood()$t1
    t2 = LogLikelihood()$t2
    t.est = LogLikelihood()$t.est
    T=simulationData()$T; 
    t0 = simulationData()$t0; L=simulationData()$L;
    rslt1 = LogLikelihood()$rslt1
    rslt2 = LogLikelihood()$rslt2
    sse1 <- round(rslt1$sigma,2)
    sse2 <- round(rslt2$sigma,2)

    df1 = t.est-3
    df2 = T-t.est-1
    var.stat = sse1/sse2
    if(var.stat>1) {
      pvalue2 = 2*pf(var.stat,df1,df2, lower.tail=FALSE)
      var.low = var.stat/qf(0.975,df1,df2)
      var.up = var.stat/qf(0.025,df1,df2)
    }
    if(var.stat<1) {
      pvalue2 = 2*pf(1/var.stat,df1,df2, lower.tail=FALSE)
      var.low = 1/((1/var.stat)/qf(0.975,df1,df2))
      var.up = 1/((1/var.stat)/qf(0.025,df1,df2))
    }
    
    tableResult = matrix(0,5,2)
    tableResult[,1] = c("Pre: ","Post: ","Ratio = Post / Pre: ","95% CI for Ratio: ","p-value: ")
    tableResult[,2] = c(round(sse1,2), round(sse2,2), round(var.stat,2) , paste("( ", as.character(round(var.low,2))," , ", as.character(round(var.up,2)), " )"), as.character(round(pvalue2,2)))
    tableResult=as.data.frame(tableResult)
    colnames(tableResult)=c("","Value")
    return(tableResult)
  })
  output$tableChangeWN<- renderTable({
    ChangeWN()
  })
  
  ###Change in level
  ChangeLevel <- eventReactive(input$showChangeLevel, {
    rslt1=LogLikelihood()$rslt1
    rslt2=LogLikelihood()$rslt2
    t.est = LogLikelihood()$t.est
    
    level = rslt2$fitted[1] - rslt1$fitted[t.est-1] #level = level.post-level.pre
    #estimated covariance matrix for beta01, beta11, beta02, beta12
    cov = matrix(0,4,4)
    cov[1:2,1:2] = rslt1$varB
    cov[3:4,3:4] = rslt2$varB
    #level.var is variance of level
    level.var = t(c(1,t.est-1,1,1))%*%cov%*%c(1,t.est-1,1,1)
    #compute 95% CI for level
    level.low = level - 1.96*sqrt(level.var)
    level.high = level + 1.96*sqrt(level.var)
    
    tableResult = matrix(0,4,2)
    tableResult[,1] = c("Post: ","Pre: ","Level Diff = Post - Pre: ","95% CI for Level Diff: ")
    tableResult[,2] = c(round(rslt2$fitted[1],2) , round(rslt1$fitted[t.est-1],2) , round(level,2) , paste("( ", as.character(round(level.low,2))," , ", as.character(round(level.high,2)), " )"))
    tableResult=as.data.frame(tableResult)
    colnames(tableResult)=c("","Value")
    return(tableResult)
  })
  output$tableChangeLevel<- renderTable({
    ChangeLevel()
  })
})