library(shiny)
library(nlme)

#Preliminary function

lcont_CI_2mod <- function( model1, model2, cont){
  ## gives estimate, 95% CI, tvalue, and p-value for any linear contrast on the 
  ## concatenated parameters of two models. 
  coef <- cont %*% c(model1$coef, model2$coef)
  VAR <- diag(length(cont));  b <- length(model1$coef)
  VAR[1:b, 1:b] <- vcov( model1 )
  VAR[(b+1):(length(cont)), (b+1):(length(cont))] <- vcov( model2 )
  
  se <- sqrt(cont %*% VAR %*% cont)
  tvalue <- (coef) / se
  df <- length(c(model1$fitted, model2$fitted)) - length(coef)
  pvalue <- 2*(1-pt(abs(tvalue), df))
  
  ci95.lo <- coef - qt(.975, df) * se
  ci95.hi <- coef+ qt(.975, df) * se
  est <- coef
  
  rslt <- round( cbind( est, ci95.lo, ci95.hi, tvalue, pvalue ), 4 )
  colnames( rslt ) <- c("Estimate", "ci95.left", "ci95.right", "t value", "Pr(>|t|)")			
  
  rslt
}

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
    
    resS = lcont_CI_2mod(rslt1, rslt2, c(0, -1, 0, 1))

    tableResult = matrix(0,5,2)
    tableResult[,1] = c("Pre: ","Post: ","Diff = Post - Pre: ","95% CI for Diff: ","p-value: ")
    tableResult[,2] = c(round(beta11.est,2), round(beta12.est,2), round(resS[1],2) , paste("( ", as.character(round(resS[2],2))," , ", as.character(round(resS[3],2)), " )"), round(resS[5],2))
    tableResult=as.data.frame(tableResult)
    colnames(tableResult)=c("","Value")
    return(tableResult)
  })
  
  output$tableChangeSlope <- renderTable({
    ChangeSlope()
  })
  
  ###Change in AR coefficients
  ChangeAR <- eventReactive(input$showChangeAR, {
    T=simulationData()$T;
    Y1.est=LogLikelihood()$Y1.est
    Y2.est=LogLikelihood()$Y2.est
    t1 = LogLikelihood()$t1
    t2 = LogLikelihood()$t2
    rslt1=LogLikelihood()$rslt1
    rslt2=LogLikelihood()$rslt2
    Y3.est = c(Y1.est, Y2.est)
    
    t3 = c(t1, t2)
    rslt3 = gls( Y3.est ~ t3, correlation = corAR1(form=~1))
    
    beta1.est = rslt1$coef
    beta2.est = rslt2$coef
    beta3.est = rslt3$coef
    x<-rslt1$modelStruct$corStruct
    PHI1.est = coef(x,unconstrained=FALSE)
    x<-rslt2$modelStruct$corStruct
    PHI2.est = coef(x,unconstrained=FALSE)
    x<-rslt3$modelStruct$corStruct
    PHI3.est = coef(x,unconstrained=FALSE)
    
    
    ROne = Y1.est - ( cbind(rep(1, length(t1)), t1) %*% beta1.est )
    RTwo = Y2.est - ( cbind(rep(1, length(t2)), t2) %*% beta2.est )
    RThree = Y3.est - ( cbind(rep(1, length(t3)), t3) %*% beta3.est )
    
    resid1.est = ROne[-1] - PHI1.est * ROne[-length(ROne)]
    resid2.est = RTwo[-1] - PHI2.est * RTwo[-length(RTwo)]
    resid3.est = RThree[-1] - PHI3.est * RThree[-length(RThree)]
    
    RSSF = sum(resid1.est^2) + sum(resid2.est^2)
    RSSR = sum(resid3.est^2)
    
    FstatRF = ((RSSR - RSSF)/2)/(RSSF/(T-2))
    pvalueRF = pf(FstatRF, 2, T-2, lower.tail=FALSE)
    
    diffAR = PHI2.est - PHI1.est 
      
   
    tableResult = matrix(0,4,2)
    tableResult[,1] = c("Pre:", "Post:", "Diff = Post - Pre: ", "p-value: ")
    tableResult[,2] = c(round(PHI1.est, 2), round(PHI2.est, 2), round(diffAR,2) , as.character(round(pvalueRF,2)))
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
    var.stat = sse1^2/sse2^2
    if(var.stat>1) {
      pvalue2 = pf(var.stat,df1,df2, lower.tail=FALSE)
    } else {
      pvalue2 = pf(1/var.stat,df1,df2, lower.tail=FALSE)
    }
    
    tableResult = matrix(0,4,2)
    tableResult[,1] = c("Pre: ","Post: ","Ratio = Pre / Post: ","p-value: ")
    tableResult[,2] = c(round(sse1^2,2), round(sse2^2,2), round(var.stat,2), as.character(round(pvalue2,2)))
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
    
    
    resCIL = lcont_CI_2mod(rslt1, rslt2, c(1,t.est,-1,-t.est))
    
    level.pre = cbind(1, t.est) %*%rslt1$coefficients
    
    
    tableResult = matrix(0,5,2)
    tableResult[,1] = c("Pre: ","Post: ","Level Diff = Pre - Post: ","95% CI for Level Diff: ", "p-value:")
    tableResult[,2] = c(round(level.pre,2) , round(rslt2$fitted[1],2) , round(resCIL[1],2) , paste("( ", as.character(round(resCIL[2],2))," , ", as.character(round(resCIL[3],2)), " )"), round(resCIL[5],2))
    tableResult=as.data.frame(tableResult)
    colnames(tableResult)=c("","Value")
    return(tableResult)
  })
  output$tableChangeLevel<- renderTable({
    ChangeLevel()
  })
})