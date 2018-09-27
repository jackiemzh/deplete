library(shiny)
library(xlsx)
library(markdown)
library(DT)
#library(ReporteRs)
#library(shinydashboard)
library(shinythemes)
# library(shinyjs)

library(shinycssloaders)
#


## server.R ##
shinyServer(function(input, output, session) {
  
  
  # reactive UI to reset (classic) inputs
  # if observe action button, reset to default
  observe({
    
    input$reset_input
    
    # reset data_colnum
    updateNumericInput(session, "data_colnum", value = 2)
    
    # Can also set the label and select items
    updateSelectInput(session,
                      "cov_yn", # input name
                      label = paste("Covariates available?"),
                      choices = c("No","Yes"),
                      selected = c("No")
                      )
  })

  # reset UI in analysis tab

  observe({
    
    input$reset_ana_button
    
    # update action buttion to NULL (never clicked before)
    # updateActionButton(session,"run_ana_button",
    #                   label = "Run analysis again", 
    #                   icon = icon("chevron-circle-right", lib = "font-awesome")
                       #style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
    #                   )
    
    # reset data_colnum
    updateNumericInput(session, "num_iteration", value = 5)
    
  })
  
  
  
  # links in classic panel
  observeEvent(input$link_to_tabpanel_set_tab, {
    newvalue <- "setting-panel"
    updateTabItems(session, "classic_panels", newvalue)
  })
  
  observeEvent(input$link_to_tabpanel_ana_tab, {
    newvalue <- "analysing-panel"
    updateTabItems(session, "classic_panels", newvalue)
  })
  
  observeEvent(input$link_to_tabpanel_results_tab, {
    newvalue <- "results-panel"
    updateTabItems(session, "classic_panels", newvalue)
  })
  
  
  
  # links in RD panel
  observeEvent(input$link_to_tabpanel_set_tab_RD, {
    newvalue <- "setting_RD-panel"
    updateTabItems(session, "RD_panels", newvalue)
  })
  

  observeEvent(input$link_to_tabpanel_ana_tab_RD, {
    newvalue <- "analysis_RD-panel"
    updateTabItems(session, "RD_panels", newvalue)
  })
  
  observeEvent(input$link_to_tabpanel_results_tab_RD, {
    newvalue <- "results_RD-panel"
    updateTabItems(session, "RD_panels", newvalue)
  })
  
  
  # reactive UI to reset (RD) inputs
  
  # if observe action button, reset to (RD) default settings
  observe({
    
    input$reset_input_RD
    
    # reset data_colnum
    updateNumericInput(session, "data_colnum_RD", value = 7)
    
    # reset covariable or not options
    updateSelectInput(session,
                      "cov_yn_RD", # input name
                      label = paste("Covariates available?"),
                      choices = c("No","Yes"),
                      selected = c("No")
                      )
    # reset equ_secondary_num_yn to null
    updateSelectInput(session,
                      "equ_secondary_num_yn", # input name
                      label = paste("Equal number of seconary samples within each primary period?"),
                      choices = c("", "No","Yes"),
                      selected = c("")
                      )
    
    # reset p type options to null
    updateSelectInput(session,
                      "p_type_RD", # input name
                      label = paste("Define capture probability type"),
                      choices = c("","All","Constant", "Covariates"),
                      selected = c("")
                      )
    
    # reset phi type options to null
    updateSelectInput(session,
                      "phi_type_RD", # input name
                      label = paste("Define transition probability type"),
                      choices = c("","All","Constant", "Covariates","Time-varying"),
                      selected = c("")
                      )
    
  })
  
  # reset UI in (RD) analysis tab
  
  observe({
    
    input$reset_ana_button_RD
    
    # update action buttion to NULL (never clicked before)
    # updateActionButton(session,"run_ana_button",
    #                   label = "Run analysis again", 
    #                   icon = icon("chevron-circle-right", lib = "font-awesome")
    #style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
    #                   )
    
    # reset data_colnum
    updateNumericInput(session, "num_iteration_RD", value = 5)
    
  })
  
  

  
  
  
  # not working
  # observeEvent(
  #  input$reset_ana_button, {
  #  shinyjs::reset("analysis-panel") # reset funtion in analysis tab
  # })
  
  # reset UI in results tab 
  # observe({
  #  input$reset_ana_button
    
  # shinyjs::reset("results-panel")
    
  #})
  
  #observe
  
  
 # analysis-panel
  
  
#  run_analysis_buttion
  
  
  
  #output$resetable_input <- renderUI({
  #  times <- input$reset_input
  #  div(id=letters[(times %% length(letters)) + 1],
        
        # add default settings
  #      )
  # })
  
  
  # functions
  
  logit <- function(x){ log(x/(1-x)) }
  expit <- function(x){ exp(x)/(1+exp(x)) }
  
  # use reactive function to update input values
  
  
  # target data
  #data1<- reactive({
  
  # data_all<- dataInput()
  #data_all[,input$data_colnum]
  
  #})
  
  ########  prep for geometric removal data
  dataInput <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$upload_data_classic)
    
    infile <- input$upload_data_classic
    
    read.xlsx(infile$datapath, sheetIndex=1,header = input$header)
    
    
  })
  
  
  # read covariates column names - are used for covariate names later
  data_cov_names<- reactive({
    
    if (input$cov_yn=="Yes") {
      
    data_all<- dataInput()
    
    column_names<- colnames(data_all)
    cov_names<- column_names[input$cov_col_range[1]:input$cov_col_range[2]]
  
    }
    
    return(cov_names)
  })
  
  dataTotalcol<- reactive({
    # total number of columns in the uploaded data
    # to limite the number of options in the input selections.
    
    data_all<- dataInput()
    Totalcol = ncol(data_all)
    Totalcol
  })
  
  D<- reactive({
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    sum(data1[which( !is.na(data1), arr.ind=TRUE)])
    
  })
  
  T<-reactive({
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    length(data1)})
  
  
  x<-reactive({
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    x0<-rep(0,T()+1)
    for( k in 2:(T()+1)){x0[k]=sum(data1[1:(k-1)])}
    x0
  })
  
  x.d<-reactive({
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    x.d0<-rep(0,T()-1)
    for (k in 1:(T()-1)){x.d0[k]=x()[T()+1]-x()[k+1]}
    x.d0
  })
  
  
  # data plot
  classic_data_plot<-reactive({
    data_all<- dataInput()
    data_ploints<-data_all[,input$data_colnum]
    
    plot(data_ploints,
         pch=10,
         xlab="sampling occasions",
         ylab="counts",
         main=paste("Predicted counts of", input$species_name, "removed.")
         )
  })
  
  
  # download_plot function
  download_plot<-reactive({
    
    if (run_button_counts$counts>0) {

    data_all<- isolate(dataInput())
    data_ploints<-data_all[,input$data_colnum]
    
    # plot data points
    plot(data_ploints,
         pch=10,
         xlab="sampling occasions",
         ylab="counts",
         main=paste("Predicted counts of", input$species_name, "removed.")
         )
    
    #est_data<- kk.geo.pt.2cov.prediction(kk=2)$fitted.data.pt.2cov.times
    #lines(c(1:T), est_data, col = 'black', lwd=2,lty=3)
    
    
    
    } else {NULL}
    
  })
  
  
  ## conditional panel geo
  
  # display hidden content if input$cov_yn=="Yes"
  output$cov_yn_select <- reactive({
    input$cov_yn=="Yes"
  })
  
  outputOptions(output, "cov_yn_select", suspendWhenHidden = FALSE) 
  
  
  
  
  ########  prep for RD data
  
  dataInput_RD <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$upload_data_RD)
    
    infile <- input$upload_data_RD
    
    read.xlsx(infile$datapath, sheetIndex=1,header = input$header)
    
  })
  
  
  # read RD covariates column names - are used for covariate names later
  data_cov_names_RD<- reactive({
    
    if (input$cov_yn_RD=="Yes") {
      
      # read RD data
      data_all<- dataInput_RD()
      
      column_names<- colnames(data_all)
      cov_names_RD<- column_names[input$cov_col_range_RD[1]:input$cov_col_range_RD[2]]
      
    }
    
    return(cov_names_RD)
  })
  
  
  # total no. of observed individuals
  D_RD<- reactive({
    data_all<- dataInput_RD()
    data1_RD=data_all[,input$data_colnum_RD]
    
    sum_RD<-sum(data1_RD[which(!is.na(data1_RD), arr.ind=TRUE)])
    
    return(sum_RD)
  })

  
  # length of study
  T_RD<-reactive({
    data_all<- dataInput_RD()
    data1_RD=data_all[,input$data_colnum_RD]
    length(data1_RD)})
  
  
  no_k<-reactive({
    input$no_secondary_occasion
  })
  
  # missing occasions
  miss_occ_RD<-reactive({
    data_all<- dataInput_RD()
    data1_RD=data_all[,input$data_colnum_RD]
    
    miss_records<-which(is.na(data1_RD), arr.ind=TRUE)
    miss_records
  })
  
  
  # download plot
  download_plot_RD<-reactive({
    
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    input$run_ana_button_RD
    
    data_all<- isolate(dataInput_RD())
    data_ploints<-data_all[,input$data_colnum]
    
    plot(data_ploints,
         pch=10,
         xlab="sampling occasions",
         ylab="counts",
         main=paste("Predicted counts of", input$species_name, "removed.")
    )
    
    
  })
  
  
  ## display hidden content if input$cov_yn_RD=="Yes"
  output$cov_yn_select_RD <- reactive({
    input$cov_yn_RD=="Yes"
  })
  
  outputOptions(output, "cov_yn_select_RD", suspendWhenHidden = FALSE) 
  
  
  
  
  ## display hidden content if input$equ_secondary_num_yn=="Yes"
  output$equ_secondary_num_yes <- reactive({
    input$equ_secondary_num_yn=="Yes"
  })
  
  outputOptions(output, "equ_secondary_num_yes", suspendWhenHidden = FALSE) 
  
  ## display hidden content if input$equ_secondary_num_yn=="No"
  output$equ_secondary_num_no <- reactive({
    input$equ_secondary_num_yn=="No"
  })
  
  outputOptions(output, "equ_secondary_num_no", suspendWhenHidden = FALSE) 
  
  
  ## display hidden content if input$equ_secondary_num_yn==""
  output$equ_secondary_num_null <- reactive({
    input$equ_secondary_num_yn==""
  })
  
  outputOptions(output, "equ_secondary_num_null", suspendWhenHidden = FALSE) 
  
  
  
  
  
  ################################  fit the geo model
  ################################  optimisation for geo
  
  geo.prediction <- reactive({
    
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    geo.ll <- function (param,data,x,T,D){
      p=expit(param[1]);n0=exp(param[2]);N=n0+D
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        D*log(p)+(T*N-sum(x))*log(1-p)
      return(-logL)
    }
    
    param=c(logit(0.3),log(10)) # random starting number
    fit.geo<-optim(par=param,
                   fn=geo.ll,method="BFGS",hessian=TRUE,
                   data=data1,x=x(),T=T(),D=D())
    
    #return(ept.data)
    return(fit.geo)
    
  })
  
  
  ################################  estimates from geo
  
  ept.p.geo<- reactive({ expit(geo.prediction()$par[1])})
  
  ept.N.geo<- reactive({ exp(geo.prediction()$par[2])+D()})
  
  ################################  predicted numbers from geo
  
  #ept.num.geo<- reactive({ 
  
  #  ept.data<-rep(0,T()); ept.data[1]<-ept.N.geo()*ept.p.geo()
  # for(i in 2:T()) {ept.data[i]<-ept.N.geo()*ept.p.geo()*(1-ept.p.geo())^(i-1)}
  
  #ept.data
  
  #  })
  
  ################################  SE from geo  
  
  
  se.p.geo<- reactive({ 
    se<-sqrt(diag(solve(geo.prediction()$hessian)))
    se<-expit(se[1])
    se
  })
  
  se.n0.geo<- reactive({ 
    se<-sqrt(diag(solve(geo.prediction()$hessian)))
    se<-exp(se[2])
    se
  })
  
  
  ################################  estimate table results from geo  
  geo.est.tab<-reactive({ 
    est.tab.results<-data.frame(
      Param_name = c("Capture probability", "Population size"),
      Notation = c("p", "N"),
      Estimate= c(ept.p.geo(),ept.N.geo()),
      Standard_error= c(se.p.geo(),se.n0.geo()),
      Confidence_interval= c("","")
    )
    
    est.tab.results
  })
  
  
  
  
  ################################  fit the geo.pt model
  ################################  optimisation for geo.pt
  
  no_covariate_available<-reactive({
    
    # calculate the number of covarates available
    
    if (input$cov_yn=="Yes") {
      no_cov <- input$cov_col_range[2]-input$cov_col_range[1]+1
    } else {
      no_cov<-0
    }
    
    return(no_cov)
  })
  
  
  # define the total no. of potential models to fit
  total_no_fits<-reactive({
    
  if (input$cov_yn=="Yes") {

    if (no_covariate_available()==1) {
      
      # if there is one cov
      no_fits = 2 # i.e. p(c) and p(cov) 
      
      no_one_cov_cases = 1
      
      no_two_cov_cases = 0
      
    } else {
      
      # no. of one cov cases
      no_one_cov_cases = no_covariate_available()
      
      # no. of two cov cases
      no_two_cov_cases = choose(no_covariate_available(),2)*2 # addictive cases + addicative_interaction cases 
      
      # constant model + p(one_cov) cases + p(two_cov) cases
      no_fits= 1 + no_one_cov_cases + no_two_cov_cases 
    }
  } else {
    
    # input$cov_yn=="No"
    # fit geometric model only
    
    no_fits<-1
    no_one_cov_cases<-0
    no_two_cov_cases<-0
  }
    
    return(list(no_fits=no_fits,
                no_one_cov_cases=no_one_cov_cases,
                no_two_cov_cases=no_two_cov_cases))
  })
  

  
  two_cov_combin_index_vec <-reactive({
    
    # all unique combinations of 2 covariates
    # note: dim(two_cov_combin_index_vec)[1] = total_no_fits()
    
    orignal_index_vec<-t(combn(no_covariate_available(),2))
    
    # adjust to covariate index in the original dataset
    
    first_cov_index<- as.numeric(orignal_index_vec[,1] + (input$cov_col_range[1]-1))
    second_cov_index<- as.numeric(orignal_index_vec[,2] + (input$cov_col_range[1]-1))
    
    return(list(orignal_index_vec=orignal_index_vec,
                first_cov_index=first_cov_index,
                second_cov_index=second_cov_index))
  })
    
    
  
  
  # define cov (max is 2) considered in the model
  zz<-reactive({

    
    if (input$cov_yn=="Yes") {
      
      data_all<- dataInput()
      
      if (no_covariate_available()==1) {
        
        # one cov case
        
        cov1= data_all[,input$cov_col_11]; covariate_1=cov1-mean(cov1)
        cov= data.frame(cov1=covariate_1)
        
      } else {
        
        # two cov case
         
        num_one_unique_cov_case<-total_no_fits()$no_one_cov_cases
        
        num_two_unique_cov_case<-choose(no_covariate_available(),2)
        
        # set up a covariate array with length total_no_fits() elements
        # cov1= data_all[,input$cov_col_range[1]]; covariate_1=cov1-mean(cov1)
        # cov2= data_all[,input$cov_col_range[1]+1]; covariate_2=cov2-mean(cov2)
        
        # column.names=c("covariate1","covariate2")
        
        # initialise covariate arrary, first empty arrary is for p(c), the rest do have covariate information
        cov= array( rep(NA,total_no_fits()$no_two_cov_case), 
                    dim=c(T(),2, 1 + num_one_unique_cov_case + num_two_unique_cov_case ) )
        
        
        ## one cov used in sub models within two cov cases
        for (jj in 1:num_one_unique_cov_case) {
          
             cov_ind <- input$cov_col_range[1] + (jj-1)
             cov[,1,jj+1]<- data_all[,cov_ind] -   mean( data_all[,cov_ind] )
             
        }
        
        
        ## two cov used in two cov models
        # update two unique combination of covs
        for (ii in 1:num_two_unique_cov_case ) {
          
          # read index vectors
          first_cov_ind<- two_cov_combin_index_vec()$first_cov_index[ii]
          second_cov_ind<- two_cov_combin_index_vec()$second_cov_index[ii]
          
          # standarlise and update covariate array
          cov[,1,ii+1+num_one_unique_cov_case]<- data_all[,first_cov_ind] -   mean( data_all[,first_cov_ind] )
          cov[,2,ii+1+num_one_unique_cov_case]<- data_all[,second_cov_ind] - mean( data_all[,second_cov_ind] )

        }
 
      }
      
    } else {
      # no cov case
      NULL
    }
    
    return(cov)
  })
  
  
  
  ### run the constant geometric model p(c) ONLY
  #   hh is an index ------ 
  hh.geo.pc.prediction<- function(hh=1){
    
    # kk index is an parameter
    force(hh)
    
    # read data
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    # pc
    geo.ll <- function (param,data,x,T,D){
      p=expit(param[1]);n0=exp(param[2]);N=n0+D
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        D*log(p)+(T*N-sum(x))*log(1-p)
      return(-logL)
    }
    
    
    
    ## constant geometric model ONLY.
    
    # function to run the model
        
        fit.model.geo.ll<- function(param) {
          try(optim(par=param,
                    fn=geo.ll, # make sure it's using geo.ll function for optimisation
                    method="BFGS",hessian=TRUE,
                    data=data1,x=x(),T=T(),D=D()),TRUE)}
        
        # make sure hessian is available
        inv <- function(m) {class(try(solve(m),silent=T))=="matrix"} 
        
        # *** custermise the number of iterations
        n.rep= isolate(input$num_iteration)
        
        # initialise numeric and matrix 
        p.test<-N0.test<-loglike.test<-numeric()
        hessians.test <- list(matrix(rep(0,2*2), ncol=2)) # 2 is the length(param)
        hessians.test <- rep(hessians.test,n.rep)
        
        
        # *** update this for all models
        time_spent<-numeric()
        
        # initialise iteration numbers
        j=1
        k=1
        
        # Start the clock for optimisation =-=-=-=-
        ptm <- proc.time()
        
        # run iterations
        while (j<(n.rep+1)) { 
          
          # random starting values
          param=c(logit(runif(1,0.2,0.8)), log(runif(1,1,20)) )
          
          # run the model
          fit <- fit.model.geo.ll(param)
          
          if (inv(fit$hessian)==TRUE) { # check if hessian is available
            if (k<(n.rep+1)) {
              
              # record estimates for each iteration
              p.test[k] <- expit(fit$par[1])
              N0.test[k] <- exp(fit$par[3])
              
              # record hessians for each iteration
              hessians.test[[k]]=fit$hessian
              
              
              # record loglike for each iteration
              # these will be double checked by users
              loglike.test[k]<- -fit$value
              
              k=k+1      
            } }
          j=j+1
          
          # the end of iterations
        }
        
        # Stop the clock =-=-=-=-
        t.record<-proc.time() - ptm
        
        # Time spent
        time_spent<- as.numeric(names(table(t.record[1])))
        
        
        # put together results for all iterations
        AIC.test <- - 2*loglike.test + (2*length(param))
        
        mle.test<- data.frame(p=p.test,
                              N0=N0.test,
                              loglike=loglike.test,
                              AIC= AIC.test)
        
        # observe the number of times hitting the MLEs
        loglike.test<-signif(loglike.test, 7)
        num.max.location<- length(which(loglike.test==max(loglike.test)))
        
        
        # observe which iteration has MLEs
        max.location<-which(loglike.test==max(loglike.test))[1]
        
        # record MLEs corresponding to the max.location
        fit.p <- p.test[max.location]
        fit.N <- N0.test[max.location]+D()
        
        
        
        # record the hessian corresponding to the max.location
        fit.hessians <- hessians.test[[max.location]]
        fit.loglike <- max(loglike.test)
        fit.AIC <- - 2*loglike.test + (2*length(param))
        
        
        # put together MLE results as well as loglike and AIC
        fit.mle.results <- data.frame(p_int=fit.p,
                                          N=fit.N,
                                          loglike=fit.loglike,
                                          AIC=fit.AIC)
        
        
        ## --- the end of running p(c) model
   
    return(list(mle.test=mle.test, # results for all iteration
                hessians.test=hessians.test, # results for all iteration
                fit.mle.results=fit.mle.results, # results for MLEs
                fit.hessians=fit.hessians, # hessian for MLEs
                time_spent=time_spent, # time spent
                num.max.location=num.max.location # the number of times hitting the MLEs
    ))
    
  }
  
  
  
  
  ### run (gg)th one covaraite geometric model ONLY (could be sub model in two cov cases)
  ### e.g. p(z1), p(z2), p(z3) ... models
  #   gg is an index ------
  gg.geo.pt.1cov.prediction<- function(gg){
    
    # gg index is an parameter
    force(gg)
    
    # read data
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    # pt function for one cov
    geo.pt.cov.ll <- function (param,data,x.d,T,D,z){
      
      c1<-param[1]; c2<-param[2];n0<-exp(param[3])
      p=expit(c1+c2*z);N=n0+D
      
      q<-numeric(); for (i in 1:(T-1)){q[i]=1-p[i]}
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        sum(x.d*log(q)) +sum(data*log(p))+ n0*sum(log(q))
      
      return(-logL)
    }
    
    
    ## the (kk)th one covaraite geometric model (sub model in two cov cases) ONLY.
    
        # choose one covariate for the (gg)th covariate
        z <- zz()[,1,gg+1]  # +1 because the first covaraiete arrary is empty for p(c) model
        
        
        ## ---- pt (z1)  model
        
        # function to run the model
        
        fit.model.geo.pt.cov.ll<- function(param) {
          try(optim(par=param,
                    fn=geo.pt.cov.ll, # make sure it's using geo.pt.cov.ll function for optimisation
                    method="BFGS",hessian=TRUE,
                    data=data1,x.d=x.d(),T=T(),D=D(),z=z),TRUE)}
        
        # make sure hessian is available
        inv <- function(m) {class(try(solve(m),silent=T))=="matrix"} 
        
        # custermise the number of iterations
        n.rep= isolate(input$num_iteration)
        
        # initialise numeric and matrix 
        p.int.1cov.test<-p.slope.1cov.test1<-N0.1cov.test<-loglike.1cov.test<-numeric()
        hessians.1cov.test <- list(matrix(rep(0,3*3), ncol=3)) # 3 is the length(param)
        hessians.1cov.test <- rep(hessians.1cov.test,n.rep)
        
        
        # *** update this for all models
        time_spent.1cov<-numeric()
        
        # initialise iteration numbers
        j=1
        k=1
        
        # Start the clock for optimisation =-=-=-=-
        ptm <- proc.time()
        
        # run iterations
        while (j<(n.rep+1)) { 
          
          # random starting values
          param=c(runif(2,-0.1,0.1), log(runif(1,1,20)) )
          
          # run the model
          fit <- fit.model.geo.pt.cov.ll(param)
          
          if (inv(fit$hessian)==TRUE) { # check if hessian is available
            if (k<(n.rep+1)) {
              
              # record estimates for each iteration
              p.int.1cov.test[k] <- fit$par[1]
              p.slope.1cov.test1[k] <- fit$par[2]
              N0.1cov.test[k] <- exp(fit$par[3])
              
              # record hessians for each iteration
              hessians.1cov.test[[k]]=fit$hessian
              
              
              # record loglike for each iteration
              # these will be double checked by users
              loglike.1cov.test[k]<- -fit$value
              
              k=k+1      
            } }
          j=j+1
          
          # the end of iterations
        }
        
        # Stop the clock =-=-=-=-
        t.record<-proc.time() - ptm
        
        # Time spent
        time_spent.1cov<- as.numeric(names(table(t.record[1])))
        
        
        # put together results for all iterations
        AIC.1cov.test <- - 2*loglike.1cov.test + (2*length(param))
        
        mle.test.1cov<- data.frame(p_int=p.int.1cov.test,
                                   p_slope1=p.slope.1cov.test1,
                                   N0=N0.1cov.test,
                                   loglike=loglike.1cov.test,
                                   AIC= AIC.1cov.test)
        
        # observe the number of times hitting the MLEs
        loglike.1cov.test<-signif(loglike.1cov.test, 7)
        num.max.location.1cov<- length(which(loglike.1cov.test==max(loglike.1cov.test)))
        
        
        # observe which iteration has MLEs
        max.location.1cov<-which(loglike.1cov.test==max(loglike.1cov.test))[1]
        
        # record MLEs corresponding to the max.location.2cov
        fit.p.int.1cov <- p.int.1cov.test[max.location.1cov]
        fit.p.slope.1cov.1 <- p.slope.1cov.test1[max.location.1cov]
        fit.N.1cov <- N0.1cov.test[max.location.1cov]+D()
        
        # record the hessian corresponding to the max.location.2cov
        fit.hessians.1cov <- hessians.1cov.test[[max.location.1cov]]
        fit.loglike.1cov <- max(loglike.1cov.test)
        fit.AIC.1cov<- - 2*max(loglike.1cov.test) + (2*length(param))
        
        
        # put together MLE results as well as loglike and AIC
        fit.mle.results.1cov<- data.frame(p_int=fit.p.int.1cov,
                                          p_slope1=fit.p.slope.1cov.1,
                                          N=fit.N.1cov,
                                          loglike=fit.loglike.1cov,
                                          AIC=fit.AIC.1cov)
        
        
        ## --- the end of running p(z1) model

    return(list(mle.test.1cov=mle.test.1cov, # results for all iteration
                hessians.1cov.test=hessians.1cov.test, # results for all iteration
                fit.mle.results.1cov=fit.mle.results.1cov, # results for MLEs
                fit.hessians.1cov=fit.hessians.1cov, # hessian for MLEs
                time_spent.1cov=time_spent.1cov, # time spent
                num.max.location.1cov=num.max.location.1cov # the number of times hitting the MLEs
    ))
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### run (kk)th two cov model ONLY, i.e. p(z1+z2) and p(z1+z2+z1*z2) models
  #   kk is an index ------
  kk.geo.pt.2cov.prediction<- function(kk){
    
    # kk index is an parameter
    force(kk)
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]

    # pt function for two covs
    geo.pt.2cov.ll <- function (param,data,x.d,T,D,z1,z2){
      
      c1<-param[1]; c2<-param[2];c3<-param[3]
      p=expit(c1+c2*z1+c3*z2)
      n0<-exp(param[4]); N=n0+D
      
      q<-numeric(); for (i in 1:(T-1)){q[i]=1-p[i]}
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        sum(x.d*log(q)) +sum(data*log(p))+ n0*sum(log(q))
      
      return(-logL)
    }
    
    
    fit.geo.pt.2cov.times.ll<-function (fit_param,data,T,z1,z2) {
      
      
      ept.p<- expit(fit_param[1]+fit_param[2]*z1+fit_param[3]*z2+fit_param[4]*z1*z2)
      ept.N<- fit_param[5]
      
      ept.data<-numeric(); ept.data[1]<-ept.N*ept.p[1]
      
      for(i in 2:T) {ept.data[i]<- ept.N*prod(1-ept.p[1:i-1])*ept.p[i]}
      
      return(ept.data)
    }
    
    
    
    # pt function for two covs
    geo.pt.2cov.times.ll <- function (param,data,x.d,T,D,z1,z2){
      
      c1<-param[1]; c2<-param[2];c3<-param[3];c4<-param[4]
      p=expit(c1+c2*z1+c3*z2+c4*z1*z2)
      n0<-exp(param[5]); N=n0+D
      
      q<-numeric(); for (i in 1:(T-1)){q[i]=1-p[i]}
      
      logL=lgamma(N+1)-sum(lgamma(data+1))-lgamma(n0+1)+
        sum(x.d*log(q)) +sum(data*log(p))+ n0*sum(log(q))
      
      return(-logL)
    }
    
    
    fit.geo.pt.2cov.times.ll<-function (fit_param,data,T,z1,z2) {
      
      
      ept.p<- expit(fit_param[1]+fit_param[2]*z1+fit_param[3]*z2+fit_param[4]*z1*z2)
      ept.N<- fit_param[5]
      
      ept.data<-numeric(); ept.data[1]<-ept.N*ept.p[1]
      
      for(i in 2:T) {ept.data[i]<- ept.N*prod(1-ept.p[1:i-1])*ept.p[i]}
      
      return(ept.data)
    }
    
    
    
    ## the (kk)th combination of two covaraites geo model ONLY.
    
    #kk_results<-reactive({
      
      # run the model according to the no. of cov
      
      if (input$cov_yn=="Yes") {
        
        if (no_covariate_available()>1) {
          
          # this function deals with two cov cases ONLY.
          
          # choose covariates for the (kk)th combination of covariates
          z1=zz()[,1,kk+1+total_no_fits()$no_one_cov_cases] 
          z2=zz()[,2,kk+1+total_no_fits()$no_one_cov_cases] 
          
          
          ## ---- pt (z1+z2) additive model
          
          # function to run the model
          
          fit.model.geo.pt.2cov.ll<- function(param) {
                      try(optim(par=param,
                      fn=geo.pt.2cov.ll, # make sure it's using geo.pt.2cov.ll function for optimisation
                      method="BFGS",hessian=TRUE,
                      data=data1,x.d=x.d(),T=T(),D=D(),z1=z1,z2=z2),TRUE)}
          
          # make sure hessian is available
          inv <- function(m) {class(try(solve(m),silent=T))=="matrix"} 
          
          # custermise the number of iterations
          n.rep= isolate(input$num_iteration)

          # initialise numeric and matrix 
          p.int.2cov.test<-p.slope.2cov.test1<-p.slope.2cov.test2<-N0.2cov.test<-loglike.2cov.test<-numeric()
          hessians.2cov.test <- list(matrix(rep(0,4*4), ncol=4)) # 4 is the length(param)
          hessians.2cov.test <- rep(hessians.2cov.test,n.rep)
          
          
          # update this for all models
          time_spent.2cov<-numeric()
          
          # initialise iteration numbers
          j=1
          k=1
          
          # Start the clock for optimisation =-=-=-=-
          ptm <- proc.time()
          
          # run iterations
          while (j<(n.rep+1)) { 
          
            # random starting values
            param=c(runif(3,-0.1,0.1), log(runif(1,1,20)) )
            
            # run the model
            fit <- fit.model.geo.pt.2cov.ll(param)
            
            if (inv(fit$hessian)==TRUE) { # check if hessian is available
              if (k<(n.rep+1)) {
                
                # record estimates for each iteration
                p.int.2cov.test[k] <- fit$par[1]
                p.slope.2cov.test1[k] <- fit$par[2]
                p.slope.2cov.test2[k] <- fit$par[3]
                N0.2cov.test[k] <- exp(fit$par[4])
                
                # record hessians for each iteration
                hessians.2cov.test[[k]]=fit$hessian
                
                
                # record loglike for each iteration
                # these will be double checked by users
                loglike.2cov.test[k]<- -fit$value
                
                k=k+1      
              } }
            j=j+1
            
            # the end of iterations
          }
          
          # Stop the clock =-=-=-=-
          t.record<-proc.time() - ptm
          
          # Time spent
          time_spent.2cov<- as.numeric(names(table(t.record[1])))
         
          
          # put together results for all iterations
          AIC.2cov.test <- - 2*loglike.2cov.test + (2*length(param))
          
                mle.test.2cov<- data.frame(p_int=p.int.2cov.test,
                                           p_slope1=p.slope.2cov.test1,
                                           p_slope2=p.slope.2cov.test2,
                                           N0=N0.2cov.test,
                                           loglike=loglike.2cov.test,
                                           AIC= AIC.2cov.test)
          
          # observe the number of times hitting the MLEs
          loglike.2cov.test<-signif(loglike.2cov.test, 7)
          num.max.location.2cov<- length(which(loglike.2cov.test==max(loglike.2cov.test)))
                
           
          # observe which iteration has MLEs
          max.location.2cov<-which(loglike.2cov.test==max(loglike.2cov.test))[1]
          
          # record MLEs corresponding to the max.location.2cov
          fit.p.int.2cov <- p.int.2cov.test[max.location.2cov]
          fit.p.slope.2cov.1 <- p.slope.2cov.test1[max.location.2cov]
          fit.p.slope.2cov.2 <- p.slope.2cov.test2[max.location.2cov]
          fit.N.2cov <- N0.2cov.test[max.location.2cov]+D()
          
          # record the hessian corresponding to the max.location.2cov
          fit.hessians.2cov <- hessians.2cov.test[[max.location.2cov]]
          fit.loglike.2cov <- max(loglike.2cov.test)
          fit.AIC.2cov<- - 2*fit.loglike.2cov + (2*length(param))

          
          # put together MLE results as well as loglike and AIC
                fit.mle.results.2cov<- data.frame(p_int=fit.p.int.2cov,
                                                  p_slope1=fit.p.slope.2cov.1,
                                                  p_slope2=fit.p.slope.2cov.2,
                                                  N=fit.N.2cov,
                                                  loglike=fit.loglike.2cov,
                                                  AIC=fit.AIC.2cov)

                    
          ## --- the end of pt (z1+z2) additive model
          
          
          
          ## ---- pt (z1+z2+z1*z2) interaction model
          
          # function to run the model
          
          fit.model.geo.pt.2cov.times.ll<- function(param) {
            try(optim(par=param,
                      fn=geo.pt.2cov.times.ll, # make sure it's using geo.pt.2cov.times.ll function for optimisation
                      method="BFGS",hessian=TRUE,
                      data=data1,x.d=x.d(),T=T(),D=D(),z1=z1,z2=z2),TRUE)}
          
          # make sure hessian is available
          inv <- function(m) {class(try(solve(m),silent=T))=="matrix"} 
          
          # custermise the number of iterations
          n.rep= isolate(input$num_iteration)
          
          # initialise numeric and matrix 
          p.int.2cov.times.test<-p.slope.2cov.times.test1<-p.slope.2cov.times.test2<-p.slope.2cov.times.test3<-N0.2cov.times.test<-loglike.2cov.times.test<-numeric()
          hessians.2cov.times.test <- list(matrix(rep(0,5*5), ncol=5)) # 5 is the length(param)
          hessians.2cov.times.test <- rep(hessians.2cov.times.test,n.rep)
          
          
          # update this for all models
          time_spent.2cov.times<-numeric()
          
          # initialise iteration numbers
          j=1
          k=1
          
          # Start the clock for optimisation =-=-=-=-
          ptm <- proc.time()
          
          # run iterations
          while (j<(n.rep+1)) { 
            
            # random starting values
            param=c(runif(4,-0.1,0.1), log(runif(1,1,20)) )
            
            # run the model
            fit <- fit.model.geo.pt.2cov.times.ll(param)
            
            if (inv(fit$hessian)==TRUE) { # check if hessian is available
              if (k<(n.rep+1)) {
                
                # record estimates for each iteration
                p.int.2cov.times.test[k] <- fit$par[1]
                p.slope.2cov.times.test1[k] <- fit$par[2]
                p.slope.2cov.times.test2[k] <- fit$par[3]
                p.slope.2cov.times.test3[k] <- fit$par[4]
                N0.2cov.times.test[k] <- exp(fit$par[5])
                
                # record hessians for each iteration
                hessians.2cov.times.test[[k]]=fit$hessian
                
                
                # record loglike for each iteration
                # these will be double checked by users
                loglike.2cov.times.test[k]<- -fit$value
                
                k=k+1      
              } }
            j=j+1
          }
            
          # Stop the clock =-=-=-=-
          t.record<-proc.time() - ptm
          
          # Time spent
          time_spent.2cov.times<- as.numeric(names(table(t.record[1])))
          
          
          # put together results for all iterations
          AIC.2cov.times.test <- - 2*loglike.2cov.times.test + (2*length(param))
          
          mle.test.2cov.times<- data.frame(p_int=p.int.2cov.times.test,
                                                  p_slope1=p.slope.2cov.times.test1,
                                                  p_slope2=p.slope.2cov.times.test2,
                                                  p_slope3=p.slope.2cov.times.test3,
                                                  N0=N0.2cov.times.test,
                                                  loglike=loglike.2cov.times.test,
                                                  AIC= AIC.2cov.times.test)
          
          
          # observe the number of times hitting the MLEs
          loglike.2cov.times.test<-signif(loglike.2cov.times.test, 7)
          num.max.location.2cov.times<- length(which(loglike.2cov.times.test==max(loglike.2cov.times.test)))
          
          # observe which iteration has MLEs
          max.location.2cov.times<-which(loglike.2cov.times.test==max(loglike.2cov.times.test))[1]
          
          # record MLEs corresponding to the max.location.2cov.times
          fit.p.int.2cov.times <- p.int.2cov.times.test[max.location.2cov.times]
          fit.p.slope.2cov.times.1 <- p.slope.2cov.times.test1[max.location.2cov.times]
          fit.p.slope.2cov.times.2 <- p.slope.2cov.times.test2[max.location.2cov.times]
          fit.p.slope.2cov.times.3 <- p.slope.2cov.times.test3[max.location.2cov.times]
          fit.N.2cov.times <- N0.2cov.times.test[max.location.2cov.times] + D()
          
          # fitted data
          fit_param<-c(fit.p.int.2cov.times,
                       fit.p.slope.2cov.times.1,
                       fit.p.slope.2cov.times.2,
                       fit.p.slope.2cov.times.3,
                       fit.N.2cov.times)
          
          fitted.data.pt.2cov.times <- fit.geo.pt.2cov.times.ll(fit_param=fit_param,data=data1,T=T(),z1=z1,z2=z2)
          
          
          # record the hessian corresponding to the max.location.2cov.times
          fit.hessians.2cov.times <- hessians.2cov.times.test[[max.location.2cov.times]]
          fit.loglike.2cov.times <- max(loglike.2cov.times.test)
          fit.AIC.2cov.times <- - 2*fit.loglike.2cov.times + (2*length(param))
          
          # put together MLE results as well as loglike and AIC
          fit.mle.results.2cov.times<- data.frame(p_int=fit.p.int.2cov.times,
                                                 p_slope1=fit.p.slope.2cov.times.1,
                                                 p_slope2=fit.p.slope.2cov.times.2,
                                                 p_slope3=fit.p.slope.2cov.times.3,
                                                 N=fit.N.2cov.times,
                                                 loglike=fit.loglike.2cov.times,
                                                 AIC=fit.AIC.2cov.times)
          
          
          
          
          ## --- pt (z1+z2+z1*z2) interaction model
            
        }
      } 
 
    return(list(mle.test.2cov=mle.test.2cov, # results for all iteration
                mle.test.2cov.times=mle.test.2cov.times, # results for all iteration
                hessians.2cov.test=hessians.2cov.test,
                hessians.2cov.times.test,hessians.2cov.times.test,
                fit.mle.results.2cov=fit.mle.results.2cov, # results for MLEs
                fit.mle.results.2cov.times=fit.mle.results.2cov.times, # results for MLEs
                fitted.data.pt.2cov.times=fitted.data.pt.2cov.times, # fitted.data
                fit.hessians.2cov=fit.hessians.2cov,
                fit.hessians.2cov.times=fit.hessians.2cov.times,
                time_spent.2cov=time_spent.2cov, # time spent
                time_spent.2cov.times=time_spent.2cov.times, # time spent,
                num.max.location.2cov=num.max.location.2cov, # the number of times hitting the MLEs
                num.max.location.2cov.times=num.max.location.2cov.times # # the number of times hitting the MLEs 
                ))
    
  }
  
  
  
  
  
  # 
  geo.pt.prediction <- reactive({
    
    
  })
  
  
  
  # run all possible models
  all.geo.prediction<- reactive({
    
    if (run_button_counts$counts!=0) {
      
      data_all<- dataInput()
      
      # always run constant model
      pc.prediction <-hh.geo.pc.prediction(hh=1)
    
      
      if (input$cov_yn=="Yes") {
        
        if (no_covariate_available()==1) {
          
          # one cov case
          
          pt.1cov.prediction <- gg.geo.pt.1cov.prediction(gg=1) 
          
          # as there is only one covariate in the data
          
          pt.2cov.prediction <- NULL
          
        } else {
          
          ### two cov case
          
          # the number of one and two cov cases 
          num_one_unique_cov_case<-total_no_fits()$no_one_cov_cases
          num_two_unique_cov_case<-choose(no_covariate_available(),2)
          
          
          ## one cov used as sub models within two cov cases
          
          # initialise pt.1cov.prediction as a list to store results for each model with one cov
          pt.1cov.prediction<-list()
          
          for (gg in 1:num_one_unique_cov_case) {
            
            pt.1cov.prediction[[gg]] <- gg.geo.pt.1cov.prediction(gg)
            
          }
          
          
          ## two cov used in two cov models
          
          # initialise pt.1cov.prediction as a list to store results for each model with one cov
          pt.2cov.prediction<-list()
          
          for (kk in 1:num_two_unique_cov_case) {
            
            pt.2cov.prediction[[kk]] <- kk.geo.pt.2cov.prediction(kk)
            
          }
          
          
        }
        
      } 
      
      
    } else {
      
      # if reset or did not hit the action button
      
      pc.prediction<- NULL
      pt.1cov.prediction<-NULL
      pt.2cov.prediction<-NULL
      
    }
    
    # --- the end of action button condition 
        
    return(list(pc.prediction=pc.prediction,
                pt.1cov.prediction=pt.1cov.prediction,
                pt.2cov.prediction=pt.2cov.prediction
                ))
    
    })
  
  
  
  
  
  
  
  
  ################################  model comparison from geo.pt for one combination of two cov
  
  
  geo.aic.models<-reactive({
    
    if (run_button_counts$counts!=0) {
      
      #*** add structure of no. of cov
      
      ## >= two cov available
      # AIC values
      geo.pt.aic<- numeric()
      
      geo.pt.aic[1]<-all.geo.prediction()$pc.prediction$fit.mle.results$AIC
      
      for (ii in 1:total_no_fits()$no_one_cov_cases ) {
        geo.pt.aic[ii+1]<- -2*(all.geo.prediction()$pt.1cov.prediction[[ii]]$fit.mle.results.1cov$loglike) + 2*3
      }
      
      for (jj in 1:choose(no_covariate_available(),2) ) {
        geo.pt.aic[jj*2-1+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$fit.mle.results.2cov$AIC
        geo.pt.aic[jj*2 +(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$fit.mle.results.2cov.times$AIC
      }
      
      
      # ordered index vector
      aic.order.index<-order(geo.pt.aic)
      
      # ordered aic
      aic.ordered<-geo.pt.aic[aic.order.index]
      
      # delta aic
      delta.aic.ordered<-geo.pt.aic[aic.order.index]- geo.pt.aic[aic.order.index[1]]
      
      
      # ordered model_name_vec
      
      model.names<-all_list_model_tab()$Model
      model.names.ordered<-model.names[aic.order.index]
      
      
      # ordered cov_used_vec
      
      p_cov_used<-all_list_model_tab()$p_cov_used
      p_cov_used_ordered<-p_cov_used[aic.order.index]
      
      # ordered no. param
      geo.pt.no.param<- c(2,
                          rep(3,total_no_fits()$no_one_cov_cases),
                          rep(c(4,5), choose(no_covariate_available(),2))
      )
      
      geo.pt.no.param.ordered<- geo.pt.no.param[aic.order.index]
      
      
      # ordered maximised loglikelihood
      geo.pt.maxlogl<- numeric()
      
      geo.pt.maxlogl[1]<- all.geo.prediction()$pc.prediction$fit.mle.results$loglike
      
      for (ii in 1:total_no_fits()$no_one_cov_cases ) {
        geo.pt.maxlogl[ii+1]<- all.geo.prediction()$pt.1cov.prediction[[ii]]$fit.mle.results.1cov$loglike
      }
      
      for (jj in 1:choose(no_covariate_available(),2) ) {
        
        geo.pt.maxlogl[jj*2-1+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$fit.mle.results.2cov$loglike
        geo.pt.maxlogl[jj*2 +(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$fit.mle.results.2cov.times$loglike
      }
      
      geo.pt.maxlogl.ordered<-geo.pt.maxlogl[aic.order.index]
      
      
      
      
      ### predicted estimates of the best model with the lowest AIC value
      
      
      
      ### predicted counts of the best model with the lowest AIC value
      
      
      
      # the end of action button
    }

    
    return(list(geo.pt.no.param=geo.pt.no.param,
                geo.pt.maxlogl=geo.pt.maxlogl,
                geo.pt.aic=geo.pt.aic,
                aic.order.index=aic.order.index,
                model.names.ordered=model.names.ordered,
                p_cov_used_ordered=p_cov_used_ordered,
                geo.pt.no.param.ordered=geo.pt.no.param.ordered,
                geo.pt.maxlogl.ordered=geo.pt.maxlogl.ordered,
                aic.ordered=aic.ordered,
                delta.aic.ordered=delta.aic.ordered
    ))
    
  })
  
  
  
  # model comparison results table for one combination of two cov
  geo.model.tab<-reactive({ 
    
  if (run_button_counts$counts!=0) {
      
    model.tab.results<-data.frame(
      Model = geo.aic.models()$model.names.ordered,
      p_cov_used = geo.aic.models()$p_cov_used_ordered,
      No_param=geo.aic.models()$geo.pt.no.param.ordered,
      max_logL=geo.aic.models()$geo.pt.maxlogl.ordered,
      AIC=geo.aic.models()$aic.ordered,
      delta.AIC= geo.aic.models()$delta.aic.ordered
    )
    } else {
      
      model.tab.results=NULL
      
    }
    model.tab.results
  })
  
  
  
  
  
  #  ################################  estimates from geo
  
  #  ept.p.geo<- reactive({ expit(geo.prediction()$par[1])})
  
  #  ept.N.geo<- reactive({ exp(geo.prediction()$par[2])+D()})
  
  #  ################################  predicted numbers from geo
  
  ept.num.geo<- reactive({ 
    
    ept.data<-rep(0,T())
    
    model.names<- c("p(c)","p(cov_1)","p(cov_2)","p(cov_1+cov_2)","p(cov_1*cov_2)")
    
    if (geo.pt.aic.models()$model.names.ordered[1]=="p(cov_1*cov_2)") {
      
      # param
      z=zz()
      z1=z[,1]
      z2=z[,2]
      
      cc<-numeric();
      cc<-geo.pt.prediction()$geo.z1.times.z2$par[1:4]
      
      ept.pt.geo<-expit(cc[1]+cc[2]*z1+cc[3]*z2+cc[4]*z1*z2) 
      
      ept.pt.N.geo<-exp(geo.pt.prediction()$geo.z1.times.z2$par[5])+D()
      
      # fitted data
      
      ept.data[1]<-ept.pt.N.geo*ept.pt.geo[1]
      
      for(i in 2:T())
      {ept.data[i]<- ept.pt.N.geo*prod(1-ept.pt.geo[1:i-1])*ept.pt.geo[i]}
      
      #    ept.data
      
    }
    
    if (geo.pt.aic.models()$model.names.ordered[1]=="p(cov_1+cov_2)") {
      
      # param
      z=zz()
      z1=z[,1]
      z2=z[,2]
      
      cc<-numeric();
      cc<-geo.pt.prediction()$geo.z1.plus.z2$par[1:3]
      #    
      ept.pt.geo<-expit(cc[1]+cc[2]*z1+cc[3]*z2) 
      
      ept.pt.N.geo<-exp(geo.pt.prediction()$geo.z1.plus.z2$par[4])+D()
      
      # fitted data
      
      ept.data[1]<-ept.pt.N.geo*ept.pt.geo[1]
      
      for(i in 2:T())
      {ept.data[i]<- ept.pt.N.geo*prod(1-ept.pt.geo[1:i-1])*ept.pt.geo[i]}
      
      
    }
    
    if (geo.pt.aic.models()$model.names.ordered[1]=="p(cov_1)") {
      
      # param
      z=zz()
      z1=z[,1]
      
      cc<-numeric();
      cc<-geo.pt.prediction()$geo.z1$par[1:2]
      
      ept.pt.geo<-expit(cc[1]+cc[2]*z1) 
      
      ept.pt.N.geo<-exp(geo.pt.prediction()$geo.z1$par[3])+D()
      
      # fitted data
      
      #    ept.data[1]<-ept.pt.N.geo*ept.pt.geo[1]
      
      for(i in 2:T())
      {ept.data[i]<- ept.pt.N.geo*prod(1-ept.pt.geo[1:i-1])*ept.pt.geo[i]}
      
      
    }
    
    if (geo.pt.aic.models()$model.names.ordered[1]=="p(cov_2)") {
      
      # param
      z=zz()
      z1=z[,1]
      z2=z[,2]
      
      cc<-numeric();
      cc<-geo.pt.prediction()$geo.z2$par[1:2]
      
      ept.pt.geo<-expit(cc[1]+cc[2]*z2) 
      
      ept.pt.N.geo<-exp(geo.pt.prediction()$geo.z2$par[3])+D()
      
      # fitted data
      
      ept.data[1]<-ept.pt.N.geo*ept.pt.geo[1]
      #   
      for(i in 2:T())
      {ept.data[i]<- ept.pt.N.geo*prod(1-ept.pt.geo[1:i-1])*ept.pt.geo[i]}
      
      
    }    
    
    if (geo.pt.aic.models()$model.names.ordered[1]=="p(c)") {
      
      ept.data[1]<-ept.N.geo()*ept.p.geo()
      for(i in 2:T()) {ept.data[i]<-ept.N.geo()*ept.p.geo()*(1-ept.p.geo())^(i-1)}
      
    }
    
    return(ept.data)
    
  })
  
  ################################  estimates from geo.pt
  
  
  ################################  predicted numbers from geo.pt
  
  ept.num.pt.geo<- reactive({ 
    
    ept.data<-rep(0,T()); ept.data[1]<-ept.pt.N.geo()*ept.pt.geo()[1]
    
    for(i in 2:T())
    {ept.data[i]<- ept.pt.N.geo()*prod(1-ept.pt.geo()[1:i-1])*ept.pt.geo()[i]
    }
    
    ept.data
    
  })
  
  ################################  SE from geo.pt
  
  se.pt.coe1.geo<- reactive({ 
    se<-sqrt(diag(solve(geo.prediction()$hessian)))
    se<-se[1]
    se
  })
  
  se.pt.coe2.geo<- reactive({ 
    se<-sqrt(diag(solve(geo.prediction()$hessian)))
    se<-se[2]
    se
  })
  
  se.pt.n0.geo<- reactive({ 
    se<-sqrt(diag(solve(geo.prediction()$hessian)))
    se<-exp(se[3])
    se
  })
  
  
  ################################  estimate table results from geo.pt 
  gep.pt.est.tab<-reactive({ 
    est.tab.results<-data.frame(
      Param_name = c("Capture probability", "Population size"),
      Notation = c("p", "N"),
      Estimate= c(ept.p.geo(),ept.N.geo()),
      Standard_error= c(se.p.geo(),se.n0.geo()),
      Confidence_interval= c(ept.p.geo(),ept.N.geo())
    )
    
    est.tab.results
  })
  
  
  
  
  # ===================================================
  # ###############################    RD models
  
  # RD no. of cov available
  no_covariate_available_RD<-reactive({
    
    if (input$cov_yn_RD=="Yes") {
    no_cov <- input$cov_col_range_RD[2]-input$cov_col_range_RD[1]+1
    } else {no_cov<- 0}
    
    return(no_cov)
  })
  
  # define the total no. of potential models to fit
  total_no_fits_RD<-reactive({
    
    input_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
    
    if ( input_not_null == TRUE ) {
      
    if (input$cov_yn_RD == "Yes") {
     
      if (input$phi_type_RD=="All" & input$p_type_RD=="All") {
        no_fits<- (2+no_covariate_available_RD()) * (1+no_covariate_available_RD())
      }
      
      if (input$phi_type_RD=="All" & input$p_type_RD=="Constant") {
        no_fits<- 2+no_covariate_available_RD()
      }
      
      if (input$phi_type_RD=="All" & input$p_type_RD=="Covariates") {
        no_fits<- (2+no_covariate_available_RD()) * (1+no_covariate_available_RD())
      }
      
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="All") {
        no_fits<- 1+no_covariate_available_RD()
      }
      
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="Constant") {
        no_fits<- 1
      }
      
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates") {
        no_fits<- 1+no_covariate_available_RD()
      }
      
      if (input$phi_type_RD=="Covariates" & input$p_type_RD=="All") {
        no_fits<- (1+no_covariate_available_RD())*(1+no_covariate_available_RD())
      }
      
      if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant") {
        no_fits<- 1+no_covariate_available_RD()
      }
      
      if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates") {
        no_fits<- (1+no_covariate_available_RD()) * (1+no_covariate_available_RD())
      }
      
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="All") {
        no_fits<- 2*(1+no_covariate_available_RD())
      }
      
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") {
        no_fits<- 2
      }
      
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Covariates") {
        no_fits<- 2* (1+no_covariate_available_RD())
      }

    } else {
      
      # input$cov_yn_RD == "No"
      if (input$phi_type_RD=="All" & input$p_type_RD=="All") { no_fits<- 2 }
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="All") { no_fits<- 1 }
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="All") { no_fits<- 2 }
      if (input$phi_type_RD=="All" & input$p_type_RD=="Constant") { no_fits<- 2 }
      if (input$phi_type_RD=="Constant" & input$p_type_RD=="Constant") { no_fits<- 1 }
      if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") { no_fits<- 2 }
      
    }
  }

    
    if ( input_not_null == FALSE ) {
      no_fits<-1
}
    
    
    # RD no_fits
    return(no_fits)
  })
  
  # define cov names vector
  
  #  zz_names<- reactive({
    
  # if (input$cov_yn_RD=="Yes") {
      
  #  data_all<- dataInput_RD()
  #  all_names_RD<- names(data_all)
  #  cov_names_RD<- all_names_RD[,input$cov_col_range_RD[1:2]]
    
  #  } else {NULL}
    
  #  return(cov_names_RD)
  # })
  
  
  output$out3_RD <- renderTable({
   
    
    #data_cov_names_RD()
    
   return()
   
  })
  
  
  
  # *** define cov (max is 1) considered in the model
  zz_RD<-reactive({

    if (input$cov_yn_RD=="Yes") {
      
      data_all<- dataInput_RD()
      
      if (no_covariate_available_RD()==1) {
        
        # one cov case
        
        cov1= data_all[,input$cov_col_range_RD[1]]; covariate_1=cov1-mean(cov1)
        cov= data.frame(cov1=covariate_1)
        
      } else {
        
        # all unique combinations of 2 covariates
        # dim(two_cov_combin_index_vec)[1] = total_no_fits()
        
        all_cov_index_vec <-1:no_covariate_available_RD()
        
        # column.names=c("phi_cov","p_cov")
        # first and second columns corresponds to phi and p respectively
        cov= array(NA, dim=c(T_RD(),2,total_no_fits_RD()) )
        

       # set up a covariate array with length total_no_fits() elements
       # phi_cov= data_all[,input$cov_col_range_RD[1]]; phi_covariate=phi_cov-mean(phi_cov)
      #  p_cov= data_all[,input$cov_col_range_RD[1]+1]; p_covariate=p_cov-mean(p_cov)
       
        #*** update two covs for each of potential models
        
        if (input$phi_type_RD=="All" & input$p_type_RD=="All") {
          
          
          for (ii in 1:no_covariate_available_RD()) {
            
            ## define covariates for p from the 2nd model to the no_covariate_available_RD()+1 model in the table in the Settings section
            # adjust to p covariate index in the original dataset
            p_cov_index <-  all_cov_index_vec[ii] + (input$cov_col_range_RD[1]-1)
            p_cov_vec<-data_all[,p_cov_index]
            not_na_index<-which(!is.na(p_cov_vec))
            
            # start from the 2nd arrary as the first model in the table in the Settings section is a constant model without any covaraites.
            # standarlise and update covariate array
            cov[,2,ii+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
            
            # adjust to phi covariate index in the original dataset
            phi_cov_index<- all_cov_index_vec[ii] + (input$cov_col_range_RD[1]-1)
            phi_cov_vec<-data_all[,phi_cov_index]
            not_na_index<-which(!is.na(phi_cov_vec))
            
            ## define covariates for phi from the no_covariate_available_RD()+2 model to the no_covariate_available_RD()*2+1 model
            # standarlise and update covariate array
            cov[,1,ii+no_covariate_available_RD()+1][not_na_index]<- phi_cov_vec[not_na_index]- mean(phi_cov_vec[not_na_index])
            
            
            ## define covariates for p 
            ## from the no_covariate_available_RD()*2+2 model to the no_covariate_available_RD()*(no_covariate_available_RD()+2)+1 model
            # standarlise and update covariate array
            cov[,2,ii+no_covariate_available_RD()*2+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
            cov[,2,ii+no_covariate_available_RD()*3+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
            cov[,2,ii+no_covariate_available_RD()*4+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
            cov[,2,ii+no_covariate_available_RD()*5+1][not_na_index]<- p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index]) 
            
            
            ## define covariates for phi
            ## from the no_covariate_available_RD()*2+2 model to the no_covariate_available_RD()*(no_covariate_available_RD()+2)+1 model
            # standarlise and update covariate array
            start_ind<- ii*no_covariate_available_RD()-(no_covariate_available_RD()-1)+no_covariate_available_RD()*2+1
            end_ind<- start_ind + no_covariate_available_RD()-1
            
            cov[,1,start_ind:end_ind][not_na_index]<- phi_cov_vec[not_na_index]- mean(phi_cov_vec[not_na_index])
            

            ## define covariates for p
            ## from the no_covariate_available_RD()*(no_covariate_available_RD()+2)+3 model to the end
            # standarlise and update covariate array
            cov[,2,ii+no_covariate_available_RD()*(no_covariate_available_RD()+2)+2][not_na_index]<- 
              p_cov_vec[not_na_index]- mean(p_cov_vec[not_na_index])

          }
          
          
        
          # the end of case input$phi_type_RD=="All" & input$p_type_RD=="All"
        }
        
        
          
      }
      
    } else {
      # no cov case
      NULL
    }
    
    # RD covariate(s)
    return(cov)
  })
  
  
  
  ###
  
  
  
  
  
  
  
  # **** run (kk)th RD model, kk_RD is an index ------

  kk.RD.prediction<- function(kk_RD=1){
    
    # kk index is an parameter
    force(kk_RD)
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    
    # read functions 
    inv <- function(m) {class(try(solve(m),silent=T))=="matrix"}
    
    # phi(c), p(c)
    RD.phic.pc.logl <- function (param,data, T, D, k, missed){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1]); phi12<-rep(phi12,sim.T-1); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[2]); p<-rep(p,sim.K)
      n01<-exp(param[3]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      L0=1-sum(alpha[1:sim.T,1:sim.k])
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data*log(L)) + n01*log(L0)
      
      return(-logL)
    }
    
    # phi(c), p(cov)
    RD.phic.pcov.logl <- function (param,data, T, D, k, missed, z){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1]); phi12<-rep(phi12,sim.T-1); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[2]+param[3]*z)
      n01<-exp(param[4]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      L0=1-sum(alpha[1:sim.T,1:sim.k])
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data*log(L)) + n01*log(L0)
      
      return(-logL)
    }
    
    # phi(cov), p(c)
    RD.phicov.pc.logl <- function (param,data, T, D, k, missed, z){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1]+param[2]*z); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[3]); p<-rep(p,sim.K)
      n01<-exp(param[4]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      L0=1-sum(alpha[1:sim.T,1:sim.k])
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data*log(L)) + n01*log(L0)
      
      return(-logL)
    }
    
    # phi(t), p(c)
    RD.phit.pc.logl <- function (param,data, T, D, k,missed){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1:((sim.T)-1)]); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[(sim.T)]); p<-rep(p,sim.K)
      n01<-exp(param[(sim.T)+1]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      L0=1-sum(alpha[1:sim.T,1:sim.k])
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data*log(L)) + n01*log(L0)
      
      return(-logL)
    }
    
    fit.phit.pc.optim<- function(param,data1=data1, T1=T1, D1=D1, k1=k1, missed=missed) {
      try(optim(par=param,
                fn=RD.phit.pc.logl,
                method="BFGS",
                #method="L-BFGS-B",lower = l.bound , upper = u.bound,
                hessian=TRUE,
                data=data1,T=T1,D=D1,k=k1,missed=missed),TRUE)}
    
    # phi(t), p(cov)
    RD.phit.pcov.logl <- function (param,data, T, D, k,missed,z){
      
      data[missed]=0
      
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1:((sim.T)-1)]); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[sim.T]+ param[sim.T+1]*z)
      n01<-exp(param[(sim.T)+2]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      L0=1-sum(alpha[1:sim.T,1:sim.k])
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data*log(L)) + n01*log(L0)
      
      return(-logL)
    }
    
    
    
    
 
    
    #*** the (k)th RD model according to the no. of cov
    
    kk_results_RD<-reactive({
      
      # run the model according to Settings
      
      nput_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
      
      if (run_button_counts_RD$counts!=0 & input_not_null == TRUE ) {
        
        input$run_ana_button_RD # run whenever RUN button is clicked
        
        
        if (input$phi_type_RD=="All" & input$p_type_RD=="All" ) {
          
          # *** update this for all models
          time_spent=numeric()
          
          # fit all possible combinations of RD models
         
          # phi(t), p(c) 
          
          #l.prob=0.001
          #u.prob=0.999
          #l.bound<- c(rep(logit(l.prob),(T_RD()/no_k()) ),log(0.0001))
          #u.bound<- c(rep(logit(u.prob),(T_RD()/no_k()) ),log(10000))
          
          # fit.phic.pc<- function(param,data1=data1, T1=T1, D1=D1, k1=k1, missed) {
          # try(optim(par=param,
          #            fn=RD.phit.pc.logl,
          #            method="BFGS",
                      #method="L-BFGS-B",lower = l.bound , upper = u.bound,
          #            hessian=TRUE,
          #            data=data1_RD,T=T_RD(),D=D_RD(),k=no_k(),missed=miss_occ_RD())),TRUE)}
          
          
          
          # input iteration number
          n.rep= isolate(input$num_iteration_RD)
          
          # initial estiamte elements
          N0.test<-p.test<-loglike.test<-numeric()
          phi12.test<-matrix(NA,ncol=T_RD()/no_k()-1,nrow=n.rep)
          
          j=1
          
          # Start the clock for optimisation =-=-=-=-
          ptm <- proc.time()
          
          while (j<(n.rep+1)) { 
            
            # initial starting param
            param=c(logit(runif((T_RD()/no_k()-1),0.1,0.9)), #phi12
                    logit(runif(1,0.1,0.9)),# p
                    log(runif(1,10,200)) ) # N0
            
            # *** save as n.rep elements
            fit.phit.pc <- fit.phit.pc.optim(param,
                                             data1=data1, T1=T_RD(), D1=D_RD(), k1=no_k(), missed=miss_occ_RD())
          }
          
          # Stop the clock =-=-=-=-
          t.record<-proc.time() - ptm
          
          # Time spent
          t.spent.phit.pc<- as.numeric(names(table(t.record[1])))

          
        }
        
       
        
        if (input$phi_type_RD=="Constant" & input$p_type_RD=="Constant" ) {
          
        }
        
        if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates" ) {
          
        }
        
        
        if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant" ) {
          
        }
        
        if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates" ) {
          
        }
        
        
        if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant" ) {
          
        }
        
        if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Covariates" ) {
          
        }
        
        
        
        
        }
        
      # kk_results_RD
      
      })
      
    
return(list(RD.phit.pc=fit.phit.pc,
            time_spent=t.spent.phit.pc))
    
  }
  
  
  
 
  
  # fit all possible models
  
  # update z1 and z2 for each of potential models
  
  all.RD.aic.models <-reactive({
    
    # Run when Run Analysis button is clicked
    # Reset when Reset button is clicked
    
    if (run_button_counts_RD$counts!=0) {
      
      input$run_ana_button
      
      
      # input$data will be NULL initially
      
      data_all<- dataInput_RD()
      data1_RD=data_all[,input$data_colnum_RD]
      
      
    
    }
    
    
    # ******* no. of param
    
    geo.pt.no.param<-c( 2 , 3, 3, 4, 5)
    
    # 
    geo.pc.logl<-geo.z1.logl<- geo.z2.logl<-geo.z1.plus.z2.logl<-geo.z1.times.z2.logl<-
      geo.pc.aic<-geo.z1.aic<-geo.z2.aic<-geo.z1.plus.z2.aic<-geo.z1.times.z2.aic<-numeric()
    
    for (kk in 1:total_no_fits()) {
      
      #cov_vec_index$cov_vec_k<- kk
      # tt<- isolate(cov_vec_index$cov_vec_k)
      
      # maximised loglikelihood
      geo.pc.logl[kk]= -kk.geo.pt.prediction(kk=kk)$geo.pc$value
      geo.z1.logl[kk]= -kk.geo.pt.prediction(kk=kk)$geo.z1$value
      geo.z2.logl[kk]= -kk.geo.pt.prediction(kk=kk)$geo.z2$value
      geo.z1.plus.z2.logl[kk]= -kk.geo.pt.prediction(kk=kk)$geo.z1.plus.z2$value
      geo.z1.times.z2.logl[kk]= -kk.geo.pt.prediction(kk=kk)$geo.z1.times.z2$value
      
      # AIC values
      geo.pc.aic[kk]= -geo.pc.logl[kk]*2 + 2*2
      geo.z1.aic[kk]= -geo.z1.logl[kk]*2 + 2*3
      geo.z2.aic[kk]= -geo.z2.logl[kk]*2 + 2*3
      geo.z1.plus.z2.aic[kk]= -geo.z1.plus.z2.logl[kk]*2 + 2*4
      geo.z1.times.z2.aic[kk]= -geo.z1.times.z2.logl[kk]*2 + 2*5
      
      # update (k)th model index
      # cov_vec_index$cov_vec_k<-isolate(cov_vec_index$cov_vec_k) + 1
      
    }
    
    geo.pt.maxlogl<-data.frame(geo.pc.logl=geo.pc.logl,
                               geo.z1.logl=geo.z1.logl,
                               geo.z2.logl=geo.z2.logl,
                               geo.z1.plus.z2.logl=geo.z1.plus.z2.logl,
                               geo.z1.times.z2.logl=geo.z1.times.z2.logl)
    
    
    return(list(geo.pt.no.param=geo.pt.no.param,
                geo.pt.maxlogl=geo.pt.maxlogl
    ))
    
  })
  
  
  
  
  
  
   
  # old RD.prediction 
  RD.prediction <- reactive({
    
    # Run when Run Analysis button is clicked
    # Reset when Reset button is clicked
    
    if (run_button_counts_RD$counts!=0) {
    
    input$run_ana_button
      
      
    # input$data will be NULL initially
      
    data_all<- dataInput_RD()
    data1_RD=data_all[,input$data_colnum_RD]
    
    
    # read functions
    
    RD.logl <- function (param,data, T, D, k,missed){
      
      data[missed]=0
      
      ######	 fixed total
      sim.K=T  #  total no. of occasions
      sim.k= k # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
      sim.T=sim.K/sim.k  # no. of primary occasions
      
      # param
      phi12<-expit(param[1:((sim.T)-1)]); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
      p <- expit(param[(sim.T)]); p<-rep(p,sim.K)
      n01<-exp(param[(sim.T)+1]);N=n01+D
      
      
      phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
      for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
      
      
      # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
      alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 1 (observable) & not being removed
      beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      # prob of being in state 2 (unobservable) & not being removed
      beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
      
      
      # jj=1, ii=1
      alpha[1,1]<- pi[1]*p[1]
      beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
      beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
      
      # jj=2:sim.k, ii=1
      
      for (jj in 2:sim.k){
        
        alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
        beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
        beta2[1,jj]<- (beta2[1,jj-1])
      }
      
      
      #   #   #   #   #   #   #   #   #   from 2nd primary period
      for (ii in 2:sim.T) {
        
        phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
        phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
        
        for (jj in 1:sim.k) {
          
          if (jj==1) {
            alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
            beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
            beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
          }
          else {
            alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
            beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
            beta2[ii,jj]<- beta2[ii,jj-1]
          }
          
        }
      }
      
      L0=1-sum(alpha[1:sim.T,1:sim.k])
      
      L=numeric()
      
      o=1
      for (ii in 1:sim.T) {
        
        for (jj in 1:sim.k) {
          
          L[o]=alpha[ii,jj]
          o=o+1
        }}
      
      L=L[1:sim.K]  
      
      L[which(L==0)]<-10^(-10)
      L[missed]<-10^(-10)
      
      logL=lgamma(N+1)-sum(lgamma(data[-missed]+1))-lgamma(n01+1)+sum(data*log(L)) + n01*log(L0)
      
      return(-logL)
    }
 
    
       ## *** add iter_num
    
    
    l.prob=0.001
    u.prob=0.999
    l.bound<- c(rep(logit(l.prob),(T_RD()/no_k()) ),log(0.0001))
    u.bound<- c(rep(logit(u.prob),(T_RD()/no_k()) ),log(10000))
    
    # initial starting param
    param=c(logit(runif((T_RD()/2-1),0.1,0.9)), #phi12
            logit(runif(1,0.1,0.9)),# p
            log(runif(1,10,100)) ) #N
    
   
    # testv=RD.logl(param=param,data=data1_RD,T=T_RD(),D=D_RD(),k=no_k(),missed=miss_occ_RD())
    
    # RD.logl(param=param,data=sim_data,T=length(sim_data),D=sum(sim_data),k=2,
    #           missed=which(is.na(sim_data)) )
    
    #optim(par=param,
    #      fn=RD.logl,
    #      #method="BFGS",
    #      method="L-BFGS-B",lower = l.bound , upper = u.bound,
    #      hessian=TRUE,
    #      data=sim_data,T=length(sim_data),D=sum(sim_data),k=2,
    #     missed=which(is.na(sim_data)) )
    
    
    
    # *** Start the clock for optimisation =-=-=-=-
    ptm <- proc.time()
    
    fit.RD<-optim(par=param,
                  fn=RD.logl,
                  method="BFGS",
                  #method="L-BFGS-B",lower = l.bound , upper = u.bound,
                  hessian=TRUE,
                  data=data1_RD,T=T_RD(),D=D_RD(),k=no_k(),missed=miss_occ_RD())
    
    # *** Stop the clock =-=-=-=-
    t.record<-proc.time() - ptm
    
    # *** Time spent
    t_spent<- as.numeric(names(table(t.record[1])))
    
    
    } else {
      
      fit.RD=NULL
      fitted=NULL
      t_spent=NULL
  }
    
    return(list(fitted=fit.RD,
                t_spent=t_spent))
    
  })
  
  

  
  
    
  ################################  estimates from RD
  
  ept.phi12.RD<- reactive({ 
    expit(RD.prediction()$par[1:(T_RD()/no_k()-1)])
  })
  
  ept.p.RD<- reactive({ 
    expit(RD.prediction()$par[T_RD()/no_k()])
  })
  
  ept.N.RD<- reactive({ 
    exp(RD.prediction()$par[T_RD()/no_k()+1]) + D_RD()
  })
  
  
  ################################  predicted numbers from RD
  
  ept.num.RD<- reactive({ 
    
    ept.data<-rep(0,T())
    
    
    ######	 fixed total
    sim.K= T_RD()  #  total no. of occasions
    sim.k= no_k() # as.integer(exp( param(1) ) )   # no. of secondary occasisons (param)
    sim.T=sim.K/sim.k  # no. of primary occasions
    
    # param
    phi12<-ept.phi12.RD(); phi21 <- 1-phi12; pi<-c(mean(phi21),1-mean(phi21))
    p <- ept.p.RD(); p<-rep(p,sim.K)
    
    
    phi <-  list(matrix(rep(c(0,1),2), ncol=2,byrow=TRUE)); phi <- rep(phi,(sim.T)-1)
    for (j in 1:((sim.T)-1)) {phi[[j]]<- matrix(c(1-phi12[j],phi12[j],phi21[j],1-phi21[j]), ncol=2,byrow=TRUE)}
    
    
    # prob of being removed, primary periods in rows (1,), secondary in cols (1,1)
    alpha<-matrix(NA,ncol=sim.K, nrow=sim.K)
    
    # prob of being in state 1 (observable) & not being removed
    beta1<-matrix(NA,ncol=sim.K, nrow=sim.K)
    
    # prob of being in state 2 (unobservable) & not being removed
    beta2<-matrix(NA,ncol=sim.K, nrow=sim.K)
    
    
    # jj=1, ii=1
    alpha[1,1]<- pi[1]*p[1]
    beta1[1,1]<- (alpha[1,1]/p[1])*(1-p[1]) # state 1 & not being removed 
    beta2[1,1]<- 1-(alpha[1,1]/p[1])                # state 2 & not being removed
    
    # jj=2:sim.k, ii=1
    
    for (jj in 2:sim.k){
      
      alpha[1,jj]<- (alpha[1,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
      beta1[1,jj]<- (beta1[1,jj-1]*(1-p[jj]))
      beta2[1,jj]<- (beta2[1,jj-1])
    }
    
    
    #   #   #   #   #   #   #   #   #   from 2nd primary period
    for (ii in 2:sim.T) {
      
      phi11=phi[[ii-1]][1,1]; phi12=phi[[ii-1]][1,2];
      phi21=phi[[ii-1]][2,1]; phi22=phi[[ii-1]][2,2];       
      
      for (jj in 1:sim.k) {
        
        if (jj==1) {
          alpha[ii,1]<- (beta1[ii-1,sim.k]*phi11 + beta2[ii-1,sim.k]*phi21)*p[ii]
          beta1[ii,1]<- (alpha[ii,1]/p[ii])*(1-p[ii])  # state 1 & not being removed 
          beta2[ii,1]<-  beta1[ii-1,sim.k]*phi12 + beta2[ii-1,sim.k]*phi22    # state 2 & not being removed
        }
        else {
          alpha[ii,jj]<- (alpha[ii,jj-1]/p[jj-1]*(1-p[jj-1])*p[jj])
          beta1[ii,jj]<- beta1[ii,jj-1]*(1-p[ii])
          beta2[ii,jj]<- beta2[ii,jj-1]
        }
        
      }
    }
    
    L0=1-sum(alpha[1:sim.T,1:sim.k])
    
    L=numeric()
    
    o=1
    for (ii in 1:sim.T) {
      
      for (jj in 1:sim.k) {
        
        L[o]=alpha[ii,jj]
        o=o+1
      }}
    
    L=L[1:sim.K]  
    
    
    ept.data=  ept.N.RD() * L
    
  })
  
  ################################  SE from RD  
  
  
  se.p.RD<- reactive({ 
    se<-sqrt(diag(solve(RD.prediction()$hessian)))
    se<-expit(se[T_RD()/no_k()])
    se
  })
  
  se.n01.RD<- reactive({ 
    se<-sqrt(diag(solve(RD.prediction()$hessian)))
    se<-exp(se[T_RD()/no_k()+1])
    se
  })
  
  
  ################################  estimate table results from RD  
  est.tab_RD<-reactive({ 
    est.tab.results<-data.frame(
      Param_name = c("Capture probability", "Population size"),
      Notation = c("p", "N"),
      Estimate= c(ept.p.RD(),ept.N.RD()),
      Standard_error= c(se.p.RD(),se.n01.RD()),
      Confidence_interval= c("","")
    )
    
    est.tab.results
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################  
  # outputs
  ################################  preview data head
  #output$data1 <- renderTable({
  
  #  data_all<- dataInput()
  #  return(head(data_all))
  #})
  
  #============================ classic model
  output$data1 <-DT::renderDataTable({
    data_all<- dataInput()
    DT::datatable(data_all)
  })
  
  ################################ predicted plot
  
  # read the number of times hitted run analysis button
  
 #tags$script("$(document).on('click', '#reset_ana_button', 
#            function () {
#              Shiny.onInputChange('run_ana_button',0);
#             }
#            );"
#            )
  
  # set new reactive values of the number of hits of actionButton
  #  as impossible to reset value of an actionButton == 0
  
  run_button_counts <- reactiveValues(counts = 0)
  
  # Each time (run) button is clicked, add 1 to reactive value of run_button_counts$counts
  observeEvent(input$run_ana_button,
               {run_button_counts$counts =  isolate(run_button_counts$counts) +1 }
               )
  
  observeEvent(input$reset_ana_button,
               {run_button_counts$counts = 0}
               )
 # run_button_counts<-reactive({isolate(input$run_ana_button)
 #    })
  
 # default_plot <- eventReactive(input$reset_ana_button, {
#    head(cars)
 # })
  
  # reset_run_button_counts<-observeEvent(input$reset_ana_button,
  #                                      isolate(run_ana_button)=0)

  
  #############################
  
  all_list_model_tab<-reactive({
    
    no_cov<-no_covariate_available()
    
     if (input$cov_yn=="Yes" && no_cov>1){
        
        ## when there are more than 1 covaraiete available
        
        # pcov name vector 
        one_cov_vec<- paste0("p(cov",1:no_cov,")") # when one cov is considered 
        
        # read index vectors
        first_cov_index<-two_cov_combin_index_vec()$orignal_index_vec[,1]
        second_cov_index<-two_cov_combin_index_vec()$orignal_index_vec[,2]
        
        two_cov_vec_plus<- paste0("p(cov",first_cov_index,
                                  "+cov",second_cov_index,")"
                                  )
        
        two_cov_vec_plus_times<- paste0("p(cov",first_cov_index,
                                        "+cov",second_cov_index,
                                        "+cov",first_cov_index,
                                        "*cov",second_cov_index,")"
                                        )
        
        two_cov_vec<-character(total_no_fits()$no_two_cov_cases)
        for (i in 1:(length(two_cov_vec)/2)) {
          two_cov_vec[i*2-1]<-two_cov_vec_plus[i]
          two_cov_vec[i*2]<-two_cov_vec_plus_times[i]
        }
        
        # model_name_vec
        model_name_vec<-c(paste("p(c)"),
                          one_cov_vec,
                          two_cov_vec)
        
        
        # p_cov_used_vec when more than one covs are considered
  
        # initialise two_cov_names_vec
        two_cov_names_vec<- character()
        
        for (ii in 1:choose(no_covariate_available(),2)) {
          
          cov1_name<-data_cov_names()[two_cov_combin_index_vec()$orignal_index_vec[ii,1]]
          cov2_name<- data_cov_names()[two_cov_combin_index_vec()$orignal_index_vec[ii,2]]
          
          two_cov_names_vec[ii*2-1] <- paste0("  ",cov1_name, " & ",cov2_name)
          two_cov_names_vec[ii*2] <- paste0("  ",cov1_name," & ",cov2_name)
        }
        
        
        p_cov_used_vec<-c(paste("  -"),
                          paste0("  ",data_cov_names()),
                          two_cov_names_vec)
        
     } 
    
    if (input$cov_yn=="Yes" && no_cov==1){
        
        # pcov vector when one cov is considered 
        model_name_vec<- paste0("p(c)"," p(cov",1:no_cov,")")
        
        # p_cov_used_vec
        p_cov_used_vec<-c(paste("-"), 
                          paste("cov"))
        
      }  
    
    if (input$cov_yn=="No"){
      
      # input$cov_yn=="No"
      # fit geometric model only
      
      model_name_vec<-paste0("p(c)")
      p_cov_used_vec<- paste0("-")
      
    }
    
    
    Num<- seq_along(model_name_vec)
    
    all_model_tab<-data.frame(Num= Num,
                              Model=model_name_vec,
                              p_cov_used=p_cov_used_vec
                              )
    
    return(list(all_model_tab=all_model_tab,
                Num=Num,
                Model=model_name_vec,
                p_cov_used=p_cov_used_vec))
    
  })
  
  
  
  
  
  # all_model_table in the Settings section.
  output$all_model_table<-renderFlexTable({
    
    print_all_list_model_tab<-all_list_model_tab()$all_model_tab
    print_all_list_model_tab<-vanilla.table(print_all_list_model_tab)
    
    print_all_list_model_tab[, c("Num","Model"), to ="header"] <- parLeft() # left text
    print_all_list_model_tab[, c("Num","Model")] <- parLeft() # left align header
    
    return(print_all_list_model_tab)
  })
  
  
  
  #########################
  # *** all_ana_model_table in the Analysing section
  #
  all_ana_model_table<-reactive({
    
    
    if (run_button_counts$counts!=0) {
      
      # num_max vector, the number of times a model hitting the maximum logl
      
      Num<-all_list_model_tab()$Num
      
      # model_name_vec
      
      model_name_vec_RD<-all_list_model_tab()$Model
      
      # cov_used_vec
      
      p_cov_used<-all_list_model_tab()$p_cov_used
      
      
      #
      
     # time_spent<-time_all_ana_model_table()
      
      time_spent<- numeric()
      time_spent[1]<-all.geo.prediction()$pc.prediction$time_spent
      #time_spent<-rep(time_spent,length(all_list_model_tab()$Num))
      
      for (ii in 1:total_no_fits()$no_one_cov_cases ) {
        
        time_spent[ii+1]<- all.geo.prediction()$pt.1cov.prediction[[ii]]$time_spent.1cov
      }
      #time_spent[2:(1+total_no_fits()$no_one_cov_cases)]<- 
      #  all.geo.prediction()$pt.1cov.prediction[[1:total_no_fits()$no_one_cov_cases]]$time_spent.1cov
      
      
      for (jj in 1:choose(no_covariate_available(),2) ) {
        
        time_spent[jj*2-1+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$time_spent.2cov
        
        time_spent[jj*2 +(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$time_spent.2cov.times
      }
      
      
      #num_max<-all_list_model_tab()$Num
      
      num_max<- numeric()
      
      num_max[1]<- all.geo.prediction()$pc.prediction$num.max.location
      
      for (jj in 1:total_no_fits()$no_one_cov_cases ) {
        num_max[jj+1]<- all.geo.prediction()$pt.1cov.prediction[[jj]]$num.max.location.1cov
        
      }
      
      
      for (jj in 1:choose(no_covariate_available(),2) ) {
        
        num_max[jj*2-1+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$num.max.location.2cov
        
        num_max[jj*2+(1+total_no_fits()$no_one_cov_cases)] <- all.geo.prediction()$pt.2cov.prediction[[jj]]$num.max.location.2cov.times
        
      }
      
      
      #num_max<- numeric()
      #num_max[1]<- all.geo.prediction()$pt.2cov.prediction[[2]]$num.max.location.2cov.times
      #num_max<- rep(num_max,length(all_list_model_tab()$Num))
        
      
      # chechinput to print out the max_logls 
      # rep("", 6)
      
      print_logLs<-rep(c(""),total_no_fits()$no_fits)
      
      
      # Table to print
      draft_all_model_ana_tab_RD<-data.frame(Num= Num,
                                             Model=model_name_vec_RD,
                                             p_cov_used=p_cov_used,
                                             time_spent=time_spent,
                                             num_max= num_max,
                                             print_logLs=print_logLs)
      
    } else {
      
      # if reset
      
      
      # num_max vector, the number of times a model hitting the maximum logl
      
      Num<-all_list_model_tab()$Num
      
      # model_name_vec_RD
      
      model_name_vec_RD<-all_list_model_tab()$Model
      
      # cov_used_vec_RD
      
      p_cov_used<-all_list_model_tab()$p_cov_used
      
      
      # time_spent vector, two cov case
       time_spent<-rep(c(""),total_no_fits()$no_fits)
      
      #time_spent<-numeric()
      #time_spent[1]<- hh.geo.pc.prediction(hh=1)$time_spent
      #time_spent[2:(1+total_no_fits()$no_one_cov_cases)]<- hh.geo.pc.prediction(hh=1)$time_spent
      
      
      # model_condition, waiting (0/num_iteration, blue), 
      # running(c(1:(num_iteration-1))/num_iteration, yellow), done (num_iteration/num_iteration, green)
      # no. of times hitting the MLE  
      
      num_max<- rep(c(""),total_no_fits()$no_fits)
      
     
      # chechinput to print out the max_logls 
      
      print_logLs<-rep(c(""),total_no_fits()$no_fits)
      
      
      # Table to print
      draft_all_model_ana_tab_RD<-data.frame(Num= Num,
                                             Model=model_name_vec_RD,
                                             p_cov_used=p_cov_used,
                                             time_spent=time_spent,
                                             num_max= num_max,
                                             print_logLs=print_logLs)
      
    }

   
    # returns one table element
    return(draft_all_model_ana_tab_RD)
    
  })
  
  
  
  
  output$all_ana_model_table<-renderFlexTable({
    
    # Create checkboxes
    #all_ana_model_table_RD()$all_ana_model_table_RD$print_max_logLs <- 
    #                        paste0('<label><input type="checkbox" id="car', 
    #                        1:6, '"> <span>') # 1:total_no_models()
    #rownames(mymtcars), '</span></label>')
    
    # flex_table <- vanilla.table(d_all_ana_model_table_RD()$draft_all_model_ana_tab_RD) # convert to FlexTable objet
    
    # flex_table[, draft_all_model_ana_tab_RD$print_max_logLs(), to = "header"] <- parLeft() # left align checkboxes
    
    #  flex_table[, draft_all_model_ana_tab_RD$print_max_logLs()] <- parLeft() # left align header
    
    # all_model_ana_tab_RD<- flex_table
    
    flex_tab<-all_ana_model_table()
    
    flex_Num<-flex_tab$Num
    
    null_values<-flex_tab$print_logLs
    
    flex_tab$print_logLs<-  paste0('<label><input type="checkbox" id="flex_Num', 
                                   1:total_no_fits()$no_fits, '"> <span>', # 1:total_no_models()
                                   null_values, '</span></label>')
    
    # convert to FlexTable objet
    flex_table<- vanilla.table(flex_tab)
    
    flex_table[, c("Num","Model"), to = "header"] <- parLeft() # left align the column "Num"
    flex_table[, c("Num","Model")] <- parLeft() # left align header

    
    return(flex_table)
  })
  
  
  
  # the inputs created are in input$car1, input$car2, ...
  output$check_mle <- renderPrint({
    
    
    test_tab<- all_ana_model_table()
    
    flex_Num<-test_tab$Num
    
    
    # results
    res <- unlist(lapply(1:length(flex_Num), function(i) input[[paste0("flex_Num", i)]]))
    
    
    if (any(res)) {
      
      # selected model number
      model_no<- flex_Num[res]
      
      if (model_no==1){
        print(all.geo.prediction()$pc.prediction$mle.test$loglike)
      }
      
      
      no_one_cov_case<-2:((total_no_fits()$no_one_cov_cases)+1)
      
      if (model_no %in% no_one_cov_case) {
        
        print(all.geo.prediction()$pt.1cov.prediction[[res-1]]$mle.test.1cov$loglike)
      }
      
      
      no_two_cov_case<-((total_no_fits()$no_one_cov_cases)+2):length(all_list_model_tab()$Num)
      
      if (model_no %in% no_two_cov_case) {
        
        if ((res-((total_no_fits()$no_one_cov_cases)+1)) %% 2 == 0  ){
          
          # even number
          
          index<-(res-(1+total_no_fits()$no_one_cov_cases))/2  
          
          print(all.geo.prediction()$pt.2cov.prediction[[index]]$mle.test.2cov.times$loglike)
          
          
          } else {
          
            # odd number
            
            index<-(res-(1+total_no_fits()$no_one_cov_cases)+1)/2
            
            print(all.geo.prediction()$pt.2cov.prediction[[index]]$mle.test.2cov$loglike)
            
        }
        
      }
      
      
      
      print(flex_Num[res])
      
    }
  
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################3
  # show predicted data plot
  output$plot <- renderPlot({

    # plot if hit run button 
    
    if (run_button_counts$counts!=0) {
    
    # isolate this to avoid reactive plot function
    data_plot<- isolate(download_plot())
    
    } else {data_plot<- NULL}
    
    #predicted.num<-ept.num.geo()
    #lines(c(1:T()), predicted.num, col = 'blue', lwd=2) 
    
    data_plot
    
  })
  
  
  
  # download plot

  output$downloadPlot <- downloadHandler(
    
    filename <- function() {
      paste('plot', 'png', sep = ".")
    },
    
    content <- function(file) {
      png(file) # add more arguments for pic
      
      #predicted.plot<-classic_data_plot()
      #predicted.num<-ept.num.geo()
      #lines(c(1:T()), predicted.num, col = 'blue', lwd=2)
      
      print(download_plot())
      
      dev.off()
    },
    contentType = "image/png"
    
  )
  
  
  # predicted data and CI as a table
  
  download_predicted_data<-reactive({
    
    data_all<- dataInput()
    data_ploints<-data_all[,input$data_colnum]
    
    predicted_data_CI_table<-data.frame(
      Sampling_occasion = c(1:T()),
      Data_used = data_ploints
      # add predicted column
      
      # add CI
    )
    
    return(predicted_data_CI_table)
    
  })
  
  
  # Output: download predicted data and confidence interval
  output$downloadPredicted <- downloadHandler(
    filename = function() {
      paste("predicted_counts",download_predicted_data(), ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(download_predicted_data(), file, row.names = FALSE)
    }
  )
  
  
  
  ################################  model comparison according to AIC
  output$fit_table <- renderTable({
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    if (input$cov_yn=="Yes"){ 
      
      output<-geo.model.tab()
      
    } else {
      output<-geo.model.tab()
      
      }
 
    return(output)
  }) 
  
  
  ################################  estimates results
  output$estimates <- renderTable({
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    estimates.tab.results<-data.frame(
      Model = geo.aic.models()$model.names.ordered,
      p_cov_used = geo.aic.models()$p_cov_used_ordered,
      No_param=geo.aic.models()$geo.pt.no.param.ordered,
      max_logL=geo.aic.models()$geo.pt.maxlogl.ordered,
      AIC=geo.aic.models()$aic.ordered,
      delta.AIC= geo.aic.models()$delta.aic.ordered
    )
    
    return()
  })  
  
  
  ################################  table of counts results
  output$predicted_table <-  DT::renderDataTable({
    
    data_all<- dataInput()
    
    data1=data_all[,input$data_colnum]
    
    est.num.results<-data.frame(
      Sampling_occasion = c(1:T()),
      Observed_data = data1,
      Predicted_data = ept.num.geo()
    )
    
    #out<-DT::datatable(est.num.results)
    return(est.num.results)
    
  }) 
  
  ################################ data summary

  
  data_summary_tab<- reactive({
    
    data_all<- dataInput()
    data1=data_all[,input$data_colnum]
    
    if (input$cov_yn=="Yes") {
      num_cov<- ceiling(no_covariate_available())
    } else {
      num_cov<-c("Not Selected")
    }
    
    data_summary<-data.frame(
      Length_of_study = T(),
      Total_removed_number = ceiling(D()),
      Number_covariate_available= num_cov
    )
    
    return(data_summary)
    
  })
  
  # Output: data_summary
  output$data_summary <- renderTable({
    
    # output depends on the reactive function data_summary_tab()
    output<-data_summary_tab()
    
    return(output)
    
  }) 
  
  ### test 
  output$out2 <-  renderTable({
    
    # Take a dependency on input$goButton. This will run once initially,
    # because the value changes from NULL to 0.
    # input$run_ana_button
    
    # Use isolate() to avoid dependency on other inputs
    
    data_all<- isolate(dataInput())
    data1=data_all[,input$data_colnum]
    
    #kk.geo.pt.2cov.prediction(kk=1)
    #pp<-all.geo.prediction()$pc.prediction
    
    #num_one_unique_cov_case<-total_no_fits()$no_one_cov_cases
    
    # initialise pt.1cov.prediction as a list to store results for each model with one cov
    
    #ooo<-all.geo.prediction$pt.2cov.prediction[[2]]
    # gg.geo.pt.1cov.prediction(gg=gg)
    
    #ppp<- all.geo.prediction()$pc.prediction
    #pt.1cov.prediction[[2]]$time_spent.1cov
    #re=ooo$time_spent.2cov.times
    
    #test.list<-list()
    #test.list[[2]]<-all.geo.prediction()$pt.2cov.prediction[[2]]
    
    
    return( )
  })
  
  
  
  
  
  
  
  
  
  #============================ RD model outputs
  
  # Output: preview data
  output$data1_RD <-DT::renderDataTable({
    data_all<- dataInput_RD()
    DT::datatable(data_all)
  })  
 
  # Output: RD data summary
  
  data_summary_tab_RD<- reactive({
    
    data_all<- dataInput_RD()
    data1=data_all[,input$data_colnum_RD]
    
    if (input$cov_yn_RD=="Yes") {
      num_cov<- ceiling(no_covariate_available_RD())
    } else {
      num_cov<-"Not Selected"
    }
    
    data_summary<-data.frame(
      Length_of_study = T_RD(),
      Total_removed_number = ceiling(D_RD()),
      Number_covariate_available= num_cov
    )
    
    return(data_summary)
    
  })
  
  
  # Output: data summary table in RD Settings section
  output$data_summary_RD <- renderTable({
    
    # depends on the reactive function data_summary_tab_RD()
    output<-data_summary_tab_RD()
    
    return(output)
    
  }) 
  
  

  ## all_list_model_tab_RD() function
  all_list_model_tab_RD<-reactive({
  
  input_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
  
  if ( input_not_null == TRUE ) {
    
    #model_name_vec_RD
    
    if (no_covariate_available_RD()>0) {
      
    no_cov<-no_covariate_available_RD()

    # phic + pcov vector
    phic_pcov_vec<- paste0("phi(c), p(cov",1:no_cov,")")
      
    # phicov + pc + pcov vector
    
      # phicov + pc
    phicov_pc0<- paste0("phi(cov",1:no_cov,")")
    phicov_pc<- paste0(phicov_pc0,", p(c)")
    
      #phicov + pcov
    pcov_vec<- paste0(", p(cov",1:no_cov,")")

    phicov_pcov<- c()
    
    for (i in 1:no_cov) {
      phicov_pcov<- c(phicov_pcov,paste0(phicov_pc0[i], pcov_vec))
    }

    # *** change phicov + pc case order
    phicov_pc_pcov<-c(phicov_pc,phicov_pcov)
    
    
    # phit + pcov
    phit_pcov<- paste("phi(t)", pcov_vec)
    
      
    # model_name_vec_RD
    model_name_vec_RD<-c(paste("phi(c), p(c)"),
                         phic_pcov_vec,
                         phicov_pc_pcov,
                         paste("phi(t), p(c)"),
                         phit_pcov
    )
    
    
    
    ####### ** covariate names vectors
    
    #** phi_cov_used_vec_RD according to covariate names
    
    # (phicov) + pcov
    phicov_pcov_phiname <- character()
    for (ii in 1:no_cov) {
      phicov_pcov_phiname[(ii*no_cov-(no_cov-1)):(ii*no_cov)] <- data_cov_names_RD()[ii]
      
    }
    
    # final phi_cov_used_vec_RD
    phi_cov_used_vec_RD<-c(paste("-"),
                           rep(paste("-"),no_cov), # (phic) + pcov
                           data_cov_names_RD(), # (phicov) + pc
                           phicov_pcov_phiname, # (phicov) + pcov
                           paste("-"),  # (phit) + pc
                           rep(paste("-"),no_cov) # (phit) + pcov
    )
    
    
    
    #** p_cov_used_vec_RD according to covariate names
    
    p_cov_used_vec_RD<-c(paste("-"),
                         data_cov_names_RD(), # phic + (pcov)
                         rep(paste("-"),no_cov), # phicov + (pc)
                         rep(data_cov_names_RD(),no_cov), # phicov + (pcov)
                         paste("-"), # phit + (pc)
                         data_cov_names_RD() # phit + (pcov)
                         )
    
    } else {
      
      # no cov 
      
      # model_name_vec_RD
      model_name_vec_RD<-c(paste("phi(c), p(c)"),
                           paste("phi(t), p(c)")
      )
      
      # phi_cov_used_vec_RD
      phi_cov_used_vec_RD<-c(paste("-"),
                             paste("-")  # (phit) + pc
      )
      
      # p_cov_used_vec_RD
      p_cov_used_vec_RD<-c(paste("-"),
                           paste("-") # phit + (pc)
      )
    }
    
    
    
    
    
    if (input$phi_type_RD=="All" & input$p_type_RD=="All" ) {
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD),
                                   Model=model_name_vec_RD,
                                   phi_cov_used=phi_cov_used_vec_RD, # p cov if length(cov)=1
                                   p_cov_used=p_cov_used_vec_RD # phi cov
      )
      
      
    } 
    
    #
    if (input$phi_type_RD=="Constant" & input$p_type_RD=="Constant") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD[1]),
                                   Model=model_name_vec_RD[1],
                                   phi_cov_used=p_cov_used_vec_RD[1], # p cov if length(cov)=1
                                   p_cov_used=p_cov_used_vec_RD[1] # phi cov
      )
    }
    
    #
    if (input$phi_type_RD=="Constant" & input$p_type_RD=="Covariates") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD[c(1:2)]),
                                   Model=model_name_vec_RD[1:2],
                                   phi_cov_used=p_cov_used_vec_RD[1:2], # p cov if length(cov)=1
                                   p_cov_used=p_cov_used_vec_RD[1:2] # phi cov
      )
    }
    
    #
    if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Constant") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD[c(1,3)]),
                                   Model=c(model_name_vec_RD[1],model_name_vec_RD[3]),
                                   phi_cov_used=c(model_name_vec_RD[1],p_cov_used_vec_RD[3]), # p cov if length(cov)=1
                                   p_cov_used=c(p_cov_used_vec_RD[1] ,p_cov_used_vec_RD[3]) # phi cov
      )
      
    }
    
    #
    if (input$phi_type_RD=="Covariates" & input$p_type_RD=="Covariates") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD[c(1:4)]),
                                   Model=c(model_name_vec_RD[1:4]),
                                   phi_cov_used=c(model_name_vec_RD[1:4]), # p cov if length(cov)=1
                                   p_cov_used=c(p_cov_used_vec_RD[1:4]) # phi cov
      )
      
    }
    
    #
    if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Constant") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD[c(1,5)]),
                                   Model=c(model_name_vec_RD[1],model_name_vec_RD[5]),
                                   phi_cov_used=c(model_name_vec_RD[1],model_name_vec_RD[5]), # p cov if length(cov)=1
                                   p_cov_used=c(p_cov_used_vec_RD[1],p_cov_used_vec_RD[5]) # phi cov
      )
      
    }
    
    #
    if (input$phi_type_RD=="Time-varying" & input$p_type_RD=="Covariates") {
      
      
      all_model_tab_RD<-data.frame(Num= seq_along(model_name_vec_RD[c(1:2)]),
                                   Model=c(model_name_vec_RD[1:2],model_name_vec_RD[5:6]),
                                   phi_cov_used=c(model_name_vec_RD[1:2],model_name_vec_RD[5:6]), # p cov if length(cov)=1
                                   p_cov_used=c(p_cov_used_vec_RD[1:2],p_cov_used_vec_RD[5:6]) # phi cov
      )
      
    }
    
  } else { all_model_tab_RD<- data.frame(Num= "",
                                         Model= "",
                                         phi_cov_used= "", # p cov if length(cov)=1
                                         p_cov_used="" # phi cov
  ) }
    
  return(all_model_tab_RD)
  
  })
    
    
    
    
  
  
  
  
  # Output: all_model_table_RD in the Settings section
  output$all_model_table_RD<-renderFlexTable({
    
     print_all_list_model_tab_RD<-all_list_model_tab_RD()
     print_all_list_model_tab_RD<-vanilla.table(print_all_list_model_tab_RD)
     
     print_all_list_model_tab_RD[, c("Num","Model"), to = "header"] <- parLeft() # left align the column "Num"
     print_all_list_model_tab_RD[, c("Num","Model")] <- parLeft() # left align header
     
    return(print_all_list_model_tab_RD)
  })
  
  
  
  
  # *** all_ana_model_table_RD in the Analysing section
  
all_ana_model_table_RD<-reactive({
    
  input_not_null <- isTRUE(input$p_type_RD!="" & input$phi_type_RD!="")
    
  if ( input_not_null == TRUE ) {
    
    if (run_button_counts_RD$counts!=0) {

      # num_max vector, the number of times a model hitting the maximum logl
      
      Num<-all_list_model_tab_RD()$Num
      
      # model_name_vec_RD
      
      model_name_vec_RD<-all_list_model_tab_RD()$Model
      
      # cov_used_vec_RD
      
      p_cov_used<-all_list_model_tab_RD()$p_cov_used
      phi_cov_used<-all_list_model_tab_RD()$phi_cov_used
      
      
      #*** time_spent vector, two cov case
      time_spent<-rep(c(""),total_no_fits_RD())
      
      
      #*** no. of times hitting the MLE  
      
      num_max<- rep(c(""),total_no_fits_RD())
      
      
      # chechinput to print out the max_logls 
      
      print_logLs<-rep(c(""),total_no_fits_RD())

      
    } else {
      
      # num_max vector, the number of times a model hitting the maximum logl
      
      Num<-all_list_model_tab_RD()$Num
      
      # model_name_vec_RD
      
      model_name_vec_RD<-all_list_model_tab_RD()$Model
      
      # cov_used_vec_RD
      
      p_cov_used<-all_list_model_tab_RD()$p_cov_used
      phi_cov_used<-all_list_model_tab_RD()$phi_cov_used
      
      # if reset or did not hit the run button
      time_spent<-rep(c(""),total_no_fits_RD())
      
      num_max<- rep(c(""),total_no_fits_RD())
      
      print_logLs<-rep(c(""),total_no_fits_RD())
    }
    
    
    # Table to print out *** all_list_
    draft_all_model_ana_tab_RD<-data.frame(Num= Num,
                                           Model=model_name_vec_RD,
                                           phi_cov_used=p_cov_used,
                                           p_cov_used=p_cov_used,
                                           time_spent=time_spent,
                                           num_max= num_max,
                                           print_logLs=print_logLs)
    
    } else { 
      
   draft_all_model_ana_tab_RD<- data.frame(Num= "",
                                           Model= "",
                                           phi_cov_used= "", 
                                           p_cov_used= "", 
                                           time_spent="",
                                           num_max= "",
                                           print_logLs=""
                                           ) 
    }
    
    # returns one table element
    return(draft_all_model_ana_tab_RD)
    
  })
  
  
  
  output$all_ana_model_table_RD<-renderFlexTable({
    
    # Create checkboxes
    #all_ana_model_table_RD()$all_ana_model_table_RD$print_max_logLs <- 
    #                        paste0('<label><input type="checkbox" id="car', 
    #                        1:6, '"> <span>') # 1:total_no_models()
                            #rownames(mymtcars), '</span></label>')
    
    # flex_table <- vanilla.table(d_all_ana_model_table_RD()$draft_all_model_ana_tab_RD) # convert to FlexTable objet
    
    # flex_table[, draft_all_model_ana_tab_RD$print_max_logLs(), to = "header"] <- parLeft() # left align checkboxes
    
    #  flex_table[, draft_all_model_ana_tab_RD$print_max_logLs()] <- parLeft() # left align header
    
    # all_model_ana_tab_RD<- flex_table
    
    flex_tab<-all_ana_model_table_RD()
    
    flex_Num<-flex_tab$Num
    
    null_values<-flex_tab$print_logLs
    
    flex_tab$print_logLs<-  paste0('<label><input type="checkbox" id="flex_Num', 
                            1:total_no_fits_RD(), '"> <span>', # 1:total_no_models()
                            null_values, '</span></label>')
    
    flex_table<- vanilla.table(flex_tab)

    flex_table[, c("Num","Model"), to = "header"] <- parLeft() # left align the column "Num"
    flex_table[, c("Num","Model")] <- parLeft() # left align header
    
    return(flex_table)
  })
  
  
  
  # the inputs created are in input$car1, input$car2, ...
  output$check_mle_RD <- renderPrint({
    
    
    flex_Num<-1:total_no_fits_RD()
    
    
    # results
    res <- unlist(lapply(1:total_no_fits_RD(), function(i) input[[paste0("flex_Num", i)]]))
    print(res)
    
    if (any(res)) {
      print(flex_Num[res])
    }
    
    
    
  })


  ################################ (RD) predicted plot
  
  # set new reactive values of the number of hits of actionButton
  #  as impossible to reset value of an actionButton == 0
  
  run_button_counts_RD <- reactiveValues(counts = 0)
  
  # Each time (RD run) button is clicked, add 1 to reactive value of run_button_counts$counts
  observeEvent(input$run_ana_button_RD,
               {run_button_counts_RD$counts =  isolate(run_button_counts_RD$counts) +1 }
  )
  
  observeEvent(input$reset_ana_button_RD,
               {run_button_counts_RD$counts = 0}
  )
  # run_button_counts<-reactive({isolate(input$run_ana_button)
  #    })
  
  # default_plot <- eventReactive(input$reset_ana_button, {
  #    head(cars)
  # })
  
  # reset_run_button_counts<-observeEvent(input$reset_ana_button,
  #                                      isolate(run_ana_button)=0)
  
  # show predicted data plot
  output$plot_RD <- renderPlot({
    
    # plot if hit run button 
    
    if (run_button_counts_RD$counts>0) {
      
      # isolate this to avoid reactive plot function
      data_plot<- isolate(download_plot_RD())
      
    } else {data_plot<- NULL}
    
    #predicted.num<-ept.num.geo()
    #lines(c(1:T()), predicted.num, col = 'blue', lwd=2) 
    
    data_plot
    
  })
  
  # download plot
  
  
  output$downloadPlot_RD <- downloadHandler(
    
    filename <- function() {
      paste('plot', 'png', sep = ".")
    },
    
    content <- function(file) {
      png(file) # add more arguments for pic
      
      #predicted.plot<-classic_data_plot()
      #predicted.num<-ept.num.geo()
      #lines(c(1:T()), predicted.num, col = 'blue', lwd=2)
      
      print(download_plot_RD())
      
      dev.off()
    },
    contentType = "image/png"
    
  )
  
##  
  # predicted data and CI as a table
  
  download_predicted_data_RD<-reactive({
    
    data_all<- dataInput_RD()
    data_ploints<-data_all[,input$data_colnum]
    
    predicted_data_CI_table<-data.frame(
      Sampling_occasion = c(1:T_RD()),
      Data_used = data_ploints
      # add predicted column
      
      # add CI
    )
    
    return(predicted_data_CI_table)
    
  })
  
  
  # Output: download predicted data and confidence interval
  output$downloadPredicted_RD <- downloadHandler(
    filename = function() {
      paste("predicted_counts",download_predicted_data_RD(), ".csv", sep = "")
    },
    content = function(file) {
      
      write.csv(download_predicted_data_RD(), file, row.names = FALSE)
    }
  )
  
  


  
  

  ################################  estimates results
  output$estimates_RD <- renderTable({
    return(est.tab_RD())
  })  
  
  
  ################################  table of counts results
  output$predicted_table_RD <- DT::renderDataTable({
    
    data_all<- dataInput_RD()
    data1_RD=data_all[,input$data_colnum_RD]
    
    est.num.results_RD<-data.frame(
      Sampling_occasion = c(1:T_RD()),
      Observed_data = data1_RD,
      Predicted_data = ept.num.RD() ## to change
    )
    
    return(est.num.results_RD)
    
  }) 
  

# the end of server function  
  
})
