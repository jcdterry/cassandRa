#' Plot the fitted network models
#'
#' Takes the output from other functions (including \code{PredictLinks()}) to visualise the fit to the
#'  data and predictions of missing links.
#'
#' See the vignette for a more through description and examples.
#'
#' @param list  A list-format network (output from xxx)
#' @param Matrix_to_plot Which matrix / matrices to plot. One or more of 'C_def','C', 'M', 'B', 'SBM'
#' @param OrderBy How to order the plot. One of 'Default','Degree','Manual', 'LatentTrait','SBM', 'AsPerMatrix'
#' @param addDots Should dots be added to show observations. TRUE, FALSE or 'Size', to plot by interaction strength
#' @param title A title. By default it will use the value of Matrix_to_plot
#' @param Combine How should multiple matrices be combined. Either '+' which averages them (default), or '*' which multiples
#' @param RemoveTP Should true positives be set to NA in order to highlight differences in predictions. Default is FALSE
#' @param GuidesOff Should the legends be switched off. Defaults to TRUE
#'
#' @return A ggplot object, which by default will print to the device, but can be added to make further tweaks

#' @export

PlotFit <- function(list, Matrix_to_plot, OrderBy = 'Default',
                    addDots = TRUE, title= NULL, Combine='+', RemoveTP=FALSE, GuidesOff = TRUE){
  x<-list
  if(is.null(title)){   title<- paste(Matrix_to_plot, collapse = Combine)}

  ####Selecting Right Data

  if(!any(Matrix_to_plot %in%  c('C_def','C', 'M', 'B', 'SBM' ) )){
    stop("Incorrect Matrix Specification. Must be one of : 'C_def','C', 'M', 'B', 'SBM' " )
  }
  if(length(Matrix_to_plot)==1){
    if(Matrix_to_plot == 'C_def'){ProbsMatrix<- x$C_defmatrix}
    if(Matrix_to_plot == 'C'){ProbsMatrix<- x$C_ProbsMatrix  }
    if(Matrix_to_plot == 'M'){ProbsMatrix<- x$M_ProbsMatrix  }
    if(Matrix_to_plot == 'B'){ProbsMatrix<- x$B_ProbsMat}
    if(Matrix_to_plot == 'SBM'){ProbsMatrix<- x$SBM_ProbsMat}
  }else{
    ## If multiple matrices selected, will either multiple or 'average', depending on 'Combine'
    if(Combine %in% c('+', '*')){
      if(Combine =='+'){
        Base<- matrix(0, nrow = x$n_host, ncol=x$n_wasps)

        for( M in Matrix_to_plot){
          if(M == 'C_def'){Base<- Base+ x$C_defmatrix/ mean(x$C_defmatrix)  }
          if(M == 'C'){ Base<- Base+  x$C_ProbsMatrix/ mean(x$C_ProbsMatrix)}
          if(M == 'M'){Base<- Base+  x$M_ProbsMatrix/ mean(x$M_ProbsMatrix)}
          if(M == 'B'){Base<- Base+  x$B_ProbsMat/ mean(x$B_ProbsMat)}
          if(M == 'SBM'){ Base<- Base+  x$SBM_ProbsMat/ mean(x$SBM_ProbsMat)}
           }
        ProbsMatrix<- Base
      }
      if(Combine == '*'){
        Base<- matrix(1, nrow = x$n_host, ncol=x$n_wasps)
        for( M in Matrix_to_plot){
          if(M == 'C_def'){Base<- Base* x$C_defmatrix}
          if(M == 'C'){ Base<- Base*x$C_ProbsMatrix}
          if(M == 'M'){Base<- Base*  x$M_ProbsMatrix}
          if(M == 'B'){Base<- Base*  x$B_ProbsMat}
          if(M == 'SBM'){ Base<- Base*  x$SBM_ProbsMat}
        }
        ProbsMatrix<- Base
      }
    }else{
      stop('Combine must be + or *')
    }
  }

  ######
  ## Sorting Out ordering
  #########

  if(!(OrderBy %in%  c('Default','Degree','Manual', 'LatentTrait','SBM', 'AsPerMatrix') )){
    stop("Incorrect  Ordering Specification. Must be one of : 'Default','SBM','Degree','Manual', 'LatentTrait', 'AsPerMatrix'" )
  }

  if(is.null(x$WaspNames) ){  WaspNames <- 1: x$n_wasps}else{WaspNames = x$WaspNames}
  if(is.null(x$HostNames) ){  HostNames <- 1: x$n_hosts }else{HostNames = x$HostNames}


  if(OrderBy== 'Default'){
    # Chooses probable best way to sort based on first specified matrix
    if( Matrix_to_plot[1] %in% c('C', 'C_def', 'B')){OrderBy <- 'Degree'}
    if( Matrix_to_plot[1] %in% c('SBM')){OrderBy <- 'SBM'}
    if( Matrix_to_plot[1] %in% c('M')){OrderBy <- 'LatentTrait'}

  }
  if(OrderBy== 'AsPerMatrix'){
    WaspOrder <- as.character(1: x$n_wasps)
    HostOrder <- as.character(1: x$n_hosts)
  }
  if(OrderBy == 'Degree'){
    WaspOrder<- rev(WaspNames[order(colSums(list$obs>0))])
    HostOrder<-     HostNames[order(rowSums(list$obs>0))]
  }
  if(OrderBy== 'Manual'){
    WaspOrder<- x$WaspNames
    HostOrder<- x$HostNames

  }
  if(OrderBy == 'SBM'){
    HostOrder<- HostNames[order(x$SBM1$SB_H)]
    WaspOrder<- WaspNames[order(x$SBM1$SB_W)]
  }

  if(OrderBy == 'LatentTrait'){

    if(Matrix_to_plot ==  'B'){
      P<-x$B_par
    }else{
      P<-x$M_par
    }
    Host_Trait<-P[1:x$n_hosts]
    HostOrder <- HostNames[order(Host_Trait)]
    Wasp_Trait<-P[(x$n_hosts+1): x$N]
    WaspOrder <- rev(WaspNames[order(Wasp_Trait)])
  }
  ###################

  reshape2::melt(ProbsMatrix)%>%
    dplyr::rename('Host' = Var1, 'Wasp' = Var2, 'Prob' = value)%>%
    mutate(Host = as.character(Host),
           Wasp = as.character(Wasp))->Fitted_DF


  if(RemoveTP){
    Fitted_DF%>%
      mutate(int_code = paste(Host, Wasp, sep='_'))%>%
      left_join(select(list$ObsSuccess,int_code, Category) , by='int_code' )%>%
      mutate(Prob = ifelse(Category== 'True Positive', NA, Prob))-> Fitted_DF
  }


  W<- data.frame(WaspName = WaspNames, Wasp=as.character(1:length(WaspNames)))
  H <- data.frame(HostName= HostNames, Host = as.character(1:length(HostNames)))

  Fitted_DF%>%
    left_join(W, by = 'Wasp')%>%
    left_join(H, by='Host')%>%
    ggplot()+
    geom_raster(aes(y=as.character(HostName), x=as.character(WaspName), fill=Prob))+
    scale_fill_viridis_c(na.value = "grey75")+
    ggtitle(title)+ coord_fixed()+
    theme(axis.text.x = element_text(angle = 90 , hjust=1),axis.title=element_blank())+
    scale_x_discrete(limits= as.character(WaspOrder))+
    scale_y_discrete(limits= as.character(HostOrder))-> PLOT

  if(is.null(list$ObsSuccess)){
    obs<- x$obs

    rownames(obs) <- HostNames
    colnames(obs) <- WaspNames


    reshape2::melt(obs)%>%
      dplyr::rename('HostName' = Var1, 'WaspName' = Var2,
                    'Category' = value)%>%
      mutate(N_obs = Category,
        Category = ifelse(Category >0,'True Positive',
                               'Negative'),
             )-> Category
  }else{
    list$ObsSuccess%>%
      mutate(Wasp= as.character(Wasp),
             Host = as.character(Host))%>%
      left_join(W, by = 'Wasp')%>%
      left_join(H, by='Host')-> Category
  }

  if(addDots=='Size'){
    #Category$LogStr <- Category$LogStr + abs(min(Category$LogStr, na.rm = TRUE))
    PLOT <- PLOT+
      geom_point(data = filter(Category,Category != 'True Negative'),
                 aes(y=as.character(HostName),
                     x=as.character(WaspName),
                     col=Category,
                     size=ObsStr))+
      scale_color_manual(values = c('False Negative' = 'red', 'True Positive' = 'black'))+
      # geom_point(data = filter(Category,
      #                          Category != 'True Negative',
      #                          Category != 'Negative'),
      #            aes(y=as.character(HostName),
      #                x=as.character(WaspName)),
      #            col='white', size=0.5 )+
      scale_size(range=c(1,3))

  }else{

    if(addDots){
      PLOT <- PLOT+geom_point(data = filter(Category,Category != 'True Negative'),
                              aes(y=as.character(HostName),
                                  x=as.character(WaspName),
                                  col=Category ,
                                  shape=Category))+
        scale_color_manual(values = c('False Negative' = 'red', 'True Positive' = 'black'))+
        scale_shape_manual(values = c('False Negative' = 13, 'True Positive' = 19))#+
        # geom_point(data = filter(Category,
        #                          Category != 'True Negative',
        #                          Category != 'Negative'),
        #            aes(y=as.character(HostName),
        #                x=as.character(WaspName)),
        #            col='white', size=0.5 )

    }
  }

  if(GuidesOff){
    PLOT <- PLOT+ guides(shape=FALSE, fill=FALSE, col=FALSE, size=FALSE)
  }

  return(PLOT)
}
