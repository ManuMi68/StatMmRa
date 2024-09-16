

# 1. Funciones básicas de análisis estadístico para apoyo a todos los Programas de Manuel Miguel
# Parte I

{
  ## A) Nuevas Funciones Depuradas
  
  # Sección A.1) Crear estructuras de datos y manejo de Ficheros o Librerías
  # Añadida marzo 2019
  Mk.pdf<-function(NmDirp="Result",NmFilep="Fichero Por Defecto") {
    NmDir<-paste0(getwd(),"/",NmDirp,"/")
    if (!dir.exists(NmDir)) {try(dir.create(NmDir))}
    NmFileF<-paste0(NmDir,NmFilep,".pdf")
    pdf(NmFileF, width=14, height=10)
    NmFileF
  }
  # Añadida mayo 2019
  EdadHastaHoy<-function(x,formato="%m/%d/%Y",Rou=1) {
    require(lubridate)
    Edad<-time_length(interval(as.Date(x,format=formato), as.Date(Sys.Date())), "years")
    Res<-paste0(
      "M = ", round(mean(Edad,na.rm = T),Rou),
      ", SD = ", round(sd(Edad,na.rm = T),Rou),
      ", Range = ", round(range(Edad,na.rm = T),Rou)[1],
      "-", round(range(Edad,na.rm = T),Rou)[2]
    )
    Res
  }
  
  EdadAPA<-function(Edadp,Rou=2) {
    Res<-paste0(
      "M = ", round(mean(Edadp,na.rm = T),Rou),
      ", SD = ", round(sd(Edadp,na.rm = T),Rou),
      ", Range = ", round(range(Edadp,na.rm = T),Rou)[1],
      "-", round(range(Edadp,na.rm = T),Rou)[2]
    )
    Res
  }
  
  Mk.dir<-function(NmDirp="Result") {
    NmDir<-paste0(getwd(),"/",NmDirp,"/")
    if (!dir.exists(NmDir)) {try(dir.create(NmDir))}
    NmDir
  }
  
  DFCreat1<-function(l=200) {
    yb=c(sort(rnorm(1:l/2,mean=50,sd=10)),sort(rnorm(1:l/2,mean=70,sd=10)))
    xb=c(sort(rnorm(1:l/2,mean=5,sd=1)),sort(rnorm(1:l/2,mean=5,sd=1)))
    gCb=rep(c(1,2),each=l)
    gb=as.factor(gCb)
    
    BEtiq=paste0(rep("V"),c(1:10))
    Etiq<-rep(BEtiq,each=l/10)
    DF<-data.frame(x=xb,y=yb,g=gb,gC=gCb,Etiq=factor(Etiq,levels=BEtiq));
    with(DF,plot(x,y,xlim=c(1,10),ylim=c(1,100)));abline(lm(y~x,data=DF))
    DF
  }
  
  DTCreat1<-function(l=300) {
    require(data.table)
    yb=c(sort(rnorm(1:(l/15),mean=30,sd=10)),
         sort(rnorm(1:(l/15),mean=40,sd=10)),
         sort(rnorm(1:(l/15),mean=50,sd=10)),
         sort(rnorm(1:(l/15),mean=50,sd=10)),
         sort(rnorm(1:(l/15),mean=60,sd=10)),
         sort(rnorm(1:(l/15),mean=40,sd=10)),
         sort(rnorm(1:(l/15),mean=50,sd=10)),
         sort(rnorm(1:(l/15),mean=50,sd=10)),
         sort(rnorm(1:(l/15),mean=50,sd=10)),
         sort(rnorm(1:(l/15),mean=60,sd=10)),
         sort(rnorm(1:(l/15),mean=40,sd=10)),
         sort(rnorm(1:(l/15),mean=50,sd=10)),
         sort(rnorm(1:(l/15),mean=60,sd=10)),
         sort(rnorm(1:(l/15),mean=70,sd=10)),
         sort(rnorm(1:(l/15),mean=80,sd=10)))
    xb=c(sort(rnorm(1:(l/3),mean=5,sd=1)),
         sort(rnorm(1:(l/3),mean=5,sd=1)),
         sort(rnorm(1:(l/3),mean=5,sd=1)))
    zb=c(sort(rnorm(1:(l/3),mean=50,sd=10)),
         sort(rnorm(1:(l/3),mean=50,sd=10)),
         sort(rnorm(1:(l/3),mean=50,sd=10)))
    gCb=rep(c(1:3),each=(l/3))
    gb=as.factor(gCb)
    g2=factor(rep(rep(c("a","b","c","d","e"),each=(l/15)),3))
    BEtiq=paste0(rep("S"),c(1:l))
    #Etiq<-rep(BEtiq,each=(l/10))
    Etiq<-BEtiq
    DT<-data.table(x=xb,y=yb,z=zb,g=gb,gC=gCb,g2=g2,Etiq=factor(Etiq,levels=BEtiq));
    DT[,plot(x,y,xlim=c(1,10),ylim=c(1,100))];
    abline(lm(y~x,data=DT))
    DT
  }
  
  DFCreat2<-function(l=200) {
    yb=c(sort(runif(l/2,0,40)),sort(runif(l/2,60,100)))
    xb=sort(runif(l,0,10))
    gCb=rep(c(1,2),each=l/2)
    gb=as.factor(gCb)
    
    BEtiq=paste0(rep("V"),c(1:10))
    Etiq<-rep(BEtiq,each=l/10)
    DF<-data.frame(x=xb,y=yb,g=gb,gC=gCb,Etiq=factor(Etiq,levels=BEtiq));
    with(DF,plot(x,y,xlim=c(1,10),ylim=c(1,100)));abline(lm(y~x,data=DF))
    DF
  }
  
  DFCreat3<-function(l=200) {
    yb=c(sort(runif(l/2,50,100)),sort(runif(l/2,0,50),decreasing=T))
    xb=c(sort(runif(l/2,0,10)),sort(runif(l/2,0,10)))
    gCb=rep(c(1,2),each=l/2)
    gb=as.factor(gCb)
    
    BEtiq=paste0(rep("V"),c(1:10))
    Etiq<-rep(BEtiq,each=l/10)
    DF<-data.frame(x=xb,y=yb,g=gb,gC=gCb,Etiq=factor(Etiq,levels=BEtiq));
    with(DF,plot(x,y,xlim=c(1,10),ylim=c(1,100)));abline(lm(y~x,data=DF))
    DF
  }
  
  DFCreat4<-function(l=200) {
    yb=c(sort(runif(l/2,0,100)),sort(runif(l/2,0,100),decreasing=T))
    xb=c(sort(runif(l/2,0,10)),sort(runif(l/2,0,10)))
    gCb=rep(c(1,2),each=l/2)
    gb=as.factor(gCb)
    
    BEtiq=paste0(rep("V"),c(1:10))
    Etiq<-rep(BEtiq,each=l/10)
    DF<-data.frame(x=xb,y=yb,g=gb,gC=gCb,Etiq=factor(Etiq,levels=BEtiq));
    with(DF,plot(x,y,xlim=c(1,10),ylim=c(1,100)));abline(lm(y~x,data=DF))
    DF
  }
  
  # Sección A.2) Para búsqueda de información a través de Data Tables
  
  searchDT <- function(x, pattern) {
    nms <- names(x)
    string <- eval(expression(paste0("grepl('",
                                     pattern, 
                                     "', ",
                                     nms,",
                                   fixed = TRUE)",
                                     collapse = " | ")))
    x[eval(as.call(parse(text=string))[[1]])]
  }
  
  # Sección A.3) Robustas
  
  
  # Trg es como winval de  Wilcox, pero discrepa al quitar los NA automáticamente
  # Corregir pues le faltaba la linea n<-length(x)
  Trg<-function(x,tr=.2,na.rm=FALSE){
    #  From Wilcox: Trimm for the data in the vector x, tr is the amount of Winsorization
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
      warning("argument is not numeric or logical: returning NA")
      return(NA_real_)
    }
    if (!is.numeric(tr) || length(tr) != 1L) stop("'trim' must be numeric of length one")
    n<-length(x)
    if (tr > 0 && n) {
      if (is.complex(x)) stop("trimmed sample is not defined for complex data")
      if (any(is.na(x)) && !na.rm) warning("Missing values ere automatically removed but the sample size has not been adjusted accordingly")
      if (tr >= 0.5) warning("trim >= 0.5 is odd...trying it anyway")
      if(na.rm)x<-x[!is.na(x)]
      y<-sort(x)
      ibot<-floor(tr*length(x))+1
      itop<-length(x)-ibot+1
      xbot<-y[ibot];xtop<-y[itop]
      x[x<=xbot]<-xbot;x[x>=xtop]<-xtop
    }
    x
  }
  NNa<- function(x,na.rm=FALSE) {ifelse(na.rm,length(na.omit(x)),length(x))}
  na.del<-function(x){xm<-as.matrix(x);xm[complete.cases(xm),]} # As elimna or elimnaMM
  
  # Al principio mis Winsorizaciones diferían de las de Wilcox pues yo eliminaba los NA y asumía que al
  # ser un valor perdido no cuenta en el cómputo del % de valores.
  # Pero tras darle muchas vueltas he optado por dejarlas como él las tenía, sólo que con mi código.
  # Ahora mi cálculo de la var coincide con el de él, pero el de la media no coincide pues ahí directamente
  # la Media de Wilcox quita los NA y el tamaño muestral queda reajustado consecuentemente de manera automática.
  winMM<-function(x,tr=.2,na.rm=FALSE){mean(na.omit(Trg(x,tr,na.rm)))} #As Wilcox' "win", Winsor Mean
  winvarMM<-function(x,tr=.2,na.rm=FALSE){var(na.omit(Trg(x,tr,na.rm)))} #As Wilcox' "winvar", Winsor Var
  trimseMM<-function(x,tr=.2,na.rm=FALSE){sqrt(winvarMM(x,tr,na.rm))/((1-2*tr)*sqrt(NNa(x,na.rm)))} #Sustituye "trimse" de Wilcox, ETM de la Media Recortada
  hpsiMM<-function(x,bend=1.28){ifelse(abs(x)<=bend,x,bend*sign(x))} #As Wilcox' hpsi
  dnormvarMM<-function(x){x^2 * dnorm(x)}
  
  winvarNMM<-function(x,tr=.2,na.rm=FALSE){
    # rescale the winsorized var so that it equals one for the standard
    # normal distribution
    require(MASS)
    if(na.rm)x<-x[!is.na(x)]
    cterm=ifelse(tr==0,1,area(dnormvarMM,qnorm(tr),qnorm(1-tr))+2*(qnorm(tr)^2)*tr)
    bot=winvarMM(x,tr=tr,na.rm = na.rm)/cterm
    bot
  }
  
  trimciBasMM<-function(x,tr=.2,alpha=.05,na.rm=FALSE){
    #  Compute a 1-alpha confidence interval for the trimmed mean
    #  The default amount of trimming is tr=.2
    if(na.rm)x<-x[!is.na(x)]
    se<-trimseMM(x,tr,na.rm)
    df<-length(x)-2*floor(tr*length(x))-1
    trimci<-qt(1-alpha/2,df)*se
    trimci
  }
  
  trimciMM<-function(x,tr=.2,alpha=.05,null.value=0,pr=T,na.rm=FALSE){
    # Compute a 1-alpha confidence interval for the trimmed mean
    # The default amount of trimming is tr=.2
    if(pr){
      print("The p-value returned by the this function is based on the")
      print("null value specified by the argument null.value, which defaults to 0")
    }
    if(na.rm)x<-x[!is.na(x)]
    se<-trimseMM(x,tr,na.rm)
    df<-length(x)-2*floor(tr*length(x))-1
    M<-mean(x,tr,na.rm)
    TrmCI<-trimciBasMM(x,tr,alpha,na.rm)
    trimci<-c(M-TrmCI,M+TrmCI)
    test<-(M-null.value)/se
    sig<-2*(1-pt(abs(test),df))
    InTxt<-paste("TW (", round(df,2), ") = ", round(test,3), "; p = ", round(sig,3), "; SE = ", round(se,3),sep="")
    list(ci=trimci,estimate=mean(x,tr),test.stat=test,se=se,p.value=sig,n=length(x),ForText=InTxt)
  }
  
  wincorMM<-function(x,y=NULL,tr=.2){
    #   Winsorized correlation between x and y (tr is the amount of Winsorization)
    #   Also returns the Winsorized covariance
    #   Pairwise deletion of missing values is performed.
    if (is.null(y[1])){y = x[, 2];x = x[, 1]}
    sig<-NA
    if(length(x)!=length(y))stop("Lengths of vectors are not equal")
    m1=na.del(cbind(x,y))
    g<-floor(tr*nrow(m1))
    vec<-cbind(Trg(m1[,1],tr),Trg(m1[,2],tr))
    wcor<-cor(vec)[1,2]
    wcov<-var(vec)[1,2]
    if(sum(x==y)!=length(x)){
      test<-wcor*sqrt((length(x)-2)/(1.-wcor^2))
      sig<-2*(1-pt(abs(test),length(x)-2*g-2))
    }
    list(cor=wcor,cov=wcov,siglevel=sig,n=nrow(m1))
  }
  
  hMM<-function(x,tr=.2,na.rm=FALSE){if(na.rm)x<-na.del(x);length(x) - 2 * floor(tr * length(x))}
  wV<-function(x,tr=.2,na.rm=FALSE){
    if(na.rm)x<-na.del(x)
    hl <- hMM(x,tr,na.rm)
    hl * (hl - 1)/((length(x) - 1) * winvar(x,tr,na.rm))
  }
  
  # A) Provenientes del Curso Doctorado 2016
  
  #+-+-+-+-+-+-+-+-+-+-Manuel Miguel Ramos Álvarez+-+-+-+-+-+-+-+-+-+-  
  #+-+-+-+-+-+-+-+-+-+-Diciembre de 2016+-+-+-+-+-+-+-+-+-+-  
  #+-+-+-+-+-+-+-+-+-+-Funciones básica de apoyo al Curso de Formación de Análisis Estadístico Avanzado+-+-+-+-+-+-+-+-+-+-  
  
  initPkg <- function(need) {
    ip <- .packages(all.available = T)
    if (any((need %in% ip) == F)) {
      install.packages(need[!(need %in% ip)],repos="http://cran.us.r-project.org")
    }
    ok <- sapply(1:length(need), function(p) require(need[[p]], character.only = T))
  }
  
  AOV_MM<-function(DatDT,GRP,DepV){
    ResF<-list()
    #M.AOV=aov(get(DepV)~get(GRP),data=DatDT)
    M.AOV<-aov(update(~. ,paste(DepV,'~',GRP)), data=DatDT)  
    #round(etaSquared(aov(update(~ GRUPO,paste(x,'~.')), data=DFn))[2],3),
    
    Res1=summary(M.AOV)
    Res2=ExtrF(Res1[[1]])
    ResF$DepVar=DepV
    ResF$AOV=Res1
    ResF$Eta=etaSquared(M.AOV)
    Eta=round(etaSquared(M.AOV)[2],3)
    ResF$ParaAPA=paste0(Res2,", EtaSq=",Eta)
    ResF$Lineal=summary.lm(M.AOV)
    ResF
  }
  
  ExtrF<-function(ff){
    paste0("F(",round(ff$"Df"[1],3),",",round(ff$"Df"[2],3),")=",round(ff$"F value"[1],3),
           ", p=",round(ff$"Pr(>F)"[1],3))
  }
  
  ExtrTuk<-function(posthoc){
    if(!is.null(names(posthoc))) posthoc=posthoc[[1]]
    NmEfs=rownames(posthoc)
    NumC=length(NmEfs)
    outpp<-lapply(c(1:NumC), function(x) {
      paste0("Dif.",NmEfs[x],"=", round(posthoc[x,1],3),"; p= ",round(posthoc[x,4],4),"; IC: [",round(posthoc[x,2],3),", ",round(posthoc[x,3],3),"]")
    })
    outpp
  }
  
  Trimean<-function(x) mean(x,tr=.2,na.rm = TRUE)
  ETM <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
  CI_MM<-function(x) qt(.975, length(na.omit(x))-1)*ETM(x)
  
  # trimciBasMM<-function(x,tr=.2,alpha=.05,null.value=0,pr=T){
  #   #
  #   #  Compute a 1-alpha confidence interval for the trimmed mean
  #   #  The default amount of trimming is tr=.2
  #   x<-elimnaMM(x)
  #   se<-trimseMM(x,tr)
  #   df<-length(x)-2*floor(tr*length(x))-1
  #   trimci<-qt(1-alpha/2,df)*se
  #   trimci
  # }
  
  
  StatsMMv2 <- function(DatDT,GRP,DepV,trP=0.2){
    Res=DatDT[,list(N=length(get(DepV)),
                    Media=round(mean(get(DepV),na.rm = T),3),
                    Md=round(median(get(DepV),na.rm = T),3),
                    TrimMean=round(mean(get(DepV),na.rm = T,tr = trP),3),
                    ETM=round(ETM(get(DepV)),3),
                    ETMTrim=round(trimseMM(get(DepV),na.rm = T,tr = trP),3),
                    #ETMTrimW=round(trimseWilcox(get(DepV),na.rm = T,tr = trP),3),
                    CI_95=round(CI_MM(get(DepV)),3),
                    CI_95Trim=round(trimciBasMM(x=get(DepV),tr = trP),3)),
              by=list(get(GRP))]
    setnames(Res,c(GRP,paste0(DepV,".N"),
                   paste0(DepV,".Mean"),
                   paste0(DepV,".Md"),
                   paste0(DepV,".TrimMean"),
                   paste0(DepV,".ETM"),
                   paste0(DepV,".ETMTrim"),
                   #paste0(DepV,".ETMTrimW"),
                   paste0(DepV,".CI_95"),
                   paste0(DepV,".CI_95Trim"))
    )
    Res
  }
  
  StatsMMv3 <- function(DatDT,GRP,DepV){
    Res=DatDT[,list(N=length(get(DepV)),Media=round(mean(get(DepV),na.rm = T),3),
                    Md=round(median(get(DepV),na.rm = T),3),ETM=round(ETM(get(DepV)),3),CI_95=round(CI_MM(get(DepV)),3)),by=list(get(GRP))]
    setnames(Res,c(GRP,paste0(DepV,".N"),paste0(DepV,".Mean"),paste0(DepV,".Md"),paste0(DepV,".ETM"),paste0(DepV,".CI_95")))
    Res
  }
  
  StatsMMF <- function(DatDT,GRP1,GRP2,DepV){
    Res=DatDT[,list(N=length(get(DepV)),Media=round(mean(get(DepV),na.rm = T),3),
                    Md=round(median(get(DepV),na.rm = T),3),ETM=round(ETM(get(DepV)),3),CI_95=round(CI_MM(get(DepV)),3)),by=list(get(GRP1),get(GRP2))]
    setnames(Res,c(GRP1,GRP2,paste0(DepV,".N"),paste0(DepV,".Mean"),paste0(DepV,".Md"),paste0(DepV,".ETM"),paste0(DepV,".CI_95")))
    Res
  }
  
  data_summary <- function(x) {
    m <-mean(x,na.rm = T,trim = 0.2)
    ymin <- m-trimseMM(x,na.rm = T,tr = 0.2)
    ymax <- m+trimseMM(x,na.rm = T,tr = 0.2)
    return(c(y=m,ymin=ymin,ymax=ymax))
  }  
  
  GraphPow<-function(NumGrp=3,AlphaPas=.05) {
    initPkg("pwr")
    # range of correlations
    r <- seq(.1,.5,.01)
    nr <- length(r)
    
    # power values
    p <- seq(.4,.9,.1)
    np <- length(p)
    
    # obtain sample sizes
    samsize <- array(numeric(nr*np), dim=c(nr,np))
    for (i in 1:np){
      for (j in 1:nr){
        result <- pwr.anova.test(k=NumGrp,f=r[j],sig.level=AlphaPas,power=p[i])
        samsize[j,i] <- ceiling(result$n)
      }
    }
    
    # set up graph
    xrange <- range(r)
    yrange <- round(range(samsize))
    colors <- rainbow(length(p))
    #quartz("powerAOVA")
    plot(xrange, yrange, type="n",xlab="ANOVA Effect Size",ylab="Sample Size (n)")
    
    # add power curves
    for (i in 1:np){
      lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
    }
    TxtTtl=paste0("Sample Size Estimation for ANOVA Studies\n","Sig=",AlphaPas,"; Groups=",NumGrp)
    # add annotation (grid lines, title, legend) 
    abline(v=0, h=seq(0,yrange[2],25), lty=2, col="grey89")
    abline(h=0, v=seq(xrange[1],xrange[2],.01), lty=2,col="grey89")
    title(TxtTtl)
    legend("topright", title="Power", as.character(p),fill=colors)
  }
  
  EfSizeAOV<-function(x, type = 2, anova = FALSE){
    if (!is(anova, "logical") | length(anova) != 1) {
      stop("\"anova\" must be a single logical value")
    }
    if (!is(x, "lm")) {
      stop("\"x\" must be a linear model object")
    }
    if (!is(type, "numeric") | length(type) != 1) {
      stop("type must be equal to 1,2 or 3")
    }
    if (type == 1) {
      ss <- anova(x)[, "Sum Sq", drop = FALSE]
      ss.res <- ss[dim(ss)[1], ]
      ss.tot <- sum(ss)
      ss <- ss[-dim(ss)[1], , drop = FALSE]
      ss <- as.matrix(ss)
    }
    else {
      if (type == 2) {
        ss.tot <- sum((x$model[, 1] - mean(x$model[, 1]))^2)
        ss.res <- sum((x$residuals)^2)
        terms <- attr(x$terms, "factors")[-1, , drop = FALSE]
        l <- attr(x$terms, "term.labels")
        ss <- matrix(NA, length(l), 1)
        rownames(ss) <- l
        for (i in seq_along(ss)) {
          vars.this.term <- which(terms[, i] != 0)
          dependent.terms <- which(apply(terms[vars.this.term, 
                                               , drop = FALSE], 2, prod) > 0)
          m0 <- lm(x$terms[-dependent.terms], x$model)
          if (length(dependent.terms) > 1) {
            m1 <- lm(x$terms[-setdiff(dependent.terms, 
                                      i)], x$model)
            ss[i] <- anova(m0, m1)$`Sum of Sq`[2]
          }
          else {
            ss[i] <- anova(m0, x)$`Sum of Sq`[2]
          }
        }
      }
      else {
        if (type == 3) {
          mod <- drop1(x, scope = x$terms)
          ss <- mod[-1, "Sum of Sq", drop = FALSE]
          ss.res <- mod[1, "RSS"]
          ss.tot <- sum((x$model[, 1] - mean(x$model[, 
                                                     1]))^2)
          ss <- as.matrix(ss)
        }
        else {
          stop("type must be equal to 1,2 or 3")
        }
      }
    }
    if (anova == FALSE) {
      eta2 <- ss/ss.tot
      eta2p <- ss/(ss + ss.res)
      E <- cbind(eta2, eta2p)
      rownames(E) <- rownames(ss)
      colnames(E) <- c("eta.sq", "eta.sq.part")
      aovMod <- x
      if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
      sumAov     <- summary(aovMod)[[1]]
      residRow   <- nrow(sumAov)
      dfError    <- sumAov[residRow,1]
      msError    <- sumAov[residRow,3]
      nTotal     <- nrow(model.frame(aovMod))
      dfEffects  <- sumAov[1:{residRow-1},1]
      nGrp<-dfEffects+1
      ssEffects  <- sumAov[1:{residRow-1},2]
      msEffects  <- sumAov[1:{residRow-1},3]
      partOmegas <- abs((dfEffects*(msEffects-msError)) /
                          (ssEffects + (nTotal -dfEffects)*msError))
      RSqadj<-1-(((nTotal-1)/(nTotal -nGrp))*(1-eta2p))
      #names(partOmegas) <- rownames(sumAov)[1:{residRow-1}]
      E2<-cbind(eta2, eta2p,partOmegas,RSqadj)
      rownames(E2) <- rownames(ss)
      colnames(E2) <- c("eta.sq", "eta.sq.part","Omega.part","R.Sq.adj")
    }
    else {
      ss <- rbind(ss, ss.res)
      eta2 <- ss/ss.tot
      eta2p <- ss/(ss + ss.res)
      k <- length(ss)
      eta2p[k] <- NA
      df <- anova(x)[, "Df"]
      ms <- ss/df
      Fval <- ms/ms[k]
      p <- 1 - pf(Fval, df, rep.int(df[k], k))
      E <- cbind(eta2, eta2p, ss, df, ms, Fval, p)
      E[k, 6:7] <- NA
      colnames(E) <- c("eta.sq", "eta.sq.part", "SS", "df", 
                       "MS", "F", "p")
      rownames(E) <- rownames(ss)
      rownames(E)[k] <- "Residuals"
    }
    return(E2)
  }
  
  CompESAPost<-function(DatDT,GRP,DepV){
    initPkg(c("compute.es"))
    source("http://www4.ujaen.es/~mramos/ADMmRa/Rallfun-v32.txt")
    # Realmente el Fichero de Wilcox está en su propio servidor: 
    # https://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v32.txt
    # Pero lo cargamos desde el nuestro para asegurar su estabilidad
    ResF<-list();cont=0;
    levGrp=levels(DatDT[, get(GRP)]); nLev=length(levGrp)
    for (i in (1:(nLev-1))) {
      for (j in ((i+1):(nLev))) {
        cont=cont+1
        DatSel=DatDT[get(GRP) %in% levGrp[c(i,j)]]
        LaT=t.test(update(~. ,paste(DepV,'~',GRP)), data=DatSel)$statistic[[1]]
        n.1=c(DatSel[, .N, by = get(GRP)]$N[1])
        n.2=c(DatSel[, .N, by = get(GRP)]$N[2])
        VarWA <- lapply(levGrp[c(i,j)], function(z) subset(DatSel,get(GRP)==z,DepV)[[1]])
        ResWil<-invisible(round(t1wayv2(VarWA)$Effect.Size,3))
        ResF[[cont]]<-cbind(contr=paste(levGrp[i],levGrp[j],sep=" vs "),tes(t=LaT, n.1, n.2,verbose = F),WilxoRob=ResWil)
      }
    }
    ResF
  }
  
  CompESAPostv2<-function(DatDT,GRP,DepV){
    initPkg(c("compute.es"))
    source("http://www4.ujaen.es/~mramos/ADMmRa/Rallfun-v32.txt")
    # Realmente el Fichero de Wilcox está en su propio servidor: 
    # https://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v32.txt
    # Pero lo cargamos desde el nuestro para asegurar su estabilidad
    ResF<-list();ResNm<-matrix();cont=0;
    levGrp=levels(DatDT[, get(GRP)]); nLev=length(levGrp)
    for (i in (1:(nLev-1))) {
      for (j in ((i+1):(nLev))) {
        cont=cont+1
        DatSel=DatDT[get(GRP) %in% levGrp[c(i,j)]]
        LaT=t.test(update(~. ,paste(DepV,'~',GRP)), data=DatSel)$statistic[[1]]
        n.1=c(DatSel[, .N, by = get(GRP)]$N[1])
        n.2=c(DatSel[, .N, by = get(GRP)]$N[2])
        EfSi=tes(t=LaT, n.1, n.2,verbose = F)
        VarWA <- lapply(levGrp[c(i,j)], function(z) subset(DatSel,get(GRP)==z,DepV)[[1]])
        ResWil<-invisible(round(t1wayv2(VarWA)$Effect.Size,3))
        #ResF[[cont]]<-cbind(contr=paste(levGrp[i],levGrp[j],sep=" vs "),tes(t=LaT, n.1, n.2,verbose = F),WilxoRob=ResWil)
        ResF[[cont]]<-cbind(tes(t=LaT, n.1, n.2,verbose = F),WilxoRob=ResWil)
        ResNm[[cont]]<-paste(levGrp[i],levGrp[j],sep=" vs ")
      }
    }
    ResF2 <- data.table(contr=c(ResNm),matrix(unlist(ResF), nrow=cont, byrow=T))
    names(ResF2)<-c("Contr",names(EfSi),"WilxoRob")
    ResF2
  }
  
  CompESAPostF<-function(DatDT,GRP,byGRP,DepV){
    initPkg(c("compute.es"))
    source("http://www4.ujaen.es/~mramos/ADMmRa/Rallfun-v32.txt")
    # Realmente el Fichero de Wilcox está en su propio servidor: 
    # https://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v32.txt
    # Pero lo cargamos desde el nuestro para asegurar su estabilidad
    ResF<-list();ResNm<-matrix();cont=0;
    levGrp=levels(DatDT[, get(GRP)]); nLev=length(levGrp)
    levGrpBy=levels(DatDT[, get(byGRP)]); nLevBy=length(levGrpBy)
    for (h in levGrpBy){
      DatSela <- subset(DatDT,get(byGRP)==h)
      for (i in (1:(nLev-1))) {
        for (j in ((i+1):(nLev))) {
          cont=cont+1
          DatSel=DatSela[get(GRP) %in% levGrp[c(i,j)]]
          LaT=t.test(update(~. ,paste(DepV,'~',GRP)), data=DatSel)$statistic[[1]]
          n.1=c(DatSel[, .N, by = get(GRP)]$N[1])
          n.2=c(DatSel[, .N, by = get(GRP)]$N[2])
          EfSi=tes(t=LaT, n.1, n.2,verbose = F)
          VarWA <- lapply(levGrp[c(i,j)], function(z) subset(DatSel,get(GRP)==z,DepV)[[1]])
          ResWil<-invisible(round(t1wayv2(VarWA)$Effect.Size,3))
          #ResF[[cont]]<-cbind(contr=paste(h,levGrp[i],levGrp[j],sep=" vs "),tes(t=LaT, n.1, n.2,verbose = F),WilxoRob=ResWil)
          ResF[[cont]]<-cbind(tes(t=LaT, n.1, n.2,verbose = F),WilxoRob=ResWil)
          ResNm[[cont]]<-paste(h,levGrp[i],levGrp[j],sep=" vs ")
        }
      }
    }
    ResF2 <- data.table(contr=c(ResNm),matrix(unlist(ResF), nrow=cont, byrow=T))
    names(ResF2)<-c("Contr",names(EfSi),"WilxoRob")
    ResF2
  }
  
  GenMultCon<-function(nLev) {
    cont=0;
    nContr=(nLev*(nLev-1))/2
    Contr<-matrix(data=0,ncol=nContr,nrow=nLev)
    for (i in (1:(nLev-1))) {
      for (j in ((i+1):(nLev))) {
        cont=cont+1
        Contr[i,cont]=1;Contr[j,cont]=-1
      }
    }
    Contr
  }
  
  CompESAPostWit<-function(DatDT,GRP,TypeC="Holm"){
    dat.mat<-as.matrix(DatDT)
    levGrp=names(DatDT); nLev=length(levGrp)
    nContr=(nLev*(nLev-1))/2
    ResF<-data.frame(matrix(data=0,nrow=nContr,ncol=5));
    cont=0;
    Contr<-matrix(data=0,ncol=nContr,nrow=nLev)
    for (i in (1:(nLev-1))) {
      for (j in ((i+1):(nLev))) {
        cont=cont+1
        Contr[i,cont]=1;Contr[j,cont]=-1
        ResContr<- t.test(dat.mat %*% (Contr[,cont]))
        p.Adj=p.adjust(ResContr$p.value,TypeC)
        ResF[cont,]<-(cbind(contr=paste(levGrp[i],levGrp[j],sep=" vs "),
                            Dif=round(ResContr$estimate,3),EC= round(ResContr$statistic,3),df=ResContr$parameter,p=round(ResContr$p.value,4)))
      }
    }
    lin.scores <- dat.mat %*% (Contr)
    Res.p<-c(lapply(c(1:nContr), function(x) t.test(lin.scores[,x])$p.value))
    p.Adj=p.adjust(Res.p,TypeC)
    ResFF<-cbind(ResF,p.Adj)
    names(ResFF)<-c("Contr","Diff","EstContr","GradosLib","p",paste0("p.Adj.",TypeC))
    ResFF
  }
  
  # Funciones ajuste CI adaptadas a partir de
  # Baguley, T. (2011). Calculating and graphing within-subject confidence
  # intervals for ANOVA. # Behavior Research Methods, 44(1), 158–175.
  # http://doi.org/10.3758/s13428-011-0123-7
  
  # Mi librería lm.ci_MM() sustituye a todas menos las dos del Diseño Factorial
  
  lm.ci_MM <- function(data.frame, conf.level = 0.95, difference = FALSE,ty="cm",cov.matrix = "unstructured",
                       wh.grp=FALSE,wh.tier=FALSE,xlab = NULL, ylab = NULL, level.labels = NULL, main = NULL,
                       pch = 19, pch.cex = 1.4, text.cex = 1.2, ylim = c(min.y, max.y),
                       line.width= c(1, 1.5),grid = FALSE) {
    require(data.table);require(nlme);require(gmodels)
    data.frame=data.table(data.frame)
    k = ncol(data.frame)
    n <- nrow(data.frame)
    df.stack <- stack(data.frame)
    parts <- rep(1:n, k)
    if (difference) diff.factor = 2^0.5/2 else diff.factor = 1
    mean.mat<-data.frame[,sapply(.SD, mean,na.rm = TRUE)]
    if (ty=="lm") {
      #loftus-masson within-subject CIs
      root.ms.error <- lme(values ~ 0 + ind, random = ~1 | parts, cbind(parts, df.stack))[[6]]
      moe <- root.ms.error/n^0.5 * qt(1 - (1 - conf.level)/2, (n - 1) * (k -1)) * diff.factor
      ci.Mas<-mean.mat+moe;ci.Menos<-mean.mat-moe
      ci.mat<-cbind(lower=ci.Menos,upper=ci.Mas)
    }
    if (ty=="bs") {
      # between-subject CIs
      if (difference == TRUE) ci.mat <- (confint(lm(values ~ 0 + ind, df.stack)) - mean.mat) * 
          2^0.5/2 + mean.mat else ci.mat <- confint(lm(values ~ 0 + ind, df.stack))
          dimnames(ci.mat) <- list(names(data.frame), c("lower", "upper"))
    }
    if (ty=="cmu") {
      #cousineau uncorrected within-subject CIs
      p.means <- tapply(df.stack$values, parts, mean)
      norm.df <- data.frame - p.means + (sum(data.frame)/(n * k))
      out<-norm.df[,lapply(.SD, CI2_MM,conf.level = conf.level)]
      ci.mat<-t(rbind(mean.mat-out,mean.mat+out))
      colnames(ci.mat)<-c("lower", "upper")
    }
    if (ty=="cm") {
      #cousineau-morey within-subject CIs
      p.means <- tapply(df.stack$values, parts, mean)
      norm.df <- data.frame - p.means + (sum(data.frame)/(n * k))
      t.mat<-unlist(norm.df[,lapply(.SD, t.test,conf.level = conf.level)][1,])
      c.factor <- (k/(k - 1))^0.5
      moe <- mean.mat/t.mat * qt(1 - (1 - conf.level)/2, n - 1) * c.factor * diff.factor
      ci.Mas<-mean.mat+moe;ci.Menos<-mean.mat-moe
      ci.mat<-cbind(lower=ci.Menos,upper=ci.Mas)
    }
    if (ty=="ml") {
      # CI based on multilevel model with covariance matrix unstructured or
      # constrained to compound symmetry
      data.long <- reshape(data.frame, idvar = "id", direction = "long", varying = 1:k, v.names = "dv")
      fix=as.formula(dv ~ 0 + factor(time))
      if (cov.matrix == "comp.symm") ml.mod <- lme(fix, random = ~1 | id, na.action = na.omit,data.long)
      if (cov.matrix == "unstructured") ml.mod <- lme(fix, random = ~0 + factor(time) | id, na.action = na.omit, data.long)
      ci.mat <- ci(ml.mod, confidence = conf.level)[, 2:3]
    }
    if (wh.grp) {
      moe.y <- max(ci.mat) - min(ci.mat)
      min.y <- min(ci.mat) - moe.y/3
      max.y <- max(ci.mat) + moe.y/3
      if (wh.tier) {
        data.long <- reshape(data.frame, idvar = "id", direction = "long", varying = 1:k, v.names = "dv")
        fix=as.formula(dv ~ 0 + factor(time))
        if (cov.matrix == "comp.symm") ml.mod <- lme(fix, random = ~1 | id, na.action = na.omit,data.long)
        if (cov.matrix == "unstructured") ml.mod <- lme(fix, random = ~0 + factor(time) | id, na.action = na.omit, data.long)
        ci.inner <- ci(ml.mod, confidence = conf.level)[, 2:3]
        moe.y <- max(ci.inner) - min(ci.inner)
        min.y <- min(ci.inner) - moe.y/3
        max.y <- max(ci.inner) + moe.y/3
      }
      if (missing(xlab)) xlab <- "levels"
      if (missing(ylab)) ylab <- "Confidence interval for mean"
      if (missing(level.labels) == FALSE) names(data.frame) <- level.labels
      plot(0, 0, ylim = ylim, xaxt = "n", xlim = c(0.7, k + 0.3), xlab = xlab, 
           ylab = ylab, main = main)
      if (grid) grid()
      points(mean.mat, pch = pch, bg = "black")
      index <- 1:k
      segments(index, ci.mat[, 1], index, ci.mat[, 2], lwd = line.width[1])
      segments(index - 0.02, ci.mat[, 1], index + 0.02, ci.mat[, 1], lwd = line.width[2])
      segments(index - 0.02, ci.mat[, 2], index + 0.02, ci.mat[, 2], lwd = line.width[2])
      axis(1, 1:k, labels = names(data.frame))
      if (wh.tier) {
        segments(index, ci.inner[, 1], index, ci.inner[, 2], lwd = line.width[1])
        segments(index - 0.02, ci.inner[, 1], index + 0.02, ci.inner[, 1], lwd = line.width[2])
        segments(index - 0.02, ci.inner[, 2], index + 0.02, ci.inner[, 2], lwd = line.width[2])
      }
    }
    ci.mat
  }
  
  ### Librerias sustituidas. Quedan para comprobaciones ####
  lm.ci <- function(data.frame, conf.level = 0.95, difference = FALSE) {
    #loftus-masson within-subject CIs
    k = ncol(data.frame)
    n <- nrow(data.frame)
    df.stack <- stack(data.frame)
    require(nlme)
    parts <- rep(1:n, k)
    root.ms.error <- lme(values ~ 0 + ind, random = ~1 | parts, cbind(parts, 
                                                                      df.stack))[[6]]
    detach(package:nlme)
    mean.mat <- matrix(NA, k, 1)
    ci.mat <- matrix(NA, k, 2)
    if (difference == TRUE) diff.factor = 2^0.5/2 else diff.factor = 1
    moe <- root.ms.error/n^0.5 * qt(1 - (1 - conf.level)/2, (n - 1) * (k - 
                                                                         1)) * diff.factor
    for (i in 1:k) mean.mat[i, ] <- mean(data.frame[,i])
    for (i in 1:k) {
      ci.mat[i, 1] <- mean.mat[i] - moe
      ci.mat[i, 2] <- mean.mat[i] + moe
    }
    dimnames(ci.mat) <- list(names(data.frame), c("lower", "upper"))
    ci.mat
  }
  
  bs.ci <- function(data.frame, conf.level = 0.95, difference = FALSE) {
    # between-subject CIs
    k = ncol(data.frame)
    n <- nrow(data.frame)
    df.stack <- stack(data.frame)
    group.means <- colMeans(data.frame, na.rm = TRUE)
    if (difference == TRUE) ci.mat <- (confint(lm(values ~ 0 + ind, df.stack)) - group.means) * 
      2^0.5/2 + group.means else ci.mat <- confint(lm(values ~ 0 + ind, df.stack))
    dimnames(ci.mat) <- list(names(data.frame), c("lower", "upper"))
    ci.mat
  }
  
  cm.ci.u <- function(data.frame, conf.level = 0.95) {
    #cousineau uncorrected within-subject CIs
    k = ncol(data.frame)
    n <- nrow(data.frame)
    df.stack <- stack(data.frame)
    index <- rep(1:n, k)
    p.means <- tapply(df.stack$values, index, mean)
    norm.df <- data.frame - p.means + (sum(data.frame)/(n * k))
    output <- matrix(NA, k, 2)
    dimnames(output) <- list(names(data.frame), c("lower", "upper"))
    for (i in 1:k) output[i, ] <- t.test(norm.df[i], conf.level = conf.level)$conf.int[1:2]
    output
  }
  
  cm.ci <- function(data.frame, conf.level = 0.95, difference = TRUE) {
    #cousineau-morey within-subject CIs
    k = ncol(data.frame)
    if (difference == TRUE) diff.factor = 2^0.5/2 else diff.factor = 1
    n <- nrow(data.frame)
    df.stack <- stack(data.frame)
    index <- rep(1:n, k)
    p.means <- tapply(df.stack$values, index, mean)
    norm.df <- data.frame - p.means + (sum(data.frame)/(n * k))
    t.mat <- matrix(NA, k, 1)
    mean.mat <- matrix(NA, k, 1)
    for (i in 1:k) t.mat[i, ] <- t.test(norm.df[,i])$statistic[1]
    for (i in 1:k) mean.mat[i, ] <- mean(norm.df[,i])
    c.factor <- (k/(k - 1))^0.5
    moe.mat <- mean.mat/t.mat * qt(1 - (1 - conf.level)/2, n - 1) * c.factor * 
      diff.factor
    ci.mat <- matrix(NA, k, 2)
    dimnames(ci.mat) <- list(names(data.frame), c("lower", "upper"))
    for (i in 1:k) {
      ci.mat[i, 1] <- mean.mat[i] - moe.mat[i]
      ci.mat[i, 2] <- mean.mat[i] + moe.mat[i]
    }
    ci.mat
  }
  
  ml.ci <- function(data.frame, conf.level = 0.95, cov.matrix = "unstructured") {
    # CI based on multilevel model with covariance matrix unstructured or
    #   constrained to compound symmetry
    k = ncol(data.frame)
    n.parts <- nrow(data.frame)
    data.long <- reshape(data.frame, idvar = "id", direction = "long", varying = 1:k, 
                         v.names = "dv")
    require(nlme)
    if (cov.matrix == "comp.symm") ml.mod <- lme(dv ~ 0 + factor(time), random = ~1 | id, na.action = na.omit, 
                                                 data.long) else ml.mod <- lme(dv ~ 0 + factor(time), random = ~0 + factor(time) | 
                                                                                 id, na.action = na.omit, data.long)
    detach(package:nlme)
    require(gmodels)
    ci.mat <- ci(ml.mod, confidence = conf.level)[, 2:3]
    detach(package:gmodels)
    ci.mat
  }
  
  plot.wsci <- function(data.frame, conf.level = 0.95, type = "cm", 
                        difference = TRUE, cov.matrix = "unstructured", xlab = NULL, ylab = NULL, 
                        level.labels = NULL, main = NULL, pch = 23, ylim = c(min.y, max.y),
                        line.width= c(1, 0)) {
    # plot within-subject CIs by various methods
    k = ncol(data.frame)
    if (type == "cm") 
      ci.mat <- cm.ci(data.frame, conf.level, difference = difference)
    if (type == "uncorrected") 
      ci.mat <- cm.ci.u(data.frame, conf.level)
    if (type == "lm") 
      ci.mat <- lm.ci(data.frame, conf.level, difference = difference)
    if (type == "bs") 
      ci.mat <- bs.ci(data.frame, conf.level, difference = difference)
    if (type == "ml") 
      ci.mat <- ml.ci(data.frame, conf.level, cov.matrix = cov.matrix)
    moe.y <- max(ci.mat) - min(ci.mat)
    min.y <- min(ci.mat) - moe.y/3
    max.y <- max(ci.mat) + moe.y/3
    means <- colMeans(data.frame, na.rm = TRUE)
    if (missing(xlab)) 
      xlab <- "levels"
    if (missing(ylab)) 
      ylab <- "Confidence interval for mean"
    if (missing(level.labels) == FALSE) 
      names(data.frame) <- level.labels
    plot(0, 0, ylim = ylim, xaxt = "n", xlim = c(0.7, k + 0.3), xlab = xlab, 
         ylab = ylab, main = main)
    points(means, pch = pch, bg = "black")
    index <- 1:k
    segments(index, ci.mat[, 1], index, ci.mat[, 2], lwd = line.width[1])
    segments(index - 0.02, ci.mat[, 1], index + 0.02, ci.mat[, 1], lwd = line.width[2])
    segments(index - 0.02, ci.mat[, 2], index + 0.02, ci.mat[, 2], lwd = line.width[2])
    axis(1, 1:k, labels = names(data.frame))
  }
  
  two.tiered.ci <- function(data.frame, conf.level = 0.95, cov.matrix = "unstructured", 
                            difference = TRUE, level.labels = NULL, xlab = NULL, ylab = NULL, main = NULL, 
                            pch = 19, pch.cex = 1.4, text.cex = 1.2, ylim = c(min.y, max.y), grid = FALSE,
                            line.width= c(1.5, 1.5)) {
    # plot two tiered CI with ml approach for outer tier and cm approach for inner tier
    k = ncol(data.frame)
    ci.outer <- ml.ci(data.frame, conf.level = conf.level, cov.matrix = cov.matrix)
    moe.y <- max(ci.outer) - min(ci.outer)
    min.y <- min(ci.outer) - moe.y/3
    max.y <- max(ci.outer) + moe.y/3
    means <- colMeans(data.frame)
    if (missing(xlab)) xlab <- "levels"
    if (missing(ylab)) ylab <- "Confidence interval for mean"
    if (missing(level.labels) == FALSE) names(data.frame) <- level.labels
    plot(0, 0, ylim = ylim, xaxt = "n", xlim = c(0.7, k + 0.3), xlab = xlab, 
         ylab = ylab, main = main, cex.lab = text.cex)
    if (grid == TRUE) grid()
    points(means, pch = pch, bg = "black", cex = pch.cex)
    index <- 1:k
    segments(index, ci.outer[, 1], index, ci.outer[, 2], lwd = line.width[1])
    axis(1, index, labels = names(data.frame))
    row.names(ci.outer) <- names(data.frame)
    if (cov.matrix == "comp.symm") ci.inner <- lm.ci(data.frame, conf.level, difference = difference) else ci.inner <- cm.ci(data.frame, conf.level, difference = difference)
    segments(index - 0.02, ci.inner[, 1], index + 0.02, ci.inner[, 1], lwd = line.width[1])
    segments(index - 0.02, ci.inner[, 2], index + 0.02, ci.inner[, 2], lwd = line.width[1])
  }
  ### Librerias sustituidas. Quedan para comprobaciones ####
  
  cm.ci.mixed <- function(data.frame, group.var = "last", conf.level = 0.95, 
                          difference = TRUE) {
    #cousineau-morey within-subject CIs for mixed design
    k = ncol(data.frame)
    if (difference == TRUE) diff.factor = 2^0.5/2 else diff.factor = 1
    if (group.var == "last") {
      within <- 1:(k - 1)
      data.frame[k] <- unclass(data.frame[[k]])[1:nrow(data.frame)]
    }
    else {
      within <- 2:k
      data.frame[1] <- unclass(data.frame[[1]])[1:nrow(data.frame)]
    }
    if (group.var == "last") n.groups <- nlevels(factor(data.frame[[k]])) else n.groups <- nlevels(factor(data.frame[[1]]))
    if (group.var == "last") l.groups <- levels(factor(data.frame[[k]])) else l.groups <- levels(factor(data.frame[[1]]))
    
    c.factor <- ((k - 1)/(k - 2))^0.5
    ci.list <- list()
    for (i in l.groups) {
      if (group.var == "last") data <- subset(data.frame, data.frame[k] == i)[within] else data <- subset(data.frame, data.frame[1] == i)[within]
      p.means <- colMeans(as.data.frame(t(data)))
      norm.dat <- data - p.means + (mean(p.means))
      t.mat <- matrix(NA, k - 1, 1)
      mean.mat <- matrix(NA, k - 1, 1)
      ci.mat <- matrix(NA, k - 1, 2)
      for (j in 1:(k - 1)) t.mat[j, ] <- t.test(norm.dat[j])$statistic[1]
      for (j in 1:(k - 1)) mean.mat[j, ] <- mean(norm.dat[,j])
      n <- nrow(data)
      moe.mat <- mean.mat/t.mat * qt(1 - (1 - conf.level)/2, n - 1) * c.factor * 
        diff.factor
      for (j in 1:(k - 1)) {
        ci.mat[j, 1] <- mean.mat[j] - moe.mat[j]
        ci.mat[j, 2] <- mean.mat[j] + moe.mat[j]
      }
      dimnames(ci.mat) <- list(names(data), c("lower", "upper"))
      ci.list[[i]] <- ci.mat
    }
    ci.list
  }
  
  ml.ci.mixed <- function(data.frame, group.var = "last", conf.level = 0.95, 
                          cov.matrix = "unstructured") {
    # mixed CI based on multilevel model unstructured or constrained to
    #   compound symmetry
    require(nlme);require(gmodels)
    k = ncol(data.frame)
    n.parts <- nrow(data.frame)
    if (group.var == "last") within <- 1:(k - 1) else within <- 2:k
    data.long <- data.table(reshape(data.frame, idvar = "id", direction = "long", varying = within, v.names = "dv"))
    setnames(data.long,names(data.long[,1,with=FALSE]),"group")
    data.long[,id:=factor(id)];data.long[,group:=factor(group)];data.long[,timeCat:=factor(time)]
    l.groups <- data.long[,levels(group)]
    n.groups <- length(l.groups)
    l.with=names(data.frame[,c(within)])
    Fix=as.formula(dv ~ 0 + timeCat:group)
    if (cov.matrix == "within.group.cs") ml.mod <- lme(Fix, random = ~0 + timeCat | id, na.action = na.omit, data.long)
    if (cov.matrix == "unstructured") ml.mod <- lme(Fix, random = list(id = pdDiag(form = ~0 + group), id = ~0 + timeCat), na.action = na.omit, data.long)
    if (cov.matrix == "comp.symm") ml.mod <- lme(Fix, random = ~1 | id, na.action = na.omit, data.long)
    ci.mat <- ci(ml.mod, confidence = conf.level)[, 2:3]
    group.id <- rep(c(l.groups), rep(k - 1, n.groups))
    wth.id <- rep(c(l.with), n.groups)
    output <- data.frame(cbind(wth.id,group.id, round(ci.mat,4)))
    output
  }
  
  two.tiered.mixed <- function(data.frame, group.var = "last", conf.level = 0.95, 
                               cov.matrix = "unstructured", difference = TRUE, lines = FALSE, level.labels = NULL, 
                               xlab = NULL, ylab = NULL, main = NULL, pch = c(21:25, 1:3), pch.cex = 1.2, 
                               pch.col = c(3:9), text.cex = 1.2, ylim = c(min.y, max.y), grid = FALSE, jitter = NULL,
                               group.labels = c(1:8), leg.loc = c(1, min(ci.outer)), line.width= c(1.25, 1.5, 1)) {
    # plot two tiered mixed CI with ml approach for outer tier and cm
    #   approach for inner tier
    k = ncol(data.frame)
    if (group.var == "last") n.groups <- nlevels(factor(data.frame[[k]])) else n.groups <- nlevels(factor(data.frame[[1]]))
    ci.outer <- ml.ci.mixed(data.frame, group.var = group.var, conf.level = conf.level, 
                            cov.matrix = cov.matrix)
    ci.inner <- cm.ci.mixed(data.frame, group.var = group.var, conf.level, 
                            difference = difference)
    ci.group.outer <- matrix(NA, k - 1, 2)
    ci.group.inner <- matrix(NA, k - 1, 2)
    index <- 1:(k - 1)
    moe.y <- max(ci.outer) - min(ci.outer)
    min.y <- min(ci.outer) - moe.y/3
    max.y <- max(ci.outer) + moe.y/3
    if (missing(xlab)) xlab <- "levels"
    if (missing(ylab)) ylab <- "Confidence interval for mean"
    if (missing(level.labels) == FALSE) names(data.frame) <- level.labels
    plot(0, 0, ylim = ylim, xaxt = "n", xlim = c(0.7, k - 1 + 0.3), xlab = xlab, 
         ylab = ylab, main = main, cex.lab = text.cex)
    if (grid == TRUE) grid()
    axis(1, index, labels = rownames(ci.inner[[1]]))
    if (missing(jitter)) jitter <- scale(1:n.groups, scale = FALSE)/(n.groups * 1.5)
    for (i in 1:n.groups) {
      ci.group.outer <- subset(as.data.frame(ci.outer), group.id == i)[2:3]
      ci.group.inner <- ci.inner[[i]]
      means <- (ci.group.inner[, 1] + ci.group.inner[, 2])/2
      points(index + jitter[i], means, pch = pch[i], bg = pch.col[i], cex = pch.cex)
      segments(index + jitter[i], ci.group.outer[, 1], index + jitter[i], 
               ci.group.outer[, 2], lwd = line.width[1])
      segments(index - 0.02 + jitter[i], ci.group.inner[, 1], index + 0.02 + 
                 jitter[i], ci.group.inner[, 1], lwd = line.width[2])
      segments(index - 0.02 + jitter[i], ci.group.inner[, 2], index + 0.02 + 
                 jitter[i], ci.group.inner[, 2], lwd = line.width[2])
      if (lines == TRUE) lines(index + jitter[i], means, lty = i + 1, lwd = line.width[3])
    }
    legend(leg.loc[1], leg.loc[2], legend = group.labels[1:n.groups], pch = pch[1:n.groups], 
           lty = 2:(n.groups + 1), pt.bg = pch.col[1:n.groups], bty = "n", horiz = TRUE, lwd = line.width[3])
  }
  
  #https://cran.r-project.org/doc/contrib/Lemon-kickstart/makerm.R
  # creates a new data frame from an existing one containing repeated
  # measures as columns, e.g. a data frame named exp1.df:
  # subject	age	repeat1	repeat2
  # 001		34	1.45	1.67
  # 002		38	1.20	1.54
  # ...
  # called like:
  # make.rm(1:2,3:4,exp1.df)
  # would multiply the "constant" variables by the number of
  # repeats and reformat the repeats to a single column.
  # subject	age	repdat	contrasts
  # 001		34	1.45	T1
  # 002		38	1.20	T1
  # ...
  # 001		34	1.67	T2
  # 002		38	1.54	T2
  # ...
  # this allows a "univariate" repeated measures analysis of the data.
  
  make.rm<-function(constant,repeated,data,contrasts) {
    if(!missing(constant) && is.vector(constant)) {
      if(!missing(repeated) && is.vector(repeated)) {
        if(!missing(data)) {
          dd<-dim(data)
          replen<-length(repeated)
          if(missing(contrasts))
            contrasts<-
            ordered(sapply(paste("T",1:length(repeated),sep=""),rep,dd[1]))
          else
            contrasts<-matrix(sapply(contrasts,rep,dd[1]),ncol=dim(contrasts)[2])
          if(length(constant) == 1) cons.col<-rep(data[,constant],replen)
          else cons.col<-lapply(data[,constant],rep,replen)
          new.df<-data.frame(cons.col,
                             repdat=as.vector(data.matrix(data[,repeated])),
                             contrasts)
          return(new.df)
        }
      }
    }
    cat("Usage: make.rm(constant, repeated, data [, contrasts])\n")
    cat("\tWhere 'constant' is a vector of indices of non-repeated data and\n")
    cat("\t'repeated' is a vector of indices of the repeated measures data.\n")
  }
  
  
  # B) Mis funciones definitivas Robustas. La versión más reciente estaba en lo de MAngeles Peinado
  
  
  
  twayMM<-function (formula, data, tr = 0.2,
                    nboot = 100, SEED = TRUE, pr = TRUE,loc.fun = median) 
  {
    library(MASS)
    if (SEED)set.seed(2)
    if (missing(data)) {
      mf <- model.frame(formula)
    }
    else {
      mf <- model.frame(formula, data)
    }
    cl <- match.call()
    x <- split(model.extract(mf, "response"), mf[, 2])
    if (tr == 0.5) 
      warning("Comparing medians should not be done with this function!")
    grp <- 1:length(x)
    J <- length(grp)
    h<-sapply(grp, function(j){hMM(x[[j]], tr)})
    w<-sapply(grp, function(j){wV(x[[j]], tr,TRUE)})
    xbar <- sapply(grp, function(j){mean(x[[j]], tr)})
    u=sum(w)
    xtil <- sum(w * xbar)/u
    glf<-sum((1 - w/u)^2/(h - 1))/(J^2 - 1)
    A <- sum(w * (xbar - xtil)^2)/(J - 1)
    B <- 2 * (J - 2) * glf
    TEST <- A/(B + 1)
    nu1 <- J - 1
    nu2 <- 1/(3 * glf)
    sig <- 1 - pf(TEST, nu1, nu2)
    
    #Estimacion Potencia añadida a partir de t1wayv2 & t1way.effect
    pts=unlist(x)
    #nv = lapply(x, length)
    nval=sapply(grp, function(j){length(x[[j]])})
    chkn = var(nval)
    if (chkn == 0){e.pow = var(xbar)/winvarNMM(pts, tr = tr)}
    if (chkn != 0) {
      vals=0;xdat = list()
      N=min(nval)
      vals<-sapply(c(1:nboot), function(i){
        xdat<-sapply(c(1:J), function(j){sample(x[[j]], N)})
        twayEff(xdat, tr = tr)$Var.Explained
      })
      e.pow = loc.fun(vals, na.rm = TRUE)
    }
    list(call = cl,Test = TEST, df1 = nu1, df2 = nu2, n = nval, p.value = sig, 
         Var.Explained = e.pow, Effect.Size = sqrt(e.pow))
    #class(result) <- c("t1way")
    #result
  }
  
  twayEff<-function(x, tr = 0.2) {
    require(MASS)
    grp <- 1:length(x)
    J <- length(grp)
    h<-sapply(grp, function(j){hMM(x[[j]], tr)})
    #w<-sapply(grp, function(j){wV(x[[j]], tr,TRUE)})
    xbar <- sapply(grp, function(j){mean(x[[j]], tr)})  
    pts=unlist(x)
    e.pow = var(xbar)/winvarNMM(pts, tr = tr)
    if (e.pow >= 1) {
      v1<-sapply(c(1:J), function(j){rep(xbar[j], length(x[[j]]))})
      v1<-unlist(v1)
      v2<-unlist(x)
      e.pow = wincorMM(v1, v2, tr = tr)$cor^2
    }
    list(Var.Explained = e.pow,Effect.Size = sqrt(e.pow))
  }
  
  
  t2wayv2_MM<-function (J, K, data, tr = 0.2, grp = c(1:p), p = J * K, g = NULL, 
                        dp = NULL, pr = T) 
  {
    if (!is.null(g[1])) {
      if (length(g) != 2) 
        stop("Argument g should have two values")
      if (is.null(dp[1])) 
        stop("Specify a value for dp, the column containing the data")
      data = fac2list(data[, dp], data[, g])
    }
    if (is.matrix(data)) 
      data = listm(data)
    if (!is.list(data)) 
      stop("Data are not stored in list mode")
    if (p != length(data)) {
      print("The total number of groups, based on the specified levels, is")
      print(p)
      print("The number of groups in data is")
      print(length(data))
      print("Warning: These two values are not equal")
    }
    tmeans <- 0
    h <- 0
    v <- 0
    for (i in 1:p) {
      data[[grp[i]]] = na.del(data[[grp[i]]])
      tmeans[i] <- mean(data[[grp[i]]], tr)
      h[i] <- length(data[[grp[i]]]) - 2 * floor(tr * length(data[[grp[i]]]))
      v[i] <- (length(data[[grp[i]]]) - 1) * winvar(data[[grp[i]]], 
                                                    tr)/(h[i] * (h[i] - 1))
    }
    v <- diag(v, p, p)
    ij <- matrix(c(rep(1, J)), 1, J)
    ik <- matrix(c(rep(1, K)), 1, K)
    jm1 <- J - 1
    cj <- diag(1, jm1, J)
    for (i in 1:jm1) cj[i, i + 1] <- 0 - 1
    km1 <- K - 1
    ck <- diag(1, km1, K)
    for (i in 1:km1) ck[i, i + 1] <- 0 - 1
    cmat <- kron(cj, ik)
    alval <- c(1:999)/1000
    for (i in 1:999) {
      irem <- i
      Qa <- johan_MM(cmat, tmeans, v, h, alval[i])
      if (Qa$teststat > Qa$crit) 
        break
    }
    A.p.value = irem/1000
    cmat <- kron(ij, ck)
    for (i in 1:999) {
      irem <- i
      Qb <- johan_MM(cmat, tmeans, v, h, alval[i])
      if (Qb$teststat > Qb$crit) 
        break
    }
    B.p.value = irem/1000
    cmat <- kron(cj, ck)
    for (i in 1:999) {
      irem <- i
      Qab <- johan_MM(cmat, tmeans, v, h, alval[i])
      if (Qab$teststat > Qab$crit) 
        break
    }
    AB.p.value = irem/1000
    tmeans = matrix(tmeans, J, K, byrow = T)
    list(Qa = Qa$teststat, A.p.value = A.p.value, dfA=Qa$df,Qb = Qb$teststat, 
         B.p.value = B.p.value, dfB=Qb$df, Qab = Qab$teststat, AB.p.value = AB.p.value,
         dfAB=Qab$df,means = tmeans)
  }
  
  johan_MM<-function (cmat, vmean, vsqse, h, alpha = 0.05) 
  {
    yvec <- matrix(vmean, length(vmean), 1)
    if (!is.matrix(vsqse)) 
      vsqse <- diag(vsqse)
    test <- cmat %*% vsqse %*% t(cmat)
    invc <- solve(test)
    test <- t(yvec) %*% t(cmat) %*% invc %*% cmat %*% yvec
    R <- vsqse %*% t(cmat) %*% invc %*% cmat
    A <- sum(diag((diag(R))^2/diag(h - 1)))
    df <- nrow(cmat)
    crit <- qchisq(1 - alpha, df)
    crit <- crit + (crit/(2 * df)) * A * (1 + 3 * crit/(df + 
                                                          2))
    list(teststat = test[1], crit = crit[1],df=df)
  }
  
  #J=nG1;K=nG2;x=VarWA;tr = 0.2; p = J * K;grp = c(1:p);nboot = 599; SEED = TRUE
  t2waybt_MM<-function (J, K, x, tr = 0.2, grp = c(1:p), p = J * K, nboot = 599, 
                        SEED = TRUE) 
  {
    if (is.data.frame(x)) 
      x = as.matrix(x)
    if (is.matrix(x)) 
      x <- listm(x)
    if (!is.list(x)) 
      stop("Data must be stored in a matrix or in list mode.")
    if (SEED) 
      set.seed(2)
    tests = t2way.no.p(J = J, K = K, x, tr = tr, grp = grp)
    TA = NULL
    TB = NULL
    TAB = NULL
    data = list()
    xcen = list()
    for (j in 1:length(x)) xcen[[j]] <- x[[j]] - mean(x[[j]], 
                                                      tr)
    print("Taking bootstrap samples. Please wait.")
    for (b in 1:nboot) {
      for (j in 1:length(x)) data[[j]] <- sample(xcen[[j]], 
                                                 size = length(x[[j]]), replace = TRUE)
      bt = t2way.no.p(J, K, data, tr = tr, grp = grp)
      TA[b] = bt$Qa
      TB[b] = bt$Qb
      TAB[b] = bt$Qab
    }
    
    pA <- sum(tests$Qa <= TA,na.rm=TRUE)/(nboot-sum(is.na(TA)))
    pB <- sum(tests$Qb <= TB,na.rm=TRUE)/(nboot-sum(is.na(TB)))
    pAB <- sum(tests$Qab <= TAB,na.rm=TRUE)/(nboot-sum(is.na(TAB)))
    list(A.p.value = pA, B.p.value = pB, AB.p.value = pAB)
  }
  
  # C) Provenientes de las librerías de apoyo a lo de MLuisa y después para MAngeles Peinado
  #Apoyo Cross Over
  
  ExtrKW<-function(DatPas){
    with (DatPas, paste0("Chi.KW(",round(parameter,3),")=",round(statistic,3),", p=",round(p.value,3)))  
  }
  
  ExtrT<-function(DatPas){
    with (DatPas, paste0("t(",round(parameter,3),")=",round(statistic,3),", p=",round(p.value,3)))
  }
  
  ExtrF2<-function(ff){
    NmVar0=gsub(" ","", rownames(ff) , fixed=TRUE)
    NmVar=gsub(":","*", NmVar0 , fixed=TRUE)
    
    LaF1<-paste0(NmVar[1],": F(",round(ff$"Df"[1],3),",",round(ff$"Df"[4],3),")=",round(ff$"F value"[1],3),
                 ", p=",round(ff$"Pr(>F)"[1],3))
    LaF2<-paste0(NmVar[2],": F(",round(ff$"Df"[2],3),",",round(ff$"Df"[4],3),")=",round(ff$"F value"[2],3),
                 ", p=",round(ff$"Pr(>F)"[2],3))
    LaF3<-paste0(NmVar[3],": F(",round(ff$"Df"[3],3),",",round(ff$"Df"[4],3),")=",round(ff$"F value"[3],3),
                 ", p=",round(ff$"Pr(>F)"[3],3))
    paste0(LaF1,"; ",LaF2,"; ",LaF3)
  }
  
  
  
  ExtrWx<-function(DatPas){
    with (DatPas, paste0("V(",round(nu2,3),")=",round(TEST,3),", p=",round(p.value,3),", Eff.Size=",round(Effect.Size,3)))
  }
  ExtrWxSimpleIC<-function(DatPas){with (DatPas, c(round(ci[1],3),round(ci[2],3)))}
  ExtrWxSimpleES<-function(DatPas){with (DatPas, round(Effect.Size,3))}
  
  ExtrWxSimplep<-function(DatPas){with (DatPas, round(p.value,4))}
  
  
  ExtrWx2<-function(DatPas){
    with (DatPas, paste0("Diff=",round(dif,3),", se=",round(se,3),", V(",round(df,3),")=",round(teststat,3),", p=",round(p.value,3),", CI=(",round(ci[1],3),",",round(ci[2],3),")"))
  }
  
  ExtrWx2_vLin<-function(DatPas){
    NumC=nrow(DatPas$test)
    outpp<-lapply(c(1:NumC), function(x) {
      with (DatPas, paste0("Dif.",test[x,1]," vs ",test[x,2],"=",round(psihat[x,3],3),", se=",round(test[x,5],3),", V(",round(test[x,6],3),")=",round(test[x,3],3),", p=",round(psihat[x,6],4),", CI=(",round(psihat[x,4],3),",",round(psihat[x,5],3),")"))
    })
  }
  
  
  ExtrWx2_vLin2<-function(DatPas,YuenPas){
    NumC=nrow(DatPas$test)
    outpp<-lapply(c(1:NumC), function(x) {
      with (DatPas, paste0("Dif.",test[x,1]," vs ",test[x,2],"=",round(psihat[x,3],3),"; se=",round(test[x,5],3),"; V(",round(test[x,6],3),")=",round(test[x,3],3),"; p=",round(psihat[x,6],4),", CI:",round(psihat[x,4],3),",",round(psihat[x,5],3),"; Ef.Size=",round(YuenPas[x,3],3)))
    })
  }
  
  
  ExtrWxF<-function(DatPas){
    with(DatPas,paste0("Var.1: V=",round(Qa,3),"; p=",round(A.p.value,4),"; Var.2: V=",round(Qb,3),"; p=",round(B.p.value,4),
                       "; Interaccion: V=",round(Qab,3),"; p=",round(AB.p.value,4)))
  }
  ExtrWxF_withdf<-function(DatPas){
    with(DatPas,paste0("Var.1: V(",df,")=",round(Qa,3),"; p=",round(A.p.value,4),"; Var.2: V=",round(Qb,3),"; p=",round(B.p.value,4),
                       "; Interaccion: V=",round(Qab,3),"; p=",round(AB.p.value,4)))
  }
  
  ExtrWxF2<-function(DatP){
    DatPas=DatP$Factor.AB
    NumC=nrow(DatPas$test)
    outpp<-lapply(c(1:NumC), function(x) {
      with (DatPas, paste0("Dif.",test[x,1],"=",round(psihat[x,2],3),"; se=",
                           round(test[x,4],3),"; V(",round(test[x,5],3),")=",
                           round(test[x,2],3),"; p=",round(psihat[x,5],4),
                           "; CI:",round(psihat[x,3],3),",",round(psihat[x,4],3),";"))
    })  
  }
  
  ExtrWxF2_MainA<-function(DatP){
    DatPas=DatP$Factor.A
    NumC=nrow(DatPas$test)
    outpp<-lapply(c(1:NumC), function(x) {
      with (DatPas, paste0("Dif.",test[x,1],"=",round(psihat[x,2],3),"; se=",
                           round(test[x,4],3),"; V(",round(test[x,5],3),")=",
                           round(test[x,2],3),"; p=",round(psihat[x,5],4),
                           "; CI:",round(psihat[x,3],3),",",round(psihat[x,4],3),";"))
    })  
  }
  
  ExtrWxF2_MainB<-function(DatP){
    DatPas=DatP$Factor.B
    NumC=nrow(DatPas$test)
    outpp<-lapply(c(1:NumC), function(x) {
      with (DatPas, paste0("Dif.",test[x,1],"=",round(psihat[x,2],3),"; se=",
                           round(test[x,4],3),"; V(",round(test[x,5],3),")=",
                           round(test[x,2],3),"; p=",round(psihat[x,5],4),
                           "; CI:",round(psihat[x,3],3),",",round(psihat[x,4],3),";"))
    })  
  }
  
  AOVF_MM<-function(DatDT,GRP1,GRP2,DVP){
    ResF<-list()
    #M.AOV=aov(get(DVP)~get(GRP1)*get(GRP2),data=DatDT)
    M.AOV<-aov(update(~. ,paste(DVP,'~',GRP1,"*",GRP2)), data=DatDT)  
    #round(etaSquared(aov(update(~ GRUPO,paste(x,'~.')), data=DFn))[2],3),
    
    Res1=summary(M.AOV)
    ff=Res1[[1]]
    NmVar0=gsub(" ","", rownames(ff) , fixed=TRUE)
    NmVar=gsub(":","*", NmVar0 , fixed=TRUE)  
    LaF1<-paste0(NmVar[1],": F(",round(ff$"Df"[1],3),",",round(ff$"Df"[4],3),")=",round(ff$"F value"[1],3),
                 ", p=",round(ff$"Pr(>F)"[1],3))
    LaF2<-paste0(NmVar[2],": F(",round(ff$"Df"[2],3),",",round(ff$"Df"[4],3),")=",round(ff$"F value"[2],3),
                 ", p=",round(ff$"Pr(>F)"[2],3))
    LaF3<-paste0(NmVar[3],": F(",round(ff$"Df"[3],3),",",round(ff$"Df"[4],3),")=",round(ff$"F value"[3],3),
                 ", p=",round(ff$"Pr(>F)"[3],3))
    #Res2=ExtrF2(Res1[[1]])
    ResF$DepVar=DVP
    ResF$AOV=Res1
    ResF$Eta=etaSquared(M.AOV)
    Eta=round(etaSquared(M.AOV)[,2],3)
    LaF1<-paste0(LaF1,", EtaSq=",Eta[1])
    LaF2<-paste0(LaF2,", EtaSq=",Eta[2])
    LaF3<-paste0(LaF3,", EtaSq=",Eta[3])  
    ResF$ParaAPA=paste0(LaF1,"; ",LaF2,"; ",LaF3)
    ResF
  }
  
  RobAOVF_MM<-function(DatPas,GRP1,GRP2,DVP){
    Res<-list()
    NumDVPas<-which(names(DatPas) == DVP)
    names(DatPas)
    nG1=with (DatPas, length(levels(get(GRP1))))
    nG2=with (DatPas, length(levels(get(GRP2))))
    #DataA <- array(DatMAn[,NumDVma], dim = c(5, 6));
    DatPrv<-DatPas
    DatPrv$Inter<-(with(DatPas, interaction(get(GRP1),  get(GRP2))))
    DatPrv$Inter<-factor(DatPrv$Inter,levels=unique(as.character(DatPrv$Inter)))
    VarWA <- lapply(levels(DatPrv$Inter), function(x) subset(DatPrv,Inter==x,NumDVPas)[[1]])
    Wil1=t2wayv2(nG1,nG2,VarWA)
    Wil2=mcp2atm(nG1,nG2,VarWA)
    Wil1$APA<-ExtrWxF(Wil1)
    Wil2$APA<-ExtrWxF2(Wil2)
    Res$RobAOV<-Wil1
    Res$RobContr<-Wil2
    Res
  }
  
  RobAOVF2_MM<-function(DatPas,GRP1,GRP2,DVP){
    Res<-list()
    NumDVPas<-which(names(DatPas) == DVP)
    names(DatPas)
    nG1=with (DatPas, length(levels(get(GRP1))))
    nG2=with (DatPas, length(levels(get(GRP2))))
    #DataA <- array(DatMAn[,NumDVma], dim = c(5, 6));
    DatPrv<-DatPas
    DatPrv$Inter<-(with(DatPas, interaction(get(GRP1),  get(GRP2))))
    DatPrv$Inter<-factor(DatPrv$Inter,levels=unique(as.character(DatPrv$Inter)))
    VarWA <- lapply(levels(DatPrv$Inter), function(x) subset(DatPrv,Inter==x,NumDVPas)[[1]])
    Wil1=t2wayv2(nG1,nG2,VarWA)
    Wil2=mcp2atm(nG1,nG2,VarWA)
    Wil1$TamEfMain=ESmainMCP_MM(nG1,nG2,VarWA)  
    Wil1$TamEfInter=esImcp(nG1,nG2,VarWA)
    Wil1$APA<-ExtrWxF(Wil1)
    Wil2$APA<-ExtrWxF2(Wil2)
    TamA=round(mean(Wil1$TamEfMain$Factor.A[,3],na.rm=TRUE),3)
    TamB=round(mean(Wil1$TamEfMain$Factor.B[,3],na.rm=TRUE),3)
    TamInt=round(mean(Wil1$TamEfInter$Effect.Sizes,na.rm=TRUE),3)
    Wil1$APATamEf<-paste0(GRP1,": ",TamA,", ",GRP2,": ",TamB,", Interacción: ",TamInt)
    Res$RobAOV<-Wil1
    Res$RobContr<-Wil2
    Res
  }
  
  RobAOVF3_MM<-function(DatPas,GRP1,GRP2,DVP){
    Res<-list()
    NumDVPas<-which(names(DatPas) == DVP)
    names(DatPas)
    nG1=with (DatPas, length(levels(get(GRP1))))
    nG2=with (DatPas, length(levels(get(GRP2))))
    #DataA <- array(DatMAn[,NumDVma], dim = c(5, 6));
    DatPrv<-DatPas
    DatPrv$Inter<-(with(DatPas, interaction(get(GRP1),  get(GRP2))))
    DatPrv$Inter<-factor(DatPrv$Inter,levels=unique(as.character(DatPrv$Inter)))
    VarWA <- lapply(levels(DatPrv$Inter), function(x) subset(DatPrv,Inter==x,NumDVPas)[[1]])
    Wil1=t2wayv2_MM(nG1,nG2,VarWA)
    Wil2=mcp2atm(nG1,nG2,VarWA)
    Wil1$TamEfMain=ESmainMCP_MM(nG1,nG2,VarWA)  
    Wil1$TamEfInter=esImcp(nG1,nG2,VarWA)
    Wil1$APA<-ExtrWxF(Wil1)
    Wil2$APA<-ExtrWxF2(Wil2)
    Wil2$APA_MainA_levels<-with (DatPas, c(levels(get(GRP1))))
    Wil2$APA_MainA<-ExtrWxF2_MainA(Wil2)
    Wil2$APA_MainA_EfSiz<-round(Wil1$TamEfMain$Factor.A[,3],3)
    Wil2$APA_MainB_levels<-with (DatPas, c(levels(get(GRP2))))
    Wil2$APA_MainB<-ExtrWxF2_MainB(Wil2)
    Wil2$APA_MainB_EfSiz<-round(Wil1$TamEfMain$Factor.B[,3],3)
    TamA=round(mean(Wil1$TamEfMain$Factor.A[,3],na.rm=TRUE),3)
    TamB=round(mean(Wil1$TamEfMain$Factor.B[,3],na.rm=TRUE),3)
    TamInt=round(mean(Wil1$TamEfInter$Effect.Sizes,na.rm=TRUE),3)
    Wil1$APATamEf<-paste0(GRP1,": ",TamA,", ",GRP2,": ",TamB,", Interacción: ",TamInt)
    Res$RobAOV<-Wil1
    Res$RobContr<-Wil2
    Res
  }
  
  
  RobAOVFBT_MM<-function(DatPas,GRP1,GRP2,DVP){
    Res<-list()
    NumDVPas<-which(names(DatPas) == DVP)
    names(DatPas)
    nG1=with (DatPas, length(levels(get(GRP1))))
    nG2=with (DatPas, length(levels(get(GRP2))))
    #DataA <- array(DatMAn[,NumDVma], dim = c(5, 6));
    DatPrv<-DatPas
    DatPrv$Inter<-(with(DatPas, interaction(get(GRP1),  get(GRP2))))
    DatPrv$Inter<-factor(DatPrv$Inter,levels=unique(as.character(DatPrv$Inter)))
    VarWA <- lapply(levels(DatPrv$Inter), function(x) subset(DatPrv,Inter==x,NumDVPas)[[1]])
    Wil1=t2waybt_MM(nG1,nG2,VarWA)
    Wil1=t2waybt(nG1,nG2,VarWA)
    Wil2=mcp2atm(nG1,nG2,VarWA)
    Wil1$TamEfMain=ESmainMCP_MM(nG1,nG2,VarWA)  
    Wil1$TamEfInter=esImcp(nG1,nG2,VarWA)
    Wil1$APA<-ExtrWxF(Wil1)
    Wil2$APA<-ExtrWxF2(Wil2)
    Wil2$APA_MainA_levels<-with (DatPas, c(levels(get(GRP1))))
    Wil2$APA_MainA<-ExtrWxF2_MainA(Wil2)
    Wil2$APA_MainA_EfSiz<-round(Wil1$TamEfMain$Factor.A[,3],3)
    Wil2$APA_MainB_levels<-with (DatPas, c(levels(get(GRP2))))
    Wil2$APA_MainB<-ExtrWxF2_MainB(Wil2)
    Wil2$APA_MainB_EfSiz<-round(Wil1$TamEfMain$Factor.B[,3],3)
    TamA=round(mean(Wil1$TamEfMain$Factor.A[,3],na.rm=TRUE),3)
    TamB=round(mean(Wil1$TamEfMain$Factor.B[,3],na.rm=TRUE),3)
    TamInt=round(mean(Wil1$TamEfInter$Effect.Sizes,na.rm=TRUE),3)
    Wil1$APATamEf<-paste0(GRP1,": ",TamA,", ",GRP2,": ",TamB,", Interacción: ",TamInt)
    Res$RobAOV<-Wil1
    Res$RobContr<-Wil2
    Res
  }
  
  #J=nG1;K=nG2;x=VarWA;tr = 0.2; alpha = 0.05; grp = NA; op = F
  mcp2atm_MM<-function (J, K, x, tr = 0.2, alpha = 0.05, grp = NA, op = F) 
  {
    JK <- J * K
    if (is.matrix(x)) 
      x <- listm(x)
    if (!is.na(grp[1])) {
      yy <- x
      x <- list()
      for (j in 1:length(grp)) x[[j]] <- yy[[grp[j]]]
    }
    if (!is.list(x)) 
      stop("Data must be stored in list mode or a matrix.")
    for (j in 1:JK) {
      xx <- x[[j]]
      x[[j]] <- xx[!is.na(xx)]
    }
    if (JK != length(x)) 
      warning("The number of groups does not match the number of contrast coefficients.")
    for (j in 1:JK) {
      temp <- x[[j]]
      temp <- temp[!is.na(temp)]
      x[[j]] <- temp
    }
    temp <- con2way(J, K)
    conA <- temp$conA
    conB <- temp$conB
    conAB <- temp$conAB
    if (!op) {
      Factor.A <- lincon(x, con = conA, tr = tr, alpha = alpha)    
      Factor.B <- lincon(x, con = conB, tr = tr, alpha = alpha)
      Factor.AB <- lincon(x, con = conAB, tr = tr, alpha = alpha)
    }
    All.Tests <- NA
    if (op) {
      Factor.A <- NA
      Factor.B <- NA
      Factor.AB <- NA
      con <- cbind(conA, conB, conAB)
      All.Tests <- lincon(x, con = con, tr = tr, alpha = alpha)
    }
    list(Factor.A = Factor.A, Factor.B = Factor.B, Factor.AB = Factor.AB, 
         All.Tests = All.Tests, conA = conA, conB = conB, conAB = conAB)
  }
  
  StatsMM <- function(DatDT,GRP,DV){
    ResF<-list()
    Res=DatDT[,list(N=length(get(DV)),Media=round(mean(get(DV),na.rm = T),3),Md=round(median(get(DV),na.rm = T),3),ETM=round(ETM(get(DV)),3)),by=list(get(GRP))]
    setnames(Res,c(GRP,"N","Md","Mean","ETM"))
    Res[,DEPV:=DV]
    Res2=summary(aov(get(DV)~get(GRP),data=DatDT))
    Res3=t.test(get(DV)~get(GRP),data=DatDT)
    ResF$Des=Res
    ResF$AOV=Res2
    ResF$t=Res3  
    Res4=cbind(AOV=ExtrF(Res2[[1]]),TTest=ExtrT(Res3))
    ResF$ParaAPA=Res4
    ResF
  }
  ESmainMCP_MM <-function (J, K, x, tr = 0.2, nboot = 100, SEED = TRUE) 
  {
    if (is.matrix(x)) 
      x = listm(x)
    x = lapply(x, na.del)
    con = con2way(J, K)
    conA = con$conA
    FA = matrix(NA, nrow = ncol(conA), ncol = 3)
    ic = 0
    for (jj in 1:J) {
      for (jjj in 1:J) {
        if (jj < jjj) {
          ic = ic + 1
          FA[ic, 1] = jj
          FA[ic, 2] = jjj
        }
      }
    }
    for (j in 1:ncol(conA)) {
      flag1 = (conA[, j] == 1)
      flagm1 = (conA[, j] == -1)
      x1 = as.vector(matl(x[flag1]))
      x2 = as.vector(matl(x[flagm1]))
      FA[j, 3] = yuenv2(x1, x2, tr = tr, nboot = nboot, SEED = SEED)$Effect.Size
    }
    dimnames(FA) <- list(NULL, c("Level", "Level", "Effect.Size"))
    conB = con$conB
    FB = matrix(NA, nrow = ncol(conB), ncol = 3)
    ic = 0
    for (jj in 1:K) {
      for (jjj in 1:K) {
        if (jj < jjj) {
          ic = ic + 1
          FB[ic, 1] = jj
          FB[ic, 2] = jjj
        }
      }
    }
    for (j in 1:ncol(conB)) {
      for (jj in 1:J) {
        for (jjj in 1:J) {
          if (jj < jjj) {
          }
        }
      }
      flag1 = (conB[, j] == 1)
      flagm1 = (conB[, j] == -1)
      x1 = as.vector(matl(x[flag1]))
      x2 = as.vector(matl(x[flagm1]))
      FB[j, 3] = yuenv2(x1, x2, tr = tr, nboot = nboot, SEED = SEED)$Effect.Size
    }
    dimnames(FB) <- list(NULL, c("Level", "Level", "Effect.Size"))
    list(Factor.A = FA, Factor.B = FB)
  }
  
  PHocYuen_MM <-function (J, x, tr = 0.2, nboot = 100, SEED = TRUE) 
  {
    if (is.matrix(x)) 
      x = listm(x)
    x = lapply(x, na.del)
    con = con1way(J)
    conA = con
    FA = matrix(NA, nrow = ncol(conA), ncol = 3)
    ic = 0
    for (jj in 1:J) {
      for (jjj in 1:J) {
        if (jj < jjj) {
          ic = ic + 1
          FA[ic, 1] = jj
          FA[ic, 2] = jjj
        }
      }
    }
    for (j in 1:ncol(conA)) {
      flag1 = (conA[, j] == 1)
      flagm1 = (conA[, j] == -1)
      x1 = as.vector(matl(x[flag1]))
      x2 = as.vector(matl(x[flagm1]))
      FA[j, 3] = yuenv2(x1, x2, tr = tr, nboot = nboot, SEED = SEED)$Effect.Size
    }
    dimnames(FA) <- list(NULL, c("Level", "Level", "Effect.Size"))
    FA
  }
  
  
  InferAll<-function(Dat2Pre,wLav=TRUE) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("TBARS", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    if (wLav) {DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Lavado","Post"))}
    if (!wLav) {DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Post"))}
    DVs <- names(DF[,nC])
    DFn<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),subset(DF,MOMENTO=="Post",nC)-subset(DF,MOMENTO=="Pre",nC))
    
    out1 <- lapply(DVs, function(x) round(summary(aov(update(~ GRUPO,paste(x,'~.')), data=DFn))[[1]][1,5],3))
    out1<-data.frame(do.call(rbind, setNames(out1,DVs)));colnames(out1)<-c("AOVDiff")
    
    out2 <- lapply(DVs, function(x){
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Pre",x)[[1]])
      round(Anova(aov(POST~PRE+GRUPO, data= DFn2))[[4]][2],3)  
    })
    out2<-data.frame(do.call(rbind, setNames(out2,DVs)));colnames(out2)<-c("AOCV")
    
    out3 <- lapply(DVs, function(x){
      lapply(split(DF, list(DF$MOMENTO)), function(z) round(summary(aov(update(~ GRUPO,paste(x,'~.')), data=z))[[1]][1,5],3))
    })
    out3<-data.frame(do.call(rbind, setNames(out3,DVs)));colnames(out3)<-paste0("AOV",colnames(out3))
    
    out4 <- lapply(DVs, function(x) round(kruskal.test(update(~ GRUPO,paste(x,'~.')), data=DFn)$p.value,3))
    out4<-data.frame(do.call(rbind, setNames(out4,DVs)));colnames(out4)<-c("NParDiff")
    
    out5 <- lapply(DVs, function(x){
      lapply(split(DF, list(DF$MOMENTO)), function(z) round(kruskal.test(update(~ GRUPO,paste(x,'~.')), data=z)$p.value,3))
    })
    out5<-data.frame(do.call(rbind, setNames(out5,DVs)));colnames(out5)<-paste0("NPar",colnames(out5))
    
    out6<- lapply(DVs, function(z){
      VarWA <- lapply(levels(DFn$GRUPO), function(x) subset(DFn,GRUPO==x,z)[[1]])
      round(t1wayv2(VarWA)$p.value,3)
    })
    out6<-data.frame(do.call(rbind, setNames(out6,DVs)));colnames(out6)<-c("RobDiff")
    
    out7 <- lapply(DVs, function(x){
      lapply(split(DF, list(DF$MOMENTO)),
             function(z) {
               VarWA<-lapply(levels(z$GRUPO), function(y) subset(z,GRUPO==y,x)[[1]])
               round(t1wayv2(VarWA)$p.value,3)
             })
    })
    out7<-data.frame(do.call(rbind, setNames(out7,DVs)));colnames(out7)<-paste0("Rob",colnames(out7));  
    
    out8 <- lapply(DVs, function(x){
      DFn3<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Pre",x)[[1]])
      try(with(DFn3, round(sm.ancova(PRE, POST, GRUPO, model="equal",display="none")$p,3)))
    })
    out8[sapply(out8, function(x) inherits(x, "try-error"))]<-NA
    out8<-data.frame(do.call(rbind, setNames(out8,DVs)));colnames(out8)<-c("NParAOCV");
    
    out8.b <- lapply(DVs, function(x){
      DFn3<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Pre",x)[[1]])
      with(DFn3, try(round(sm.ancova(PRE, POST, GRUPO, model="parallel",display="none")$p,3)))
    })
    out8.b[sapply(out8.b, function(x) inherits(x, "try-error"))]<-NA
    out8.b<-data.frame(do.call(rbind, setNames(out8.b,DVs)));colnames(out8.b)<-c("NParParal");
    
    out8<-data.frame(cbind(out8,out8.b));colnames(out8)<-list("NParAOCV","NParParal");  
    
    out9 <- lapply(DVs, function(x){
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Pre",x)[[1]])
      VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
      VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
      try(round(ancsm(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],plotit=F)$p.value,3))    
    })  
    out9[sapply(out9, function(x) inherits(x, "try-error"))]<-NA
    #out9<-lapply(out9,function(l) ifelse(l=="Error in if (m == 0) { : valor ausente donde TRUE/FALSE es necesario\n",NA,l))
    out9<-data.frame(do.call(rbind, setNames(out9,DVs)));colnames(out9)<-c("RobAOCV")
    #out9b<-data.table(t(out9),key="RobAOCV");out9b["Error in if (m == 0) { : valor ausente donde TRUE/FALSE es necesario\n"]<-"NA"
    #out9c<-as.matrix(t(out9b))
    
    out10 <- lapply(DVs, function(x){
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Pre",x)[[1]])
      VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
      VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
      try(round(reg2ci(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],plotit=F)[[2]][,4],3))  
    })
    out10[sapply(out10, function(x) inherits(x, "try-error"))]<-NA
    #out10<-lapply(out10,function(l) ifelse(l=="Error in if (omega > 0) { : argumento tiene longitud cero\n",c(NA,NA),l))
    out10<-data.frame(do.call(rbind, setNames(out10,DVs)));colnames(out10)<-list("RobAOCVOrig","RobAOCVPar");
    
    out11 <- lapply(DVs, function(x){
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Pre",x)[[1]])
      VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
      VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
      try(round(ancGpar(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],xout=T,plotit=F)$p.value,3))  
    })
    out11[sapply(out11, function(x) inherits(x, "try-error"))]<-NA
    #out11<-lapply(out11,function(l) ifelse(l=="Error in if (omega > 0) { : argumento tiene longitud cero\n",NA,l))
    out11<-data.frame(do.call(rbind, setNames(out11,DVs)));colnames(out11)<-c("RobAOCV3");
    ##Para hacer pruebas
    #DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",2),POST=subset(DF,MOMENTO=="Post",TBARS)[[1]],PRE=subset(DF,MOMENTO=="Pre",TBARS)[[1]])
    #VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
    #VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
    #ancboot(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]])
    LasP<-data.table(cbind(VARS=DVs,out3,out5,out7,out1,out4,out6,out2,out8,out9,out10,out11));
    LasP
  }
  
  InferAllLav<-function(Dat2Pre) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("TBARS", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    DFn<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),subset(DF,MOMENTO=="Post",nC)-subset(DF,MOMENTO=="Pre",nC))
    DFnLav<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),subset(DF,MOMENTO=="Post",nC)-subset(DF,MOMENTO=="Lavado",nC))
    DVs <- names(DF[,nC])
    
    
    out1b <- lapply(DVs, function(x) round(summary(aov(update(~ GRUPO,paste(x,'~.')), data=DFnLav))[[1]][1,5],3))
    out1b<-data.frame(do.call(rbind, setNames(out1b,DVs)));colnames(out1b)<-c("AOVDiff")
    
    out2b <- lapply(DVs, function(x){
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Lavado",x)[[1]])
      round(Anova(aov(POST~PRE+GRUPO, data= DFn2))[[4]][2],3)  
    })
    out2b<-data.frame(do.call(rbind, setNames(out2b,DVs)));colnames(out2b)<-c("AOCV")
    
    
    out3 <- lapply(DVs, function(x){
      lapply(split(DF, list(DF$MOMENTO)), function(z) round(summary(aov(update(~ GRUPO,paste(x,'~.')), data=z))[[1]][1,5],3))
    })
    out3<-data.frame(do.call(rbind, setNames(out3,DVs)));colnames(out3)<-paste0("AOV",colnames(out3))
    
    out4b <- lapply(DVs, function(x) round(kruskal.test(update(~ GRUPO,paste(x,'~.')), data=DFnLav)$p.value,3))
    out4b<-data.frame(do.call(rbind, setNames(out4b,DVs)));colnames(out4b)<-c("NParDiff")
    
    out5 <- lapply(DVs, function(x){
      lapply(split(DF, list(DF$MOMENTO)), function(z) round(kruskal.test(update(~ GRUPO,paste(x,'~.')), data=z)$p.value,3))
    })
    out5<-data.frame(do.call(rbind, setNames(out5,DVs)));colnames(out5)<-paste0("NPar",colnames(out5))
    
    out6b<- lapply(DVs, function(z){
      VarWA <- lapply(levels(DFnLav$GRUPO), function(x) subset(DFnLav,GRUPO==x,z)[[1]])
      round(t1wayv2(VarWA)$p.value,3)
    })
    out6b<-data.frame(do.call(rbind, setNames(out6b,DVs)));colnames(out6b)<-c("RobDiff")
    
    out7 <- lapply(DVs, function(x){
      lapply(split(DF, list(DF$MOMENTO)),
             function(z) {
               VarWA<-lapply(levels(z$GRUPO), function(y) subset(z,GRUPO==y,x)[[1]])
               round(t1wayv2(VarWA)$p.value,3)
             })
    })
    out7<-data.frame(do.call(rbind, setNames(out7,DVs)));colnames(out7)<-paste0("Rob",colnames(out7))
    
    
    out8 <- lapply(DVs, function(x){
      DFn3<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Lavado",x)[[1]])
      try(with(DFn3, round(sm.ancova(PRE, POST, GRUPO, model="equal",display="none")$p,3)))
    })
    out8[sapply(out8, function(x) inherits(x, "try-error"))]<-NA
    out8<-data.frame(do.call(rbind, setNames(out8,DVs)));colnames(out8)<-c("NParAOCV");
    
    out8.b <- lapply(DVs, function(x){
      DFn3<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Lavado",x)[[1]])
      with(DFn3, try(round(sm.ancova(PRE, POST, GRUPO, model="parallel",display="none")$p,3)))
    })
    out8.b[sapply(out8.b, function(x) inherits(x, "try-error"))]<-NA
    out8.b<-data.frame(do.call(rbind, setNames(out8.b,DVs)));colnames(out8.b)<-c("NParParal");
    
    out8b<-data.frame(cbind(out8,out8.b));colnames(out8b)<-list("NParAOCV","NParParal");
    
    
    out9b <- lapply(DVs, function(x){
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Lavado",x)[[1]])
      VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
      VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
      try(round(ancsm(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],plotit=F)$p.value,3))  
    })
    out9b[sapply(out9b, function(x) inherits(x, "try-error"))]<-NA
    #out9b<-lapply(out9b,function(l) ifelse(l=="Error in if (m == 0) { : valor ausente donde TRUE/FALSE es necesario\n",NA,l))
    out9b<-data.frame(do.call(rbind, setNames(out9b,DVs)));colnames(out9b)<-c("RobAOCV")
    
    out10b <- lapply(DVs, function(x){
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Lavado",x)[[1]])
      VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
      VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
      try(round(reg2ci(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],plotit=F)[[2]][,4],3))  
    })
    out10b[sapply(out10b, function(x) inherits(x, "try-error"))]<-NA
    #out10b<-lapply(out10b,function(l) ifelse(l=="Error in if (omega > 0) { : argumento tiene longitud cero\n",c(NA,NA),l))
    out10b<-data.frame(do.call(rbind, setNames(out10b,DVs)));colnames(out10b)<-list("RobAOCVOrig","RobAOCVPar");
    
    out11b <- lapply(DVs, function(x){
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Lavado",x)[[1]])
      VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
      VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
      try(round(ancGpar(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],xout=T,plotit=F)$p.value,3))  
    })
    out11b[sapply(out11b, function(x) inherits(x, "try-error"))]<-NA
    #out11b<-lapply(out11b,function(l) ifelse(l=="Error in if (omega > 0) { : argumento tiene longitud cero\n",NA,l))
    out11b<-data.frame(do.call(rbind, setNames(out11b,DVs)));colnames(out11b)<-c("RobAOCV3");
    
    ##Para hacer pruebas
    #DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",2),POST=subset(DF,MOMENTO=="Post",TBARS)[[1]],PRE=subset(DF,MOMENTO=="Pre",TBARS)[[1]])
    #VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
    #VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
    #ancboot(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]])
    LasPLav<-data.table(cbind(VARS=DVs,out3,out5,out7,out1b,out4b,out6b,out2b,out8b,out9b,out10b,out11b));
    LasPLav
  }
  
  InferAllNew<-function(Dat2Pre,wLav=TRUE) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("TBARS", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    if (wLav) {DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Lavado","Post"))}
    if (!wLav) {DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Post"))}
    DVs <- names(DF[,nC])
    #DF=data.table(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Lavado","Post"))
    #setkey(DF,MOMENTO)  
    #DFn<-cbind(DF["Post",nG,with=F],DF["Post",nC,with=F]-DF["Pre",nC,with=F])
    DFn<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),subset(DF,MOMENTO=="Post",nC)-subset(DF,MOMENTO=="Pre",nC))
    
    out1 <- lapply(DVs, function(x) {
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Pre",x)[[1]])
      VarWA <- lapply(levels(DFn$GRUPO), function(z) subset(DFn,GRUPO==z,x)[[1]])
      VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
      VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
      list(
        round(summary(aov(update(~ GRUPO,paste(x,'~.')), data=DFn))[[1]][1,5],3),
        round(kruskal.test(update(~ GRUPO,paste(x,'~.')), data=DFn)$p.value,3),
        round(t1wayv2(VarWA)$p.value,3),
        round(Anova(aov(POST~PRE+GRUPO, data= DFn2))[[4]][2],3),
        with(DFn2, try(round(sm.ancova(PRE, POST, GRUPO, model="equal",display="none")$p,3),silent=T)),
        with(DFn2, try(round(sm.ancova(PRE, POST, GRUPO, model="parallel",display="none")$p,3),silent=T)),
        try(round(ancsm(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],plotit=F)$p.value,3),silent=T),
        try(round(reg2ci(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],plotit=F)[[2]][,4],3),silent=T),
        try(round(ancGpar(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],xout=T,plotit=F)$p.value,3),silent=T)
      )})
    NumDv=1:length(DVs);NumV=1:9
    for(i in NumDv) for (j in NumV) if (inherits(out1[[i]][[j]],"try-error")) out1[[i]][[j]]<-NA
    out1b<-data.frame(matrix(unlist(out1), ncol = 10, byrow = TRUE))
    #out1c<-data.frame(do.call(rbind, setNames(out1,DVs)));
    colnames(out1b)<-list("AOVDiff","NParDiff","RobDiff","AOCV","NParAOCV","NParParal","RobAOCV","RobAOCVOrig","RobAOCVPar","RobAOCV3");
    
    out3 <- lapply(DVs, function(x){
      lapply(split(DF, list(DF$MOMENTO)), function(z) {
        VarWA<-lapply(levels(z$GRUPO), function(y) subset(z,GRUPO==y,x)[[1]])
        list(
          round(summary(aov(update(~ GRUPO,paste(x,'~.')), data=z))[[1]][1,5],3),
          round(kruskal.test(update(~ GRUPO,paste(x,'~.')), data=z)$p.value,3),
          round(t1wayv2(VarWA)$p.value,3)
        )}
      )})
    nLv=length(levels(DF$MOMENTO))
    out3<-data.frame(matrix(unlist(out3), ncol = 3*nLv, byrow = TRUE))
    #colnames(out3)<-c(paste0("AOV",levels(DF$MOMENTO)),paste0("NPar",levels(DF$MOMENTO)),paste0("Rob",levels(DF$MOMENTO)))
    NmLe<-c()
    for (l in levels(DF$MOMENTO)) NmLe<-c(NmLe, paste0(c("AOV","NPar","Rob"),l))
    colnames(out3)<-NmLe
    
    LasP<-data.table(cbind(VARS=DVs,out3,out1b));
    LasP
  }
  
  #En construccion
  #Las estimaciones del tamaño del efecto no van del todo bien, salen valores raros debidos creo a los NA
  InferAllNewEfS<-function(Dat2Pre,wLav=TRUE) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("IMC", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    if (wLav) {DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Lavado","Post"))}
    if (!wLav) {DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Post"))}
    DVs <- names(DF[,nC])
    
    #DF=data.table(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Lavado","Post"))
    #setkey(DF,MOMENTO)  
    #DFn<-cbind(DF["Post",nG,with=F],DF["Post",nC,with=F]-DF["Pre",nC,with=F])
    DFn<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),subset(DF,MOMENTO=="Post",nC)-subset(DF,MOMENTO=="Pre",nC))
    
    out1 <- lapply(DVs, function(x) {
      DFn2<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),POST=subset(DF,MOMENTO=="Post",x)[[1]],PRE=subset(DF,MOMENTO=="Pre",x)[[1]])
      VarWA <- lapply(levels(DFn$GRUPO), function(z) subset(DFn,GRUPO==z,x)[[1]])
      VarWAPre <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,PRE)[[1]])
      VarWAPos <- lapply(levels(DFn2$GRUPO), function(x) subset(DFn2,GRUPO==x,POST)[[1]])
      list(
        round(summary(aov(update(~ GRUPO,paste(x,'~.')), data=DFn))[[1]][1,5],3),
        round(kruskal.test(update(~ GRUPO,paste(x,'~.')), data=DFn)$p.value,3),
        round(t1wayv2(VarWA)$p.value,3),
        round(Anova(aov(POST~PRE+GRUPO, data= DFn2))[[4]][2],3),
        with(DFn2, try(round(sm.ancova(PRE, POST, GRUPO, model="equal",display="none")$p,3),silent=T)),
        with(DFn2, try(round(sm.ancova(PRE, POST, GRUPO, model="parallel",display="none")$p,3),silent=T)),
        try(round(ancsm(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],plotit=F)$p.value,3),silent=T),
        try(round(reg2ci(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],plotit=F)[[2]][,4],3),silent=T),
        try(round(ancGpar(VarWAPre[[1]],VarWAPos[[1]],VarWAPre[[2]],VarWAPos[[2]],xout=T,plotit=F)$p.value,3),silent=T),
        round(etaSquared(aov(update(~ GRUPO,paste(x,'~.')), data=DFn))[2],3),
        round(t1wayv2(VarWA)$Effect.Size,3)
      )})
    NumDv=1:length(DVs);NumV=1:11
    for(i in NumDv) for (j in NumV) if (inherits(out1[[i]][[j]],"try-error")) out1[[i]][[j]]<-NA
    out1b<-data.frame(matrix(unlist(out1), ncol = 12, byrow = TRUE))
    #out1c<-data.frame(do.call(rbind, setNames(out1,DVs)));
    colnames(out1b)<-list("AOVDiff","NParDiff","RobDiff","AOCV","NParAOCV","NParParal","RobAOCV","RobAOCVOrig","RobAOCVPar","RobAOCV3","EfSizeDiff","EfSizeRobDiff");
    
    out3 <- lapply(DVs, function(x){
      lapply(split(DF, list(DF$MOMENTO)), function(z) {
        VarWA<-lapply(levels(z$GRUPO), function(y) subset(z,GRUPO==y,x)[[1]])
        list(
          round(summary(aov(update(~ GRUPO,paste(x,'~.')), data=z))[[1]][1,5],3),
          round(kruskal.test(update(~ GRUPO,paste(x,'~.')), data=z)$p.value,3),
          round(t1wayv2(VarWA)$p.value,3)
        )}
      )})
    nLv=length(levels(DF$MOMENTO))
    out3<-data.frame(matrix(unlist(out3), ncol = 3*nLv, byrow = TRUE))
    #colnames(out3)<-c(paste0("AOV",levels(DF$MOMENTO)),paste0("NPar",levels(DF$MOMENTO)),paste0("Rob",levels(DF$MOMENTO)))
    NmLe<-c()
    for (l in levels(DF$MOMENTO)) NmLe<-c(NmLe, paste0(c("AOV","NPar","Rob"),l))
    colnames(out3)<-NmLe
    
    LasP<-data.table(cbind(VARS=DVs,out3,out1b));
    LasP
  }
  
  PlotBox<-function(Dat2Pre,iVar=1,NamesS1,NamesS2,Limita) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("TBARS", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    
    
    DatDF<-data.frame(Dat2Pre)
    DVs <- names(DatDF[,nC])
    
    NmPasS1=NamesS1[iVar];NmPasS2=NamesS2[iVar]
    Limites=Limita[,iVar];
    #LimitesG=LimitaG[,iVar]
    DFSel<-DatDF[,c(nMm,nG,iVar+(nIn-1))]; names(DFSel)<-c("Moment","Group","DV")
    levels(DFSel$Group) <- list(HPC="Oliva", LPC="Refinado",APC="Funcional")
    summ <- ddply(DFSel, .(Moment, Group), summarise, DV = median(DV,na.rm = T))
    
    cbbPalette <- c("#999999", "#FFFFFF", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    g<-ggplot(DFSel, aes(x = Moment, y = DV, fill = Group)) + geom_boxplot(outlier.size = 5)
    g + theme_classic(base_size = 24) + 
      labs(title=NmPasS1,y = NmPasS2) +
      #theme(axis.text.x = TextConf, axis.text.y=TextConf,axis.title=TextConf) +
      #scale_y_continuous(limits=Limites) +
      scale_x_discrete(limit = c("Pre", "Post")) +
      scale_fill_manual(values=cbbPalette) +
      geom_point(data = summ, aes(group=Group), colour="black", 
                 position = position_dodge(width=0.75)) +
      geom_line(data = summ, aes(group=Group), 
                position = position_dodge(width=0.75)) +
      geom_hline(yintercept=Limites,lty=2)
  }
  
  PlotBoxNm2<-function(Dat2Pre,iVar=1,NamesS1,NamesS2,Limita) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("TBARS", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    
    
    DatDF<-data.frame(Dat2Pre)
    DVs <- names(DatDF[,nC])
    
    NmPasS1=NamesS1[iVar];NmPasS2=NamesS2[iVar]
    Limites=Limita[,iVar];
    #LimitesG=LimitaG[,iVar]
    DFSel<-DatDF[,c(nMm,nG,iVar+(nIn-1))]; names(DFSel)<-c("Moment","Group","DV")
    levels(DFSel$Group) <- list(EVOO="Oliva", ROO="Refinado",APC="Funcional")
    summ <- ddply(DFSel, .(Moment, Group), summarise, DV = median(DV,na.rm = T))
    
    cbbPalette <- c("#999999", "#FFFFFF", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    g<-ggplot(DFSel, aes(x = Moment, y = DV, fill = Group)) + geom_boxplot(outlier.size = 5)
    g + theme_classic(base_size = 24) + 
      labs(title=NmPasS1,y = NmPasS2) +
      #theme(axis.text.x = TextConf, axis.text.y=TextConf,axis.title=TextConf) +
      #scale_y_continuous(limits=Limites) +
      scale_x_discrete(limit = c("Pre", "Post")) +
      scale_fill_manual(values=cbbPalette) +
      geom_point(data = summ, aes(group=Group), colour="black", 
                 position = position_dodge(width=0.75)) +
      geom_line(data = summ, aes(group=Group), 
                position = position_dodge(width=0.75)) +
      geom_hline(yintercept=Limites,lty=2)
  }
  
  PlotBoxEsp<-function(Dat2Pre,iVar=1,NamesS1,NamesS2,Limita) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("TBARS", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    
    DatDF<-data.frame(Dat2Pre)
    DVs <- names(DatDF[,nC])
    
    NmPasS1=NamesS1[iVar];NmPasS2=NamesS2[iVar]
    Limites=Limita[,iVar];
    #LimitesG=LimitaG[,iVar]
    DFSel<-DatDF[,c(nMm,nG,iVar+(nIn-1))]; names(DFSel)<-c("Momento","Group","DV")
    levels(DFSel$Group) <- list(HPC="Oliva", LPC="Refinado",APC="Funcional")
    summ <- ddply(DFSel, .(Momento, Group), summarise, DV = median(DV,na.rm = T))
    
    cbbPalette <- c("#999999", "#FFFFFF", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    g<-ggplot(DFSel, aes(x = Momento, y = DV, fill = Group)) + geom_boxplot(outlier.size = 5)
    g + theme_classic(base_size = 24) + 
      labs(title=NmPasS1,y = NmPasS2) +
      #theme(axis.text.x = TextConf, axis.text.y=TextConf,axis.title=TextConf) +
      #scale_y_continuous(limits=Limites) +
      scale_x_discrete(limit = c("Pre", "Post")) +
      scale_fill_manual(values=cbbPalette) +
      geom_point(data = summ, aes(group=Group), colour="black", 
                 position = position_dodge(width=0.75)) +
      geom_line(data = summ, aes(group=Group), 
                position = position_dodge(width=0.75)) +
      geom_hline(yintercept=Limites,lty=2)
  }
  
  PlotBoxEspObe<-function(Dat2Pre,iVar=1,NamesS1,NamesS2,Limita) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("TBARS", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    nOb=grep("Obes", colnames(Dat2Pre))
    DatDF<-data.frame(Dat2Pre)
    DVs <- names(DatDF[,nC])
    NmPasS1=NamesS1[iVar];NmPasS2=NamesS2[iVar]
    Limites=Limita[,iVar];
    #LimitesG=LimitaG[,iVar]
    DFSel<-DatDF[,c(nMm,nG,nOb,iVar+(nIn-1))]; names(DFSel)<-c("Momento", "Group","Obesidad","DV")
    levels(DFSel$Group) <- list(HPC="Oliva", LPC="Refinado",APC="Funcional")
    DFSel$Momento<-factor(DFSel$Momento,levels=c("Pre","Post"))
    summ <- ddply(DFSel, .(Momento, Group,Obesidad), summarise, DV = median(DV,na.rm = T))
    
    cbbPalette <- c("#999999", "#FFFFFF", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    g<-ggplot(DFSel, aes(x = Momento, y = DV, fill = Group)) + geom_boxplot(outlier.size = 5)
    g + theme_classic(base_size = 24) + 
      labs(title=NmPasS1,y = NmPasS2) +
      #theme(axis.text.x = TextConf, axis.text.y=TextConf,axis.title=TextConf) +
      #scale_y_continuous(limits=Limites) +
      scale_x_discrete(limit = c("Pre", "Post")) +
      scale_fill_manual(values=cbbPalette) +
      geom_point(data = summ, aes(group=Group), colour="black", 
                 position = position_dodge(width=0.75)) +
      geom_line(data = summ, aes(group=Group), 
                position = position_dodge(width=0.75)) +
      geom_hline(yintercept=Limites,lty=2) + facet_grid(. ~ Obesidad)
  }
  
  DotCorRob<-function(DatDTPas,ElTit="High Phenolic Compound (HPC) Group",NamesCor1,NamesCor2,SigP=1) {
    corrRob<-pball(as.matrix(DatDTPas));  
    correl2<-round(corrRob$pbcorm,4)
    correl3<-correl2[1:18,19:22]
    correl3<-correl3[-2,]
    correl3<-correl3[-3,]
    rownames(correl3)<-NamesCor1;colnames(correl3)<-NamesCor2
    correlP<-corrRob$siglevel[1:18,19:22]
    correlP<-correlP[-2,]
    correlP<-correlP[-3,]
    if(SigP==1) SimbSig=c(16,1,13)  else  SimbSig=c(15,0,12)
    maskSig=correl3;maskSig[correlP<=.05]<-SimbSig[1];maskSig[correlP>.05]<-SimbSig[2];maskSig[correlP>.05&correlP<.1]<-SimbSig[3]
    CorDF<-data.frame(DepVar=c(correl3),Vars=rep(rownames(correl3),ncol(correl3)),Grp=rep(colnames(correl3),each=nrow(correl3)))
    CorDF$CPoi=c(maskSig)
    quartz("corr")
    op<-par(mar=c(2,1,1,1))
    ug <- unique(as.character(CorDF$Grp))
    CorDF$Grp <- factor(as.character(CorDF$Grp), levels = ug)
    #library(gdata)
    #CorDF$Vars <- reorder.factor(CorDF$Vars, NamesCorReor)
    #CorDF$Vars<-reorder(CorDF$Vars, new.order = NamesCorReor)
    #CorDF$Vars<-factor(CorDF$Vars, labels = NamesCorReor)
    with(CorDF,dotchart2(DepVar,labels=Vars,groups=Grp,pch=CPoi,xlim=c(-1,1),cex.lab=.90,dotsize=1.2,main=ElTit,sort.=FALSE))
    abline(v=-.5,lty=3);abline(v=.5,lty=3)
    CorDF
  }
  
  
  DotCorRobv2<-function(DatDTPas,ElTit="High Phenolic Compound (HPC) Group",NamesCor1,NamesCor2,SigP=1) {
    corrRob<-pball(as.matrix(DatDTPas));  
    correl2<-round(corrRob$pbcorm,4)
    TheL=ncol(DatDTPas);TheLm=TheL-3
    correl3<-correl2[1:(TheLm-1),TheLm:TheL]
    #correl3<-correl3[-2,]
    #correl3<-correl3[-3,]
    rownames(correl3)<-NamesCor1;colnames(correl3)<-NamesCor2
    correlP<-corrRob$siglevel[1:(TheLm-1),TheLm:TheL]
    #correlP<-correlP[-2,]
    #correlP<-correlP[-3,]
    if(SigP==1) SimbSig=c(16,1,13)
    if(SigP==2) SSimbSig=c(15,0,12)
    if(SigP==3) SSimbSig=c(17,0,14)
    maskSig=correl3;maskSig[correlP<=.05]<-SimbSig[1];maskSig[correlP>.05]<-SimbSig[2];maskSig[correlP>.05&correlP<.1]<-SimbSig[3]
    CorDF<-data.frame(DepVar=c(correl3),Vars=rep(rownames(correl3),ncol(correl3)),Grp=rep(colnames(correl3),each=nrow(correl3)))
    CorDF$CPoi=c(maskSig)
    CorDF$p=c(correlP)
    #quartz("corr")
    op<-par(mar=c(2,1,1,1))
    ug <- unique(as.character(CorDF$Grp))
    CorDF$Grp <- factor(as.character(CorDF$Grp), levels = ug)
    #library(gdata)
    #CorDF$Vars <- reorder.factor(CorDF$Vars, NamesCorReor)
    #CorDF$Vars<-reorder(CorDF$Vars, new.order = NamesCorReor)
    #CorDF$Vars<-factor(CorDF$Vars, labels = NamesCorReor)
    with(CorDF,dotchart2(DepVar,labels=Vars,groups=Grp,pch=CPoi,xlim=c(-1,1),cex.lab=.90,dotsize=1.2,main=ElTit,sort.=FALSE))
    abline(v=-.5,lty=3);abline(v=.5,lty=3)  
    CorDF
  }
  
  ProcAOV<-function(ModPas,ResPas) {
    AOV1<-list();prvPrbb<-NA;prvPrbb2<-NA;LasPP<-list()
    AOV1<-aov(ModPas, data= ResPas)
    prvPrbb=round(summary(AOV1)[[1]]$"Pr(>F)"[1],3);
    LasPP[2]=as.character(prvPrbb)
    if (prvPrbb <=.05) prvPrbb2 <-paste0(prvPrbb,"**")
    if (prvPrbb >.05 & prvPrbb<=.1) prvPrbb2 <-paste0(prvPrbb,"..")
    if (prvPrbb >.1) prvPrbb2 =prvPrbb
    LasPP[1]=as.character(prvPrbb2)
    LasPP[3]=as.character(round(etaSquared(AOV1)[2],3))
    LasPP
  }
  
  
  ProcAOVRob<-function(ResPas) {
    AOV1<-list();prvPrbb<-NA;prvPrbb2<-NA;LasPP<-list()
    VarWA<-list();cont=0;MeansT=list();  
    for (il in levels(ResPas$trat)) {
      cont=cont+1
      VarWA[cont]=subset(ResPas,trat==il,Post);
      MeansT[cont]=mean(VarWA[[cont]],trim=.2,na.rm=TRUE)
    }
    AOV1 <-t1wayv2(VarWA);
    RestTestRob<-yuen(VarWA[[1]], VarWA[[2]]);
    prvPrbb=round(AOV1$p.value,3);
    LasPP[2]=as.character(prvPrbb)
    if (prvPrbb <=.05) prvPrbb2 <-paste0(prvPrbb,"**")
    if (prvPrbb >.05 & prvPrbb<=.1) prvPrbb2 <-paste0(prvPrbb,"..")
    if (prvPrbb >.1) prvPrbb2 =prvPrbb
    LasPP[1]=as.character(prvPrbb2)
    LasPP[3]=as.character(round(AOV1$Effect.Size,3))  
    LasPP
  }
  LaP=0.004
  
  SimProb<-function(LaP) {
    prvPrbb=round(LaP,4);prvPrbbCh=as.character(prvPrbb)
    try(if (prvPrbb <=.05) prvPrbbCh <-paste0(prvPrbbCh,"*"))
    try(if (prvPrbb >.05 & prvPrbb<=.1) prvPrbbCh <-paste0(prvPrbbCh,"-"))
    prvPrbbCh
  }
  
  ProcAOVRobDifAll<-function(Dat2Pre) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("TBARS", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Post"))
    DVs <- names(DF[,nC])
    DFn<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),subset(DF,MOMENTO=="Post",nC)-subset(DF,MOMENTO=="Pre",nC))
    
    #z=DVs[1]
    out6<- lapply(DVs, function(z){
      VarWA <- lapply(levels(DFn$GRUPO), function(x) subset(DFn,GRUPO==x,z)[[1]])
      Eltst<-t1wayv2(VarWA)
      ResANWilcContr<-lincon(VarWA)
      #ResAov<-paste0("V= ",round(Eltst$TEST,3),", p=",round(Eltst$p.value,3),", Eff.Size =",round(Eltst$Effect.Size,3))
      
      c(round(Eltst$TEST,3),SimProb(Eltst$p.value),round(Eltst$Effect.Size,3),
        as.vector(c(round(ResANWilcContr$test[1,c(3,5)],3),SimProb(ResANWilcContr$psihat[1,6]))),
        as.vector(c(round(ResANWilcContr$test[2,c(3,5)],3),SimProb(ResANWilcContr$psihat[2,6]))),
        as.vector(c(round(ResANWilcContr$test[3,c(3,5)],3),SimProb(ResANWilcContr$psihat[3,6])))
      )
    })
    out6<-data.frame(do.call(rbind, setNames(out6,DVs)));colnames(out6)<-c("V","p","Eff.Size",rep(c("V","SE","p"),3))
    out6
  }
  
  ProcAOVRobDifSinContr<-function(Dat2Pre) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=min(grep("TBARS", colnames(Dat2Pre)))
    nC<-nIn:ncol(Dat2Pre)
    DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Post"))
    DVs <- names(DF[,nC])
    DFn<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),subset(DF,MOMENTO=="Post",nC)-subset(DF,MOMENTO=="Pre",nC))
    
    #z=DVs[1]
    out6<- lapply(DVs, function(z){
      VarWA <- lapply(levels(DFn$GRUPO), function(x) subset(DFn,GRUPO==x,z)[[1]])
      Eltst<-t1wayv2(VarWA) 
      c(round(Eltst$TEST,3),SimProb(Eltst$p.value),round(Eltst$Effect.Size,3))
    })
    out6<-data.frame(do.call(rbind, setNames(out6,DVs)));colnames(out6)<-c("V","p","Eff.Size")
    out6
  }
  
  ProcAOVRobDiff<-function(ResPas) {
    AOV1<-list();prvPrbb<-NA;prvPrbb2<-NA;LasPP<-list()
    VarWA<-list();cont=0;MeansT=list();  
    for (il in levels(ResPas$trat)) {
      cont=cont+1
      VarWA[cont]=subset(ResPas,trat==il,Diff);
      MeansT[cont]=mean(VarWA[[cont]],trim=.2,na.rm=TRUE)
    }
    AOV1 <-t1wayv2(VarWA);
    RestTestRob<-yuen(VarWA[[1]], VarWA[[2]]);
    prvPrbb=round(AOV1$p.value,3);
    LasPP[2]=as.character(prvPrbb)
    LasPP[3]=as.character(round(RestTestRob$p.value,3))
    if (prvPrbb <=.05) prvPrbb2 <-paste0(prvPrbb,"**")
    if (prvPrbb >.05 & prvPrbb<=.1) prvPrbb2 <-paste0(prvPrbb,"..")
    if (prvPrbb >.1) prvPrbb2 =prvPrbb
    LasPP[1]=as.character(prvPrbb2)
    LasPP[4]=as.character(round(AOV1$Effect.Size,3))
    LasPP[5]=as.character(round(AOV1$TEST,3))
    LasPP[6]=as.character(round(AOV1$nu2,3))
    LasPP[7]=as.character(round(MeansT[[1]]-MeansT[[2]],3))
    LasPP
  }
  
  
  ProcAOVRobDifSinContr2<-function(Dat2Pre) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=3
    nC<-nIn:ncol(Dat2Pre)
    DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Post"))
    DVs <- names(DF[,nC])
    DFn<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),subset(DF,MOMENTO=="Post",nC)-subset(DF,MOMENTO=="Pre",nC))
    
    #z=DVs[1]
    out6<- lapply(DVs, function(z){
      VarWA <- lapply(levels(DFn$GRUPO), function(x) subset(DFn,GRUPO==x,z)[[1]])
      Eltst<-t1wayv2(VarWA) 
      c(round(Eltst$TEST,3),SimProb(Eltst$p.value),round(Eltst$Effect.Size,3))
    })
    out6<-data.frame(do.call(rbind, setNames(out6,DVs)));colnames(out6)<-c("V","p","Eff.Size")
    out6
  }
  
  
  ProcAOVRobDifAll2<-function(Dat2Pre) {
    nG=grep("GRUPO", colnames(Dat2Pre))
    nMm=grep("MOMENTO", colnames(Dat2Pre))
    nIn=3
    nC<-nIn:ncol(Dat2Pre)
    DF=data.frame(Dat2Pre);DF$MOMENTO<-factor(DF$MOMENTO,levels=c("Pre","Post"))
    DVs <- names(DF[,nC])
    DFn<-data.frame(GRUPO=subset(DF,MOMENTO=="Post",nG),subset(DF,MOMENTO=="Post",nC)-subset(DF,MOMENTO=="Pre",nC))
    
    #z=DVs[1]
    out6<- lapply(DVs, function(z){
      VarWA <- lapply(levels(DFn$GRUPO), function(x) subset(DFn,GRUPO==x,z)[[1]])
      Eltst<-t1wayv2(VarWA)
      ResANWilcContr<-lincon(VarWA)
      #ResAov<-paste0("V= ",round(Eltst$TEST,3),", p=",round(Eltst$p.value,3),", Eff.Size =",round(Eltst$Effect.Size,3))
      
      c(round(Eltst$TEST,3),SimProb(Eltst$p.value),round(Eltst$Effect.Size,3),
        as.vector(c(round(ResANWilcContr$test[1,c(3,5)],3),SimProb(ResANWilcContr$psihat[1,6]))),
        as.vector(c(round(ResANWilcContr$test[2,c(3,5)],3),SimProb(ResANWilcContr$psihat[2,6]))),
        as.vector(c(round(ResANWilcContr$test[3,c(3,5)],3),SimProb(ResANWilcContr$psihat[3,6])))
      )
    })
    out6<-data.frame(do.call(rbind, setNames(out6,DVs)));colnames(out6)<-c("V","p","Eff.Size",rep(c("V","SE","p"),3))
    out6
  }
  
  ProcNPar<-function(DV,Trat) {
    AOV1<-list();prvPrbb<-NA;prvPrbb2<-NA;LasPP<-list()
    AOV1<-kruskal.test(DV,Trat)
    prvPrbb=round(AOV1$p.value,3);
    LasPP[2]=as.character(prvPrbb)
    if (prvPrbb <=.05) prvPrbb2 <-paste0(prvPrbb,"**")
    if (prvPrbb >.05 & prvPrbb<=.1) prvPrbb2 <-paste0(prvPrbb,"..")
    if (prvPrbb >.1) prvPrbb2 =prvPrbb
    LasPP[1]=as.character(prvPrbb2)  
    LasPP
  }
  
  ancsm_MM<-function (x1, y1, x2, y2, crit.mat = NULL, nboot = 200, SEED = TRUE, 
                      REP.CRIT = FALSE, LP = TRUE, est = tmean, fr = NULL, plotit = TRUE, 
                      sm = FALSE, xout = FALSE, outfun = out, xlab = "X", ylab = "Y", 
                      ...) 
  {
    if (ncol(as.matrix(x1)) > 1) 
      stop("One covariate only is allowed")
    if (xout) {
      flag1 = outfun(x1, ...)$keep
      flag2 = outfun(x2, ...)$keep
      x1 = x1[flag1]
      y1 = y1[flag1]
      x2 = x2[flag2]
      y2 = y2[flag2]
    }
    xy = na.del(cbind(x1, y1))
    x1 = xy[, 1]
    xord = order(x1)
    x1 = x1[xord]
    y1 = xy[xord, 2]
    xy = na.del(cbind(x2, y2))
    x2 = xy[, 1]
    xord = order(x2)
    x2 = x2[xord]
    y2 = xy[xord, 2]
    n1 = length(y1)
    n2 = length(y2)
    if (is.null(fr)) {
      fr = 1
      if (min(n1, n2) > 150) 
        fr = 0.2
      if (max(n1, n2) < 35) 
        fr = 0.5
    }
    if (SEED) 
      set.seed(2)
    if (is.null(crit.mat[1])) {
      crit.val = NA
      yall = c(y1, y2)
      xall = c(x1, x2)
      nn = n1 + n2
      il = n1 + 1
      for (i in 1:nboot) {
        data = sample(nn, nn, TRUE)
        yy1 = yall[data[1:n1]]
        yy2 = yall[data[il:nn]]
        xx1 = xall[data[1:n1]]
        xx2 = xall[data[il:nn]]
        crit.mat[i] = depthcom(xx1, yy1, xx2, yy2, est = est, 
                               fr = fr)
      }
    }
    if (plotit) 
      runmean2g(x1, y1, x2, y2, fr = fr, est = est, sm = sm, 
                xlab = xlab, ylab = ylab, LP = LP, ...)
    dep = depthcom(x1, y1, x2, y2, est = est, fr = fr)
    n = min(n1, n2)
    pv = 1 - mean(crit.mat < dep)
    if (!REP.CRIT) 
      crit.mat = NULL
    list(p.value = pv, crit.mat = crit.mat, test.depth = dep)
  }
  
  DotEff<-function(Sel,NameVar) {
    EtiqX="Effect Size"
    with(Sel,dotchart(V2,labels=Names,xlim=c(0,1),main=NameVar,xlab=EtiqX));
    abline(v=.04,lty=3,col="red");abline(v=.25,lty=3,col="blue");abline(v=.64,lty=3,col="black")	
  }
  
  ANOVACross<-function(Result){
    Result[,layer:=factor(layer)]
    Result[,trat:=factor(trat)]
    Result[,time:=factor(time)]
    Result[,suj:=factor(suj)]
    
    Ntt=nrow(Result)
    GM=((Result[,sum(dv)])^2)/Ntt
    TT=Result[,dv^2];
    TT=sum(TT)-GM
    
    Layer=Result[,sum(dv)^2/length(dv),by=layer]
    Layer=Layer[,sum(V1)]-GM
    
    Betw=Result[,sum(dv)^2/length(dv),by=suj]
    Betw=Betw[,sum(V1)]-GM
    Within=TT-Betw
    Subj=Result[,sum(dv^2)]-GM
    ErrS=Betw-Layer
    Trat=Result[,sum(dv)^2/length(dv),by=trat]
    Trat=Trat[,sum(V1)]-GM
    Order=Result[,sum(dv)^2/length(dv),by=time]
    Order=Order[,sum(V1)]-GM
    ErrWith=Within-Order-Trat
    
    n.lay=Result[,length(levels(layer))]
    n.trat=Result[,length(levels(trat))]
    n.ord=Result[,length(levels(time))]
    n.suj=Result[,length(levels(suj))]/n.lay
    
    df.TT=Ntt-1
    df.Betw=(n.suj*n.trat)-1
    df.Within=n.suj*n.trat*(n.ord-1)
    df.Layer=n.lay-1
    df.ErrS=n.lay*(n.suj-1)
    df.Order=n.ord-1
    df.Trat=n.trat-1
    df.ErrWith=(n.ord-1)*(n.suj*n.trat-2)
    
    MS.TT=TT/df.TT
    
    SS=rbind("Between"=Betw," Layer"=Layer," ErrS/Lay"=ErrS,"Within"=Within," Order"=Order," Trat"=Trat," Err(Within)"=ErrWith,"Total"=TT);
    df=rbind("Between"=df.Betw," Layer"=df.Layer," ErrS/Lay"=df.ErrS, "Within"=df.Within," Order"=df.Order," Trat"=df.Trat," Err(Within)"=df.ErrWith,"TT"=df.TT);
    MS=data.frame("MS"=round((SS/df),3));MS[c(1,4,8),]=''
    F1=round((Order/df.Order)/(ErrWith/df.ErrWith),3)
    F2=round((Trat/df.Trat)/(ErrWith/df.ErrWith),3)
    FEs=c('','','','',F1,F2,'','')
    p1=round(1-pf(F1,df.Order,df.ErrWith),4)
    p2=round(1-pf(F2,df.Trat,df.ErrWith),4)
    
    
    if (p1<=.05) p1=paste0(p1,"*")
    if (p2<=.05) p2=paste0(p2,"*")
    pEs=c('','','','',p1,p2,'','')
    
    ANOVA=data.frame(SS,df,MS,"F"=FEs,p=pEs)
    ANOVA
  }
  
  #Para Contrastes
  
  contrMat_MM <-function (n, type = c("Dunnett", "Tukey", "Sequen", "AVE", "Changepoint", 
                                      "Williams", "Marcus", "McDermott", "UmbrellaWilliams", "GrandMean"), 
                          base = 1) 
  {
    if (length(n) < 2) 
      stop("less than two groups")
    if (!is.numeric(n)) 
      stop(sQuote("n"), " is not numeric")
    m <- NULL
    type <- match.arg(type)
    if (type %in% c("AVE", "Williams", "McDermott") && length(n) < 
        3) 
      stop("less than three groups")
    k <- length(n)
    if (base < 1 || base > k) 
      stop("base is not between 1 and ", k)
    CM <- c()
    rnames <- c()
    if (!is.null(names(n))) 
      varnames <- names(n)
    else varnames <- 1:length(n)
    kindx <- 1:k
    switch(type, Dunnett = {
      for (i in kindx[-base]) CM <- rbind(CM, as.numeric(kindx == 
                                                           i) - as.numeric(kindx == base))
      rnames <- paste(varnames[kindx[-base]], "-", varnames[base])
    }, Tukey = {
      for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
          CM <- rbind(CM, as.numeric(kindx == j) - as.numeric(kindx == 
                                                                i))
          rnames <- c(rnames, paste(varnames[j], "-", varnames[i]))
        }
      }
    }, Sequen = {
      for (i in 2:k) {
        CM <- rbind(CM, as.numeric(kindx == i) - as.numeric(kindx == 
                                                              i - 1))
        rnames <- c(rnames, paste(varnames[i], "-", varnames[i - 
                                                               1]))
      }
    }, AVE = {
      help <- c(1, -n[2:k]/sum(n[2:k]))
      CM <- rbind(CM, help)
      for (i in 2:(k - 1)) {
        x <- sum(n[1:(i - 1)]) + sum(n[(i + 1):k])
        help <- c(-n[1:(i - 1)]/x, 1, -n[(i + 1):k]/x)
        CM <- rbind(CM, help)
      }
      help <- c(-n[1:(k - 1)]/sum(n[1:(k - 1)]), 1)
      CM <- rbind(CM, help)
      rnames <- paste("C", 1:nrow(CM))
    }, Changepoint = {
      for (i in 1:(k - 1)) {
        help <- c(-n[1:i]/sum(n[1:i]), n[(i + 1):k]/sum(n[(i + 
                                                             1):k]))
        CM <- rbind(CM, help)
      }
      rnames <- c(rnames, paste("C", 1:nrow(CM)))
    }, Williams = {
      for (i in 1:(k - 2)) {
        help <- c(-1, rep(0, k - i - 1), n[(k - i + 1):k]/sum(n[(k - 
                                                                   i + 1):k]))
        CM <- rbind(CM, help)
      }
      help <- c(-1, n[2:k]/sum(n[2:k]))
      CM <- rbind(CM, help)
      rnames <- c(rnames, paste("C", 1:nrow(CM)))
    }, Marcus = {
      cm1 <- matrix(0, nrow = k - 1, ncol = k)
      cm2 <- cm1
      for (i in 1:(k - 1)) {
        cm1[i, (i + 1):k] <- n[(i + 1):k]/sum(n[(i + 1):k])
        cm2[i, 1:i] <- n[1:i]/sum(n[1:i])
      }
      index <- 1
      for (i in 1:(k - 1)) {
        for (j in 1:i) {
          help <- cm1[i, ] - cm2[j, ]
          CM <- rbind(CM, help)
          index <- index + 1
        }
      }
      rnames <- c(rnames, paste("C", 1:nrow(CM)))
    }, McDermott = {
      for (i in 1:(k - 2)) {
        help <- c(-n[1:i]/sum(n[1:i]), 1, rep(0, k - i - 
                                                1))
        CM <- rbind(CM, help)
      }
      help <- c(-n[1:(k - 1)]/sum(n[1:(k - 1)]), 1)
      CM <- rbind(CM, help)
      rnames <- c(rnames, paste("C", 1:nrow(CM)))
    }, Tetrade = {
      if (is.null(m)) stop(sQuote("m"), " is missing")
      a <- length(n)
      b <- length(m)
      if (!is.null(names(m))) varnamesm <- names(m) else varnamesm <- 1:length(m)
      idi <- 1:a
      idj <- 1:b
      for (i1 in 1:(a - 1)) {
        for (i2 in (i1 + 1):a) {
          for (j1 in 1:(b - 1)) {
            for (j2 in (j1 + 1):b) {
              CM <- rbind(CM, kronecker((as.numeric(idi == 
                                                      i1) - as.numeric(idi == i2)), (as.numeric(idj == 
                                                                                                  j1) - as.numeric(idj == j2))))
              rnames <- c(rnames, paste("(", paste(varnames[i1], 
                                                   varnamesm[j1], sep = ":"), "-", paste(varnames[i1], 
                                                                                         varnamesm[j2], sep = ":"), ")", "-", "(", 
                                        paste(varnames[i2], varnamesm[j1], sep = ":"), 
                                        "-", paste(varnames[i2], varnamesm[j2], 
                                                   sep = ":"), ")", sep = ""))
            }
          }
        }
      }
    }, UmbrellaWilliams = {
      for (j in 1:(k - 1)) {
        for (i in 1:(k - j)) {
          helper <- c(-1, rep(0, k - i - j), n[((k - i + 
                                                   1):k) - (j - 1)]/sum(n[((k - i + 1):k) - (j - 
                                                                                               1)]), rep(0, j - 1))
          CM <- rbind(CM, helper)
        }
      }
      rnames <- c(rnames, paste("C", 1:nrow(CM)))
    }, GrandMean = {
      CM <- matrix(rep(-n/sum(n), k), nrow = k, byrow = TRUE)
      diag(CM) <- diag(CM) + 1
      rnames <- c(rnames, paste("C", 1:nrow(CM)))
    })
    rownames(CM) <- rnames
    if (type == "Tetrade") 
      colnames(CM) <- NULL
    else colnames(CM) <- varnames
    attr(CM, "type") <- type
    class(CM) <- c("contrMat", "matrix")
    CM
  }
  
}


# 2. Funciones básicas de análisis estadístico para apoyo a todos los Programas de Manuel Miguel
 # Parte II. Versión 23 marzo de 2019
 # Aspectos Psicométricos
{
  RecodDT<-function(DTp) {
    DTp[DTp[,.SD=="a"]]<-5
    DTp[DTp[,.SD=="b"]]<-4
    DTp[DTp[,.SD=="c"]]<-3
    DTp[DTp[,.SD=="d"]]<-2
    DTp[DTp[,.SD=="e"]]<-1
    DTp[DTp[,.SD=="A"]]<-5
    DTp[DTp[,.SD=="B"]]<-4
    DTp[DTp[,.SD=="C"]]<-3
    DTp[DTp[,.SD=="D"]]<-2
    DTp[DTp[,.SD=="E"]]<-1
    return(DTp)
  }
  
  RecodDTf<-function(DTpp) {
    DTp<-data.table(do.call(cbind,lapply(DTpp,as.character)))
    DTp[DTp[,.SD=="a"]]<-5
    DTp[DTp[,.SD=="b"]]<-4
    DTp[DTp[,.SD=="c"]]<-3
    DTp[DTp[,.SD=="d"]]<-2
    DTp[DTp[,.SD=="e"]]<-1
    DTp[DTp[,.SD=="A"]]<-5
    DTp[DTp[,.SD=="B"]]<-4
    DTp[DTp[,.SD=="C"]]<-3
    DTp[DTp[,.SD=="D"]]<-2
    DTp[DTp[,.SD=="E"]]<-1
    DTp.a<-data.table(do.call(cbind,lapply(DTp,as.numeric)))
    return(DTp.a)
  }
  
  IDnMM<-function(Data,Ordin=NA,Number,Categ=NA) {
    require(parallel);require(data.table)
    IDn<-list()
    DTOmit<-data.table(na.omit(Data))
    X<-mclapply(Ordin, function(i) {polycor::polyserial(DTOmit[,rowSums(DTOmit[,-i,with=F])],DTOmit[,i,with=F][[1]])[[1]]})
    Y<-mclapply(Number, function(i) {cor(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]])[[1]]})
    IDn<-c(rep(NA,length(DTOmit)))
    IDn[Ordin]<-X;IDn[Number]<-Y
    IDnF<- data.table(Names=names(DTOmit),Corr=do.call(rbind,setNames( IDn,names(DTOmit))))
    return(IDnF)
  }
  
  IDnMMOrd<-function(Data,Ordin) {
    require(parallel);require(data.table)
    IDn<-list()
    DTOmit<-data.table(na.omit(Data))
    X<-mclapply(Ordin, function(i) {polycor::polyserial(DTOmit[,rowSums(DTOmit[,-i,with=F])],DTOmit[,i,with=F][[1]])[[1]]})
    IDn<-c(rep(NA,length(DTOmit)))
    IDn[Ordin]<-X;
    IDnF<- data.table(Names=names(DTOmit),Corr=do.call(rbind,setNames( IDn,names(DTOmit))))
    return(IDnF)
  }
  
  fzero = function(X1)gsub("0\\.","\\.", X1)
  
  fitlav<-function(Model,Data) {
    result<-list()
    fit<-sem(Model,data=Data,fixed.x=F)
    #result$Summ<-summary(fit,standardized=T,fit.measures=T)
    fitSt<-fitMeasures(fit)
    result$Par<-parameterEstimates(fit,standardized=T)
    Val1=round(fitSt[[2]],4)
    Val2=fitSt[[3]]
    Val3=as.character(round(fitSt[[4]],4))
    Lap=fitSt[[4]];
    if (Lap>=0.05) Val3=paste0(Val3,"*")
    Val4=as.character(round(fitSt[[8]],4))
    Lap=fitSt[[8]];
    if (Lap>=0.95) Val4=paste0(Val4,"*")
    Val5=as.character(round(fitSt[[23]],4))
    Lap=fitSt[[26]];
    if (Lap>=0.05) Val5=paste0(Val5,"*");
    Val5=paste0(Val5," (",round(fitSt[[24]],4),", ",round(fitSt[[25]],4),")")
    Val6=as.character(round(fitSt[[29]],4))
    Lap=fitSt[[29]];
    if (Lap<=0.08) Val6=paste0(Val6,"*")
    Val7=fitSt[[21]]
    #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
    result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=Val3,CFI=Val4,RMSE=Val5,SRMR=Val6,N=Val7)
    result
  }
  
  fitlavPar<-function(fit,Prec=2) {
    result<-list()
    
    fitSt<-fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea","rmsea.ci.lower", "rmsea.ci.upper" ,"SRMR","ntotal","gfi","nnfi"))
    
    result$Par<-parameterEstimates(fit,standardized=T)
    #CHI
    Val1=round(fitSt[[1]],Prec)
    Val2=fitSt[[2]]
    ValDiv=as.character(round((fitSt[[1]]/fitSt[[2]]),Prec))
    if ((Val1/Val2)<2) ValDiv=paste0(ValDiv,"*")
    Val3=as.character(round(fitSt[[3]],Prec))
    Lap=fitSt[[3]];
    if (Lap>=0.05) Val3=paste0(Val3,"*")
    #CFI
    Val4=as.character(round(fitSt[[4]],Prec))
    Lap=fitSt[[4]]; if (Lap>=0.95) Val4=paste0(Val4,"*")
    #RMSEA
    Val5=as.character(round(fitSt[[5]],Prec))
    Lap=fitSt[[5]];if (Lap<=0.06) Val5=paste0(Val5,"*");
    Val5b=paste0(Val5," (",round(fitSt[[6]],Prec),",",round(fitSt[[7]],Prec),")")
    #SRMR
    Val6=as.character(round(fitSt[[8]],Prec))
    Lap=fitSt[[8]];if (Lap<=0.08) Val6=paste0(Val6,"*")
    #N
    Val7=fitSt[[9]]
    
    #GFI
    Val8=as.character(round(fitSt[[10]],Prec))
    Lap=fitSt[[10]]; if (Lap>=0.95) Val8=paste0(Val8,"*");
    #NNFI
    Val9=as.character(round(fitSt[[11]],Prec))
    Lap=fitSt[[11]]; if (Lap>=0.95) Val9=paste0(Val9,"*");
    
    #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
    result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=Val3,GFI=Val8,CFI=Val4,NNFI=Val9,RMSE=Val5b,SRMR=Val6,N=Val7,Ratio=ValDiv)
    result$All<-fitMeasures(fit)
    result
  }
  
  fitlavCFA<-function(Model,Data) {
    result<-list()
    fit <- cfa(Model, Data)
    #fit<-sem(Model,data=Data,fixed.x=F)
    #result$Summ<-summary(fit,standardized=T,fit.measures=T)
    
    fitSt<-fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea","rmsea.ci.lower", "rmsea.ci.upper" ,"SRMR","ntotal"))
    
    result$Par<-parameterEstimates(fit,standardized=T)
    Val1=round(fitSt[[1]],4)
    Val2=fitSt[[2]]
    Val3=as.character(round(fitSt[[3]],4))
    Lap=fitSt[[3]];
    if (Lap>=0.05) Val3=paste0(Val3,"*")
    Val4=as.character(round(fitSt[[4]],4))
    Lap=fitSt[[4]];
    if (Lap>=0.95) Val4=paste0(Val4,"*")
    Val5=as.character(round(fitSt[[5]],4))
    Lap=fitSt[[5]];
    if (Lap<=0.06) Val5=paste0(Val5,"*");
    Val5b=paste0(Val5," (",round(fitSt[[6]],4),",",round(fitSt[[7]],4),")")
    Val6=as.character(round(fitSt[[8]],4))
    Lap=fitSt[[8]];
    if (Lap<=0.08) Val6=paste0(Val6,"*")
    Val7=fitSt[[9]]
    #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
    result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=Val3,CFI=Val4,RMSE=Val5b,SRMR=Val6,N=Val7)
    result$All<-fitMeasures(fit)
    result
  }
  
  EstimaCorr<-function(DTP,scalP,CasesP="complete.obs",TCor=1,wT=FALSE,CompP=FALSE,MLp=FALSE) {
    Corrs<-list()
    if (TCor==1) { 
      co <- corr.test(DTP, use=CasesP, method="pearson");
      Corrs$v <- co$r
      if (wT) Corrs$t <- co$p
    }
    if (TCor==2) { 
      da <- DTP
      n <- ncol(DTP)
      for (i in 1:n) { da[,i]<-ordered(DTP[,i])}  # for calculation of polychoric correlations
      co <- hetcor(da,  ML = MLp, std.err = FALSE, pd=TRUE, use=CasesP, digits=6)
      Corrs$v <- co$correlations
    } 
    if (TCor==3) { 
      da <- DTP
      n <- ncol(DTP)
      for (i in 1:n) { da[,i]<-ordered(DTP[,i])}  # for calculation of polychoric correlations
      co <- hetcor(da,   ML = MLp, std.err = FALSE, pd=TRUE, use=CasesP, digits=6)
      Corrs$v <- co$correlations
    }
    if (TCor==4) { 
      co <- corr.test(DTP, use=CasesP, method="spearman");
      Corrs$v <- co$r
      if (wT) Corrs$t <- co$p
    }
    if (TCor==5) { 
      dah <- DTP
      n <- ncol(DTP)
      for (i in 1:n) {			
        if  (is.factor(scalP[,i])) {dah[,i]<-as.factor(DTP[,i]) } 
        else { if  (is.ordered(scalP[,i])) { dah[,i]<-ordered(DTP[,i]) } } 
      }  #for calculation of correlations according to their scales
      co <- hetcor(dah,   ML = MLp , std.err = FALSE, pd=TRUE, use=CasesP, digits=6)
      Corrs$v <- co$correlations
    } 
    if (TCor==6) { 
      dah <- DTP
      n <- ncol(DTP)
      for (i in 1:n) {			
        if  (is.factor(scalP[,i])) {dah[,i]<-as.factor(DTP[,i]) } 
        else { if  (is.ordered(scalP[,i])) { dah[,i]<-ordered(DTP[,i]) } } 
      }  #for calculation of correlations according to their scales
      co <- hetcor(dah,   ML = MLp , std.err = FALSE, pd=TRUE, use=CasesP, digits=6)
      Corrs$v <- co$correlations
    } 
    if (CompP) {Corrs$evt <- eigen(Corrs$v, only.values = T) }
    if (!CompP) {Corrs$evt <- eigen(Corrs$v - ginv(diag(diag(ginv(Corrs$v)))), only.values=T) }
    return(Corrs)
  }
  
  EstimaCorrSimple<-function(scal) {
    n=ncol(scal)
    AlmCorr<-matrix(NA,nrow = n,ncol=n)
    for (i in 1:n) {
      for (j in i:n) {
        Ty1<-scal[,i];Ty2<-scal[,j]
        if(is.ordered(Ty1) && is.ordered(Ty2)) {AlmCorr[i,j]<-polycor::polychor(Ty1,Ty2)}
        if(is.ordered(Ty1) && is.numeric(Ty2)) {AlmCorr[i,j]<-polycor::polyserial(Ty2,Ty1)}
        if(is.numeric(Ty1) && is.ordered(Ty2)) {AlmCorr[i,j]<-polycor::polyserial(Ty1,Ty2)}
        if(is.numeric(Ty1) && is.numeric(Ty2)) {AlmCorr[i,j]<-cor(Ty1,Ty2)}
        AlmCorr[j,i]<-AlmCorr[i,j]
      }
    }
    diag(AlmCorr)<-1
    return(AlmCorr)
  }
  
  quant <- function(x, sprobs = sprobs) { return(as.vector(quantile(x, probs = c(QuanSim)))) }
  
  ### Function 1/3: EFA.Comp.Data
  EFA.Comp.Data <- function(DataP, F.Max=7, N.Pop=10000,N.Samples=500, Alpha = 0.3,scalP ,CasesP,TCor,UntilSig=T) {
    # Data = N by k data matrix
    # F.Max = largest number of factors to consider
    # N.Pop = size of finite populations of comparison data
    # N.Samples = number of samples drawn from each population
    # Alpha = alpha level testing significance of improvement with adding factor
    sig2 <- data.frame(1)
    N <- dim(DataP)[1]
    k <- dim(DataP)[2]
    Data2 <- as.data.frame(DataP)
    
    cor.DataF <-EstimaCorr(DTP=Data2,scalP = scalP,CasesP=CasesP,TCor = TCor,wT=F,CompP=T)
    cor.Data<-cor.DataF$v
    #evpea[k, ] <- cor.DataF$evt$values
    
    #Eigs.Data <- eigen(cor.Data)$values
    Eigs.Data <- cor.DataF$evt$values
    RMSR.Eigs <- matrix(0, nrow = N.Samples, ncol = F.Max)
    Sig <- T
    F.CD <- 1
    nf <- -1
    
    while (F.CD <= F.Max) {
      Pop <- GenData(DataP, N.Factors = F.CD, N = N.Pop, Target.Corr = cor.Data,scalP = scalP,CasesP=CasesP,TCor = TCor)
      
      for (j in 1:N.Samples) {
        Samp <- Pop[sample(1:N.Pop, size = N, replace = T),]
        Samp2 <- as.data.frame(Samp)
        cor.SampF <- EstimaCorr(DTP = Samp2,scalP = scalP,CasesP=CasesP,TCor = TCor,wT=F,CompP=T)
        cor.Samp<-cor.SampF$v
        
        Eigs.Samp <- eigen(cor.Samp)$values
        #Eigs.Samp <- cor.SampF$evt$values
        RMSR.Eigs[j,F.CD] <- sqrt(sum((Eigs.Samp - Eigs.Data) * (Eigs.Samp - Eigs.Data)) / k) 
      }
      
      if (F.CD > 1) {sig2[F.CD, ] <- wilcox.test(RMSR.Eigs[,F.CD], RMSR.Eigs[,(F.CD - 1)], "less")$p.value }
      else { sig2[1,] <- NA }
      
      if (F.CD > 1)  { Sig <- (wilcox.test(RMSR.Eigs[,F.CD], RMSR.Eigs[,(F.CD - 1)], "less")$p.value < Alpha) }
      
      F.CD <- F.CD + 1
      if (Sig == FALSE && nf == -1) { nf = F.CD - 2; if (UntilSig) { break } }
    }
    
    
    if (nf == -1) { nf = F.Max }
    # graph
    quartz("CD Method")
    if (UntilSig) { x.max <- min(nf+1, F.Max) }
    else { x.max <- F.Max }
    ys <- apply(RMSR.Eigs[,1:x.max], 2, mean)
    plot(x = 1:x.max, y = ys, ylim = c(0, max(ys)), xlab = "Factor", 
         ylab = "RMSR Eigenvalue", type = "b", main = "Fit to Comparison Data")
    abline(v = nf, lty = 3) 
    lis <- list(ys = ys, nf = nf, sig2 = sig2, x.max = x.max)
    return(lis)
  } # Function EFA.Comp.Data
  
  ### Function 2/3: Gen.Data
  GenData <- function(Supplied.Data, N.Factors, N, Max.Trials = 5, Initial.Multiplier = 1, Target.Corr,
                      scalP = scalP,CasesP=CasesP,TCor = TCor) {
    # Ruscio, J., & Kaczetow, W. (2008). 
    # Simulating multivariate nonnormal data using an iterative algorithm. 
    # Multivariate Behavioral Research, 43(3), 355-381.
    k <- dim(Supplied.Data)[2]
    Iteration <- 0   # Iteration counter
    Best.RMSR <- 1   # Lowest RMSR correlation
    Trials.Without.Improvement <- 0   # Trial counter
    Data <- matrix(0, nrow = N, ncol = k)   # Matrix to store the simulated data
    # Calculate and store a copy of the target correlation matrix
    Intermediate.Corr <- Target.Corr
    # Generate random normal data, initialize factor loadings
    Shared.Comp <- matrix(rnorm(N * N.Factors, 0, 1), nrow = N, ncol = N.Factors)
    Unique.Comp <- matrix(rnorm(N.Pop * dim(sdata)[2], 0, 1), nrow = N.Pop, ncol =  dim(sdata)[2])
    Shared.Load <- matrix(0, nrow = k, ncol = N.Factors)
    Unique.Load <- matrix(0, nrow = k, ncol = 1)
    # Begin loop that ends when specified n of iterations pass without improvement in RMSR correlation
    while (Trials.Without.Improvement < Max.Trials)  {
      Iteration <- Iteration + 1
      # Calculate factor loadings and apply to reproduce desired correlations
      Fact.Anal <- Factor.Analysis(Intermediate.Corr, N.Factors = N.Factors)
      if (N.Factors == 1) { Shared.Load[,1] <- Fact.Anal$loadings }
      else {for (i in 1:N.Factors) { Shared.Load[,i] <- Fact.Anal$loadings[,i] } }
      
      Shared.Load[Shared.Load > 1] <- 1
      Shared.Load[Shared.Load < -1] <- -1
      if (Shared.Load[1,1] < 0) { Shared.Load <- Shared.Load * -1 }
      for (i in 1:k) {
        if (sum(Shared.Load[i,] * Shared.Load[i,]) < 1) { 
          Unique.Load[i,1] <- (1 - sum(Shared.Load[i,] * Shared.Load[i,])) }
        else { Unique.Load[i,1] <- 0 } }
      
      Unique.Load <- sqrt(Unique.Load)
      for (i in 1:k) {Data[,i] <- (Shared.Comp %*% t(Shared.Load))[,i] + Unique.Comp[,i] * Unique.Load[i,1] }
      
      # Replace normal with nonnormal distributions
      for (i in 1:k) {Data <- Data[sort.list(Data[,i]),]; Data[,i] <- Distributions[,i] }
      
      # Calculate RMSR correlation, compare to lowest value, take appropriate action
      Data2 <- as.data.frame(Data)
      
      Reproduced.CorrF <-EstimaCorr(Data2,scalP = scalP,CasesP=CasesP,TCor = TCor,wT=F,CompP=T)
      Reproduced.Corr<-Reproduced.CorrF$v
      
      Residual.Corr <- Target.Corr - Reproduced.Corr
      RMSR <- sqrt(sum(Residual.Corr[lower.tri(Residual.Corr)]*Residual.Corr[lower.tri(Residual.Corr)]) / (.5* (k*k - k)))
      if (RMSR < Best.RMSR)  {
        Best.RMSR <- RMSR
        Best.Corr <- Intermediate.Corr
        Best.Res <- Residual.Corr
        Intermediate.Corr <- Intermediate.Corr + Initial.Multiplier * Residual.Corr
        Trials.Without.Improvement <- 0 }
      else  {
        Trials.Without.Improvement <- Trials.Without.Improvement + 1
        Current.Multiplier <- Initial.Multiplier * .5 ^ Trials.Without.Improvement
        Intermediate.Corr <- Best.Corr + Current.Multiplier * Best.Res 
      }  
    } 
    
    # Construct the data set with the lowest RMSR correlation
    Fact.Anal <- Factor.Analysis(Best.Corr, N.Factors = N.Factors)
    if (N.Factors == 1) { Shared.Load[,1] <- Fact.Anal$loadings }
    else {for (i in 1:N.Factors) {Shared.Load[,i] <- Fact.Anal$loadings[,i] }}
    Shared.Load[Shared.Load > 1] <- 1
    Shared.Load[Shared.Load < -1] <- -1
    if (Shared.Load[1,1] < 0) { Shared.Load <- Shared.Load * -1 }
    for (i in 1:k) {
      if (sum(Shared.Load[i,] * Shared.Load[i,]) < 1)  { 
        Unique.Load[i,1] <- (1 - sum(Shared.Load[i,] * Shared.Load[i,])) }
      else { Unique.Load[i,1] <- 0 } 
    }
    
    Unique.Load <- sqrt(Unique.Load)
    for (i in 1:k) {Data[,i] <- (Shared.Comp %*% t(Shared.Load))[,i] + Unique.Comp[,i] * Unique.Load[i,1] }
    Data <- apply(Data, 2, scale) # standardizes each variable in the matrix
    for (i in 1:k) {Data <- Data[sort.list(Data[,i]),]; Data[,i] <- Distributions[,i] }
    # Return the simulated data set
    return(Data) 
  } # Function GenData
  
  ### Funcition 3/3: Factor.Analysis
  Factor.Analysis <- function(Data, Max.Iter = 50, N.Factors = 0) {
    Data <- as.matrix(Data)
    k <- dim(Data)[2]
    if (N.Factors == 0) {N.Factors <- k; Determine <- T  }
    else { Determine <- F }
    Cor.Matrix <- Data
    Criterion <- .001
    Old.H2 <- rep(99, k)
    H2 <- rep(0, k)
    Change <- 1
    Iter <- 0
    Factor.Loadings <- matrix(nrow = k, ncol = N.Factors)
    while ((Change >= Criterion) & (Iter < Max.Iter)) {
      Iter <- Iter + 1
      Eig <- eigen(Cor.Matrix)
      L <- sqrt(Eig$values[1:N.Factors])
      for (i in 1:N.Factors) {Factor.Loadings[,i] <- Eig$vectors[,i] * L[i] }
      for (i in 1:k) {H2[i] <- sum(Factor.Loadings[i,] * Factor.Loadings[i,]) }
      Change <- max(abs(Old.H2 - H2))
      Old.H2 <- H2
      diag(Cor.Matrix) <- H2
    }
    if (Determine) { N.Factors <- sum(Eig$values > 1) }
    return(list(loadings = Factor.Loadings[,1:N.Factors], factors = N.Factors))
  } # Function Factor.Analysis
  
  TypCorrF <- function(x) {
    Typ<-c("Pearson", "Polychoric (Two Step)", "Polychoric (Max. Lik.)", "Spearman", 
           "Heterogenous (Two Step)", "Heterogenous (Max. Lik.)")
    if (x>length(Typ) || x<0) {stop("Ha sobrepasado el número de Tipos contemplados")}
    Typ[x]
  }
  
  TypCorrDifF <- function(x) {
    Typ<-c("Pearson - Polychoric (Two Step)", "Pearson - Polychoric (Max. Lik.)",
           "Polychoric (Two Step) - Polychoric (Max. Lik.)", "Pearson - Spearman",
           "Polychoric (Two Step) - Spearman", "Polychoric (Max. Lik.) - Spearman")
    if (x>length(Typ) || x<0) {stop("Ha sobrepasado el número de Tipos contemplados")}
    Typ[x]
  }
  
  TypExtrF <- function(x) {
    Typ<-c("pc","mle","fa","minres","wls","gls");
    # Que simboliza:
    # c("Principal Components","Maximum Lielihood","Principal Axis Factor","Minimum Residual",
    #    "Weighted Least Sqared","Generalized Least Squares")
    if (x>length(Typ) || x<0) {stop("Ha sobrepasado el número de Tipos contemplados")}
    Typ[x]
  }
  
  TypExtr2F <- function(x) {
    Typ<-c("pca","ml","pa","minres","wls","gls","ipa","nipa")
    if (x>length(Typ) || x<0) {stop("Ha sobrepasado el número de Tipos contemplados")}
    Typ[x]
  }
  
  TypRotF <- function(x) {
    Typ<-c("none", "varimax", "oblimin","promax")
    if (x>length(Typ) || x<0) {stop("Ha sobrepasado el número de Tipos contemplados")}
    Typ[x]
  }
  
  TypRot2F <- function(x) {
    Typ<-c("none","varimax","quartimax","quartimin","equamax","parsimax","factor.parsimony",
           "biquartimin","covarimin","oblimax","entropy","simplimax","bentlerT",
           "bentlerQ","tandemI","tandemII","geominT","geominQ","infomaxT",
           "infomaxQ","mccammon")
    # c("none","varimax-T","quartimax-T","quartimin (oblimin-Quartimin-Q)","equamax-T","parsimax-T","factor.parsimony-T",
    #  "biquartimin" (Oblimin Biquartimin-Q),"covarimin (Oblimin Covarmin-Q","oblimax-Q","entropy-T","simplimax-Q","bentlerT-T",
    #  "bentlerQ-Q","tandemI-T","tandemII-T","geominT-T","geominQ-Q","infomaxT-T",
    #  "infomaxQ-Q","mccammon-T")
    if (x>length(Typ) || x<0) {stop("Ha sobrepasado el número de Tipos contemplados")}
    Typ[x]
  }
  
  TyEstrInAxisF <- function(x) {
    Typ<-c("component","maxr","ginv","multiple") 
    # Exclusivamente para los tipos de extracción "ipa" ó "nipa"
    # Four different choices of initial communalities are given:
    # components, maximum correlation, generalized inverse and multiple correlation
    if (x>length(Typ) || x<0) {stop("Ha sobrepasado el número de Tipos contemplados")}
    Typ[x]
  }
  
  ResMM<- function(DTb) {
    require(data.table)
    rbind(Media=DTb[,mean(communality)],
          vapply(DTb, fivenum, c(Min.=0, Q1=0,Md=0,Q3=0,Max=0)),
          "Menor0.7"=nrow(DTb[communality<0.7]),
          "%Menor0.7"=100*(nrow(DTb[communality<0.7])/nrow(DTb)),
          "Menor0.5"=nrow(DTb[communality<0.5]),
          "%Menor0.5"=100*(nrow(DTb[communality<0.5])/nrow(DTb)))
  }
  
  Sort1<- function(ad,Sorting=F,suprime=0) {
    if (!Sorting) {
      load<-ad$loadings
      supre <- load
      for (j in 1:pr) {for (i in 1:nrow(load)) {if (abs(load[i,j]) < suprime) { supre[i,j] <- NA }}}
    }
    if (Sorting) {
      ad <- fa.sort(ad)
      load2 <- ad$loadings
      supre <- load2
      for (j in 1:pr) {for (i in 1:nrow(load2)) {if (abs(load2[i,j]) < suprime) { supre[i,j] <- NA } } }
    }
    colnames(supre)<-paste ("F",1:ncol(supre), sep="")
    return(supre)
  }
  
  Sort2<- function(ad2,Sorting=F,suprime=0) {
    ResFinal<-list()
    if (Sorting) {ad2 <- fa.sort(ad2)}
    load <- ad2$loadings	
    supre <- load;
    colnames(supre)<-paste ("F",1:ncol(load), sep="")
    for (j in 1:ncol(load)) {for (i in 1:nrow(load)) {if (abs(load[i,j]) < suprime) { supre[i,j] <- NA } } }
    ResFinal$Patt.Mat <-supre
    ResFinal$Str.Mat<-NA
    if (!ad2$orthogonal) {    	
      p <- as.matrix(load[,])%*%ad2$Phi
      for (j in 1:ncol(load)) {for (i in 1:nrow(p)) {if (abs(p[i,j]) < suprime) { p[i,j] <- NA } } }
      ResFinal$Str.Mat<-p
    }
    return(ResFinal)
  }
  
  VarExplan1<-function(Eigenvalues) {
    n<-length(Eigenvalues)
    sume  <- 0
    sum2 <- rep(0,n)
    for (i in 1:n) { sume <- sume  + Eigenvalues[i];sum2[i] = sum2[i] + sume }
    junto <-round(data.frame(Eigenvalues,Eigenvalues/sume*100,sum2/sume*100),5)
    names(junto) <- c("Eigenvalues","% of Variance","Cumulative %")
    return(junto)
  }
  
  VarExplan2<-function(load,n,sume) {
    pr=ncol(load)
    pro <- rep(0,pr)
    for (j in 1:pr) { sum3 <- 0; for (i in 1:n) { sum3<- sum3 + load[i,j]^2 }; pro[j] <- sum3 }
    sum3 <- 0
    sum2 <- rep(0,pr)
    for (i in 1:pr) { sum3<- sum3 + pro[i];sum2[i] = sum2[i] + sum3 }
    junto <- round(data.frame(pro,pro/sume*100,sum2/sume*100),5)
    names(junto) <- c("Sums of Squared Loadings","% of Variance","Cumulative %")
    return(junto)
  }
  
  VarExplan2Prv<-function(Eigenvalues,load) {
    pr=ncol(load)
    n<-length(Eigenvalues)
    sume  <- 0
    sum2 <- rep(0,n)
    for (i in 1:n) { sume <- sume  + Eigenvalues[i];sum2[i] = sum2[i] + sume }
    pro <- rep(0,pr)
    for (j in 1:pr) { sum3 <- 0; for (i in 1:n) { sum3<- sum3 + load[i,j]^2 }; pro[j] <- sum3 }
    sum3 <- 0
    sum2 <- rep(0,pr)
    for (i in 1:pr) { sum3<- sum3 + pro[i];sum2[i] = sum2[i] + sum3 }
    junto <- round(data.frame(pro,pro/sume*100,sum2/sume*100),5)
    names(junto) <- c("Sums of Squared Loadings","% of Variance","Cumulative %")
    return(junto)
  }
  
  # Para las Oblicuas
  VarExplan3<-function(p,n) {
    pr=ncol(load)
    pro <- rep(0,pr)
    for (j in 1:pr) { sum3 <- 0; for (i in 1:n) { sum3<- sum3 + p[i,j]^2 }; pro[j] <- sum3 }
    junto <- round(data.frame(pro),5)
    names(junto) <- c("Sums of Squared Loadings")
    return(junto)
  }
  
  CleanCorr<-function(x) {
    lower<-round(x,4)
    lower[lower.tri(x, diag=TRUE)]<-""
    lower[abs(x)<.4]<-""
    lower<-as.data.frame(lower)
    return(lower)
  }
  
  GrpRotMM<-function(Cargas,uFCut2=0.25,uSimMax=0.3,uSimMin=0.1,Simpl=F) {
    loadings.my<-melt(Cargas);colnames(loadings.my)<-c("Item","Factor","Loading")
    load.DT<-data.table(loadings.my); load.DT.Calcs<-data.table(loadings.my);
    load.DT[,Loading2:=Loading]
    load.DT[abs(Loading)<uFCut2,Loading2:=NA]
    title="Factor Diagram (rotated solution)"
    if (Simpl) {load.DT[abs(Loading)<uFCut,Loading:=NA];title=paste0(title," Simplificado")}
    gP<-ggplot(load.DT, aes(Item, abs(Loading), fill=Loading,label =fzero(round(Loading2,2)))) + 
      facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
      geom_bar(stat="identity") + #make the bars
      coord_flip() + #flip the axes so the test names can be horizontal  
      #define the fill color gradient: blue=positive, red=negative
      scale_fill_gradient2(name = "Loading", 
                           high = "blue", mid = "white", low = "red", 
                           midpoint=0, guide=F) +
      ylab("Loading Strength") +  ggtitle(title) + #improve y-axis label
      theme_bw(base_size=10) + #use a black-and0white theme with set font size
      geom_text(size = 2, position = position_stack(vjust = 0.5),check_overlap = T, colour= "black")
    if (!Simpl) {
      gP<-gP + geom_hline(yintercept = uFCut, linetype="dashed", color = "orange", size=.5) +
        geom_hline(yintercept = uSimMax, linetype="dashed", color = "orange", size=.5) +
        geom_hline(yintercept = uSimMin, linetype="dashed", color = "blue", size=.5)
    }
    return(gP)
  }
  
  ExtrFA<-function(co,ExtrMet2="pca",NFExtr=2,ncase,ncase_list,InitExtrAx="component") {
    #Factor Analysis (functions "principal", "fa", "iterativePrincipalAxis", "PrincipalAxis")
    pr <- NFExtr; n<-nrow(co)
    
    if (ExtrMet2=="pca" && pr > n) { pr <- n }
    if (ExtrMet2!="pca" && pr > (n-1)) { pr <- n-1 }
    if (ExtrMet2=="pca") {ad<-principal(co, nfactors = pr, residuals = T, rotate="none", n.obs=ncase, scores=F) }
    if (ExtrMet2=="ipa") {
      ad<-principal(co, nfactors = pr, rotate="none", n.obs=ncase, scores=F)
      ad3 <- iterativePrincipalAxis(co, nFactors=pr, communalities=InitExtrAx, iterations=200, tolerance=1e-6)
      ad$loadings <- ad3$loadings
      sum2 <- rep(0,n)
      for(i in 1:pr) { sum2 <-  ad$loadings[,i]^2 + sum2 }
      ad$communality <- sum2
    }
    if (ExtrMet2=="nipa") {
      ad<-principal(co, nfactors = pr, rotate="none", n.obs=ncase, scores=F)
      ad3 <- principalAxis(co, nFactors=pr, communalities=InitExtrAx) 
      ad$loadings <- ad3$loadings
      sum2 <- rep(0,n)
      for(i in 1:pr) { sum2 <-  ad$loadings[,i]^2 + sum2 }
      ad$communality <- sum2
    }
    if (!(ExtrMet2 %in% c("pca","ipa","nipa"))) {
      ad <- fa(co, nfactors=pr, residuals = T, rotate = "none", n.obs=ncase_list, SMC=T, 
               min.err = 1e-6, digits =8, max.iter=200, symmetric=T, warnings=T, fm=ExtrMet2)
      ad$loadings <- ad$loadings[,order(colnames(ad$loadings))]
    }
    ad$loadings <- as.matrix(ad$loadings)
    row.names(ad$loadings) <- rownames(co)
    #load <- ad$loadings
    if (ExtrMet2=="ipa" || ExtrMet2=="nipa") {
      sum2 <- rep(0,n)
      for(i in 1:pr) { sum2 <- ad$loadings[,i]^2 + sum2 }
      ad$communality <- sum2
    }
    return(ad)
  } #Extr
  
  RotatFA <- function(RotPas=2,load,KaissNorm=T,kSimpliMax=0,PromaxMake=F,KppPromax=4) {
    TypRot2 <- c("none","varimax","quartimax","quartimin","equamax","parsimax","factor.parsimony",
                 "biquartimin","covarimin","oblimax","entropy","simplimax","bentlerT",
                 "bentlerQ","tandemI","tandemII","geominT","geominQ","infomaxT",
                 "infomaxQ","mccammon")
    
    pr=ncol(load)
    Rott2=TypRot2[RotPas]
    if (Rott2==TypRot2[2]) {ad2 <- Varimax(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) } 
    if (Rott2==TypRot2[3]) {ad2 <- quartimax(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[4]) {ad2 <- quartimin(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[5]) {ad2 <- cfT(load, Tmat=diag(ncol(load)), kappa=ncol(load)/(2*nrow(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[6]) {ad2 <- cfT(load,Tmat=diag(ncol(load)), kappa=(ncol(load)-1)/(nrow(load)+ncol(load)-2), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[7]) {ad2 <- cfT(load, Tmat=diag(ncol(load)), kappa=1, normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[8]) {ad2 <- oblimin(load, Tmat=diag(ncol(load)), gam=0.5, normalize=KaissNorm, eps=1e-5, maxit=500)}
    if (Rott2==TypRot2[9]) {
      compru<-try(ad2 <- oblimin(load, Tmat=diag(ncol(load)), gam=1, normalize=KaissNorm, eps=1e-5, maxit=500),silent=T)
      if ('try-error' %in% class(compru)) {ad2 <- oblimin(load, Tmat=diag(ncol(load)), gam=0.9, normalize=KaissNorm, eps=1e-5, maxit=500)}
    }
    if (Rott2==TypRot2[10]) {ad2 <- oblimax(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[11]) {ad2 <- entropy(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[12]) {
      if (kSimpliMax==0) { kSimpliMax <- ncol(load)*nrow(load) - nrow(load); kk <- kSimpliMax }
      ad2 <- simplimax(load, Tmat=diag(ncol(load)), k=kSimpliMax, normalize=KaissNorm, eps=1e-5, maxit=500)}
    if (Rott2==TypRot2[13]) {ad2 <- bentlerT(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500)}
    if (Rott2==TypRot2[14]) {ad2 <- bentlerQ(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[15]) {ad2 <- tandemI(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[16]) {ad2 <- tandemII(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[17]) {ad2 <- geominT(load, Tmat=diag(ncol(load)), delta=0.01, normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[18]) {ad2 <- geominQ(load, Tmat=diag(ncol(load)), delta=0.01, normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[19]) {ad2 <- infomaxT(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500)}
    if (Rott2==TypRot2[20]) {ad2 <- infomaxQ(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
    if (Rott2==TypRot2[21]) {ad2 <- mccammon(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) } 
    
    if (Rott2!="none") {  promai <- ad2$orthogonal } else { promai <- FALSE }
    if ((promai) || (Rott2=="none")) {
      if (PromaxMake) { 
        m <- KppPromax
        if (ncol(load) < 2) { return(load)}
        dn <- dimnames(load)
        if (Rott2!="none") { load <- ad2$loadings }
        Q <- load * abs(load)^(m - 1)
        U <- lm.fit(load, Q)$coefficients
        d <- diag(solve(t(U) %*% U))
        U <- U %*% diag(sqrt(d))
        dimnames(U) <- NULL
        z <- load %*% U
        if (Rott2!="none") {U <- ad2$Th %*% U}
        ui <- solve(U)
        Phi <- ui %*% t(ui)
        dimnames(z) <- dn
        ad2<- list(loadings = z, Th = U%*%Phi, Phi = Phi, orthogonal = FALSE, method ="promax", initial=Rott2) 
      } 
    }
    return(ad2)
  } # Rott
  
  NormMult<-function(mdata) {
    # "Test of Multivariate Normality based on Skewness and Kurtosis";
    # Yo he añadido los tests del paquete MVN {Mardia, Henze-Zirkler, & Royston}
    require(MVN)
    mn <- mvnorm.skew.test(mdata, na.action = na.omit)
    mn2 <- mvnorm.kur.test(mdata, method = "satterthwaite", n.simu = 1000, na.action=na.omit)
    mardia<-mardiaTest(mdata)
    hZ <- hzTest(mdata, qqplot=FALSE)
    Roys<-roystonTest(mdata)
    mm <- c("Skewness","Kurtosis","Mardia.Skew","Mardia.Kurtosis", "Mardia.SkewSmall","Henze.Zirkler","Royston")
    mn3 <- round(c(mn$statistic, mn2$statistic, mardia@chi.skew,mardia@z.kurtosis,mardia@chi.small.skew,hZ@HZ,Roys@H),2)
    mn4 <- c(mn$p.value, mn2$p.value,mardia@p.value.skew,mardia@p.value.kurt,mardia@p.value.small,hZ@p.value,Roys@p.value)
    ResF <- data.frame(Moment=mm, Test.Statistic=mn3, p.value=round(mn4,5))
    return(ResF)
  }
  
  KMO_MM <-function(ad,co,n,ncase,uSimMin) {
    require(data.table)
    # KMO
    Res<-list()
    nam=row.names(co)
    NFExtr=ncol(ad$loadings[,])
    g <- diag(0,n)
    par <- cor2pcor(co)
    cco <- co
    cco[upper.tri(cco, TRUE)] <- g[upper.tri(cco, TRUE)]	
    par[upper.tri(par, TRUE)] <- g[upper.tri(par, TRUE)]
    cco <- sum(cco^2)
    kmo <- cco/(sum(par^2)+cco)
    Res$KMO.RFactor <-c(kmo)
    Res$KMO.ROrig <- KMO(co)[[1]]
    # MSA
    ms <- rep(0,n)
    for (i in 1:n) { ms[i] <- ( sum(co[i,]^2)-1 )  / (sum(co[i,]^2)+sum(par[i,]^2)-2) }
    Res$MSA.RFactor <- data.frame(c(ms), row.names=row.names(co))
    Res$MSA.R_AntiImg <- KMO(co)[[2]]
    # GFI and AGFI
    s <- ad$residual
    som <- sum(diag(s%*%s))
    som2 <- sum(diag(co%*%co))
    Res$GFI_ULS <- c(1-som/som2)
    aus <- matrix(0,n,n)
    for (i in 1:(n-1)) { for (j in (i+1):n) {aus[i,j] <- sum(ConFac[i,]*ConFac[j,]) } }
    aus2 <- t(aus)
    diag(aus2) <- 1
    aus <- aus + aus2
    som <- sum(diag((solve(aus)%*%co-diag(n))%*%(solve(aus)%*%co-diag(n))))
    som2 <- sum(diag(solve(aus)%*%co%*%solve(aus)%*%co))
    som <- 1-som/som2
    Res$GFI_ML <- c(som)
    # RMSR (Root Mean Square Residual)
    g <- diag(0,n)
    s <- ad$residual
    s[upper.tri(s, TRUE)] <- g[upper.tri(s, TRUE)]
    rmsr <- sqrt(2*sum(s^2)/(n*(n-1)))
    Res$RMSR <- c(rmsr)
    # Root Mean Square Partials Correlations Controlling Factors
    cc <- co-(ConFac%*%t(ConFac))
    ca <- sqrt(diag(cc))
    d <- diag(1/ca)
    ea <- d%*%cc%*%d   #partial correlations controlling factors
    rmsp <- sqrt((sum(ea^2)-n)/(n*(n-1)))
    Res$RMSP <- c(rmsp)
    # Partial Correlations controlling all other variables
    par <- data.frame(par=cor2pcor(co));
    row.names(par) <- nam
    names(par) <- nam
    # Partial Correlations controlling Factors
    cc <- co-(ConFac%*%t(ConFac))
    ca <- sqrt(diag(cc))
    d <- diag(1/ca)
    ea <- d%*%cc%*%d   #partial correlations controlling factors
    row.names(ea) <- nam
    ea <- data.frame(ea)
    names(ea) <- nam
    # Residual correlations
    cont <- 0
    s <- ad$residual
    for (i in 2:n) { for (j in 1:(i-1)) { if (abs(s[i,j])> 0.05) { cont <- cont+1 } } }
    p <- 2*cont/(n*(n-1))*100;
    Res$Resid<-data.frame("Residuals>0.05"=cont,"%Residuals>0.05"=p)
    # Determinant
    Res$Determ <- round(c(Determinant=determinant(co, logarithm = FALSE)$modulus),5)[[1]]
    # Tests to correlation matrix
    Res$Cor.Bartlett <- cortest.bartlett(co, n = ncase)
    Res$Cor.Steiger <- cortest.normal(co,n1=ncase, fisher = FALSE)
    Res$Cor.Jennrich <- cortest.jennrich(co, diag(rep(1,n)), n1 = ncase, n2=ncase)
    # Mis cálculos propios
    Res$CompleHoff<-mean(ad$complexity)
    loadings.my<-melt(ad$loadings[,]);colnames(loadings.my)<-c("Item","Factor","Loading")
    load.DT.Calcs<-data.table(loadings.my);
    NumerPorc=load.DT.Calcs[,.N/nrow(load.DT.Calcs),abs(Loading)<uSimMin][abs==T,V1]
    Denom=(NFExtr-1)/NFExtr
    #Hyperplane=Numer/Denom
    HyperplaneCatt=NumerPorc/Denom
    if (length(HyperplaneCatt)==0) {HyperplaneCatt=NA}
    Res$HyperplaneCatt<-HyperplaneCatt
    return(Res)
  }
  
  paf_MM<-function (object, rP,eigcrit = 1, convcrit = 0.001) {
    if (is.null(row.names(t(object)))) {
      print("Dataset must be coerced into matrix from prior data frame.")
    }
    else {
      if (!is.numeric(object)) {
        print("Your dataset is not a numeric object.")
      }
      else {
        object <- (na.omit(object))
        yy <- sort(abs(as.numeric(var(object))))
        yy <- yy[1]
        yy <- ifelse(yy == 0, NA, yy)
        if (is.na(yy)) {
          print("One of your variables is a constant. Constants are disallowed as part of a scale.")
        }
        else {
          if (dim(cbind(object))[2] < 2) {
            print("Your set contains only one variable. At least two are required.")
          }
          else {
            if (dim(cbind(object))[1] < 2) {
              print("Your set contains only one case. You need at least two cases.")
            }
            else {
              same <- function(x) {
                for (i in 1:ncol(x)) {
                  for (j in 1:ncol(x)) {
                    if (i != j) {
                      test <- x[, i] - x[, j]
                      if (sum(abs(test)) == 0) {
                        print(paste("WARNING: Items ", 
                                    i, " (", colnames(x)[i], ") ", 
                                    "and ", j, " (", colnames(x)[j], 
                                    ") ", "are duplicates."))
                      }
                    }
                  }
                }
              }
              same(object)
              x <- na.omit(object)
              x <- rP
              N <- nrow(na.omit(object))
              options(digits = 5)
              x <- as.matrix(x)
              x1 <- x
              rownames(x1) = colnames(x)
              bt <- -((N - 1) - ((2 * ncol(x) + 5)/6)) * 
                log(det(x))
              invx <- solve(x)
              ssqr <- matrix(0, nrow(x), ncol(x))
              diag(ssqr) <- diag(invx)
              ssqr <- solve(ssqr)
              Q <- ssqr %*% invx %*% ssqr
              colnames(Q) <- colnames(x)
              rownames(Q) <- colnames(x)
              qdi <- diag(diag(Q))
              sqdi <- solve(qdi)
              ssqdi <- sqrt(sqdi)
              Qr <- ssqdi %*% Q %*% ssqdi
              colnames(Qr) <- colnames(x)
              rownames(Qr) <- colnames(x)
              xnod <- x
              diag(xnod) <- 0
              Qrnod <- Qr
              diag(Qrnod) <- 0
              KMO <- sum(xnod^2)/(sum(xnod^2) + sum(Qrnod^2))
              MSA <- 0
              for (i in 1:nrow(xnod)) (MSA <- c(MSA, sum(xnod[i, 
              ]^2)/(sum(xnod[i, ]^2) + sum(Qrnod[i, ]^2))))
              MSA <- matrix(MSA[-1], , 1)
              rownames(MSA) <- colnames(x)
              colnames(MSA) <- "MSA"
              comm0 <- 1 - diag(Q)
              comm1 <- comm0
              allcomm <- 0
              diffscores <- 0
              iter <- 0
              count <- 0
              eigenval <- 0
              x0 <- x
              repeat {
                allcomm <- cbind(allcomm, comm1)
                eigs <- 0
                diag(x) <- comm1
                for (i in 1:length(eigen(x)$values)) if (eigen(x0)$values[i] > 
                                                         eigcrit) {
                  eigs <- c(eigs, eigen(x)$values[i])
                }
                eigs <- eigs[-1]
                eigenval <- cbind(eigenval, eigen(x)$values)
                eigmat <- sqrt(diag(eigs, length(eigs), 
                                    length(eigs)))
                eigvecr <- matrix(eigen(x)$vector[, 0:length(eigs)], 
                                  , length(eigs))
                one <- c((1:ncol(eigvecr))/(1:ncol(eigvecr)))
                factload <- eigvecr %*% eigmat
                comm2 <- t(rowsum(t(factload^2), one))
                dif <- abs(comm1 - comm2)
                iter <- iter + 1
                count <- c(count, iter)
                diffscores <- cbind(diffscores, dif)
                comm1 <- comm2
                endtest <- matrix(1, nrow(dif), 1)
                for (i in 1:nrow(dif)) if (dif[i, 1] < 
                                           convcrit) {
                  endtest[i, 1] = NA
                }
                if (length(na.omit(endtest)) == 0) 
                  break
              }
              firstlast <- cbind(comm0, comm1)
              colnames(firstlast) = c("Initial Communalities", 
                                      "Final Extraction")
              allcomm <- cbind(allcomm, comm1)
              allcomm <- allcomm[, -1]
              colnames(allcomm) = count
              diffscores <- diffscores[, -1]
              colnames(diffscores) = c(1:iter)
              eigenval <- cbind(eigen(x0)$values, eigenval[, 
                                                           -1])
              colnames(eigenval) = c(0:iter)
              rownames(factload) <- colnames(x)
              facttest <- factload[, 1]
              for (i in 1:length(facttest)) if (facttest[i] < 
                                                0) {
                facttest[i] <- NA
              }
              if (length(na.omit(facttest)) == 0) 
                (factload[, 1] <- -factload[, 1])
              correp <- factload %*% t(factload)
              residuals <- x - correp
              colnames(correp) <- colnames(x)
              rownames(correp) <- colnames(x)
              colnames(residuals) <- colnames(x)
              rownames(residuals) <- colnames(x)
              diag(correp) <- 1
              diag(residuals) <- 0
              RMS <- sqrt(sum(residuals^2)/((nrow(x)^2) - 
                                              (nrow(x))))
              output <- list("PRINCIPAL AXIS FACTORING", 
                             Correlation = x1, Anti.Image.Cov = Q, Anti.Image.Cor = Qr, 
                             KMO = KMO, MSA = MSA, Bartlett = bt, Communalities = firstlast, 
                             Iterations = iter, Eigenvalues = eigenval, 
                             Communality.Iterations = allcomm, Criterion.Differences = diffscores, 
                             Factor.Loadings = factload, Reproduced.Cor = correp, 
                             Residuals = residuals, RMS = RMS)
              ob <- as.character(match.call()[2])
              cl <- call("paf", object = ob, eigcrit = eigcrit, 
                         convcrit = convcrit)
              output$call <- cl
              output$items <- names(output)
              class(output) <- c("paf", class(output))
              output
            }
          }
        }
      }
    }
  }
  
  paf_MM2 <-function(rP,N,eigcrit = 1, convcrit = 0.001) {
    options(digits = 5)
    x <- as.matrix(rP)
    x1 <- x
    rownames(x1) = colnames(x)
    bt <- -((N - 1) - ((2 * ncol(x) + 5)/6)) * log(det(x))
    invx <- solve(x)
    ssqr <- matrix(0, nrow(x), ncol(x))
    diag(ssqr) <- diag(invx)
    ssqr <- solve(ssqr)
    Q <- ssqr %*% invx %*% ssqr
    colnames(Q) <- colnames(x)
    rownames(Q) <- colnames(x)
    qdi <- diag(diag(Q))
    sqdi <- solve(qdi)
    ssqdi <- sqrt(sqdi)
    Qr <- ssqdi %*% Q %*% ssqdi
    colnames(Qr) <- colnames(x)
    rownames(Qr) <- colnames(x)
    xnod <- x
    diag(xnod) <- 0
    Qrnod <- Qr
    diag(Qrnod) <- 0
    KMO <- sum(xnod^2)/(sum(xnod^2) + sum(Qrnod^2))
    MSA <- 0
    for (i in 1:nrow(xnod)) (MSA <- c(MSA, sum(xnod[i,]^2)/(sum(xnod[i, ]^2) + sum(Qrnod[i, ]^2))))
    MSA <- matrix(MSA[-1], , 1)
    rownames(MSA) <- colnames(x)
    colnames(MSA) <- "MSA"
    comm0 <- 1 - diag(Q)
    comm1 <- comm0
    allcomm <- 0
    diffscores <- 0
    iter <- 0
    count <- 0
    eigenval <- 0
    x0 <- x
    repeat {
      allcomm <- cbind(allcomm, comm1)
      eigs <- 0
      diag(x) <- comm1
      for (i in 1:length(eigen(x)$values)) if (eigen(x0)$values[i] > eigcrit) {eigs <- c(eigs, eigen(x)$values[i])}
      eigs <- eigs[-1]
      eigenval <- cbind(eigenval, eigen(x)$values)
      eigmat <- sqrt(diag(eigs, length(eigs), length(eigs)))
      eigvecr <- matrix(eigen(x)$vector[, 0:length(eigs)], , length(eigs))
      one <- c((1:ncol(eigvecr))/(1:ncol(eigvecr)))
      factload <- eigvecr %*% eigmat
      comm2 <- t(rowsum(t(factload^2), one))
      dif <- abs(comm1 - comm2)
      iter <- iter + 1
      count <- c(count, iter)
      diffscores <- cbind(diffscores, dif)
      comm1 <- comm2
      endtest <- matrix(1, nrow(dif), 1)
      for (i in 1:nrow(dif)) if (dif[i, 1] < convcrit) { endtest[i, 1] = NA }
      if (length(na.omit(endtest)) == 0) 
        break
    }
    firstlast <- cbind(comm0, comm1)
    colnames(firstlast) = c("Initial Communalities", "Final Extraction")
    allcomm <- cbind(allcomm, comm1)
    allcomm <- allcomm[, -1]
    colnames(allcomm) = count
    diffscores <- diffscores[, -1]
    colnames(diffscores) = c(1:iter)
    eigenval <- cbind(eigen(x0)$values, eigenval[, -1])
    colnames(eigenval) = c(0:iter)
    rownames(factload) <- colnames(x)
    facttest <- factload[, 1]
    for (i in 1:length(facttest)) if (facttest[i] < 0) {facttest[i] <- NA }
    if (length(na.omit(facttest)) == 0) {(factload[, 1] <- -factload[, 1])}
    correp <- factload %*% t(factload)
    residuals <- x - correp
    colnames(correp) <- colnames(x)
    rownames(correp) <- colnames(x)
    colnames(residuals) <- colnames(x)
    rownames(residuals) <- colnames(x)
    diag(correp) <- 1
    diag(residuals) <- 0
    RMS <- sqrt(sum(residuals^2)/((nrow(x)^2) - (nrow(x))))
    output <- list("PRINCIPAL AXIS FACTORING", 
                   Correlation = x1, Anti.Image.Cov = Q, Anti.Image.Cor = Qr, 
                   KMO = KMO, MSA = MSA, Bartlett = bt, Communalities = firstlast, 
                   Iterations = iter, Eigenvalues = eigenval, 
                   Communality.Iterations = allcomm, Criterion.Differences = diffscores, 
                   Factor.Loadings = factload, Reproduced.Cor = correp, 
                   Residuals = residuals, RMS = RMS)
    ob <- as.character(match.call()[2])
    cl <- call("paf", object = ob, eigcrit = eigcrit, convcrit = convcrit)
    output$call <- cl
    output$items <- names(output)
    class(output) <- c("paf", class(output))
    output
  }
  
  ElimVar<-function(DataP=Data,numVar=1,OrdinP=Ordin,NumberP=Number) {
    #Servirá para eleiminar más items y actualizr todo adecuadamente.
    # Basta con incluir lineas adicionales de eliminX y anexarlas a Elimna
    ResIn<-list()
    Etiq<-names(DataP)
    Categ<-NA
    
    OrdinE<-Etiq[OrdinP]
    NumberE<-Etiq[NumberP]
    Elimin<-Etiq[numVar]
    InxElimin<-match(Elimin, Etiq)
    InxEliminO<-c(na.omit(match(Elimin,OrdinE)))
    InxEliminN<- c(na.omit(match(Elimin,NumberE)))
    
    if (length(InxEliminO)>0) {OrdinO2<-OrdinP[-InxEliminO]} else {OrdinO2<-OrdinP}
    if (length(InxEliminN)>0) {NumberO2<-NumberP[-InxEliminN]} else {NumberO2<-NumberP}
    
    OrdinE2<-Etiq[OrdinO2]
    NumberE2<-Etiq[NumberO2]
    Data2<-Data[-InxElimin]
    Etiq2<-names(Data2)
    Ordin<-match(OrdinE2,Etiq2)
    Number<-match(NumberE2,Etiq2)
    ResIn$Data<-na.omit(Data2)
    ResIn$Ordin<-Ordin
    ResIn$Number<-Number
    scal<-ResIn$Data
    
    ncase <- nrow(mdata)
    if (!anyNA(Ordin)) {for (i in Ordin) {scal[,i]<-ordered(ResIn$Data[,i])}}
    if (!anyNA(Number)) {for (i in Number) {scal[,i]<-as.numeric(ResIn$Data[,i])}}
    if (!anyNA(Categ)) {for (i in Categ) { scal[,i]<-as.factor(ResIn$Data[,i])}}
    ResIn$scal<-scal
    return(ResIn)
  }
  
  # Añadidas en marzo de 2019
  EstimaFiab<-function(DTPas,NmFileGrp) {
    require(data.table)
    require(psych)
    
    RslFiab<-list()
    DTOmit2<-data.table(na.omit(DTPas))
    RslFiab$Alpha<-psych::alpha(DTOmit2)
    # Con Omega (variante de Alfa)
    RslFiab$Omega<-psych::omega(DTOmit2,nfactors=Nfac)
    # Y con el Ind Discriminac clásico basado en la biserial-puntual
    RslFiab$Clas<-item.exam(DTOmit2, discrim=TRUE)
    RslFiab$Mensa<-c("Análisis Alpha de Fiabilidad: \n",
                     "Análisis Omega de Fiabilidad: \n",
                     "Análisis Ind Discriminac clásico biserial-puntual de Fiabilidad: \n",
                     "Análisis Factoral por defecto y Oblimin: \n",
                     "Análisis Factoral por defecto y Oblimin Varianza Explicada: \n",
                     "Análisis Factoral minrank-Oblimin: \n",
                     "Análisis Factoral minrank-Oblimin Varianza Explicada: \n",
                     "Análisis Alternativo basado en Clusters: \n")
    RslFiab$fa1<-psych::fa(DTOmit2,Nfac,rotate="oblimin")
    RslFiab$fa2<-psych::fa(fm="minrank",DTOmit2,Nfac,rotate="oblimin")
    RslFiab$iClus<-psych::iclust(DTOmit2)
    
    pdf(NmFileGrp,width=8.27,height=11.69)
    psych::omega.diagram(RslFiab$Omega,cex=5)
    psych::fa.diagram(RslFiab$fa1,rsize=.5)
    psych::fa.diagram(RslFiab$fa2,rsize=.5)
    psych::iclust.diagram(RslFiab$iClus,cex=.4)
    dev.off()
    RslFiab
  }
  
  ProcItAlt<-function(Tk=1,Tasks,LosNm,DTNm,EscalaL=c("1 Nada","2 Algo","3 Medio","4 Bastante","5 Totalmente"),EstrFac) {
    options(warn=-1)
    ResIt<-list()
    NumL<-length(EscalaL)
    TkO<-grep(Tasks[Tk],LosNm,value=F)
    TkO2<-grep(Tasks[Tk],DTNm$Item,value=F)
    stre<-LosNm[last(TkO)]
    PattID<-c(strsplit(stre,"_"))[[1]]
    try(Compru<-as.numeric(PattID[length(PattID)]))
    if (is.na(as.numeric(PattID[length(PattID)-1]))) {
      NumIt<-as.numeric(PattID[length(PattID)])
      NumAlt<-1
    } else {
      NumAlt<-which(last(PattID) == letters)
      if (identical(NumAlt, integer(0))) {NumAlt<-as.numeric(PattID[length(PattID)])}
      NumIt<-as.numeric(PattID[length(PattID)-1])
    }
    #Conservar para escala de MSPSS precisamente
    #if (Tk==2) {NumIt<-which(last(PattID) == letters);NumAlt=1;NumL=2}
    ResIt$Task<-Tasks[[Tk]]
    ResIt$NumI<-NumIt
    ResIt$NumOpc<-NumAlt
    ResIt$NLik<-NumL
    ResIt$EscalaL<-EscalaL
    ResIt$TkO<-TkO
    ResIt$TkO2<-TkO2
    ResIt$SQ<-seq(from=0, to=(NumIt*NumAlt),by=NumAlt)
    ResIt$nm<-names(DT[,TkO,with=F])
    ResIt$nm2<-as.vector(DTNm[TkO2,2][[1]])
    ResIt$lbl<-paste0("Tarea ",Tk,"_",as.vector(DTNm[TkO2[1],3][[1]]),"_Factor ",as.vector(DTNm[TkO2[1],4][[1]]))
    ResIt$NFac<-length(EstrFac)
    if (ResIt$NFac>0) ResIt$Factor<-EstrFac
    options(warn=0)
    
    ResIt
  }
  
  ProcPrev<-function(Tk=1,ResI,DT,Acum=FALSE) {
    #require(parallel)
    #Expert<-DT[Grupo==1]
    #Lego<-DT[Grupo==2]
    
    #DT1<-DT[,ResI$TkO,with=F]
    ExpDT<-DT[Sexo=="Hombre"][,ResI$TkO,with=F]
    LegoDT<-DT[Sexo=="Mujer"][,ResI$TkO,with=F]
    #if (Tk==12) {ExpDT<-ExpDT+1;LegoDT<-LegoDT+1}
    lE<-length(ExpDT)
    lL<-length(LegoDT)
    ResExp2<-ResExp<-matrix(NA,nrow =lE,ncol =ResI$NLik)
    ResLego2<-ResLego<-matrix(NA,nrow =lL,ncol =ResI$NLik)
    
    for (i in c(1:lE))  {
      RR<-ExpDT[,.N,by=eval(ResI$nm[i])]
      RR<-na.omit(RR)
      setkeyv(RR,eval(ResI$nm[i]))
      cont<-0
      for (j in c(RR[,1][[1]])) {
        cont<-cont+1
        ResExp[i,j]<-RR$N[cont]
        ResExp2[i,j]<-RR$N[cont]/sum(RR$N)
      }
      for (j in 1:ResI$NLik) {
        if (is.na(ResExp[i,j])) ResExp[i,j]<-0
        if (is.na(ResExp2[i,j])) ResExp2[i,j]<-0
      }
    }
    
    for (i in c(1:lL))  {
      RR<-LegoDT[,.N,by=eval(ResI$nm[i])]
      setkeyv(RR,eval(ResI$nm[i]))
      cont<-0
      for (j in c(RR[,1][[1]])) {
        cont<-cont+1
        ResLego[i,j]<-RR$N[cont]
        ResLego2[i,j]<-RR$N[cont]/sum(RR$N)
      }
      for (j in 1:ResI$NLik) {
        if (is.na(ResLego[i,j])) ResLego[i,j]<-0
        if (is.na(ResLego2[i,j])) ResLego2[i,j]<-0
      }
    }
    
    if (Acum) {
      DTF1<-data.table(Question=ResI$nm2,ResLego2,Subtable="Mujer")
      DTF2<-data.table(Question=ResI$nm2,ResExp2,Subtable="Hombre")
    }
    if (!Acum){
      DTF1<-data.table(Question=ResI$nm2,ResLego,Subtable="Mujer")
      DTF2<-data.table(Question=ResI$nm2,ResExp,Subtable="Hombre")
    }
    DTF<-rbind(DTF2,DTF1)
    names(DTF)<-c("Question",ResI$EscalaL,"Subtable")
    DTF
  }
  
  ProcGrp<-function(DTFa,DTF,lbl,wGrp=FALSE,Path) {
    #Ordenado
    LikCou <- ## 8in x 9in  ## ProfChal.pdf
      HH::likert(Question ~ . | Subtable, DTF, 
                 as.percent=TRUE,    ## implies display Row Count Totals
                 ylab=NULL,
                 main=list(lbl, x=unit(.65, "npc")),
                 strip.left=strip.custom(bg="gray85"),
                 strip=FALSE,
                 positive.order=TRUE
      )
    print(LikCou)
    
    #Sin Ordenar
    LikCou2 <- ## 8in x 9in  ## ProfChal.pdf
      HH::likert(Question ~ . | Subtable, DTF,
                 as.percent=TRUE,    ## implies display Row Count Totals
                 ylab=NULL,
                 main=list(lbl, x=unit(.65, "npc")),
                 strip.left=strip.custom(bg="gray85"),
                 strip=FALSE
      )
    print(LikCou2)
    
    LikCou3 <- ## 8in x 9in  ## ProfChal.pdf
      HH::likert(Question ~ . , DTFa, 
                 as.percent=TRUE,    ## implies display Row Count Totals
                 ylab=NULL,
                 main=list(lbl, x=unit(.65, "npc")),
                 positive.order=TRUE
      )
    print(LikCou3)
    LikCou4 <- ## 8in x 9in  ## ProfChal.pdf
      HH::likert(Question ~ . , DTFa, 
                 as.percent=TRUE,    ## implies display Row Count Totals
                 ylab=NULL,
                 main=list(lbl, x=unit(.65, "npc"))
      )
    print(LikCou4)
    
    if (wGrp) {
      #NmFic<-paste0(Path,lbl,".pdf")
      NmFic<-paste0(Path,".pdf")
      pdf(NmFic, width=14, height=14)
      print(LikCou2)
      print(LikCou)
      print(LikCou4)
      print(LikCou3)
      dev.off()
    }
  }
  
  ProcPrevCom<-function(VarG,Tk=1,ResI,DTpp,Acum=FALSE) {
    require(data.table)
    if (DT[,is.numeric(get(VarG))]) {
      DTpp[,New:=cut(get(VarG),5)]
      VarG<-c("New")
    }
    lv<-levels(DTpp[,get(VarG)])
    keycols = c(VarG)
    setkeyv(DTpp,keycols)
    #ResExpA<-ResExp2A<-list()
    DTF<-data.table()
    
    #ilv="Hombre"
    for (ilv in lv) {
      sel<-DTpp[ilv][,ResI$TkO,with=F]
      #if (Tk==12) {sel<-sel+1}
      lE<-length(sel)
      ResExp2<-ResExp<-matrix(NA,nrow =lE,ncol =ResI$NLik)
      for (i in c(1:lE))  {
        RR<-sel[,.N,by=eval(ResI$nm[i])]
        RR<-na.omit(RR)
        setkeyv(RR,eval(ResI$nm[i]))
        cont<-0
        for (j in c(RR[,1][[1]])) {
          cont<-cont+1
          ResExp[i,j]<-RR$N[cont]
          ResExp2[i,j]<-RR$N[cont]/sum(RR$N)
        }
        for (j in 1:ResI$NLik) {
          if (is.na(ResExp[i,j])) ResExp[i,j]<-0
          if (is.na(ResExp2[i,j])) ResExp2[i,j]<-0
        }
      }
      
      if (Acum) {DTF<-rbind(DTF,data.table(Question=ResI$nm2,ResExp2,Subtable=ilv))}
      if (!Acum){DTF<-rbind(DTF,data.table(Question=ResI$nm2,ResExp,Subtable=ilv))}
      #ResExpA[[ilv]]<-ResExp
      #ResExp2A[[ilv]]<-ResExp2
    }
    names(DTF)<-c("Question",ResI$EscalaL,"Subtable")
    DTF
  }
  
  ProcPrevBas<-function(Tk=1,ResI,DTpp,Acum=FALSE) {
    require(data.table)
    DTF<-data.table()
    
    sel<-DTpp[,ResI$TkO,with=F]
    #if (Tk==12) {sel<-sel+1}
    lE<-length(sel)
    ResExp2<-ResExp<-matrix(NA,nrow =lE,ncol =ResI$NLik)
    for (i in c(1:lE))  {
      RR<-sel[,.N,by=eval(ResI$nm[i])]
      RR<-na.omit(RR)
      setkeyv(RR,eval(ResI$nm[i]))
      cont<-0
      for (j in c(RR[,1][[1]])) {
        cont<-cont+1
        ResExp[i,j]<-RR$N[cont]
        ResExp2[i,j]<-RR$N[cont]/sum(RR$N)
      }
      for (j in 1:ResI$NLik) {
        if (is.na(ResExp[i,j])) ResExp[i,j]<-0
        if (is.na(ResExp2[i,j])) ResExp2[i,j]<-0
      }
    }
    
    if (Acum) {DTF<-data.table(Question=ResI$nm2,ResExp2)}
    if (!Acum){DTF<-data.table(Question=ResI$nm2,ResExp)}
    names(DTF)<-c("Question",ResI$EscalaL)
    DTF
  }
  
  fitlavPar19<-function(fit,Prec=2) {
    result<-list()
    
    fitSt<-fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea","rmsea.ci.lower", "rmsea.ci.upper" ,"SRMR","ntotal","gfi","nnfi"))
    
    result$Par<-parameterEstimates(fit,standardized=T)
    #CHI
    Val1=round(fitSt[[1]],Prec)
    Val2=fitSt[[2]]
    ValDiv=as.character(round((fitSt[[1]]/fitSt[[2]]),Prec))
    if ((Val1/Val2)<2) ValDiv=paste0(ValDiv,"*")
    Val3=as.character(round(fitSt[[3]],Prec))
    Lap=fitSt[[3]];
    if (Lap>=0.05) Val3=paste0(Val3,"*")
    #CFI
    Val4=as.character(round(fitSt[[4]],Prec))
    Lap=fitSt[[4]]; if (Lap>=0.95) Val4=paste0(Val4,"*")
    #RMSEA
    Val5=as.character(round(fitSt[[5]],Prec))
    Lap=fitSt[[5]];if (Lap<=0.06) Val5=paste0(Val5,"*");
    Val5b=paste0(Val5," (",round(fitSt[[6]],Prec),",",round(fitSt[[7]],Prec),")")
    #SRMR
    Val6=as.character(round(fitSt[[8]],Prec))
    Lap=fitSt[[8]];if (Lap<=0.08) Val6=paste0(Val6,"*")
    #N
    Val7=fitSt[[9]]
    
    #GFI
    Val8=as.character(round(fitSt[[10]],Prec))
    Lap=fitSt[[10]]; if (Lap>=0.95) Val8=paste0(Val8,"*");
    #NNFI
    Val9=as.character(round(fitSt[[11]],Prec))
    Lap=fitSt[[11]]; if (Lap>=0.95) Val9=paste0(Val9,"*");
    
    #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
    result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=paste0(Val3," {>=.05}"),GFI=paste0(Val8," {>=.95}"),CFI=paste0(Val4," {>=.95}"),
                            NNFI=paste0(Val9," {>=.95}"), RMSE=paste0(Val5b," {<=.06}"),SRMR=paste0(Val6," {<=.08}"),N=Val7,Ratio=paste0(ValDiv," {<2}"))
    result$All<-fitMeasures(fit)
    result
  }
  
  # para llevar al programa General pues no son expresamente de psicometría
  AlmS<-function(PCat,PPrint,NmF,App=F) {
    Lcd<-"\n-------------------------------------------\n";
    capture.output(cat(PCat),print(PPrint),cat(Lcd),file=NmF,append = App);
  }
  
  # Estima la correlación adecuada en función del tipo de item (Ordinal, Numérico o Categórico)
  # y aporta de manera compartativa Pearson
  # puede sustituir a las dos anteriores: IDnMM, IDnMMOrd
  # Los parámetros hacen referencia a la posición ocupada por cada tipo en Datos.
  # Por ejemplo, se le pasan 12 columnas, y Ordin= 1:10, Number=11, Categ=12
  IDnMM_19<-function(Data,Ordin=0,Number,Categ=0) {
    require(parallel);require(data.table);require(polycor);require(psych)
    IDn<-list()
    DTOmit<-data.table(na.omit(Data))
    IDn<-c(rep(NA,length(DTOmit)))
    if (sum(Ordin)>0) {
      X<-mclapply(Ordin, function(i) {polycor::polyserial(DTOmit[,rowSums(DTOmit[,-i,with=F])],DTOmit[,i,with=F][[1]])[[1]]});
      IDn[Ordin]<-X}
    if (sum(Number)>0) {
      Y<-mclapply(Number, function(i) {cor(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]])[[1]]});
      IDn[Number]<-Y}
    if (sum(Categ)>0) {
      Z<-mclapply(Categ, function(i) {psych::biserial(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]])[[1]]});
      IDn[Categ]<-Z}
    Pears<-mclapply(c(1:length(DTOmit)), function(i) {cor(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]])[[1]]})
    IDnF<- data.table(Names=names(DTOmit),Corr=do.call(rbind,setNames( IDn,names(DTOmit))), Pearson=do.call(rbind,setNames( Pears,names(DTOmit))))
    names(IDnF)<-c("Names","Corr.IDn","Corr.Pearson")
    return(IDnF)
  } 
  
  # Tkkp=12; ResIAllp=ResIAll
  AnItemsMM<-function(DTp,Tkkp, ResIAllp, LosNmp, SelecIDn=T,SelDn=0.25, CritVal=.15) {
    require(psych)
    require(parallel);require(data.table);require(psychometric);
    require(GPArotation)
    require(Rcsdp)
    
    Nfac<-ResIAllp[[Tkkp]]$NFac # Nº Factores impuesto para las estimaciones de CFA
    if (Nfac==0) Nfac=1
    Tkn<-Tkkp
    LaTask<-ResIAll[[Tkkp]]$Task
    RtNmFile<-paste0("IndiceDiscrim_",LaTask)
    
    IDn<-list()
    Sel<-as.vector(DT[,grep(LaTask,LosNmp,value=F)])
    Resfinal<-DT[,Sel,with=F]
    
    DTOmit<-data.table(na.omit(Resfinal))
    Number=1:length(Resfinal)
    
    if (Tkn ==2) { IDnF<- IDnMM_19(DTOmit,Number=0,Ordin=0,Categ=Number)}
    if (Tkn !=2) { IDnF<- IDnMM_19(DTOmit,Number=0,Ordin=Number,Categ=0)}
    
    #Antiguo Formato
    #if (Tkn ==2) {Y<-lapply(Number, function(i) {biserial.cor(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]],level=2)[[1]]})}
    #if (Tkn !=2) { Y<-lapply(Number, function(i) {cor(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]])[[1]]})}
    #IDn<-c(rep(NA,length(DTOmit)))
    #IDn[Number]<-Y
    #IDnF<- data.table(Names=names(DTOmit),Corr=do.call(rbind,setNames(IDn,names(DTOmit))))
    
    ElMensa<-"Índices de Discriminación \n";
    tags$b(ElMensa)
    print(IDnF)
    IDnF[,dotchart(Corr.IDn , labels=Names,cex=0.75,main="Indice Discriminacion Cálculos Rutina Propia")]
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    abline(v=.3,lty=2);abline(v=-.3,lty=2)
    
    # Ahora comparo mis cálculos con los del paquete alpha
    
    ResAlpha<-psych::alpha(DTOmit)
    #summary(ResAlpha)
    # Con Omega (variante de Alfa)
    om.results<-psych::omega(DTOmit,nfactors=Nfac)
    # Y con el Ind Discriminac clásico basado en la biserial-puntual
    ResPsych<-item.exam(DTOmit, discrim=TRUE)
    
    # Añado las 2 estimaciones
    #ResAlpha$item.stats$r.drop
    IDnF[,rdrop:=ResAlpha$item.stats$r.drop]
    IDnF[,rbis:=ResPsych$Item.Tot.woi]
    IDnF[,Discrim:=ResPsych$Discrimination]
    IDnF[,Dific:=ResPsych$Difficulty]
    IDnF[,Fiab:=ResPsych$Item.Rel.woi]
    
    # Vuelvo a ordenar sobre la base del primero de los 3 cálculos
    IDfOrd<-IDnF[order(Corr.IDn)]
    ElMensa<-"\nÍndices de Discriminación Ordenados \n";
    tags$b(ElMensa)
    print(IDfOrd)
    
    #quartz("Indice Discriminacion Alpha")
    IDfOrd[,dotchart(rdrop , labels=Names,cex=0.75,main="Indice Discriminacion Alpha")]
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    abline(v=.3,lty=2);abline(v=-.3,lty=2)
    
    ElMensa<-"\nÍndices de la Dificultad \n";
    tags$b(ElMensa)
    IDfOrd[,plot(density(Dific),main="Distribución Dificultad a través ítems")]
    IDfOrd[,hist(Dific, freq=FALSE,add=T)]
    # Kernel density plot
    
    #Almacena Resultados en el fichero IDn.pdf
    NmFileGrp<-paste0(PathRes,"IDn",RtNmFile,".pdf")
    pdf(NmFileGrp,width=8.27,height=11.69)
    IDnF[,dotchart(Corr.IDn , labels=Names,cex=0.75,
                   main="Indice Discriminacion Cálculos Rutina Propia")]
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    abline(v=.3,lty=2);abline(v=-.3,lty=2)
    
    IDfOrd[,dotchart(rdrop , labels=Names,cex=0.75,
                     main="Indice Discriminacion Alpha Ordenado")]
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    abline(v=.3,lty=2);abline(v=-.3,lty=2)
    
    IDfOrd[,dotchart(rbis , labels=Names,cex=0.75,
                     main="Indice Discriminacion Cálculos Básicos Ordenado")]
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    abline(v=.3,lty=2);abline(v=-.3,lty=2)
    
    IDnF[,dotchart(Dific , labels=Names,cex=0.75,
                   main="Indice Dificultad Cálculos Básicos")]
    abline(v=.1,lty=2);abline(v=-.1,lty=2)
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    
    IDfOrd[,dotchart(Dific , labels=Names,cex=0.75,
                     main="Indice Dificultad Cálculos Básicos Ordenado según Discriminación")]
    abline(v=.1,lty=2);abline(v=-.1,lty=2)
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    
    IDnF[,dotchart(Fiab , labels=Names,cex=0.75,main="Indice Fiabilidad Cálculos Básicos")]
    IDfOrd[,dotchart(Fiab , labels=Names,cex=0.75,
                     main="Indice Fiabilidad Cálculos Básicos Ordenado según Discriminación")]
    
    IDfOrd[,plot(density(Dific),main="Distribución Dificultad a través ítems")]
    IDfOrd[,hist(Dific, freq=FALSE,add=T)]
    dev.off()
    
    #Almacena tb los resultados
    NmFileGrp<-paste0(PathRes,"ResInDis",RtNmFile,".csv")
    write.table(IDfOrd,NmFileGrp)
    
    # Otros índices para probar
    ElMensa<-"Análisis Fiabilidad basado en Alpha: \n";tags$b(ElMensa)
    print(summary(ResAlpha))
    ElMensa<-"Análisis Fiabilidad basado en Omega: \n";tags$b(ElMensa)
    print(om.results);
    print(psych::omega.diagram(om.results,cex=2))
    ElMensa<-"Análisis Fiabilidad basado en Ind Discriminac clásico según la biserial-puntual: \n";tags$b(ElMensa)
    print(ResPsych)
    
    test.simple1 <-psych::fa(DTOmit,Nfac,rotate="oblimin")
    test.simple2 <-psych::fa(fm="minrank",DTOmit,Nfac,rotate="oblimin")
    #if(require(Rgraphviz)) {fa.graph(test.simple) } 
    ElMensa<-"Análisis Factoral por defecto y Oblimin: \n";tags$b(ElMensa)
    psych::fa.diagram(test.simple1)
    ElMensa<-"Análisis Factoral por defecto y Oblimin Varianza Explicada: \n";tags$em(ElMensa)
    test.simple1$Vaccounted
    ElMensa<-"Análisis Factoral minrank-Oblimin: \n";tags$b(ElMensa)
    psych::fa.diagram(test.simple2)
    ElMensa<-"Análisis Factoral minrank-Oblimin Varianza Explicada: \n";tags$em(ElMensa)
    test.simple2$Vaccounted
    ElMensa<-"Análisis Alternativo basado en Clusters: \n";tags$em(ElMensa)
    ic.out <- psych::iclust(DTOmit)
    #psych::iclust.diagram(ic.out,cex=.4)
    
    NmFileGrp<-paste0(PathRes,"Omega_SinFiltro",RtNmFile,".pdf")
    pdf(NmFileGrp,width=8.27,height=11.69)
    psych::omega.diagram(om.results,cex=5)
    psych::fa.diagram(test.simple1,rsize=.5)
    psych::fa.diagram(test.simple2,rsize=.5)
    psych::iclust.diagram(ic.out,cex=.4)
    dev.off()
    
    # NmFileGrp<-paste0(PathData,"ResFact1_SinFil",RtNmFile,".txt")
    # NmFileGrp2<-paste0(PathData,"ResFact2_SinFil",RtNmFile,".txt")
    # NmFileGrp3<-paste0(PathData,"ResConsis_SinFil",RtNmFile,".txt")
    # 
    # sink(NmFileGrp); print(test.simple1$Vaccounted); print(test.simple1);sink()
    # sink(NmFileGrp2); print(test.simple2$Vaccounted); print(test.simple2);sink()
    # sink(NmFileGrp3); print(summary(ResAlpha)); print(om.results);sink()
    
    PCatP<-"\n F.1) Análisis Factoral por defecto y Oblimin sin filtrado por IDn: \n";AlmS(PCatP,test.simple1$Vaccounted,NmAlmacen,T);
    PCatP<-"\n Detalles: \n";AlmS(PCatP,test.simple1,NmAlmacen,T);
    PCatP<-"\n F.2) Análisis Factoral minrank-Oblimin sin filtrado por IDn: \n";AlmS(PCatP,test.simple2$Vaccounted,NmAlmacen,T);
    PCatP<-"\n Detalles: \n"; AlmS(PCatP,test.simple2,NmAlmacen,T);
    PCatP<-"\n F.3) Análisis Alternativo basado en Alpha sin filtrado por IDn: \n";AlmS(PCatP,summary(ResAlpha),NmAlmacen,T);
    PCatP<-"\n Detalles: \n";AlmS(PCatP,om.results,NmAlmacen,T);
    
    #sink(NmAlmacen,append = T); print("F.1) Análisis Factoral por defecto y Oblimin sin filtrado por IDn: \n");print(test.simple1$Vaccounted); print(test.simple1);print(LaLinea);sink()
    #sink(NmAlmacen,append = T); print("F.2) Análisis Factoral minrank-Oblimin sin filtrado por IDn:  \n ");print(test.simple2$Vaccounted); print(test.simple2);print(LaLinea);sink()
    #sink(NmAlmacen,append = T); print("F.3) Análisis Alternativo basado en Alpha sin filtrado por IDn: \n ");print(summary(ResAlpha));print(om.results);print(LaLinea);sink()
    
    #La selección final:
    #IDfOrd[Corr.V1<0.3,Names]
    Todos<-names(Resfinal)
    Sobrantes<-IDnF[Corr.IDn<=SelDn,Names]
    ElMensa<-"Conclusión: Ítems a eleminar según el Ind Discriminación: \n";tags$p(ElMensa)
    print(data.table(Sobrantes))
    
    SeQuedan<-IDnF[Corr.IDn>SelDn,Names]
    TkOcF <- unique (grep(paste(SeQuedan,collapse="|"), 
                          names(Resfinal), value=F))
    ElMensa<-"Conclusión: Y los Ítems que no se eliminarían según el Ind Discriminación: \n";tags$p(ElMensa)
    print(data.table(names(Resfinal[,TkOcF,with=F])))
    tags$hr();
    
    PCatP<-"\n G) Conclusión Filtrados: \n";AlmS(PCatP,"",paste0(PathRes,Tasks[Tkk],".txt"),T);
    PCatP<-"\n Ítems a eleminar según el Ind Discriminación: \n";AlmS(PCatP,data.table(Sobrantes),paste0(PathRes,Tasks[Tkk],".txt"),T);
    PCatP<-"\n Y los Ítems que no se eliminarían según el Ind Discriminación: \n";AlmS(PCatP,data.table(names(Resfinal[,TkOcF,with=F])),paste0(PathRes,Tasks[Tkk],".txt"),T);
    
    #sink(NmAlmacen,append = T); print("G) Conclusión Filtrados: \n");print("Ítems a eleminar según el Ind Discriminación: \n");print(data.table(Sobrantes));print("Y los Ítems que no se eliminarían según el Ind Discriminación: \n"); print(data.table(names(Resfinal[,TkOcF,with=F])));print(LaLinea);sink();
    
    #Aquí se aplica este 2º filtro basado en el Índ Discirminación, tras el filtro 1º basado en el Ind Conc
    ResfinalSel<-Resfinal[,TkOcF,with=F]
    if (SelecIDn) Resfinal<-ResfinalSel
    Resfinal
  }
  
  #Simula Todos AF
  SimulFA<-function(DTOmitp,Ordin,Number=NA,Categ=NA,TypData,PromaxMake = T) {
    # DTOmitp: De donde partimos en esa escala
    
    NmDir<-paste0(PathData,TypData,"/")
    #NmDir<-paste0(PathData,TypData,"/",Corr,"_",ExtrMet2)
    if (!dir.exists(NmDir)) {try(dir.create(NmDir))}
    
    #Ordin=c(length(DTOmitp))
    #Number=NA
    #Categ=NA
    Data<-data.frame(DTOmit)
    mdata <- Data
    scal<-Data
    if (!anyNA(Ordin)) {for (i in Ordin) {scal[,i]<-ordered(Data[,i])}}
    if (!anyNA(Number)) {for (i in Number) {scal[,i]<-as.numeric(Data[,i])}}
    if (!anyNA(Categ)) {for (i in Categ) { scal[,i]<-as.factor(Data[,i])}}
    
    
    TypCase<-c("pairwise.complete.obs","complete.obs")
    TypCorr = c("Pearson", "Polychoric (Two Step)", "Polychoric (Max. Lik.)", "Spearman", 
                "Heterogenous (Two Step)", "Heterogenous (Max. Lik.)")
    TypCorrDif=c("Pearson - Polychoric (Two Step)", "Pearson - Polychoric (Max. Lik.)",
                 "Polychoric (Two Step) - Polychoric (Max. Lik.)", "Pearson - Spearman",
                 "Polychoric (Two Step) - Spearman", "Polychoric (Max. Lik.) - Spearman")
    TypMod=c("Components","Factors")
    TypSim<-c("Normal Distribution","Permutation")
    TypEsta<-c("Mean","Quantile")
    TypRot<- c("none", "varimax", "oblimin","promax")
    TypRot2<-c("none","varimax","quartimax","quartimin","equamax","parsimax","factor.parsimony",
               "biquartimin","covarimin","oblimax","entropy","simplimax","bentlerT",
               "bentlerQ","tandemI","tandemII","geominT","geominQ","infomaxT",
               "infomaxQ","mccammon")
    TypExtr<-c("pc","mle","fa","minres","wls","gls");
    # Que simboliza:
    # c("Principal Components","Maximum Lielihood","Principal Axis Factor","Minimum Residual",
    #    "Weighted Least Sqared","Generalized Least Squares")
    # TypFA<-c("pa", "minres" ,"mle", "pc")
    TypExtr2<-c("pca","ml","pa","minres","wls","gls","ipa","nipa")
    TyEstrInAxis<-c("component","maxr","ginv","multiple") #Exclusivamente para los tipos ipa ó nipa
    # Four different choices of initial communalities are given:
    # components, maximum correlation, generalized inverse and multiple correlation
    Ortog<- c("varimax","quartimax","equamax","parsimax","factor.parsimony","entropy","bentlerT","tandemI","tandemII","geominT","infomaxT","mccammon")
    
    TypeCorrInic<-5 ; #1 es Pearson y 5 es Heterogénea;
    CrTs=1;CrTCD=1 # Para fijar el tipo de Corr en las simulaciones
    Model=TypMod[1] # Se establece el modelo de 1: Componentes o 2: Factores
    if (Model==TypMod[1]) compPP=TRUE else compPP=FALSE
    DelCas=TRUE # True: "Delete cases listwise"
    Cases<-TypCase[2]
    suprNA<-0
    CrT=TypeCorrInic; # Tengo que fijar el tipo de Correlación desde el principio
    
    is.na(mdata) <- is.na(mdata)
    m1 <- 0; m2 <- 0; m3 <- 0; m4 <- 0; m5<-0; m6<-0
    nam <- names(mdata)
    # Missing values (pairwise or listwise)
    n <- ncol(mdata)
    ncase <- nrow(mdata)
    ncase2 <- ncase
    
    # counting listwise cases
    cont <- 0
    for (i in 1:ncase) {
      ind <- 0; j <- 0
      while (j<n && ind ==0) {
        j <- j+1
        if (is.na(mdata[i,j])) {
          cont <- cont+1
          ind <- 1 } 
      } 
    }
    ncase_list <- ncase-cont
    
    if (Cases==TypCase[2]) {     # counting listwise cases
      ncase <- ncase_list
      #title="Listwise deletion";print(title)
      junto <- data.frame("Number of cases"=ncase,  "Number of cases excluded"=cont)
      #print(junto)
    }
    # if (Cases==TypCase[1]) {
    #   title="Number of valid cases for each pairwise correlation";print(title);print(count.pairwise(mdata))}
    
    da <- mdata
    dah <- mdata								
    for (i in 1:n) { da[,i]<-ordered(mdata[,i])}  # for calculation of polychoric correlations
    for (i in 1:n) {			
      if  (is.factor(scal[,i])) {dah[,i]<-as.factor(mdata[,i]) } 
      else { if  (is.ordered(scal[,i])) { dah[,i]<-ordered(mdata[,i]) } } 
    }  #for calculation of correlations according to their scales
    
    suprime <-suprNA
    corF <-EstimaCorr(DTP=mdata,scalP = scal,CasesP=Cases,TCor = CrT,wT=F,CompP=T)
    co<-corF$v
    
    
    ###### B. Para las simulaciones de todos los tipos de análisis factoriales.
    conti2=0
    ResCargAll<-list();
    BondAj<-list()
    
    for (iEx in 1:length(TypExtr2)) {
      for (iRot in 2:length(TypRot2)) {
        conti2=conti2+1    
        NFExtr=Nfac      # Nº Factores, lo marca todo ¡¡Ojo!!
        TyExI<-1 # TypExtr<-c("pc","mle","fa","minres","wls","gls");
        TyRttI<-2 #  TypRot<- c("none", "varimax", "oblimin","promax")
        TyExI2<-iEx  #TypExtr2<-c("pca","ml","pa","minres","wls","gls","ipa","nipa")
        TyRttI2<-iRot # c("none","varimax","quartimax","quartimin","equamax","parsimax","factor.parsimony",
        #  "biquartimin","covarimin","oblimax","entropy","simplimax","bentlerT",
        #  "bentlerQ","tandemI","tandemII","geominT","geominQ","infomaxT",
        #  "infomaxQ","mccammon")
        #PromaxMake=T; # Promax después de una que sea ortogonal
        TyItAx=1 # TyEstrInAxis<-c("component","maxr","ginv","multiple") #Exclusivamente para los tipos ipa ó nipa
        uFCut2=.25 #2º punto de corte únicamente para limpiar un poco los gráficos finales de números
        uSimMax=.3;uSimMin=.1 # Criterios Solución Única
        
        # c("none","varimax-T","quartimax-T","quartimin (oblimin-Quartimin-Q)","equamax-T","parsimax-T","factor.parsimony-T",
        #  "biquartimin" (Oblimin Biquartimin-Q),"covarimin (Oblimin Covarmin-Q","oblimax-Q","entropy-T","simplimax-Q","bentlerT-T",
        #  "bentlerQ-Q","tandemI-T","tandemII-T","geominT-T","geominQ-Q","infomaxT-T",
        #  "infomaxQ-Q","mccammon-T")
        
        
        QuanSim=0.50
        Repla=FALSE
        Corr=TypCorr[CrT];CorrSim=TypCorr[CrTs];CorrCD=TypCorr[CrT]
        CorrDif=TypCorrDif[CrT]
        Seed=NULL
        repi=1000
        
        Sim=TypSim[2]
        EstaSim=TypEsta[2]
        Rott=TypRot[TyRttI]   # Tipo de Rotación Inicial
        Diago=FALSE
        ExtrMet=TypExtr[TyExI] # Tipo de Extracción Inicial
        #CD Method
        DelCasCD=TRUE # True: "Delete cases listwise"
        UntilSig=TRUE
        F.Max=20; N.Pop=10000;N.Samples=500; Alpha = 0.3
        
        Rtt2=TyRttI2
        Rott2<-TypRot2[Rtt2]; #Tipo Rotación Importante
        
        ExtrMet2=TypExtr2[TyExI2]; #Tipo Extracción Importante
        Sorting=FALSE
        UFacPt=c(1,2)
        RFacPt=c(1,2)
        StgUFD=FALSE
        StgRFD=FALSE
        uFCut=0.30     # Umbral asignación cargas factoriales
        KaissNorm=T
        KppPromax<-4
        kSimpliMax<-0
        InitExtrAx<-TyEstrInAxis[TyItAx]
        
        #quant <- function(x, sprobs = sprobs) { return(as.vector(quantile(x, probs = c(QuanSim)))) }
        pr <- NFExtr
        
        NmDir<-paste0(PathData,TypData,"/",Corr,"_",ExtrMet2)
        if (!dir.exists(NmDir)) {try(dir.create(NmDir))}
        
        #Factor Analysis (functions "principal", "fa", "iterativePrincipalAxis", "PrincipalAxis")
        ResFA<-list()
        require(ggplot2)
        
        if (ExtrMet2=="pca" && pr > n) { pr <- n }
        if (ExtrMet2!="pca" && pr > (n-1)) { pr <- n-1 }
        if (ExtrMet2=="pca") {ad<-principal(co, nfactors = pr, residuals = F, rotate="none", n.obs=ncase, scores=F) }
        if (ExtrMet2=="ipa") {
          ad<-principal(co, nfactors = pr, rotate="none", n.obs=ncase, scores=FALSE)
          ad3 <- iterativePrincipalAxis(co, nFactors=pr, communalities=InitExtrAx, iterations=200, tolerance=1e-6)
          ad$loadings <- ad3$loadings 
        }
        if (ExtrMet2=="nipa") {
          ad<-principal(co, nfactors = pr, rotate="none", n.obs=ncase, scores=FALSE)
          ad3 <- principalAxis(co, nFactors=pr, communalities=InitExtrAx) 
          ad$loadings <- ad3$loadings 
        }
        if (!(ExtrMet2 %in% c("pca","ipa","nipa"))) {
          ad <- fa(co, nfactors=pr, residuals = F, rotate = "none", n.obs=ncase_list, SMC=T, min.err = 1e-6, digits =8, max.iter=200, symmetric=T, warnings=T, fm=ExtrMet2)
          ad$loadings <- ad$loadings[,order(colnames(ad$loadings))]
        }
        
        mat<-TypCorr[CrT]
        
        if (Cases == TypCase[1]) { caso <- "pairwise"}
        if (Cases == TypCase[2]) { caso  <- "listwise"}
        junto <- data.frame("Correlation Matrix"=mat, "Factor Extraction"= ExtrMet2, "Missing values" = caso)
        title="Factor Analysis";
        #print(title);print(junto);
        
        ad$loadings <- as.matrix(ad$loadings)
        row.names(ad$loadings) <- nam
        
        load <- ad$loadings
        ConFac <- load
        if (ExtrMet2=="ipa" || ExtrMet2=="nipa") {
          sum2 <- rep(0,n)
          for(i in 1:pr) { sum2 <- load[,i]^2 + sum2 }
          ad$communality <- sum2
        }
        communality <- ad$communality
        b <- data.frame(communality, row.names=nam)
        title="Communalities"
        #print(title);print(b)
        
        DTb<-data.table(b)
        ResfMM<- rbind(Media=DTb[,mean(communality)],
                       vapply(DTb, fivenum, c(Min.=0, Q1=0,Md=0,Q3=0,Max=0)),
                       "Menor0.7"=nrow(DTb[communality<0.7]),
                       "%Menor0.7"=100*(nrow(DTb[communality<0.7])/nrow(DTb)),
                       "Menor0.5"=nrow(DTb[communality<0.5]),
                       "%Menor0.5"=100*(nrow(DTb[communality<0.5])/nrow(DTb)))
        
        ResFaAll<-data.table(Var=nam,DTb)
        ResFaAll[communality<0.5]
        NmFileGrp<-paste0(NmDir,"/AF1 CommPequeñas_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(ResFaAll[communality<0.5],file=NmFileGrp)
        
        Cargas<-round(load[,]  ,4);colnames(Cargas)<-paste0("UnRot",paste0("UnRot_F",1:NFExtr))
        ResFaAll<-cbind(ResFaAll,Cargas);
        
        
        # Sorting or not loadings
        if (!Sorting) {
          supre <- load
          for (j in 1:pr) {for (i in 1:nrow(load)) {if (abs(load[i,j]) < suprime) { supre[i,j] <- NA }}}
          title="Factor Loadings (unrotated)";
          colnames(supre)<-paste ("F",1:pr, sep="")
          #print(title);print(supre[,])
          # spsspivottable.Display(supre[,], 
          # title="Factor Loadings (unrotated)", collabels=paste ("F",1:pr, sep=""))
        }
        if (Sorting) {
          ad <- fa.sort(ad)
          load2 <- ad$loadings
          supre <- load2
          for (j in 1:pr) {for (i in 1:nrow(load2)) {if (abs(load2[i,j]) < suprime) { supre[i,j] <- NA } } }
          title="Sorted Factor Loadings (unrotated)";
          colnames(supre)<-paste ("F",1:pr, sep="");
          #print(title);print(supre[,])
          # spsspivottable.Display(supre[,], 
          # title="Sorted Factor Loadings (unrotated)", collabels=paste ("F",1:pr, sep="") ) 
        }
        # Initial Eigenvalues
        if (ExtrMet2 %in% c("pca","ipa","nipa")) {Eigenvalues <- ad$values } else { Eigenvalues <- ad$e.values }
        
        # Scree plot
        #quartz("Scree Plot")
        #plotuScree(Eigenvalues, ylab = "Eigenvalues", xlab = "Components", main = "Scree Plot") 
        
        NmFileGrp<-paste0(NmDir,"/AFGrp1 Scree Plot_",TypData,"_",Corr,"_",ExtrMet2,".pdf")
        pdf(NmFileGrp)
        plotuScree(Eigenvalues, ylab = "Eigenvalues", xlab = "Components", main = "Scree Plot") 
        dev.off()
        
        sume  <- 0
        sum2 <- rep(0,n)
        for (i in 1:n) { sume <- sume  + Eigenvalues[i];sum2[i] = sum2[i] + sume }
        title=" Variance Explained (initial)";
        junto <- data.frame(Eigenvalues,Eigenvalues/sume*100,sum2/sume*100)
        names(junto) <- c("Eigenvalues","% of Variance","Cumulative %")
        #print(title);print(junto)
        NmFileGrp<-paste0(NmDir,"/AF2_Var.Expl (initial)_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(junto,file=NmFileGrp)
        
        #tit1<-paste0("Var.Expl.Init",pr," Fact")
        ResfMM<-rbind(ResfMM, "Var.Expl.Init"=junto[pr,3])
        
        
        # Calculation of Sums of Squared Loadings
        pro <- rep(0,pr)
        for (j in 1:pr) { sum3 <- 0; for (i in 1:n) { sum3<- sum3 + load[i,j]^2 }; pro[j] <- sum3 }
        sum3 <- 0
        sum2 <- rep(0,pr)
        for (i in 1:pr) { sum3<- sum3 + pro[i];sum2[i] = sum2[i] + sum3 }
        
        title="Variance Explained (extracted)";
        junto <- data.frame(pro,pro/sume*100,sum2/sume*100)
        names(junto) <- c("Sums of Squared Loadings","% of Variance","Cumulative %")
        #print(title);print(junto)
        NmFileGrp<-paste0(NmDir,"/AF3_Var.Expl Extracted_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(junto,file=NmFileGrp)
        VarExplica<-junto
        ResfMM<-rbind(ResfMM, "Var.Expl.Extrac"=junto[pr,3])
        
        # Factor plot unrotated
        l1 <- UFacPt[1]
        l2 <- UFacPt[2]
        ll1 <- min(l1,l2); ll2 <- max(l1,l2)
        if (ll1 < ll2 && ll2 < (pr+1)) {
          x <-matrix(c(load[,ll1],load[,ll2]),ncol=2)
          x1 <- paste("Factor", ll1, sep=" ")
          y1 <- paste("Factor", ll2, sep=" ")
          #quartz("Factor Plot")
          #factor.plot(x, cut = uFCut, labels=nam, xlim = c(-1, 1), ylim = c(-1, 1), xlab=x1,
          #            ylab=y1, title = "Factor Plot (initial solution)")
          
          NmFileGrp<-paste0(NmDir,"/AFGrp2 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,".pdf")
          pdf(NmFileGrp)
          factor.plot(x, cut = uFCut, labels=nam, xlim = c(-1, 1), ylim = c(-1, 1), xlab=x1,
                      ylab=y1, title = "Factor Plot (initial solution)")
          dev.off()
        } 
        
        # Factor diagram unrotated
        #quartz("Factor Diagram")
        # fa.diagram(ad, sort=StgUFD, labels=NULL, cut=uFCut, simple=FALSE, errors=TRUE, digits=2,
        #            e.size=0.05, rsize=0.15, side=2, main="Factor Diagram (initial solution)", cex=NULL) 
        
        NmFileGrp<-paste0(NmDir,"/AFGrp3 Factor Diagram_",TypData,"_",Corr,"_",ExtrMet2,".pdf")
        pdf(NmFileGrp)
        fa.diagram(ad, sort=StgUFD, labels=NULL, cut=uFCut, simple=FALSE, errors=TRUE, digits=2,
                   e.size=0.05, rsize=0.15, side=2, main="Factor Diagram (initial solution)", cex=NULL) 
        dev.off()
        
        # Rotations
        if (Rott2!="none" || "no"=="yes") {
          
          if (Rott2==TypRot2[2]) {ad2 <- Varimax(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) } 
          if (Rott2==TypRot2[3]) {ad2 <- quartimax(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[4]) {ad2 <- quartimin(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[5]) {ad2 <- cfT(load, Tmat=diag(ncol(load)), kappa=pr/(2*nrow(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[6]) {ad2 <- cfT(load, Tmat=diag(ncol(load)), kappa=(pr-1)/(nrow(load)+pr-2), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[7]) {ad2 <- cfT(load, Tmat=diag(ncol(load)), kappa=1, normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[8]) {ad2 <- oblimin(load, Tmat=diag(ncol(load)), gam=0.5, normalize=KaissNorm, eps=1e-5, maxit=500)}
          if (Rott2==TypRot2[9]) {
            compru<-try(ad2 <- oblimin(load, Tmat=diag(ncol(load)), gam=1, normalize=KaissNorm, eps=1e-5, maxit=500),silent=T)
            if ('try-error' %in% class(compru)) {ad2 <- oblimin(load, Tmat=diag(ncol(load)), gam=0, normalize=KaissNorm, eps=1e-5, maxit=500)}
          }
          if (Rott2==TypRot2[10]) {ad2 <- oblimax(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[11]) {ad2 <- entropy(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[12]) {
            if (kSimpliMax==0) { kSimpliMax <- ncol(load)*nrow(load) - nrow(load); kk <- kSimpliMax }
            ad2 <- simplimax(load, Tmat=diag(ncol(load)), k=kSimpliMax, normalize=KaissNorm, eps=1e-5, maxit=500)}
          if (Rott2==TypRot2[13]) {ad2 <- bentlerT(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500)}
          if (Rott2==TypRot2[14]) {ad2 <- bentlerQ(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[15]) {ad2 <- tandemI(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[16]) {ad2 <- tandemII(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[17]) {ad2 <- geominT(load, Tmat=diag(ncol(load)), delta=0.01, normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[18]) {ad2 <- geominQ(load, Tmat=diag(ncol(load)), delta=0.01, normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[19]) {ad2 <- infomaxT(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500)}
          if (Rott2==TypRot2[20]) {ad2 <- infomaxQ(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) }
          if (Rott2==TypRot2[21]) {ad2 <- mccammon(load, Tmat=diag(ncol(load)), normalize=KaissNorm, eps=1e-5, maxit=500) } 
          
          
          prom<-0
          # Promax rotation (adapted from Promax function from psych package)
          if (Rott2!="none") {  promai <- ad2$orthogonal } else { promai <- FALSE }
          if ((promai) || (Rott2=="none")) {
            if (PromaxMake) { 
              m <- KppPromax
              if (ncol(load) < 2) { return(load)}
              dn <- dimnames(load)
              if (Rott2!="none") { load <- ad2$loadings }
              Q <- load * abs(load)^(m - 1)
              U <- lm.fit(load, Q)$coefficients
              d <- diag(solve(t(U) %*% U))
              U <- U %*% diag(sqrt(d))
              dimnames(U) <- NULL
              z <- load %*% U
              if (Rott2!="none") {U <- ad2$Th %*% U}
              ui <- solve(U)
              Phi <- ui %*% t(ui)
              dimnames(z) <- dn
              prom <-1
              ad2<- list(loadings = z, Th = U%*%Phi, Phi = Phi, orthogonal = FALSE, method ="promax", initial="quartimin") 
            } 
          }
          load <- ad2$loadings   # if oblique rotation, this is the pattern matrix
          
          Cargas2<-round(load[,]  ,4);colnames(Cargas2)<-paste0("RotPatt",1:NFExtr)
          #Cargas2<-round(load[,]  ,4);colnames(Cargas2)<-paste0("RotPatt_F",1:NFExtr))
          #Cargas2<-round(load[,]  ,4);colnames(Cargas2)<-paste0("RotPatt",colnames(ad$loadings))
          ResFaAll<-cbind(ResFaAll,Cargas2);
          if (!ad2$orthogonal) {
            Cargas3<-round(as.matrix(load[,])%*%ad2$Phi  ,4);colnames(Cargas3)<-paste0("RotStruc",1:NFExtr)
            ResFaAll<-cbind(ResFaAll,Cargas3);}
          
          
          # Sorting or not loadings after rotation
          if (!Sorting) {
            if (ad2$orthogonal == FALSE) {
              supre <- load
              for (j in 1:pr) {
                for (i in 1:nrow(load)) {
                  if (abs(load[i,j]) < suprime) { supre[i,j] <- NA } } } 
              supreT<-supre;colnames(supreT)<-paste ("F",1:pr, sep="")
              title="Pattern Matrix";
              #print(title);print(supreT);
              # spsspivottable.Display(supre[,], title="Pattern Matrix", 
              # collabels=paste ("F",1:pr, sep="") )
              p <- as.matrix(load[,])%*%ad2$Phi
              supre <- p
              for (j in 1:pr) {
                for (i in 1:nrow(p)) {
                  if (abs(p[i,j]) < suprime) { supre[i,j] <- NA } } }
              #spsspivottable.Display(supre[,], 
              title="Structure Matrix";
              #print(title);print(supre[,]);
            }
            else { 
              supre <- load
              for (j in 1:pr) {
                for (i in 1:nrow(load)) {
                  if (abs(load[i,j]) < suprime) { supre[i,j] <- NA } } }
              title="Rotated Factor Loadings";
              colnames(supre)<-paste ("F",1:pr, sep="")
              #print(title);print(supre[,]);
            } 
          } 
          if (Sorting) {
            ad$loadings <- ad2$loadings
            ad <- fa.sort(ad)
            load2 <- ad$loadings
            supre <- load2
            for (j in 1:pr) {for (i in 1:nrow(load2)) {if (abs(load2[i,j]) < suprime) { supre[i,j] <- NA } } }
            if ( ! ad2$orthogonal) {
              title="Sorted Pattern Matrix";
              colnames(supre)<-paste ("F",1:pr, sep="")
              #print(title);print(supre[,]);
              p <- as.matrix(load2[,])%*%ad2$Phi
              supre <- p
              for (j in 1:pr) { for (i in 1:nrow(p)) { if (abs(p[i,j]) < suprime) { supre[i,j] <- NA } } }
              title="Sorted Structure Matrix";
              colnames(supre)<-paste ("F",1:pr, sep="")
              #print(title);print(supre[,]);
            }
            else { 
              supre <- load2
              for (j in 1:pr) { for (i in 1:nrow(load2)) { if (abs(load2[i,j]) < suprime) { supre[i,j] <- NA } } }
              title="Sorted Rotated Factor Loadings";
              colnames(supre)<-paste ("F",1:pr, sep="");
              #print(title);print(supre[,]);
            } 
          }
          
          # Calculation of Sums of Squared Loadings after rotation
          if (ad2$orthogonal) {
            pro <- rep(0,pr)
            for (j in 1:pr) { sum3 <- 0
            for (i in 1:n) { sum3<- sum3 + load[i,j]^2 }
            pro[j] <- sum3 }
            sum3 <- 0
            sum2 <- rep(0,pr)
            for (i in 1:pr) { sum3<- sum3 + pro[i]
            sum2[i] = sum2[i] + sum3 }
            junto <- data.frame(pro,pro/sume*100,sum2/sume*100)
            names(junto) <- c("Sums of Squared Loadings","% of Variance","Cumulative %")
            title="Rotated Variance Explained (extracted)";
            #print(title);print(junto)
          } 
          if (!ad2$orthogonal) {
            pro <- rep(0,pr)
            for (j in 1:pr) { sum3 <- 0; for (i in 1:n) { sum3<- sum3 + p[i,j]^2 }; pro[j] <- sum3 }
            junto <- data.frame(pro)
            title="Rotation Sums of Squared Loadings from Structure Matrix";
            #print(title);print(junto)
          }
          VarExplica<-cbind(VarExplica,junto)
          NmFileGrp<-paste0(NmDir,"/AF3_Var.Expl Extracted_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
          write.table(VarExplica,file=NmFileGrp)
          #VarExplica<-junto
          
          title="Rotation Matrix";
          colnames(ad2$Th)<-paste ("F",1:pr, sep="");
          row.names(ad2$Th)<-paste ("F",1:pr, sep="")
          #print(title);print(ad2$Th)
          NmFileGrp<-paste0(NmDir,"/AF4_Rott Matrx_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
          write.table(ad2$Th,file=NmFileGrp)
          
          if (!is.null(ad2$Phi)){
            title="Correlation matrix of rotated factors";
            colnames(ad2$Phi)<-paste ("F",1:pr, sep="");
            row.names(ad2$Phi)<-paste ("F",1:pr, sep="")
            #print(title);print(ad2$Phi)
            InterFac<-(round(ad2$Phi,4))
            InterFac[abs(InterFac)<.4]<-NA
            InterFac[abs(InterFac)==1]<-NA
            InterFac
            NmFileGrp<-paste0(NmDir,"/AF5_Corr Matrx Rott Factors_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
            write.table(ad2$Phi,file=NmFileGrp)
            NmFileGrp<-paste0(NmDir,"/AF5b_Corr Matrx Rott Factors (0.4)_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
            write.table(InterFac,file=NmFileGrp)
          }
          
          # spsspivottable.Display(ad2$Phi, 
          # title="Correlation matrix of rotated factors", 
          # collabels=paste ("F",1:pr, sep=""), 
          # rowlabels=paste ("F",1:pr, sep="") )
          if (prom==0) { 
            junto <- data.frame(ad2$method,ad2$orthogonal,ad2$convergence)
            names(junto) <- c("method","orthogonal","convergence")
            title="Rotation";
            #print(title);print(junto);
            # spsspivottable.Display(junto, 
            # title="Rotation", hiderowdimlabel=TRUE) 
          }
          if (prom!=0) {
            junto <- data.frame(ad2$method,ad2$orthogonal, ad2$initial)
            names(junto) <- c("method","orthogonal","initial rotation")
            title="Rotation";
            #print(title);print(junto);
            # spsspivottable.Display(junto, 
            # title="Rotation", hiderowdimlabel=TRUE) 
          }
          
          # Factor plot after rotation
          l1 <- RFacPt[1]
          l2 <- RFacPt[2]
          ll1 <- min(l1,l2); ll2 <- max(l1,l2)
          if (ll1 < ll2 && ll2 < (pr+1)) {
            x <-matrix(c(load[,ll1],load[,ll2]), ncol=2)
            x1 <- paste("Factor", ll1, sep=" ")
            y1 <- paste("Factor", ll2, sep=" ")
            #quartz("Factor Plot")
            #factor.plot(x, cut = uFCut, labels=nam,  xlim = c(-1, 1), ylim = c(-1, 1), xlab=x1, ylab=y1, title = "Factor Plot (rotated solution)")  
            
            NmFileGrp<-paste0(NmDir,"/AFGrp4 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
            pdf(NmFileGrp,width=8.27,height=11.69)
            factor.plot(x, cut = uFCut, labels=nam,  xlim = c(-1, 1), ylim = c(-1, 1), xlab=x1, ylab=y1, title = "Factor Plot (rotated solution)")  
            dev.off()
          } 
          
          # Factor diagram after rotation
          ad$loadings <- ad2$loadings
          colnames(ad$loadings) <- paste("F", 1:pr, sep = "")
          
          #quartz("Factor Diagram")
          #fa.diagram(ad, sort=T, labels=NULL, cut=uFCut, simple=, errors=TRUE, digits=2, e.size=0.05, rsize=0.15, side=2, main="Factor Diagram (rotated solution)", cex=1)
          
          NmFileGrp<-paste0(NmDir,"/AFGrp5 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
          pdf(NmFileGrp,width=8.27,height=11.69)
          fa.diagram(ad, sort=T, labels=NULL, cut=uFCut, simple=, errors=TRUE, digits=2, e.size=0.05, rsize=0.15, side=2, main="Factor Diagram (rotated solution)", cex=1)
          dev.off()
          
          #quartz("Factor Diagram Sin Ordenar")
          #fa.diagram(ad, sort=F, labels=NULL, cut=uFCut, simple=T, errors=TRUE, digits=2, e.size=0.05, rsize=0.2, side=2, main="Factor Diagram (rotated solution) II", cex=1) 
          
          NmFileGrp<-paste0(NmDir,"/AFGrp6 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
          pdf(NmFileGrp,width=8.27,height=11.69)
          fa.diagram(ad, sort=F, labels=NULL, cut=uFCut, simple=T, errors=TRUE, digits=2, e.size=0.05, rsize=0.2, side=2, main="Factor Diagram (rotated solution) II", cex=1) 
          dev.off()
          
          #quartz("Factor Diagram Sin Ordenar y Sin Simplificar")
          #fa.diagram(ad, sort=F, labels=NULL, cut=uFCut, simple=F, errors=TRUE, digits=2, e.size=0.05, rsize=0.2, side=2, main="Factor Diagram (rotated solution) III", cex=1) 
          
          NmFileGrp<-paste0(NmDir,"/AFGrp7 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
          pdf(NmFileGrp,width=8.27,height=11.69)
          fa.diagram(ad, sort=F, labels=NULL, cut=uFCut, simple=F, errors=TRUE, digits=2, e.size=0.05, rsize=0.2, side=2, main="Factor Diagram (rotated solution) III", cex=1) 
          dev.off()
          
          
          loadings.my<-melt(ad$loadings);colnames(loadings.my)<-c("Item","Factor","Loading")
          load.DT<-data.table(loadings.my); load.DT.Calcs<-data.table(loadings.my);
          load.DT[,Loading2:=Loading]
          load.DT[abs(Loading)<uFCut2,Loading2:=NA]
          gP<-ggplot(load.DT, aes(Item, abs(Loading), fill=Loading,label =fzero(round(Loading2,2)))) + 
            facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
            geom_bar(stat="identity") + #make the bars
            coord_flip() + #flip the axes so the test names can be horizontal  
            #define the fill color gradient: blue=positive, red=negative
            scale_fill_gradient2(name = "Loading", 
                                 high = "blue", mid = "white", low = "red", 
                                 midpoint=0, guide=F) +
            ylab("Loading Strength") +  ggtitle("Factor Diagram (rotated solution)") + #improve y-axis label
            theme_bw(base_size=10) + #use a black-and0white theme with set font size
            geom_text(size = 2, position = position_stack(vjust = 0.5),check_overlap = T, colour= "black") +
            geom_hline(yintercept = uFCut, linetype="dashed", color = "orange", size=.5) +
            geom_hline(yintercept = uSimMax, linetype="dashed", color = "orange", size=.5) +
            geom_hline(yintercept = uSimMin, linetype="dashed", color = "blue", size=.5)
          
          #plot(gP)
          NmFileGrp<-paste0(NmDir,"/AFGrp8 Factor Plot Def1_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
          #pdf(NmFileGrp,width=8.27,height=11.69)
          ggsave(file=NmFileGrp,plot=gP,width=8.27,height=11.69)
          #dev.off()
          
          load.DT[abs(Loading)<uFCut,Loading:=NA]
          gP<-ggplot(load.DT, aes(Item, abs(Loading), fill=Loading,label =fzero(round(Loading,2)))) + 
            facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
            geom_bar(stat="identity") + #make the bars
            coord_flip() + #flip the axes so the test names can be horizontal  
            #define the fill color gradient: blue=positive, red=negative
            scale_fill_gradient2(name = "Loading", 
                                 high = "blue", mid = "white", low = "red", 
                                 midpoint=0, guide=F) +
            ylab("Loading Strength") +  ggtitle("Factor Diagram (rotated solution) Simplificado") + #improve y-axis label
            theme_bw(base_size=10) + #use a black-and0white theme with set font size
            geom_text(size = 2, position = position_stack(vjust = 0.5),check_overlap = T, colour= "black")
          
          #plot(gP)
          NmFileGrp<-paste0(NmDir,"/AFGrp9 Factor Plot Def2_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
          ggsave(file=NmFileGrp,plot=gP,width=8.27,height=11.69)
          #pdf(NmFileGrp,width=8.27,height=11.69)
          
          # Crea Esturctura Simple
          fa.results=ad
          cutP=0
          if ((!is.matrix(ad)) && (!is.data.frame(ad))) {
            factors <- as.matrix(ad$loadings)
            if (!is.null(ad$Phi)) 
              Phi <- ad$Phi
          } else {
            factors <- as.matrix(ad)
          }
          num.var <- dim(factors)[1]
          if (is.null(num.var)) {
            num.var <- length(factors)
            num.factors <- 1
          } else {
            num.factors <- dim(factors)[2]
          }
          
          rowmax <- apply(abs(factors), 1, max)
          factors[abs(factors) < rowmax] <- 0
          conti=0;
          factores<-NA;Variables<-NA;Variables2<-NA;cargasP<-NA
          for (nf in 1:num.factors) {
            for (i in 1:num.var) {
              if (abs(factors[i, nf]) > cutP) {
                conti<-conti+1
                factores[conti]<-nf
                Variables[conti]<-i
                Variables2[conti]<-row.names(ad$loadings)[i]
                cargasP[conti]<-round(factors[i, nf], 4)
                #print(paste(colnames(factors)[nf], "-> V", i, " [ label = ", round(factors[i, nf], 4), sep = ""))
              }
            }
          }
          #Tipifica<-scale(cargasP,T,T)[,1]
          #ResCarg<-data.table(Factor=factores,Variable=Variables,Nombres=Variables2,Cargas=cargasP)
          ResCarg<-data.table(Factor=factores,Variable=Variables,Nombres=paste0(Variables,"_",Variables2),Cargas=cargasP,
                              Tipif=scale(cargasP,T,T)[,1],TypeExtrac=ExtrMet2,TypeRotat=Rott2,Ortog=ad2$orthogonal)
          
          #setkey(ResCarg,Variable)
          
          extractedR <- ad$communality
          bR <- data.frame(extractedR, row.names=nam)
          
          DTb2<-data.table(bR)
          ResfMMR<- rbind(Media=DTb2[,mean(extractedR)],
                          vapply(DTb2, fivenum, c(Min.=0, Q1=0,Md=0,Q3=0,Max=0)),
                          "Menor0.7"=nrow(DTb2[extractedR<0.7]),
                          "%Menor0.7"=100*(nrow(DTb2[extractedR<0.7])/nrow(DTb)),
                          "Menor0.5"=nrow(DTb2[extractedR<0.5]),
                          "%Menor0.5"=100*(nrow(DTb2[extractedR<0.5])/nrow(DTb)))
          
        }   #end of rotation
        #############
        
        # KMO
        g <- diag(0,n)
        par <- cor2pcor(co)
        cco <- co
        cco[upper.tri(cco, TRUE)] <- g[upper.tri(cco, TRUE)]	
        par[upper.tri(par, TRUE)] <- g[upper.tri(par, TRUE)]
        cco <- sum(cco^2)
        kmo <- cco/(sum(par^2)+cco) 
        KMO <- c(kmo)
        b <- data.frame(KMO)
        title="KMO - Kaiser-Meyer-Olkin measure of sampling adequacy";
        #print(title);print(b)
        # spsspivottable.Display(b, 
        # title="KMO - Kaiser-Meyer-Olkin measure of sampling adequacy", 
        #hiderowdimlabel=TRUE)
        ResFA[[1]]<-b
        KMO2<-KMO(co)
        
        # MSA
        par <- cor2pcor(co)
        ms <- rep(0,n)
        for (i in 1:n) { ms[i] <- ( sum(co[i,]^2)-1 )  / (sum(co[i,]^2)+sum(par[i,]^2)-2) }
        MSA <- c(ms)
        b <- data.frame(MSA, row.names=nam)
        title="MSA - Measures of sampling adequacy";
        #print(title);print(b)
        # spsspivottable.Display(b, 
        # title="MSA - Measures of sampling adequacy") 
        ResFA[[2]]<-b
        ResFaAll$MSA<-b
        ResFaAll$AntiImag<-KMO(co)[[2]]
        ResFaAll$Complex<-ad$complexity # Aqui añado la Complejidad (çOndice de Hoffman's)
        # Residual matrix for extraction using NFactors package
        if ("pca"=="ipa" || "pca"=="nipa") {
          res <- diag(0,n)
          for (i in 2:n) { for (j in 1:(i-1)) { res [i,j] <- co[i,j] - sum(ConFac[i,]*ConFac[j,]) } }
          junto <- res + t(res) + diag(1 - ad$communality)
          row.names(junto) <- nam
          names(junto) <- nam
          ad$residual <- junto
          junto <- data.frame(junto) 
        }
        
        # GFI and AGFI
        s <- ad$residual
        som <- sum(diag(s%*%s))
        som2 <- sum(diag(co%*%co))
        som <- 1-som/som2
        b <- data.frame(som)
        names(b) <- c("GFI (ULS)")
        title="GFI (Goodness of Fit)";
        #print(title);print(b)
        ResFA[[3]]<-b
        # spsspivottable.Display(b,
        # title="GFI (Goodness of Fit)", hiderowdimlabel=TRUE) 
        ##
        aus <- matrix(0,n,n)
        for (i in 1:(n-1)) { for (j in (i+1):n) {aus[i,j] <- sum(ConFac[i,]*ConFac[j,]) } }
        aus2 <- t(aus)
        diag(aus2) <- 1
        aus <- aus + aus2
        som <- sum(diag((solve(aus)%*%co-diag(n))%*%(solve(aus)%*%co-diag(n))))
        som2 <- sum(diag(solve(aus)%*%co%*%solve(aus)%*%co))
        som <- 1-som/som2
        b <- data.frame(som)
        names(b) <- c("GFI (ML)")
        title="GFI (Goodness of Fit)";
        #print(title);print(b)
        ResFA[[4]]<-b
        
        
        # RMSR
        g <- diag(0,n)
        s <- ad$residual
        s[upper.tri(s, TRUE)] <- g[upper.tri(s, TRUE)]
        rmsr <- sqrt(2*sum(s^2)/(n*(n-1)))
        RMSR <- c(rmsr)
        b <- data.frame(RMSR)
        title="RMSR (Root Mean Square Residual)";
        #print(title);print(b)
        ResFA[[5]]<-b
        
        
        # Root Mean Square Partials Correlations Controlling Factors
        cc <- co-(ConFac%*%t(ConFac))
        ca <- sqrt(diag(cc))
        d <- diag(1/ca)
        ea <- d%*%cc%*%d   #partial correlations controlling factors
        rmsp <- sqrt((sum(ea^2)-n)/(n*(n-1)))
        RMSP <- c(rmsp)
        b <- data.frame(RMSP)
        title="Root mean square partial correlations controlling factors";
        #print(title);print(b)
        ResFA[[6]]<-b
        
        
        # Partial Correlations controlling all other variables
        par <- cor2pcor(co)
        row.names(par) <- nam
        par <- data.frame(par)
        names(par) <- nam
        title="Partial correlations controlling all other variables";
        #print(title);print(par)
        ResFA[[7]]<-par
        NmFileGrp<-paste0(NmDir,"/AF4_PartCorr Control Var",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(par,file=NmFileGrp)
        
        
        # Partial Correlations controlling Factors
        cc <- co-(ConFac%*%t(ConFac))
        ca <- sqrt(diag(cc))
        d <- diag(1/ca)
        ea <- d%*%cc%*%d   #partial correlations controlling factors
        row.names(ea) <- nam
        ea <- data.frame(ea)
        names(ea) <- nam
        title="Partial correlations controlling factors";
        #print(title);print(ea)
        ResFA[[8]]<-ea
        NmFileGrp<-paste0(NmDir,"/AF5_PartCorr Control Fac",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(ea,file=NmFileGrp)
        
        # Residual correlations
        title="Residual correlations with uniqueness on the diagonal (computed between observed and reproduced correlations)";
        #print(title);print(ad$residual)
        # spsspivottable.Display(ad$residual, 
        # title="Residual correlations with uniqueness on the diagonal (computed between observed and reproduced correlations)")
        cont <- 0
        s <- ad$residual
        for (i in 2:n) { for (j in 1:(i-1)) { if (abs(s[i,j])> 0.05) { cont <- cont+1 } } }
        p <- 2*cont/(n*(n-1))*100;
        #cat("Fit of the model to the correlation matrix","OMS","\n","Residual fit values");
        ResFA[[9]]<-data.table("Residuals>0.05"=cont,"%Residuals>0.05"=p)
        #print(ResFA[[9]])
        
        
        # Determinant
        b <- determinant(co, logarithm = FALSE)
        junto <- data.frame(b$modulus)
        names(junto) <- c("Determinant")
        title="Determinant (modulus)";
        #print(title);print(junto)
        ResFA[[10]]<-junto
        
        # Tests to correlation matrix
        bar <- cortest.bartlett(co, n = ncase)
        bar2 <- cortest.normal(co,n1=ncase, fisher = FALSE)
        bar3 <- cortest.jennrich(co, diag(rep(1,n)), n1 = ncase, n2=ncase)
        #cat("Tests of whether a correlation matrix is an identity matrix","OMS","\n","Chisquare tests");
        ResFA[[11]]<-data.table("ChiSq"=c("Chisquare","Degrees of freedom","p-values"),
                                "Bartlett's Test"=c(round(bar$chisq,5), round(bar$df,5), round(bar$p,5)),
                                "Steiger Test"=c(round(bar2$chi2,5), round(bar2$df,5),round(bar2$p,5)),
                                "Jennrich Test"=c(round(bar3$chi2,5), round(bar2$df,5),round(bar3$p,5)))
        
        #print(ResFA[[11]])
        
        
        
        
        # Chisquare test for Maximum Likelihood
        ResFA[[12]]<-0
        if (ExtrMet2 =="ml") {
          ResFA[[12]]<-data.table("Chi.ML"=ad$STATISTIC,"Df (Chi.ML)"=round(ad$dof,6),"p (Chi.ML)"=ad$PVAL)
          #cat("Chisquare test for Maximum Likelihood procedure","OMS","\n","Chisquare tests");
          #print(ResFA[[12]])
        }
        
        
        NumerPorc=load.DT.Calcs[,.N/nrow(load.DT.Calcs),abs(Loading)<uSimMin][abs==T,V1]
        #Numer=load.DT.Calcs[,.N,abs(Loading)<uSimMin][abs==T,N]
        Denom=(NFExtr-1)/NFExtr
        #Hyperplane=Numer/Denom
        HyperplaneCatt=NumerPorc/Denom
        if (length(HyperplaneCatt)==0) {HyperplaneCatt=NA}
        CompleHoff<-mean(ResFaAll$Complex)
        
        ResfMM1<-list()
        ResfMM1$Basic<-round(cbind(ResFA[[1]],ResFA[[3]],ResFA[[4]],ResFA[[5]],ResFA[[6]],ResFA[[9]],ResFA[[10]],ResFA[[12]]),5)
        ResfMM2<-      round(cbind(ResFA[[1]],ResFA[[3]],ResFA[[4]],ResFA[[5]],ResFA[[6]],ResFA[[9]],ResFA[[10]]),5)
        ResfMM1$MM<-ResfMM
        ResfMM1$Corr<-ResFA[[11]]
        NmFileGrp<-paste0(NmDir,"/AFResum1_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(ResfMM1$Basic,file=NmFileGrp)
        NmFileGrp<-paste0(NmDir,"/AFResum2_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(ResfMM1$MM,file=NmFileGrp)
        NmFileGrp<-paste0(NmDir,"/AFResum3_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(ResfMM1$Corr,file=NmFileGrp)
        NmFileGrp<-paste0(NmDir,"/AFResum4_Structr",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(ResFaAll,file=NmFileGrp)
        NmFileGrp<-paste0(NmDir,"/AFResum5_Structr2",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
        write.table(ResCarg,file=NmFileGrp)
        ResCargAll[[conti2]]<-ResCarg
        print(paste0(round(100*(conti2/(length(TypExtr2)*length(TypRot2))),2),"%: ", TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt"))
        BondAj[[conti2]]<-data.table(KMOClass=KMO2[[1]],ResfMM2,fit=round(ad$fit,4),fit.off=round(ad$fit.off,4),
                                     Chi=round(ad$STATISTIC,4),ChiEmp=round(ad$chi,4), df=ad$dof,pVal=round(ad$PVAL,5),Ratio=round(ad$STATISTIC,4)/ad$dof, 
                                     rms=round(ad$rms,4), t(round(ResfMM,4)), (ResFA[[11]][1,2:4]), (ResFA[[11]][3,2:4]),
                                     TypeExtrac=ExtrMet2,TypeRotat=Rott2,Ortog=ad2$orthogonal,HypCatt=HyperplaneCatt,CmplxHoff=CompleHoff,
                                     MaxCorObliq=max(abs(ad2$Phi[lower.tri(ad2$Phi)])), MeanCorObliq= mean(abs(ad2$Phi[lower.tri(ad2$Phi)])))
        
        
      }
    }
    
    
    NmBdAj<-c("KMO {>.6}", "KMO(MSA) {>.6}", "GFI(ULS) {>.9}", "GFI(ML) {>.9}", "RMSR {<.06.08}", "RMSP {<.06.08}", "Residuals>0.05", "%Residuals>0.05",
              "Determinant {0}", "fit", "fit.off", "Chi", "ChiEmp", "df", "pVal","RatioCh_df {<2}", "rms {<.06.08}",  
              "Mean {>.7}", "Min", "Q1", "Md", "Q3", "Max",
              "Menor0.7", "%Menor0.7", "Menor0.5", "%Menor0.5", "Var.Expl.Init", "Var.Expl.Extrac", 
              "Bartlett Test", "p.Bartlett", "Steiger Test", "p.Steiger", "Jennrich Test", "p.Jennrich",
              "TypeExtrac", "TypeRotat", "Ortog", "HypCatt", "CmplxHoff","MaxCorObliq {.32}", "MeanCorObliq {.32}")
    
    # Antes de Promax
    ResCar2<-do.call(rbind,ResCargAll)
    ResBond2<-do.call(rbind,BondAj)
    names(ResBond2)<-NmBdAj
    
    
    
    ResBond2[,Ord.Res.05:=rank(ResBond2[,7][[1]],ties.method="min")]
    ResBond2[,Ord.Fit:=rank(-ResBond2$fit,ties.method="min")]
    ResBond2[,Ord.Carga.7:=rank(ResBond2[,24][[1]],ties.method="min")]
    ResBond2[,Ord.VarEx:=rank(-ResBond2$Var.Expl.Extrac,ties.method="min")]
    ResBond2[,Ord.HypCatt:=rank(ResBond2$HypCatt,ties.method="min")]
    ResBond2[,Ord.CmplxHoff:=rank(-ResBond2$CmplxHoff,ties.method="min")]
    ResBond2<-ResBond2[,c(1:8,43,9:10,44,11:25,45,26:29,46,30:39,47,40,48,41:42)]
    ResBond2$Ortog <- ResBond2$TypeRotat %in% Ortog
    setorder(ResBond2,Ortog,-HypCatt,CmplxHoff) #Estudio Índices Complejidad
    
    ResBond3<-ResBond2[TypeRotat=="varimax"]
    ResBond3[,Ortog:=NULL];ResBond3[,TypeRotat:=NULL]
    NmDirG<-paste0(PathData,TypData,"/")
    
    NmFileGrp<-paste0(NmDirG,"AFResumAll2_Compara Extraccion_",TypData,"_",Corr,"_",".txt")
    write.table(ResBond3,file=NmFileGrp)
    NmFileGrp<-paste0(NmDirG,"AFResumAll1_Compara Extracc&Rott_",TypData,"_",Corr,"_",".txt")
    write.table(ResBond2,file=NmFileGrp)
    NmFileGrp<-paste0(NmDirG,"AFResumAll3_Compara Todo_",TypData,"_",Corr,"_",".txt")
    write.table(ResCar2,file=NmFileGrp)
    
    ResBond2
  }
  
  
}


 
# 3. Librerías desde LibrPathFam (2)
 {
   # Las he incorporado en 2024 desde otros ficheros fuente (CodePaper)
   # 0) Preliminares
   # 0.A Funciones propias
   
   frmMM<-function(x,d=2) formatC(round(x,d),d,format="f")
   
   percent <- function(num, digits = 2, ...) {
     # To compute percentage
     percentage <-formatC(num * 100, format = "f", digits = digits, ...)
     # appending "%" symbol at the end of
     # calculate percentage value
     paste0(percentage, "%")
   }
   
   EdadHastaHoy<-function(x,formato="%m/%d/%Y",Rou=1) {
     require(lubridate)
     Edad<-time_length(interval(as.Date(x,format=formato), as.Date(Sys.Date())), "years")
     Res<-paste0(
       "M = ", round(mean(Edad,na.rm = T),Rou),
       ", SD = ", round(sd(Edad,na.rm = T),Rou),
       ", Range = ", round(range(Edad,na.rm = T),Rou)[1],
       "-", round(range(Edad,na.rm = T),Rou)[2]
     )
     Res
   }
   
   EdadAPA<-function(Edadp,Rou=2) {
     Res<-paste0(
       "M = ", round(mean(Edadp,na.rm = T),Rou),
       ", SD = ", round(sd(Edadp,na.rm = T),Rou),
       ", Range = ", round(range(Edadp,na.rm = T),Rou)[1],
       "-", round(range(Edadp,na.rm = T),Rou)[2]
     )
     Res
   }
   
   IDnMM_19<-function(Data,Ordin=0,Number,Categ=0) {
     require(parallel);require(data.table);require(polycor);require(psych)
     IDn<-list()
     DTOmit<-data.table(na.omit(Data))
     IDn<-c(rep(NA,length(DTOmit)))
     if (sum(Ordin)>0) {
       X<-mclapply(Ordin, function(i) {polycor::polyserial(DTOmit[,rowSums(DTOmit[,-i,with=F])],DTOmit[,i,with=F][[1]])[[1]]});
       IDn[Ordin]<-X}
     if (sum(Number)>0) {
       Y<-mclapply(Number, function(i) {cor(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]])[[1]]});
       IDn[Number]<-Y}
     if (sum(Categ)>0) {
       Z<-mclapply(Categ, function(i) {psych::biserial(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]])[[1]]});
       IDn[Categ]<-Z}
     Pears<-mclapply(c(1:length(DTOmit)), function(i) {cor(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]])[[1]]})
     IDnF<- data.table(Names=names(DTOmit),Corr=do.call(rbind,setNames( IDn,names(DTOmit))), Pearson=do.call(rbind,setNames( Pears,names(DTOmit))))
     names(IDnF)<-c("Names","Corr.IDn","Corr.Pearson")
     return(IDnF)
   }
   
   fitlavPar19.2<-function(fit,Prec=2) {
     require(semTools)
     result<-list()
     
     fitSt<-fitMeasures(fit, c("chisq", "df", "pvalue", "cfi","tli", "nnfi","SRMR","rmsea","rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "CN_05", "ntotal","ifi","AIC","BIC2"))
     
     result$Par<-parameterEstimates(fit,standardized=T)
     #CHI
     Val1=round(fitSt[[1]],Prec)
     Val2=fitSt[[2]]
     ValDiv=as.character(round((fitSt[[1]]/fitSt[[2]]),Prec))
     if ((Val1/Val2)<2) ValDiv=paste0(ValDiv,"❖")
     Val3=as.character(round(fitSt[[3]],Prec))
     Lap=fitSt[[3]];
     if (Lap>=0.05) Val3=paste0(Val3,"*")
     
     #CFI
     Val4=as.character(round(fitSt[[4]],Prec))
     Lap=fitSt[[4]]; if (Lap>=0.95) Val4=paste0(Val4,"❖")
     #TLI
     Val5=as.character(round(fitSt[[5]],Prec))
     Lap=fitSt[[5]]; if (Lap>=0.95) Val5=paste0(Val5,"❖")
     #NNFI
     Val6=as.character(round(fitSt[[6]],Prec))
     Lap=fitSt[[6]]; if (Lap>=0.95) Val6=paste0(Val6,"❖");
     #SRMR
     Val7=as.character(round(fitSt[[7]],Prec))
     Lap=fitSt[[7]];if (Lap<=0.08) Val7=paste0(Val7,"❖")
     
     #RMSEA
     Val8=as.character(round(fitSt[[8]],Prec))
     Val8.b=fitSt[[11]]
     if (Val8.b>0.05) Val8=paste0(Val8,"*");
     Val8c=paste0(Val8," (95% CI ",round(fitSt[[9]],Prec),"-",round(fitSt[[10]],Prec),")")
     
     #Hoelter CN
     Val9=as.character(round(fitSt[[12]],0))
     Lap=fitSt[[12]];if (Lap>200) Val9=paste0(Val9,"❖")
     
     #N
     Val10=fitSt[[13]]
     
     #IFI Bollen
     Val11=as.character(round(fitSt[[14]],Prec))
     Lap=fitSt[[14]]; if (Lap>=0.95) Val11=paste0(Val11,"❖")
     
     
     #AIC
     Val12=round(fitSt[[15]],0)
     #BIC Adj
     Val13=round(fitSt[[16]],0)
     
     MoreIdx<-round(moreFitIndices(fit),4)
     Val14=as.character(round(MoreIdx[[1]],Prec))
     Lap=MoreIdx[[1]]; if (Lap>=0.95) Val14=paste0(Val14,"❖")
     Val15=as.character(round(MoreIdx[[2]],Prec))
     Lap=MoreIdx[[2]]; if (Lap>=0.95) Val15=paste0(Val15,"❖")
     Val16=as.character(MoreIdx[[3]])
     Lap=MoreIdx[[3]]; if (Lap>0.158) Val16=paste0(Val16,"❖")
     
     #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
     #result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=paste0(Val3," {>=.05}"),GFI=paste0(Val8," {>=.95}"),CFI=paste0(Val4," {>=.95}"),
     #                       NNFI=paste0(Val9," {>=.95}"), RMSE=paste0(Val5b," {<=.06}"),SRMR=paste0(Val6," {<=.08}"),N=Val7,Ratio=paste0(ValDiv," {<2}"))
     
     result$Bond<-data.frame(CHI=Val1, df=Val2, p.CHI= Val3, CFI=Val4, IFI=Val11, TLI= Val5, NNFI=Val6, SRMR= Val7, RMSEA = Val8c, CN= Val9, N= Val10,
                             Ratio=ValDiv,AIC=Val12,BIC=Val13)
     names(result$Bond)<-c( "CHI",  "df", "p.CHI {>=05}", "CFI {>=0.95}","IFI {>=0.95}","TLI {>=0.95}", "NNFI {>=0.95}", "SRMR {<=.08}", 
                            "RMSEA {<=.06}", "Hoelter CN {>200}","N","Ratio {<2}","AIC","BIC Adj")
     result$All<-fitMeasures(fit)
     
     result$Inter<- paste0("X2(", Val2, ") = ", Val1, ", p < ", Val3, ", Ratio {<2} = ", ValDiv, ", Hoelter CN {>200} = ", Val9, 
                           ", CFI {>=0.95} = ", Val4, ", IFI {>=0.95} = ", Val11, ", TLI {>=0.95} = ", Val5, ", NNFI {>=0.95} = ", Val6,
                           ", SRMR {<=.08} = ", Val7, ", RMSEA {<=.06} = ", Val8c, ", baseline RMSEA {>0.158} = ", Val16, 
                           ", AIC = ", Val12, ", Adj BIC = ", Val13,
                           ", gammaHat {>=0.95} = ", Val14, ", adjGammaHat {>=0.95} = ", Val15)
     
     MoreIdx[1:3]
     
     result
   }
   
   corstars2 <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                        result=c("none", "html", "latex")){
     #Compute correlation matrix
     require(Hmisc)
     require(xtable)
     require(psych)
     x <- as.matrix(x)
     correlation_matrix<-corr.test(x)
     R <- correlation_matrix$r # Matrix of correlation coeficients
     p <- correlation_matrix$p # Matrix of p-value 
     
     ## Define notions for significance levels; spacing is important.
     mystars <- ifelse(p < .01, "**", ifelse(p < .05, "* ", "  "))
     
     ## trunctuate the correlation matrix to two decimal
     R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
     
     ## build a new matrix that includes the correlations with their apropriate stars
     Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
     diag(Rnew) <- paste(diag(R), " ", sep="")
     rownames(Rnew) <- colnames(x)
     colnames(Rnew) <- paste(colnames(x), "", sep="")
     
     ## remove upper triangle of correlation matrix
     if(removeTriangle[1]=="upper"){
       Rnew <- as.matrix(Rnew)
       Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
       Rnew <- as.data.frame(Rnew)
     }
     
     ## remove lower triangle of correlation matrix
     else if(removeTriangle[1]=="lower"){
       Rnew <- as.matrix(Rnew)
       Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
       Rnew <- as.data.frame(Rnew)
     }
     
     ## remove last column and return the correlation matrix
     Rnew <- cbind(Rnew[1:length(Rnew)-1])
     if (result[1]=="none") return(Rnew)
     else{
       if(result[1]=="html") print(xtable(Rnew), type="html")
       else print(xtable(Rnew), type="latex") 
     }
   }
   
   ProcItAlt19<-function(Tk=1,Tasks,LosNm,DTNm,EscalaL=c("1 Nada","2 Algo","3 Medio","4 Bastante","5 Totalmente"),EstrFac) {
     options(warn=-1)
     ResIt<-list()
     NumL<-length(EscalaL)
     TkO<-grep(Tasks[Tk],LosNm,value=F)
     TkO2<-grep(Tasks[Tk],DTNm$Item,value=F)
     stre<-LosNm[last(TkO)]
     PattID<-c(strsplit(stre,"_"))[[1]]
     try(Compru<-as.numeric(PattID[length(PattID)]))
     if (is.na(as.numeric(PattID[length(PattID)-1]))) {
       #NumIt<-as.numeric(PattID[length(PattID)])
       NumIt<-length(TkO)
       NumAlt<-1
     } else {
       NumAlt<-which(last(PattID) == letters)
       if (identical(NumAlt, integer(0))) {NumAlt<-as.numeric(PattID[length(PattID)])}
       NumIt<-as.numeric(PattID[length(PattID)-1])
     }
     #Conservar para escala de MSPSS precisamente
     #if (Tk==2) {NumIt<-which(last(PattID) == letters);NumAlt=1;NumL=2}
     ResIt$Task<-Tasks[[Tk]]
     ResIt$NumI<-NumIt
     ResIt$NumOpc<-NumAlt
     ResIt$NLik<-NumL
     ResIt$EscalaL<-EscalaL
     ResIt$TkO<-TkO
     ResIt$TkO2<-TkO2
     ResIt$SQ<-seq(from=0, to=(NumIt*NumAlt),by=NumAlt)
     ResIt$nm<-names(DT[,TkO,with=F])
     ResIt$nm2<-as.vector(DTNm[TkO2,2][[1]])
     ResIt$lbl<-paste0("Tarea ",Tk,"_",as.vector(DTNm[TkO2[1],3][[1]]),"_Factor ",as.vector(DTNm[TkO2[1],4][[1]]))
     ResIt$NFac<-length(EstrFac)
     if (ResIt$NFac>0) ResIt$Factor<-EstrFac
     if (ResIt$NFac==0) ResIt$NFac<-1
     options(warn=0)
     
     ResIt
   }
   #x=ResNewAll$MSPSS.TT
   GausslTest<-function(x) {
     require(nortest)
     require(moments)
     Normali<-matrix(NA,2,7)
     Normali[1,1]<-round(shapiro.test(x)$statistic,8)
     Normali[2,1]<-round(shapiro.test(x)$p.value,8)
     Normali[1,2]<-round(ad.test(x)$statistic,8)
     Normali[2,2]<-round(ad.test(x)$p.value,8)
     Normali[1,3]<-round(sf.test(x)$statistic,8)
     Normali[2,3]<-round(sf.test(x)$p.value,8)
     Normali[1,4]<-round(lillie.test(x)$statistic,8)
     Normali[2,4]<-round(lillie.test(x)$p.value,8)
     Normali[1,5]<-round(pearson.test(x)$statistic,8)
     Normali[2,5]<-round(pearson.test(x)$p.value,8)
     Normali[1,6]<-round(cvm.test(x)$statistic,8)
     Normali[2,6]<-round(cvm.test(x)$p.value,8)
     Normali[1,7]<-round(agostino.test(x)$statistic[[1]],3)
     Normali[2,7]<-round(agostino.test(x)$p.value,8)
     rownames(Normali)<-c("statistic","p_value")
     colnames(Normali)<-c("Shapiro-Wilk","Anderson-Darling","Shapiro-Francia","Lilliefors(Kol-Smi)","Pearson chi-square","Cramer-von_Mises","D'Agostino Skewness")
     Normali
   }
   
   stats_MM <- function(x) {
     NAPerc<-nadt <-0
     summ<-summary(x)
     if (length(summ)>6) nadt= summ[[7]]
     n = length(x)-nadt
     if (length(summ)>6) NAPerc=percent(nadt/n)
     min <- frmMM(min(x, na.rm = TRUE),2)
     max <- frmMM(max(x, na.rm = TRUE),2)
     mean <- frmMM(mean(x, na.rm = TRUE),2)
     med <- frmMM(median(x, na.rm = TRUE))
     trimm = frmMM(mean(x, na.rm = TRUE,trim=.2),2)
     Q1 <- frmMM(summ[[2]],2)
     Q3 <- frmMM(summ[[5]],2)
     Iqr <- frmMM(summ[[5]]-summ[[2]],2)
     sd <- frmMM(sd(x, na.rm =TRUE),2)
     se <- frmMM(sd(x, na.rm =TRUE)/sqrt(n),2)
     mad <- frmMM(mad(x, na.rm = TRUE),2)
     summary <- data.table(n=n, 'NAs' = nadt, 'NAs%' = NAPerc, Min = min, '1st Qu'=Q1, 
                           Median=med, Mean = mean, 'Trimmed(20%)' = trimm,
                           '3rd Qu'=Q3, Max = max, 
                           SD = sd, SEM = se, IQR = Iqr, MAD = mad)
     summary
   }
   # Las he incorporado en 2024 desde otros ficheros fuente
   
   
   instalPackFam <- function() {
     pack<-c("psych","Unicode","QuantPsyc","Hmisc","irr","lavaan","semTools")
     install.packages(pack, dependencies=TRUE)
   }
   
   chpckFam <- function() {  
     library(psych)
     library(Unicode)
     library(QuantPsyc)
     library(Hmisc)
     library(irr)
     library(lavaan)
     library(semTools)	
   }
   
   #Ya la tengo incorporada en el fichero de psicofisica
   is.even <- function(x){ x %% 2 == 0 }
   #Ya la tengo incorporada en el fichero de psicofisica
   
   multi.fun <- function(x) {
     cbind(freq = table(x), 
           percentage = round(prop.table(table(x))*100, 0))
   }
   
   multi.fun2 <- function(x) {
     paste0(table(x), " (", round(prop.table(table(x))*100, 0),"%)")
   }
   PercMM <- function(x) {round(prop.table(table(x))*100, 0)}
   
   TranDT <-function(x) {
     data.table(x[,V1],paste0(round(x[,V2],2)," (",round(x[,V3],2),")"),paste0(round(x[,V4],2)," (",round(x[,V5],2),")"))  
   }
   
   
   
   CompCH_DT <-function(Datos,nmCH=paste0("ch",1:17),
                        itCH1=paste0("ch",c(1,8,12,15,16)),
                        itCH2=paste0("ch",c(2,3,5,7,11)),
                        itCH3=paste0("ch",c(4,6,9,10,13,14,17)))
   {
     #maxCH=max(unlist(lapply(estrCH,FUN=max)))
     #DTSel<-DT[,nmCH,with=FALSE]
     require(data.table)
     niCH1<-length(itCH1);niCH2<-length(itCH2);niCH3<-length(itCH3);niCH<-niCH1+niCH2+niCH3
     
     DT<-data.table(Datos);setkey(DT,Familia)
     sumaCH<-DT[,rowSums(.SD),.SDcols=nmCH]
     sumaCH1<-DT[,rowSums(.SD),.SDcols=itCH1]
     sumaCH2<-DT[,rowSums(.SD),.SDcols=itCH2]
     sumaCH3<-DT[,rowSums(.SD),.SDcols=itCH3]
     numFam<-DT[,length(unique(Familia))];numCorr<-numFam*2;numTT<-numFam*4
     
     FinalCH4<-data.table(TestCH,"chTotal"=sumaCH/niCH,"CH1"=sumaCH1/niCH1,"CH2"=sumaCH2/niCH2,"CH3"=sumaCH3/niCH3,"Familia"=Datos$Familia,"Sexo"=Datos$Sexo,"Psicfam"=Datos$Psicfam,"Edad"=Datos$Edad,"Estudia"= Datos$Estudios,"Trabaja"=Datos$Trabaja)
     
     
     out1b <- lapply(DT[,unique(Familia)], function(x) round(summary(aov(update(~ GRUPO,paste(x,'~.')), data=DFnLav))[[1]][1,5],3))
     DT[Familia==1,Sexo[1]]
     unique(DT)
     contIn<-contIn+1
     
     #computa adecuadamente orden de los hermanos: el nº3 es el Hijo mayor de los 2 y el nº4 es el menor
     contIn<-0
     NewOrd<-array(0,c(numTT,1))
     for (j in 1:numFam) {	
       StimN<-subset(FinalCH4,Familia==j);
       contIn<-contIn+1
       if (StimN$Sexo[1]==2) {NewOrd[contIn,]<-1;NewOrd[contIn+1,]<-2}
       if (StimN$Sexo[1]==1) {NewOrd[contIn,]<-2;NewOrd[contIn+1,]<-1}
       if (StimN$Edad[3]>=StimN$Edad[4]) {NewOrd[contIn+2,]<-3;NewOrd[contIn+3,]<-4}
       if (StimN$Edad[3]<StimN$Edad[4]) {NewOrd[contIn+2,]<-4;NewOrd[contIn+3,]<-3}
       contIn<-contIn+3
     }
     identical(FinalCH4$PiscFam<-NewOrd,NewOrd)
     FinalCH4
   }
   
   
   CompCH <-function(Datos) {
     TestCH<-Datos[10:26]
     itCH1<-c(1,8,12,15,16);
     itCH2<-c(2,3,5,7,11);
     itCH3<-c(4,6,9,10,13,14,17);
     niCH1<-length(itCH1);niCH2<-length(itCH2);niCH3<-length(itCH3);niCH<-niCH1+niCH2+niCH3
     numFam<-max(Datos$Familia);numCorr<-numFam*2;numTT<-numFam*4
     
     sumaCH<-rowSums(TestCH)
     sumaCH1<-rowSums(cbind(TestCH[itCH1]))
     sumaCH2<-rowSums(cbind(TestCH[itCH2]))
     sumaCH3<-rowSums(cbind(TestCH[itCH3]))
     
     
     FinalCH4<-cbind(TestCH,"chTotal"=sumaCH/niCH,"CH1"=sumaCH1/niCH1,"CH2"=sumaCH2/niCH2,"CH3"=sumaCH3/niCH3,"Familia"=Datos$Familia,"Sexo"=Datos$Sexo,"Psicfam"=Datos$Psicfam,"Edad"=Datos$Edad,"Estudia"= Datos$Estudios,"Trabaja"=Datos$Trabaja)
     #computa adecuadamente orden de los hermanos: el nº3 es el Hijo mayor de los 2 y el nº4 es el menor
     contIn<-0
     NewOrd<-array(0,c(numTT,1))
     for (j in 1:numFam) {	
       StimN<-subset(FinalCH4,Familia==j);
       contIn<-contIn+1
       if (StimN$Sexo[1]==2) {NewOrd[contIn,]<-1;NewOrd[contIn+1,]<-2}
       if (StimN$Sexo[1]==1) {NewOrd[contIn,]<-2;NewOrd[contIn+1,]<-1}
       if (StimN$Edad[3]>=StimN$Edad[4]) {NewOrd[contIn+2,]<-3;NewOrd[contIn+3,]<-4}
       if (StimN$Edad[3]<StimN$Edad[4]) {NewOrd[contIn+2,]<-4;NewOrd[contIn+3,]<-3}
       contIn<-contIn+3
     }
     identical(FinalCH4$PiscFam<-NewOrd,NewOrd)
     FinalCH4
   }
   
   CompTrasCH<-function(FinalCH4){
     
     #Obtiene datos de Cultura Honor organizado en una matriz adecuada para los análisis
     numFam<-max(FinalCH4$Familia)
     NmCol<-c("P","M","H1","H2","IC1","IC2","SP","SM","S1","S2","EP","EM","E1","E2","EPm","EHm","ESP","ESM","ES1","ES2","TP","TM","T1","T2")
     TraspCH<-array(0,c(numFam,24),dimnames=list(NULL,NmCol))
     TraspCH1<-array(0,c(numFam,24),dimnames=list(NULL,NmCol))
     TraspCH2<-array(0,c(numFam,24),dimnames=list(NULL,NmCol))
     TraspCH3<-array(0,c(numFam,24),dimnames=list(NULL,NmCol))
     
     Rav1<-array(0,c(17,4))
     RavF1<-array(0,c(5,4))
     RavF2<-array(0,c(5,4))
     RavF3<-array(0,c(7,4))
     for (j in 1:numFam) {	
       StimN<-subset(FinalCH4,Familia==j);
       #TraspCH[StimN$Familia[1],StimN$PiscFam[1,]]<-StimN$chTotal[1]
       #TraspCH[StimN$Familia[1],StimN$PiscFam[2,]]<-StimN$chTotal[2]
       #TraspCH[StimN$Familia[1],StimN$PiscFam[3,]]<-StimN$chTotal[3]
       #TraspCH[StimN$Familia[1],StimN$PiscFam[4,]]<-StimN$chTotal[4]
       
       for (h in 1:4) {
         TraspCH[StimN$Familia[1],StimN$PiscFam[h,]]<-StimN$chTotal[h]
         TraspCH1[StimN$Familia[1],StimN$PiscFam[h,]]<-StimN$CH1[h]
         TraspCH2[StimN$Familia[1],StimN$PiscFam[h,]]<-StimN$CH2[h]
         TraspCH3[StimN$Familia[1],StimN$PiscFam[h,]]<-StimN$CH3[h]
         Rav1[,StimN$PiscFam[h]]<-t(StimN[h,1:17])
         RavF1[,StimN$PiscFam[h]]<-t(StimN[h,c(1,8,12,15,16)])
         RavF2[,StimN$PiscFam[h]]<-t(StimN[h,c(2,3,5,7,11)])
         RavF3[,StimN$PiscFam[h]]<-t(StimN[h,c(4,6,9,10,13,14,17)])		
         TraspCH[StimN$Familia[1],6+StimN$PiscFam[h,]]<-StimN$Sexo[h]
         TraspCH1[StimN$Familia[1],6+StimN$PiscFam[h,]]<-StimN$Sexo[h]
         TraspCH2[StimN$Familia[1],6+StimN$PiscFam[h,]]<-StimN$Sexo[h]
         TraspCH3[StimN$Familia[1],6+StimN$PiscFam[h,]]<-StimN$Sexo[h]
       }
       EdadM<-mean(StimN$Edad[1:2])
       EdadH<-mean(StimN$Edad[3:4])
       #EdadH<-abs(StimN$Edad[3]-StimN$Edad[4])
       TraspCH[StimN$Familia[1],5]<-icc(Rav1[,1:2],model="o",type="a",unit="s")$value
       TraspCH[StimN$Familia[1],6]<-icc(Rav1[,3:4],model="o",type="a",unit="s")$value
       TraspCH[StimN$Familia[1],11:14]<-StimN$Edad[1:4]
       TraspCH[StimN$Familia[1],15]<-EdadM
       TraspCH[StimN$Familia[1],16]<-EdadH
       TraspCH[StimN$Familia[1],17:20]<-StimN$Estudia[1:4]
       TraspCH[StimN$Familia[1],21:24]<-StimN$Trabaja[1:4]	
       
       TraspCH1[StimN$Familia[1],5]<-icc(RavF1[,1:2],model="o",type="a",unit="s")$value
       TraspCH1[StimN$Familia[1],6]<-icc(RavF1[,3:4],model="o",type="a",unit="s")$value
       TraspCH1[StimN$Familia[1],11:14]<-StimN$Edad[1:4]
       TraspCH1[StimN$Familia[1],15]<-EdadM
       TraspCH1[StimN$Familia[1],16]<-EdadH
       TraspCH1[StimN$Familia[1],17:20]<-StimN$Estudia[1:4]
       TraspCH1[StimN$Familia[1],21:24]<-StimN$Trabaja[1:4]	
       
       TraspCH2[StimN$Familia[1],5]<-icc(RavF2[,1:2],model="o",type="a",unit="s")$value
       TraspCH2[StimN$Familia[1],6]<-icc(RavF2[,3:4],model="o",type="a",unit="s")$value
       TraspCH2[StimN$Familia[1],11:14]<-StimN$Edad[1:4]
       TraspCH2[StimN$Familia[1],15]<-EdadM
       TraspCH2[StimN$Familia[1],16]<-EdadH
       TraspCH2[StimN$Familia[1],17:20]<-StimN$Estudia[1:4]
       TraspCH2[StimN$Familia[1],21:24]<-StimN$Trabaja[1:4]
       
       TraspCH3[StimN$Familia[1],5]<-icc(RavF3[,1:2],model="o",type="a",unit="s")$value
       TraspCH3[StimN$Familia[1],6]<-icc(RavF3[,3:4],model="o",type="a",unit="s")$value
       TraspCH3[StimN$Familia[1],11:14]<-StimN$Edad[1:4]
       TraspCH3[StimN$Familia[1],15]<-EdadM
       TraspCH3[StimN$Familia[1],16]<-EdadH
       TraspCH3[StimN$Familia[1],17:20]<-StimN$Estudia[1:4]
       TraspCH3[StimN$Familia[1],21:24]<-StimN$Trabaja[1:4]
     }
     AllTrasp<-list()
     AllTrasp$CH<-TraspCH
     AllTrasp$CH1<-TraspCH1
     AllTrasp$CH2<-TraspCH2
     AllTrasp$CH3<-TraspCH3
     AllTrasp
   }
   
   
   
   
   CorrStim1<-function(StimN,Orden) {	
     CorrTyp1<-array(0,c(6,4))		
     CorrTyp1[1,1]<-c(StimN$chTotal[1])
     CorrTyp1[2,1]<-c(StimN$chTotal[2])
     CorrTyp1[3,1]<-c(StimN$chTotal[Orden])
     CorrTyp1[4,1]<-c(StimN$chTotal[1])-c(StimN$chTotal[2])
     CorrTyp1[5,1]<-c(StimN$chTotal[1])-c(StimN$chTotal[Orden])
     CorrTyp1[6,1]<-c(StimN$chTotal[2])-c(StimN$chTotal[Orden])
     
     CorrTyp1[1,2]<-c(StimN$CH1[1])
     CorrTyp1[2,2]<-c(StimN$CH1[2])
     CorrTyp1[3,2]<-c(StimN$CH1[Orden])
     CorrTyp1[4,2]<-c(StimN$CH1[1])-c(StimN$CH1[2])
     CorrTyp1[5,2]<-c(StimN$CH1[1])-c(StimN$CH1[Orden])
     CorrTyp1[6,2]<-c(StimN$CH1[2])-c(StimN$CH1[Orden])
     
     CorrTyp1[1,3]<-c(StimN$CH2[1])
     CorrTyp1[2,3]<-c(StimN$CH2[2])
     CorrTyp1[3,3]<-c(StimN$CH2[Orden])
     CorrTyp1[4,3]<-c(StimN$CH2[1])-c(StimN$CH2[2])
     CorrTyp1[5,3]<-c(StimN$CH2[1])-c(StimN$CH2[Orden])
     CorrTyp1[6,3]<-c(StimN$CH2[2])-c(StimN$CH2[Orden])
     
     CorrTyp1[1,4]<-c(StimN$CH3[1])
     CorrTyp1[2,4]<-c(StimN$CH3[2])
     CorrTyp1[3,4]<-c(StimN$CH3[Orden])
     CorrTyp1[4,4]<-c(StimN$CH3[1])-c(StimN$CH3[2])
     CorrTyp1[5,4]<-c(StimN$CH3[1])-c(StimN$CH3[Orden])
     CorrTyp1[6,4]<-c(StimN$CH3[2])-c(StimN$CH3[Orden])
     CorrTyp1
   }
   
   #1.2. Extrae Edad y 5 factores Personalidad N, E, Ap, Am y R
   CorrStimPers<-function(StimN,Orden) {	
     CorrTyp1<-array(0,c(3,6))		
     CorrTyp1[1,1]<-c(StimN$Edad[1])
     CorrTyp1[2,1]<-c(StimN$Edad[2])
     CorrTyp1[3,1]<-c(StimN$Edad[Orden])
     
     CorrTyp1[1,2]<-c(StimN$N[1])
     CorrTyp1[2,2]<-c(StimN$N[2])
     CorrTyp1[3,2]<-c(StimN$N[Orden])
     
     CorrTyp1[1,3]<-c(StimN$E[1])
     CorrTyp1[2,3]<-c(StimN$E[2])
     CorrTyp1[3,3]<-c(StimN$E[Orden])
     
     CorrTyp1[1,4]<-c(StimN$Ap[1])
     CorrTyp1[2,4]<-c(StimN$Ap[2])
     CorrTyp1[3,4]<-c(StimN$Ap[Orden])
     
     CorrTyp1[1,5]<-c(StimN$Am[1])
     CorrTyp1[2,5]<-c(StimN$Am[2])
     CorrTyp1[3,5]<-c(StimN$Am[Orden])
     
     CorrTyp1[1,6]<-c(StimN$R[1])
     CorrTyp1[2,6]<-c(StimN$R[2])
     CorrTyp1[3,6]<-c(StimN$R[Orden])
     CorrTyp1
   }
   
   CompIC<-function(TraspCH) {
     
     ResIC <-array(0,c(7,7))	
     
     #install.packages("irr")
     #library(irr)
     icc(TraspCH[,1:4],model="o",type="a",unit="s")
     val1<-icc(TraspCH[,1:4],model="o",type="c",unit="a")
     
     #Padres
     icc(TraspCH[,1:2],model="o",type="a",unit="s")
     val2<-icc(TraspCH[,1:2],model="o",type="a",unit="a")
     
     #Hijos
     icc(TraspCH[,3:4],model="o",type="a",unit="s")
     val3<-icc(TraspCH[,3:4],model="o",type="a",unit="a")
     
     #P-H1
     icc(TraspCH[,c(1,3)],model="o",type="a",unit="s")
     val4<-icc(TraspCH[,c(1,3)],model="o",type="a",unit="a")
     
     #P-H2
     icc(TraspCH[,c(1,4)],model="o",type="a",unit="s")
     val5<-icc(TraspCH[,c(1,4)],model="o",type="a",unit="a")
     
     #M-H1
     icc(TraspCH[,c(2,3)],model="o",type="a",unit="s")
     val6<-icc(TraspCH[,c(2,3)],model="o",type="a",unit="a")
     
     #M-H2
     icc(TraspCH[,c(2,4)],model="o",type="a",unit="s")
     val7<-icc(TraspCH[,c(2,4)],model="o",type="a",unit="a")
     
     ResIC[1,1]<-round(val1$value,3)
     ResIC[1,2]<-round(val1$Fvalue,3)
     ResIC[1,3]<-val1$df1
     ResIC[1,4]<-val1$df2
     ResIC[1,5]<-round(val1$p.value,3)
     ResIC[1,6]<-round(val1$lbound,3)
     ResIC[1,7]<-round(val1$ubound,3)
     
     ResIC[2,1]<-round(val2$value,3)
     ResIC[2,2]<-round(val2$Fvalue,3)
     ResIC[2,3]<-val2$df1
     ResIC[2,4]<-val2$df2
     ResIC[2,5]<-round(val2$p.value,3)
     ResIC[2,6]<-round(val2$lbound,3)
     ResIC[2,7]<-round(val2$ubound,3)
     
     ResIC[3,1]<-round(val3$value,3)
     ResIC[3,2]<-round(val3$Fvalue,3)
     ResIC[3,3]<-val3$df1
     ResIC[3,4]<-val3$df2
     ResIC[3,5]<-round(val3$p.value,3)
     ResIC[3,6]<-round(val3$lbound,3)
     ResIC[3,7]<-round(val3$ubound,3)
     
     ResIC[4,1]<-round(val4$value,3)
     ResIC[4,2]<-round(val4$Fvalue,3)
     ResIC[4,3]<-val4$df1
     ResIC[4,4]<-val4$df2
     ResIC[4,5]<-round(val4$p.value,3)
     ResIC[4,6]<-round(val4$lbound,3)
     ResIC[4,7]<-round(val4$ubound,3)
     
     ResIC[5,1]<-round(val5$value,3)
     ResIC[5,2]<-round(val5$Fvalue,3)
     ResIC[5,3]<-val5$df1
     ResIC[5,4]<-val5$df2
     ResIC[5,5]<-round(val5$p.value,3)
     ResIC[5,6]<-round(val5$lbound,3)
     ResIC[5,7]<-round(val5$ubound,3)
     
     ResIC[6,1]<-round(val6$value,3)
     ResIC[6,2]<-round(val6$Fvalue,3)
     ResIC[6,3]<-val6$df1
     ResIC[6,4]<-val6$df2
     ResIC[6,5]<-round(val6$p.value,3)
     ResIC[6,6]<-round(val6$lbound,3)
     ResIC[6,7]<-round(val6$ubound,3)
     
     ResIC[7,1]<-round(val7$value,3)
     ResIC[7,2]<-round(val7$Fvalue,3)
     ResIC[7,3]<-val7$df1
     ResIC[7,4]<-val7$df2
     ResIC[7,5]<-round(val7$p.value,3)
     ResIC[7,6]<-round(val7$lbound,3)
     ResIC[7,7]<-round(val7$ubound,3)
     
     colnames(ResIC)<-c("ICC","n1","n2","F","P","ci.lower","ci.upper")
     rownames(ResIC)<-c("Global","Parents","Children","S1~F","S2~F","S1~M","S2~M")
     ResIC
   }
   
   CompICCor<-function(TraspCH) {
     
     ResIC <-array(0,c(7,8))	
     
     #install.packages("irr")
     #library(irr)
     icc(TraspCH[,1:4],model="o",type="a",unit="s")
     val1<-icc(TraspCH[,1:4],model="o",type="c",unit="a")
     
     #Padres
     icc(TraspCH[,1:2],model="o",type="a",unit="s")
     val2<-icc(TraspCH[,1:2],model="o",type="a",unit="a")
     
     #Hijos
     icc(TraspCH[,3:4],model="o",type="a",unit="s")
     val3<-icc(TraspCH[,3:4],model="o",type="a",unit="a")
     
     #P-H1
     icc(TraspCH[,c(1,3)],model="o",type="a",unit="s")
     val4<-icc(TraspCH[,c(1,3)],model="o",type="a",unit="a")
     
     #P-H2
     icc(TraspCH[,c(1,4)],model="o",type="a",unit="s")
     val5<-icc(TraspCH[,c(1,4)],model="o",type="a",unit="a")
     
     #M-H1
     icc(TraspCH[,c(2,3)],model="o",type="a",unit="s")
     val6<-icc(TraspCH[,c(2,3)],model="o",type="a",unit="a")
     
     #M-H2
     icc(TraspCH[,c(2,4)],model="o",type="a",unit="s")
     val7<-icc(TraspCH[,c(2,4)],model="o",type="a",unit="a")
     
     TodasC=cor(TraspCH[,1:4])
     VCr1=TodasC[1,2]
     VCr2=TodasC[3,4]
     VCr3=TodasC[1,3]
     VCr4=TodasC[1,4]
     VCr5=TodasC[2,3]
     VCr6=TodasC[2,4]
     VCr0=mean(cbind(VCr1,VCr2,VCr3,VCr4,VCr5,VCr6))
     
     
     ResIC[1,1]<-round(val1$value,3)
     ResIC[1,2]<-round(VCr0,3)
     ResIC[1,3]<-val1$df1
     ResIC[1,4]<-val1$df2
     ResIC[1,5]<-round(val1$Fvalue,3)
     ResIC[1,6]<-round(val1$p.value,3)
     ResIC[1,7]<-round(val1$lbound,3)
     ResIC[1,8]<-round(val1$ubound,3)
     
     ResIC[2,1]<-round(val2$value,3)
     ResIC[2,2]<-round(VCr1,3)
     ResIC[2,3]<-val2$df1
     ResIC[2,4]<-val2$df2
     ResIC[2,5]<-round(val2$Fvalue,3)
     ResIC[2,6]<-round(val2$p.value,3)
     ResIC[2,7]<-round(val2$lbound,3)
     ResIC[2,8]<-round(val2$ubound,3)
     
     ResIC[3,1]<-round(val3$value,3)
     ResIC[3,2]<-round(VCr2,3)
     ResIC[3,3]<-val3$df1
     ResIC[3,4]<-val3$df2
     ResIC[3,5]<-round(val3$Fvalue,3)
     ResIC[3,6]<-round(val3$p.value,3)
     ResIC[3,7]<-round(val3$lbound,3)
     ResIC[3,8]<-round(val3$ubound,3)
     
     ResIC[4,1]<-round(val4$value,3)
     ResIC[4,2]<-round(VCr3,3)
     ResIC[4,3]<-val4$df1
     ResIC[4,4]<-val4$df2
     ResIC[4,5]<-round(val4$Fvalue,3)
     ResIC[4,6]<-round(val4$p.value,3)
     ResIC[4,7]<-round(val4$lbound,3)
     ResIC[4,8]<-round(val4$ubound,3)
     
     ResIC[5,1]<-round(val5$value,3)
     ResIC[5,2]<-round(VCr4,3)
     ResIC[5,3]<-val5$df1
     ResIC[5,4]<-val5$df2
     ResIC[5,5]<-round(val5$Fvalue,3)
     ResIC[5,6]<-round(val5$p.value,3)
     ResIC[5,7]<-round(val5$lbound,3)
     ResIC[5,8]<-round(val5$ubound,3)
     
     ResIC[6,1]<-round(val6$value,3)
     ResIC[6,2]<-round(VCr5,3)
     ResIC[6,3]<-val6$df1
     ResIC[6,4]<-val6$df2
     ResIC[6,5]<-round(val6$Fvalue,3)
     ResIC[6,6]<-round(val6$p.value,3)
     ResIC[6,7]<-round(val6$lbound,3)
     ResIC[6,8]<-round(val6$ubound,3)
     
     ResIC[7,1]<-round(val7$value,3)
     ResIC[7,2]<-round(VCr6,3)
     ResIC[7,3]<-val7$df1
     ResIC[7,4]<-val7$df2
     ResIC[7,5]<-round(val7$Fvalue,3)
     ResIC[7,6]<-round(val7$p.value,3)
     ResIC[7,7]<-round(val7$lbound,3)
     ResIC[7,8]<-round(val7$ubound,3)
     
     colnames(ResIC)<-c("ICC","r","n1","n2","F","P","ci.lower","ci.upper")
     rownames(ResIC)<-c("Global","Parents","Children","S1~F","S2~F","S1~M","S2~M")
     ResIC
   }
   
   MultiGrp<-function(datos,Model=Model3,Grp="S1",ParFix) {
     
     Result<-list()
     Result$Inv<-measurementInvariance(Model,data=datos,fixed.x=F,group=Grp,strict=T)
     PatRes<-c("loadings","intercepts","means","residuals","residual.covariances","regressions")
     Result$TyMod<-c("g:Sin Restricc","r1:Libera Params Regres s/Restricc Regres","r2:Libera Params Regres s/Restricc Regres+Strong invariance","r3:Libera Params Regres s/Restricc Regres+Errors","r4:Libera Params Regres s/Restricc Regres+All",paste("Cada subíndice añadido se refiere a cada path: ", ParFix,sep=""))
     
     fitg<-sem(Model,data=datos,fixed.x=F,group=Grp)
     fitg.r1<-sem(Model,data=datos,fixed.x=F,group=Grp,group.equal = PatRes[6])
     fitg.r2<-sem(Model,data=datos,fixed.x=F,group=Grp,group.equal = PatRes[c(1,2,3,6)])
     fitg.r3<-sem(Model,data=datos,fixed.x=F,group=Grp,group.equal = PatRes[4:6])
     fitg.r4<-sem(Model,data=datos,fixed.x=F,group=Grp,group.equal = PatRes)
     Result$aovg1<-anova(fitg,fitg.r1,fitg.r2,fitg.r3,fitg.r4)
     Result$aovg2<-anova(fitg,fitg.r1)
     Result$Mgm<-fitg
     
     Result$Mgp<-parameterEstimates(fitg,standardized=T)
     dotchart(Result$Mgp$std.all,groups=Result$Mgp$group,labels=paste(Result$Mgp$lhs,Result$Mgp$op,Result$Mgp$rhs,Result$Mgp$group),xlim=c(-1,1))
     abline(v=0)
     for (i in 1:length(ParFix)) {
       fitg.f1<-sem(Model,data=datos,fixed.x=F,group=Grp,group.equal = PatRes[6],group.partial = c(ParFix[i]))
       fitg.f2<-sem(Model,data=datos,fixed.x=F,group=Grp,group.equal = PatRes[c(1,2,3,6)],group.partial = c(ParFix[i]))
       fitg.f3<-sem(Model,data=datos,fixed.x=F,group=Grp,group.equal = PatRes[4:6],group.partial = c(ParFix[i]))
       fitg.f4<-sem(Model,data=datos,fixed.x=F,group=Grp,group.equal = PatRes,group.partial = c(ParFix[i]))
       Result$aovg3[[i]]<-anova(fitg.f1,fitg.f2,fitg.f3,fitg.f4)
       Result$aovr1[[i]]<-anova(fitg.r1,fitg.f1)
       Result$aovr2[[i]]<-anova(fitg.r2,fitg.f2)
       Result$aovr3[[i]]<-anova(fitg.r3,fitg.f3)
       Result$aovr4[[i]]<-anova(fitg.r4,fitg.f4)
       
       Result$cfr1[[i]]<-anova(fitg.r1,fitg.f1)
       Result$cfr2[[i]]<-anova(fitg.r2,fitg.f2)
       Result$cfr3[[i]]<-anova(fitg.r3,fitg.f3)
       Result$cfr4[[i]]<-anova(fitg.r4,fitg.f4)
       
       Result$cfr1[[i]]<-compareFit(fitg.r1,fitg.f1)
       Result$cfr2[[i]]<-compareFit(fitg.r2,fitg.f2)
       Result$cfr3[[i]]<-compareFit(fitg.r3,fitg.f3)
       Result$cfr4[[i]]<-compareFit(fitg.r4,fitg.f4)
     }
     Result
   }
   
   IntSexEdad<-function(TraspCH) {
     datos<-data.frame(TraspCH)
     if (ncol(TraspCH)==18) {colnames(datos)<-c("P","M","H1","H2","IC1","IC2","SP","SM","S1","S2")}
     
     if (ncol(TraspCH)>18) {
       colnames(datos)<-c("P","M","H1","H2","IC1","IC2","SP","SM","S1","S2","EP","EM","E1","E2","EMP","EH","ESP","ESM","ES1","ES2","TP","TM","T1","T2")
       
       datos$T1[datos$T1==0]<-NA
       datos$T2[datos$T2==0]<-NA
       datos$EM2<-datos$EMP
       datos$EM2[datos$EMP<=median(datos$EMP)]<-1
       datos$EM2[datos$EMP>median(datos$EMP)]<-2
       datos$EM2<-as.factor(datos$EM2)
       
       datos$EH2<-datos$EH
       datos$EH2[datos$EH<=median(datos$EH)]<-1
       datos$EH2[datos$EH>median(datos$EH)]<-2
       datos$EH2<-as.factor(datos$EH2)
       
       datos$EH2_H1<-datos$E1
       datos$EH2_H1[datos$E1<=median(datos$E1)]<-1
       datos$EH2_H1[datos$E1>median(datos$E1)]<-2
       datos$EH2_H1<-as.factor(datos$EH2_H1)
       datos$EH2_H2<-datos$E2
       datos$EH2_H2[datos$E2<=median(datos$E2)]<-1
       datos$EH2_H2[datos$E2>median(datos$E2)]<-2
       datos$EH2_H2<-as.factor(datos$EH2_H2)
       
       datos$IntS1<-interaction(datos$EM2,datos$S1,lex.order=T)
       datos$IntS2<-interaction(datos$EM2,datos$S2,lex.order=T)
       
       #datos$ESTP2<-datos$ESP+datos$ESM
       datos$ESTP2<-rowMeans(cbind(datos$ESP,datos$ESM));
       #LaSuma=summary(as.factor(datos$ESTP2))
       #LaSuma2=as.vector(LaSuma)   
       #rollapply(LaSuma2, 2, sum, by=2)
       datos$ESTP2[datos$ESTP2<=median(datos$ESTP2)]<-1
       datos$ESTP2[datos$ESTP2>median(datos$ESTP2)]<-2
       datos$ESTP2<-as.factor(datos$ESTP2)
       
       #datos$ESTH2<-datos$ES1+datos$ES2
       datos$ESTH2<-rowMeans(cbind(datos$ES1,datos$ES2));
       datos$ESTH2[datos$ESTH2<=median(datos$ESTH2)]<-1
       datos$ESTH2[datos$ESTH2>median(datos$ESTH2)]<-2
       datos$ESTH2<-as.factor(datos$ESTH2)
       
       datos$EST2_H1<-datos$ES1
       datos$EST2_H1[datos$ES1<=median(datos$ES1)]<-1
       datos$EST2_H1[datos$ES1>median(datos$ES1)]<-2
       datos$EST2_H1<-as.factor(datos$EST2_H1)
       datos$EST2_H2<-datos$ES2
       datos$EST2_H2[datos$ES2<=median(datos$ES2)]<-1
       datos$EST2_H2[datos$ES2>median(datos$ES2)]<-2
       datos$EST2_H2<-as.factor(datos$EST2_H2)
       
       datos$IntEST1<-interaction(datos$ESTP2,datos$EST2_H1,lex.order=T)
       datos$IntEST2<-interaction(datos$ESTP2,datos$EST2_H2,lex.order=T)
       
       #datos$TP2<-datos$TP+datos$TM
       datos$TP2<-rowMeans(cbind(datos$TP,datos$TM));
       #summary((datos$TP2))
       #LaSuma=summary(as.factor(datos$TP2))
       #LaSuma2=as.vector(LaSuma)  
       #rollapply(LaSuma2, 2, sum, by=2)		
       datos$TP2[datos$TP2<=median(datos$TP2)]<-1
       datos$TP2[datos$TP2>median(datos$TP2)]<-2
       datos$TP2<-as.factor(datos$TP2)
       
       #datos$TH2<-datos$T1+datos$T2
       datos$TH2<-rowMeans(cbind(datos$T1,datos$T2));
       datos$TH2[datos$TH2<=median(datos$TH2)]<-1
       datos$TH2[datos$TH2>median(datos$TH2)]<-2
       datos$TH2<-as.factor(datos$ESTH2)
       
       datos$T2_H1<-datos$T1
       #datos$T2_H1[datos$T1<=median(datos$T1)]<-1
       #datos$T2_H1[datos$T1>median(datos$T1)]<-2
       datos$T2_H1<-as.factor(datos$T2_H1)
       datos$T2_H2<-datos$T2
       #datos$T2_H2[datos$T2<=median(datos$T2)]<-1
       #datos$T2_H2[datos$T2>median(datos$T2)]<-2
       datos$T2_H2<-as.factor(datos$T2_H2)
       
       #print(summary(datos$IntS1))
       #print(summary(datos$EM2))
       #print(summary(datos$IntS2))
     }
     datos
   }
   
   IntTodasVar<-function(TraspCH) {
     datos<-data.frame(TraspCH)
     if (ncol(TraspCH)==18) {colnames(datos)<-c("P","M","H1","H2","IC1","IC2","SP","SM","S1","S2")}
     
     if (ncol(TraspCH)>18) {
       colnames(datos)<-c("P","M","H1","H2","IC1","IC2","SP","SM","S1","S2","EP","EM","E1","E2","EPm","EHm","ESP","ESM","ES1","ES2","TP","TM","T1","T2")
       
       datos$T1[datos$T1==0]<-NA
       datos$T2[datos$T2==0]<-NA
       
       #Edad
       datos$EPm<-rowMeans(cbind(datos$EP,datos$EM));
       datos$EPmd<-datos$EPm
       datos$EPmd[datos$EPm<=median(datos$EPm)]<-"Menor"
       datos$EPmd[datos$EPm>median(datos$EPm)]<-"Mayor"
       datos$EPmd<-as.factor(datos$EPmd)
       
       datos$EHm<-rowMeans(cbind(datos$E1,datos$E2));
       datos$EHmd<-datos$EHm
       datos$EHmd[datos$EHm<=median(datos$EHm)]<-"Menor"
       datos$EHmd[datos$EHm>median(datos$EHm)]<-"Mayor"
       datos$EHmd<-as.factor(datos$EHmd)
       
       datos$ETm<-rowMeans(cbind(datos$EP,datos$EM,datos$E1,datos$E2));
       datos$ETmd<-datos$ETm
       datos$ETmd[datos$ETm<=median(datos$ETm)]<-"Menor"
       datos$ETmd[datos$ETm>median(datos$ETm)]<-"Mayor"
       datos$ETmd<-as.factor(datos$ETmd)
       
       datos$EH2_H1<-datos$E1
       datos$EH2_H1[datos$E1<=median(datos$E1)]<-"Menor"
       datos$EH2_H1[datos$E1>median(datos$E1)]<-"Mayor"
       datos$EH2_H1<-as.factor(datos$EH2_H1)
       datos$EH2_H2<-datos$E2
       datos$EH2_H2[datos$E2<=median(datos$E2)]<-"Menor"
       datos$EH2_H2[datos$E2>median(datos$E2)]<-"Mayor"
       datos$EH2_H2<-as.factor(datos$EH2_H2)
       
       #Estudios
       datos$ESPm<-rowMeans(cbind(datos$ESP,datos$ESM));
       datos$ESPmd<-datos$ESPm
       datos$ESPmd[datos$ESPm<=median(datos$ESPm)]<-"Menor"
       datos$ESPmd[datos$ESPm>median(datos$ESPm)]<-"Mayor"
       datos$ESPmd<-as.factor(datos$ESPmd)
       
       datos$ESHm<-rowMeans(cbind(datos$ES1,datos$ES2));
       datos$ESHmd<-datos$ESHm
       datos$ESHmd[datos$ESHm<=median(datos$ESHm)]<-"Menor"
       datos$ESHmd[datos$ESHm>median(datos$ESHm)]<-"Mayor"
       datos$ESHmd<-as.factor(datos$ESHmd)
       
       datos$ESTm<-rowMeans(cbind(datos$ESP,datos$ESM,datos$ES1,datos$ES2));
       datos$ESTmd<-datos$ESTm
       datos$ESTmd[datos$ESTm<=median(datos$ESTm)]<-"Menor"
       datos$ESTmd[datos$ESTm>median(datos$ESTm)]<-"Mayor"
       datos$ESTmd<-as.factor(datos$ESTmd)
       
       datos$EST2_H1<-datos$ES1
       datos$EST2_H1[datos$ES1<=median(datos$ES1)]<-"Menor"
       datos$EST2_H1[datos$ES1>median(datos$ES1)]<-"Mayor"
       datos$EST2_H1<-as.factor(datos$EST2_H1)
       datos$EST2_H2<-datos$ES2
       datos$EST2_H2[datos$ES2<=median(datos$ES2)]<-"Menor"
       datos$EST2_H2[datos$ES2>median(datos$ES2)]<-"Mayor"
       datos$EST2_H2<-as.factor(datos$EST2_H2)
       
       datos$IntEST1<-interaction(datos$ESPmd,datos$EST2_H1,lex.order=T)
       datos$IntEST2<-interaction(datos$ESPmd,datos$EST2_H2,lex.order=T)
       
       #Trabajo
       datos$TPm<-rowMeans(cbind(datos$TP,datos$TM));
       datos$TPmd<-datos$TPm
       datos$TPmd[datos$TPm<=median(datos$TPm)]<-"Menor"
       datos$TPmd[datos$TPm>median(datos$TPm)]<-"Mayor"
       datos$TPmd<-as.factor(datos$TPmd)
       
       datos$THm<-rowMeans(cbind(datos$T1,datos$T2));
       datos$THmd<-datos$THm
       datos$THmd[datos$THm<=median(datos$THm)]<-"Menor"
       datos$THmd[datos$THm>median(datos$THm)]<-"Mayor"
       datos$THmd<-as.factor(datos$THmd)
       
       datos$TTm<-rowMeans(cbind(datos$TP,datos$TM,datos$T1,datos$T2));
       datos$TTmd<-datos$TTm
       datos$TTmd[datos$TTm<=median(datos$TTm)]<-"Menor"
       datos$TTmd[datos$TTm>median(datos$TTm)]<-"Mayor"
       datos$TTmd<-as.factor(datos$TTmd)
       
       datos$T2_H1<-datos$T1
       #datos$T2_H1[datos$T1<=median(datos$T1)]<-1
       #datos$T2_H1[datos$T1>median(datos$T1)]<-2
       datos$T2_H1<-as.factor(datos$T2_H1)
       datos$T2_H2<-datos$T2
       #datos$T2_H2[datos$T2<=median(datos$T2)]<-1
       #datos$T2_H2[datos$T2>median(datos$T2)]<-2
       datos$T2_H2<-as.factor(datos$T2_H2)
       
       #print(summary(datos$IntS1))
       #print(summary(datos$EM2))
       #print(summary(datos$IntS2))
     }
     datos
   }
   
   CorrigeCH<-function(TestCH) {
     #Factor 1 (HonorIndiv) = 1, 8, 12, 15, 16, 18, 19
     #Factor 2 (Sociedad) =  2, 3, 5, 7, 11, 20
     #Factor 3 (Legitimidad) = 4, 6, 9, 10, 13, 14, 17 
     
     itCH1<-c(1, 8, 12, 15, 16, 18, 19);
     itCH2<-c(2, 3, 5, 7, 11, 20);
     itCH3<-c(4, 6, 9, 10, 13, 14, 17);
     niCH1<-length(itCH1);niCH2<-length(itCH2);niCH3<-length(itCH3);niCH<-niCH1+niCH2+niCH3
     
     sumaCH<-rowSums(TestCH,na.rm=TRUE)
     sumaCH1<-rowSums(cbind(TestCH[itCH1]),na.rm=TRUE)
     sumaCH2<-rowSums(cbind(TestCH[itCH2]),na.rm=TRUE)
     sumaCH3<-rowSums(cbind(TestCH[itCH3]),na.rm=TRUE)
     MedCH<-rowMeans(TestCH,na.rm=TRUE)
     MedCH1<-rowMeans(cbind(TestCH[itCH1]),na.rm=TRUE)
     MedCH2<-rowMeans(cbind(TestCH[itCH2]),na.rm=TRUE)
     MedCH3<-rowMeans(cbind(TestCH[itCH3]),na.rm=TRUE)
     
     FinalCH<-cbind(TestCH,"TotalSum"=sumaCH,"F1Sum.Indiv"=sumaCH1,"F2Sum.Soc"=sumaCH2,"F3Sum.Legit"=sumaCH3,"Total"= MedCH,"F1.Indiv"=MedCH1,"F2.Soc"=MedCH2,"F3.Legit"=MedCH3)
     FinalCH
   }
   
   CorrigeCodeH<-function(TestCodeH) {
     #Factor Integridad Moral (MoralInteg) = ítems (2, 5, 8, 11, 17, 19, 23)/7.
     #Factor Atributos de honor Familiar (HonorFamil) = 1, 7, 13, 16 
     #Factor Atributos de honor Femenino (HonorFemen)= 3, 6, 9, 12, 15, 18, 22
     #Factor Atributos de honor Masculino (HonorMascul)= 4, 10, 14, 20, 21, 24
     
     itCH1<-c(2, 5, 8, 11, 17, 19, 23);
     itCH2<-c(1, 7, 13, 16);
     itCH3<-c(3, 6, 9, 12, 15, 18, 22);
     itCH4<-c(4, 10, 14, 20, 21, 24);
     niCH1<-length(itCH1);niCH2<-length(itCH2);niCH3<-length(itCH3);niCH4<-length(itCH4); niCH<-niCH1+niCH2+niCH3+niCH4
     
     sumaCH<-rowSums(TestCodeH,na.rm=TRUE)
     sumaCH1<-rowSums(cbind(TestCodeH[itCH1]),na.rm=TRUE)
     sumaCH2<-rowSums(cbind(TestCodeH[itCH2]),na.rm=TRUE)
     sumaCH3<-rowSums(cbind(TestCodeH[itCH3]),na.rm=TRUE)
     sumaCH4<-rowSums(cbind(TestCodeH[itCH4]),na.rm=TRUE)
     MedCH<-rowMeans(TestCodeH,na.rm=TRUE)
     MedCH1<-rowMeans(cbind(TestCodeH[itCH1]),na.rm=TRUE)
     MedCH2<-rowMeans(cbind(TestCodeH[itCH2]),na.rm=TRUE)
     MedCH3<-rowMeans(cbind(TestCodeH[itCH3]),na.rm=TRUE)
     MedCH4<-rowMeans(cbind(TestCodeH[itCH4]),na.rm=TRUE)
     
     FinalCodeH<-cbind(TestCodeH,"TotalSum"=sumaCH,"F1Sum.IntMoral"=sumaCH1,"F2Sum.Familia"=sumaCH2,"F3Sum.Fem"=sumaCH3,"F4Sum.Masc"=sumaCH4,"Total"=MedCH,"F1.IntMoral"=MedCH1,"F2.Familia"=MedCH2,"F3.Fem"=MedCH3,"F4.Masc"=MedCH4)
     FinalCodeH
   }
   
   CorrigeAS<-function(TestAS,Tope=5) {
     #Factor 1 actitudes en el ámbito público y laboral
     #(ARS1 = 1, 2, 3, 5, 8, 10, 12, 13, 15, 16, 19, 20, 22, 26, 27, 28)/16.
     #Factor 2 sexualidad y matrimonio = (ARS2= 6, 11, 21,23, 25, 29 /6)
     #Factor 3 familia = (ARS3= 4, 7, 9, 14, 17, 18, 24, 30/8)
     #Los ítems subrayados son invertidos, a mayor puntuación mayor actitud sexista.
     
     itCH1<-c(1, 8, 12, 15, 16, 18, 19);
     itCH2<-c(2, 3, 5, 7, 11, 20);
     itCH3<-c(4, 6, 9, 10, 13, 14, 17);
     
     CH1.Inv =  c(2, 5, 10, 13, 19, 26, 27);
     CH2.nv= c(6)
     CH3.Inv= c(4, 30)
     ItemsInv<-sort(c(CH1.Inv,CH2.nv,CH3.Inv))
     TestAS[ItemsInv]=(Tope+1)-TestAS[ItemsInv]
     
     niCH1<-length(itCH1);niCH2<-length(itCH2);niCH3<-length(itCH3);niCH<-niCH1+niCH2+niCH3
     
     sumaCH<-rowSums(TestAS,na.rm=TRUE)
     sumaCH1<-rowSums(cbind(TestAS[itCH1]),na.rm=TRUE)
     sumaCH2<-rowSums(cbind(TestAS[itCH2]),na.rm=TRUE)
     sumaCH3<-rowSums(cbind(TestAS[itCH3]),na.rm=TRUE)
     MedCH<-rowMeans(TestAS,na.rm=TRUE)
     MedCH1<-rowMeans(cbind(TestAS[itCH1]),na.rm=TRUE)
     MedCH2<-rowMeans(cbind(TestAS[itCH2]),na.rm=TRUE)
     MedCH3<-rowMeans(cbind(TestAS[itCH3]),na.rm=TRUE)
     
     FinalAS<-cbind(TestAS,"TotalSum"=sumaCH,"F1Sum.Lab"=sumaCH1,"F2Sum.Sex"=sumaCH2,"F3Sum.Matrim"=sumaCH3,"Total"= MedCH,"F1.Lab"=MedCH1,"F2.Sex"=MedCH2,"F3.Matrim"=MedCH3)
     FinalAS
   }
   
   
   CorrigeIndiv<-function(TestAutCol) {
     #Factor 1: búsqueda del propio beneficio (Beneficio)	1, 6, 11
     #Factor 2: Solidaridad (Solidar)	2, 7, 12
     #Factor 3: Instrumentación de las relaciones (Instrument)	3, 8, 13
     #Factor 4: Cooperación grupal (Cooperación)	4, 9, 14
     #Factor 5: Independencia vs. Dependencia del grupo (Independ)	5, 10, 15
     
     #De estos factores se sacan otros dos
     #Colectivismo= factor 2 + factor 4 +factor 5
     #Individualismo= factor 1 + factor 3
     
     itCH1<-c(1, 6, 11);
     itCH2<-c(2, 7, 12);
     itCH3<-c(3, 8, 13);
     itCH4<-c(4, 9, 14);
     itCH5<-c(5, 10, 15);
     itCHT1<-sort(c(itCH2,itCH4,itCH5))
     itCHT2<-sort(c(itCH1,itCH3))
     niCH1<-length(itCH1);niCH2<-length(itCH2);niCH3<-length(itCH3);niCH4<-length(itCH4);niCH5<-length(itCH5); niCH<-niCH1+niCH2+niCH3+niCH4+niCH5
     
     sumaCH<-rowSums(TestAutCol,na.rm=TRUE)
     sumaCH1<-rowSums(cbind(TestAutCol[itCH1]),na.rm=TRUE)
     sumaCH2<-rowSums(cbind(TestAutCol[itCH2]),na.rm=TRUE)
     sumaCH3<-rowSums(cbind(TestAutCol[itCH3]),na.rm=TRUE)
     sumaCH4<-rowSums(cbind(TestAutCol[itCH4]),na.rm=TRUE)
     sumaCH5<-rowSums(cbind(TestAutCol[itCH5]),na.rm=TRUE)
     sumaCHT1<-rowSums(cbind(TestAutCol[itCHT1]),na.rm=TRUE)
     sumaCHT2<-rowSums(cbind(TestAutCol[itCHT2]),na.rm=TRUE)
     MedCH<-rowMeans(TestAutCol,na.rm=TRUE)
     MedCH1<-rowMeans(cbind(TestAutCol[itCH1]),na.rm=TRUE)
     MedCH2<-rowMeans(cbind(TestAutCol[itCH2]),na.rm=TRUE)
     MedCH3<-rowMeans(cbind(TestAutCol[itCH3]),na.rm=TRUE)
     MedCH4<-rowMeans(cbind(TestAutCol[itCH4]),na.rm=TRUE)
     MedCH5<-rowMeans(cbind(TestAutCol[itCH5]),na.rm=TRUE)
     MedCHT1<-rowMeans(cbind(TestAutCol[itCHT1]),na.rm=TRUE)
     MedCHT2<-rowMeans(cbind(TestAutCol[itCHT2]),na.rm=TRUE)
     
     FinalIndiv<-cbind(TestAutCol,"TotalSum"=sumaCH,"T1Sum.Colect"= sumaCHT1,"T2Sum.Indiv"= sumaCHT2, "F1Sum.Benef"=sumaCH1,"F2Sum.Solida"=sumaCH2,"F3Sum.Instrum"=sumaCH3,"F4Sum.Coopera"=sumaCH4,"F5Sum.Indep"=sumaCH5,"Total"=MedCH,"T1.Colect"=MedCHT1,"T2.Indiv"=MedCHT2, "F1.Benef"=MedCH1,"F2.Solida"=MedCH2,"F3.Instrum"=MedCH3,"F4.Coopera"=MedCH4,"F5.Indep"=MedCH5)
     FinalIndiv
   }
   
   CorrigeIG<-function(TestIG) {
     #Masculinidad (Mascul) = ítems 1, 4, 5, 7, 11, 12, 14, 17
     #Feminidad (Femen) = ítems 2, 3, 6, 8, 9, 10, 13, 15, 16, 18
     
     
     itCH1<-c(1, 4, 5, 7, 11, 12, 14, 17);
     itCH2<-c(2, 3, 6, 8, 9, 10, 13, 15, 16, 18);
     
     niCH1<-length(itCH1);niCH2<-length(itCH2);niCH<-niCH1+niCH2
     
     sumaCH<-rowSums(TestIG,na.rm=TRUE)
     sumaCH1<-rowSums(cbind(TestIG[itCH1]),na.rm=TRUE)
     sumaCH2<-rowSums(cbind(TestIG[itCH2]),na.rm=TRUE)
     MedCH<-rowMeans(TestIG,na.rm=TRUE)
     MedCH1<-rowMeans(cbind(TestIG[itCH1]),na.rm=TRUE)
     MedCH2<-rowMeans(cbind(TestIG[itCH2]),na.rm=TRUE)
     
     
     FinalIG<-cbind(TestIG,"TotalSum"=sumaCH,"F1Sum.Mascul"=sumaCH1,"F2Sum.Femen"=sumaCH2,"Total"= MedCH,"F1.Mascul"=MedCH1,"F2.Femen"=MedCH2)
     FinalIG
   }
   
   DesSocio<-function(FinalCH4_New,VarEs="Estudia") {
     ResumEstadMas<-list()  
     VarMiem="Todos"  
     SelMom<-FinalCH4_New[,VarEs]
     #SelMom<-FinalCH4_New$Estudia[FinalCH4_New$PiscFam==Miembro]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     #hist(SelMom,xlab="Studies",main="")
     
     VarMiem="Padre"
     Miembro<-1
     SelMom<-FinalCH4_New[FinalCH4_New$PiscFam==Miembro,VarEs]
     #SelMom<-FinalCH4_New$Estudia[FinalCH4_New$PiscFam==Miembro]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     #hist(SelMom,xlab="Studies",main="")
     
     VarMiem="Madre"
     Miembro<-2
     SelMom<-FinalCH4_New[FinalCH4_New$PiscFam==Miembro,VarEs]
     #SelMom<-FinalCH4_New$Estudia[FinalCH4_New$PiscFam==Miembro]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     
     VarMiem="Hijo1"
     Miembro<-3
     SelMom<-FinalCH4_New[FinalCH4_New$PiscFam==Miembro,VarEs]
     #SelMom<-FinalCH4_New$Estudia[FinalCH4_New$PiscFam==Miembro]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     
     VarMiem="Hijo2"
     Miembro<-4
     SelMom<-FinalCH4_New[FinalCH4_New$PiscFam==Miembro,VarEs]
     #SelMom<-FinalCH4_New$Estudia[FinalCH4_New$PiscFam==Miembro]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     
     VarMiem="Hijas"
     SelMom<-FinalCH4_New[((FinalCH4_New$PiscFam==3|FinalCH4_New$PiscFam==4)&FinalCH4_New$Sexo==1),VarEs]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     
     VarMiem="Hijos"
     SelMom<-FinalCH4_New[((FinalCH4_New$PiscFam==3|FinalCH4_New$PiscFam==4)&FinalCH4_New$Sexo==2),VarEs]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     
     VarMiem="Primogénita"
     SelMom<-FinalCH4_New[(FinalCH4_New$PiscFam==3&FinalCH4_New$Sexo==1),VarEs]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     
     VarMiem="Primogénito"
     SelMom<-FinalCH4_New[(FinalCH4_New$PiscFam==3&FinalCH4_New$Sexo==2),VarEs]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     
     VarMiem="Hijo2Mujer"
     SelMom<-FinalCH4_New[(FinalCH4_New$PiscFam==4&FinalCH4_New$Sexo==1),VarEs]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     
     VarMiem="Hijo2Varon"
     SelMom<-FinalCH4_New[(FinalCH4_New$PiscFam==4&FinalCH4_New$Sexo==2),VarEs]
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(SelMom),"SD"=sd(SelMom)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(SelMom))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(SelMom),"TT"=sum(SelMom))
     
     VarMiem="MediaPadres"
     SelMom<-FinalCH4_New[FinalCH4_New$PiscFam==1,VarEs];
     SelMom2<-FinalCH4_New[FinalCH4_New$PiscFam==2,VarEs];
     LaMed<-rowMeans(cbind(SelMom,SelMom2))
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(LaMed),"SD"=sd(LaMed)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(LaMed))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(LaMed),"TT"=sum(LaMed))
     LaMed2=LaMed
     LaMed2[LaMed<=median(LaMed)]<-1
     LaMed2[LaMed>median(LaMed)]<-2
     ResumEstadMas[[VarMiem]][[4]]<-summary(as.factor(LaMed2))
     
     VarMiem="MediaHijos"
     SelMom<-FinalCH4_New[FinalCH4_New$PiscFam==3,VarEs];
     SelMom2<-FinalCH4_New[FinalCH4_New$PiscFam==4,VarEs];
     LaMed<-rowMeans(cbind(SelMom,SelMom2))
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(LaMed),"SD"=sd(LaMed)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(LaMed))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(LaMed),"TT"=sum(LaMed))
     LaMed2=LaMed
     LaMed2[LaMed<=median(LaMed)]<-1
     LaMed2[LaMed>median(LaMed)]<-2
     ResumEstadMas[[VarMiem]][[4]]<-summary(as.factor(LaMed2))
     
     VarMiem="MediaTodos"
     SelMom<-FinalCH4_New[FinalCH4_New$PiscFam==1,VarEs];
     SelMom2<-FinalCH4_New[FinalCH4_New$PiscFam==2,VarEs];
     SelMom3<-FinalCH4_New[FinalCH4_New$PiscFam==3,VarEs];
     SelMom4<-FinalCH4_New[FinalCH4_New$PiscFam==4,VarEs];
     LaMed<-rowMeans(cbind(SelMom,SelMom2,SelMom3,SelMom4))
     ResumEstadMas[[VarMiem]][[1]]<-round(c(summary(LaMed),"SD"=sd(LaMed)),3)
     ResumEstadMas[[VarMiem]][[2]]<-summary(as.factor(LaMed))
     ResumEstadMas[[VarMiem]][[3]]<-c("N"=length(LaMed),"TT"=sum(LaMed))
     LaMed2=LaMed
     LaMed2[LaMed<=median(LaMed)]<-1
     LaMed2[LaMed>median(LaMed)]<-2
     ResumEstadMas[[VarMiem]][[4]]<-summary(as.factor(LaMed2))
     
     ResumEstadMas
   }
   
   fitlav<-function(Model,Data) {
     result<-list()
     fit<-sem(Model,data=Data,fixed.x=F)
     #result$Summ<-summary(fit,standardized=T,fit.measures=T)
     fitSt<-fitMeasures(fit)
     result$Par<-parameterEstimates(fit,standardized=T)
     Val1=round(fitSt[[2]],4)
     Val2=fitSt[[3]]
     Val3=as.character(round(fitSt[[4]],4))
     Lap=fitSt[[4]];
     if (Lap>=0.05) Val3=paste0(Val3,"*")
     Val4=as.character(round(fitSt[[8]],4))
     Lap=fitSt[[8]];
     if (Lap>=0.95) Val4=paste0(Val4,"*")
     Val5=as.character(round(fitSt[[23]],4))
     Lap=fitSt[[26]];
     if (Lap>=0.05) Val5=paste0(Val5,"*");
     Val5=paste0(Val5," (",round(fitSt[[24]],4),", ",round(fitSt[[25]],4),")")
     Val6=as.character(round(fitSt[[29]],4))
     Lap=fitSt[[29]];
     if (Lap<=0.08) Val6=paste0(Val6,"*")
     Val7=fitSt[[21]]
     #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
     result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=Val3,CFI=Val4,RMSE=Val5,SRMR=Val6,N=Val7)
     result
   }
   
   #fit.s1<-sem(Model1,data=DatSelS1,fixed.x=F,group="S1")
   fitlavgrp<-function(Model,Data,Grp) {
     result<-list()
     fit<-sem(Model,data=Data,fixed.x=F,group=Grp)
     #result$Summ<-summary(fit,standardized=T,fit.measures=T)
     fitSt<-fitMeasures(fit)
     result$Par<-parameterEstimates(fit,standardized=T)
     Val1=round(fitSt[[2]],4)
     Val2=fitSt[[3]]
     Val3=as.character(round(fitSt[[4]],4))
     Lap=fitSt[[4]];
     if (Lap>=0.05) Val3=paste0(Val3,"*")
     Val4=as.character(round(fitSt[[8]],4))
     Lap=fitSt[[8]];
     if (Lap>=0.95) Val4=paste0(Val4,"*")
     Val5=as.character(round(fitSt[[23]],4))
     Lap=fitSt[[26]];
     if (Lap>=0.05) Val5=paste0(Val5,"*")
     Val6=as.character(round(fitSt[[29]],4))
     Lap=fitSt[[29]];
     if (Lap<=0.08) Val6=paste0(Val6,"*")
     Val7=fitSt[[21]]
     #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
     result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=Val3,CFI=Val4,RMSE=Val5,SRMR=Val6,N=Val7)
     result
   }
   
   
   
   ReinitMod<-function() {
     if (exists("Mod.1A")) rm(Mod.1A)
     if (exists("Mod.1B")) rm(Mod.1B)
     if (exists("Mod.1C")) rm(Mod.1C)
     if (exists("Mod.2A")) rm(Mod.2A)
     if (exists("Mod.2B")) rm(Mod.2B)
     if (exists("Mod.2C")) rm(Mod.2C)
     if (exists("Mod.2D")) rm(Mod.2D)
     if (exists("Mod.3A")) rm(Mod.3A)
     if (exists("Mod.3B")) rm(Mod.3B)
     if (exists("Mod.3C")) rm(Mod.3C)
     if (exists("Mod.4A")) rm(Mod.4A)
     if (exists("Mod.4B")) rm(Mod.4B)
     if (exists("Mod.4C")) rm(Mod.4C)
     
     if (exists("Mod.1Ar")) rm(Mod.1Ar)
     if (exists("Mod.1Br")) rm(Mod.1Br)
     if (exists("Mod.1Cr")) rm(Mod.1Cr)
     if (exists("Mod.2Ar")) rm(Mod.2Ar)
     if (exists("Mod.2Br")) rm(Mod.2Br)
     if (exists("Mod.2Cr")) rm(Mod.2Cr)
     if (exists("Mod.3Ar")) rm(Mod.3Ar)
     if (exists("Mod.3Br")) rm(Mod.3Br)
     if (exists("Mod.3Cr")) rm(Mod.3Cr)
     if (exists("Mod.4Ar")) rm(Mod.4Ar)
     if (exists("Mod.4Br")) rm(Mod.4Br)
     if (exists("Mod.4Cr")) rm(Mod.4Cr)
     
     if (exists("Mod.1Ci")) rm(Mod.1Ci)
     if (exists("Mod.1C_2")) rm(Mod.1C_2)
     
     if (exists("Mod.3r")) rm(Mod.3r)
     if (exists("Mod.3Cb")) rm(Mod.3Cb)
     
     if (exists("Mod.LM")) rm(Mod.LM)
     if (exists("Mod.LMMx")) rm(Mod.LMMx)
   }
   
   FixModelCH<-function(Result1){
     ReinitMod()
     Mod.Fix<<-as.formula("CH ~ Prim*Gender");
     Result1$Interac <- interaction(Result1$Prim, Result1$Gender);
     Mod.Fixb<<-as.formula("CH ~ Interac");
     Mod.Fixc<<-as.formula("CH ~ Prim");
     Mod.Fixd<<-as.formula("CH ~ Gender");
     Mod.Lin<<-as.formula("CH ~ Prim*Gender + Error(Fam/Prim*Gender)");
     
     Mod.Ran.2A<<-as.formula(" ~ 1 | Fam / Prim / Gender");
     Mod.Ran.2B<<-as.formula(" ~ 1 | Fam / Prim");
     Mod.Ran.2C<<-as.formula(" ~ 1 | Fam / Gender");
     Mod.Ran.2D<<-as.formula(" ~ 1 | Fam");
     
     try(Mod.2A<<-lme(fixed=Mod.Fix, random = Mod.Ran.2A, data=Result1,na.action = na.omit));
     try(Mod.2B<<-lme(fixed=Mod.Fix, random = Mod.Ran.2B, data=Result1,na.action = na.omit));
     try(Mod.2C<<-lme(fixed=Mod.Fix, random = Mod.Ran.2C, data=Result1,na.action = na.omit));
     try(Mod.2D<<-lme(fixed=Mod.Fix, random = Mod.Ran.2D, data=Result1,na.action = na.omit));
     
     
     try(Mod.2Ar<<-lmer(CH ~ Prim*Gender + (1 | Fam / Prim / Gender), data=Result1,na.action = na.omit));
     try(Mod.2Br<<-lmer(CH ~ Prim*Gender + (1 | Fam / Prim), data=Result1,na.action = na.omit));
     try(Mod.2Cr<<-lmer(CH ~ Prim*Gender + (1 | Fam / Gender), data=Result1,na.action = na.omit));
     try(Mod.2Dr<<-lmer(CH ~ Prim*Gender + (1 | Fam), data=Result1,na.action = na.omit));
     
     
     try(Mod.3r<<-as.formula(paste(" ~ 1 | Fam",sep=" ")));
     try(Mod.3Cb<<-update(Mod.1C, random=Mod.3r));
     
     try(Mod.LM<<-lm(Mod.Fix, data=Result1,na.action = na.omit));
     try(Mod.LMMx<<-aov(Mod.Lin,data=Result1));
     #Mod.Ty<<-lm(VarWith~VarGrp)
   }
   
   fitlavNew<-function(Model,Data) {
     result<-list()
     fit<-sem(Model,data=Data,fixed.x=F)
     #result$Summ<-summary(fit,standardized=T,fit.measures=T)
     
     fitSt<-fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea","rmsea.ci.lower", "rmsea.ci.upper" ,"SRMR","ntotal"))
     
     result$Par<-parameterEstimates(fit,standardized=T)
     Val1=round(fitSt[[1]],4)
     Val2=fitSt[[2]]
     Val3=as.character(round(fitSt[[3]],4))
     Lap=fitSt[[3]];
     if (Lap>=0.05) Val3=paste0(Val3,"*")
     Val4=as.character(round(fitSt[[4]],4))
     Lap=fitSt[[4]];
     if (Lap>=0.95) Val4=paste0(Val4,"*")
     Val5=as.character(round(fitSt[[5]],4))
     Lap=fitSt[[5]];
     if (Lap<=0.06) Val5=paste0(Val5,"*");
     Val5b=paste0(Val5," (",round(fitSt[[6]],4),",",round(fitSt[[7]],4),")")
     Val6=as.character(round(fitSt[[8]],4))
     Lap=fitSt[[8]];
     if (Lap<=0.08) Val6=paste0(Val6,"*")
     Val7=fitSt[[9]]
     #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
     result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=Val3,CFI=Val4,RMSE=Val5b,SRMR=Val6,N=Val7)
     result$All<-fitMeasures(fit)
     result
   }
   
   fitlavPar<-function(fit,Prec=2) {
     result<-list()
     
     fitSt<-fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea","rmsea.ci.lower", "rmsea.ci.upper" ,"SRMR","ntotal","gfi","nnfi"))
     
     result$Par<-parameterEstimates(fit,standardized=T)
     #CHI
     Val1=round(fitSt[[1]],Prec)
     Val2=fitSt[[2]]
     ValDiv=as.character(round((fitSt[[1]]/fitSt[[2]]),Prec))
     if ((Val1/Val2)<2) ValDiv=paste0(ValDiv,"*")
     Val3=as.character(round(fitSt[[3]],Prec))
     Lap=fitSt[[3]];
     if (Lap>=0.05) Val3=paste0(Val3,"*")
     #CFI
     Val4=as.character(round(fitSt[[4]],Prec))
     Lap=fitSt[[4]]; if (Lap>=0.95) Val4=paste0(Val4,"*")
     #RMSEA
     Val5=as.character(round(fitSt[[5]],Prec))
     Lap=fitSt[[5]];if (Lap<=0.06) Val5=paste0(Val5,"*");
     Val5b=paste0(Val5," (",round(fitSt[[6]],Prec),",",round(fitSt[[7]],Prec),")")
     #SRMR
     Val6=as.character(round(fitSt[[8]],Prec))
     Lap=fitSt[[8]];if (Lap<=0.08) Val6=paste0(Val6,"*")
     #N
     Val7=fitSt[[9]]
     
     #GFI
     Val8=as.character(round(fitSt[[10]],Prec))
     Lap=fitSt[[10]]; if (Lap>=0.95) Val8=paste0(Val8,"*");
     #NNFI
     Val9=as.character(round(fitSt[[11]],Prec))
     Lap=fitSt[[11]]; if (Lap>=0.95) Val9=paste0(Val9,"*");
     
     #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
     result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=Val3,GFI=Val8,CFI=Val4,NNFI=Val9,RMSE=Val5b,SRMR=Val6,N=Val7,Ratio=ValDiv)
     result$All<-fitMeasures(fit)
     result
   }
   
   
   #' @title Dichotomize variables
   #' @name dicho
   #'
   #' @description Dichotomizes variables into dummy variables (0/1). Dichotomization is
   #'                either done by median, mean or a specific value (see \code{dich.by}).
   #'                Either single vectors, a complete data frame or a list of
   #'                variables can be dichotomized.
   #'
   #' @param x Variable (vector), \code{data.frame} or \code{list} of variables
   #'          that should be dichotomized
   #' @param dich.by Indicates the split criterion where a variable is dichotomized.
   #'          Must be one of the following values (may be abbreviated):
   #'          \describe{
   #'            \item{\code{"median"} or \code{"md"}}{by default, \code{x} is split into two groups at the median.}
   #'            \item{\code{"mean"} or \code{"m"}}{splits \code{x} into two groups at the mean of \code{x}.}
   #'            \item{numeric value}{splits \code{x} into two groups at the specific value. Note that the value is inclusive, i.e. \code{dich.by = 10} will split \code{x} into one group with values from lowest to 10 and another group with values greater than 10.}
   #'            }
   #' @param dich.val Deprecated, use \code{dich.by}.
   #' @param as.num Logical, if \code{TRUE}, return value will be numeric, not a factor.
   #' @param var.label Optional string, to set variable label attribute for the
   #'          dichotomized variable (see \code{\link{set_label}}). If \code{NULL}
   #'          (default), variable label attribute of \code{x} will be used (if present).
   #' @param val.labels Optional character vector (of length two), to set value label
   #'          attributes of dichotomized variable (see \code{\link{set_labels}}).
   #'          If \code{NULL} (default), no value labels will be set.
   #' @return A dichotomized factor (or numeric, if \code{as.num = TRUE}) variable (0/1-coded),
   #'           respectively a data frame or list of dichotomized factor (or numeric) variables.
   #'
   #' @note Variable label attributes (see, for instance, \code{\link{set_label}}) are preserved
   #'         (unless changes via \code{var.label}-argument).
   #'
   #' @examples
   #' data(efc)
   #' summary(efc$c12hour)
   #' # split at median
   #' table(dicho(efc$c12hour))
   #' # split at mean
   #' table(dicho(efc$c12hour, "mean"))
   #' # split between value lowest to 30, and above 30
   #' table(dicho(efc$c12hour, 30))
   #'
   #' # sample data frame, values from 1-4
   #' head(efc[, 6:10])
   #' # dichtomized values (1 to 2 = 0, 3 to 4 = 1)
   #' head(dicho(efc[, 6:10], 2))
   #'
   #' # dichtomize several variables in a list
   #' dummy <- list(efc$c12hour, efc$e17age, efc$c160age)
   #' dicho(dummy)
   #'
   #' # dichotomize and set labels
   #' frq(dicho(efc$e42dep, var.label = "Dependency (dichotomized)",
   #'           val.labels = c("lower", "higher")))
   #'
   #' @export
   dichoMM <- function(x,
                       dich.by = "median",
                       dich.val = -1,
                       as.num = FALSE,
                       var.label = NULL,
                       val.labels = NULL) {
     # check deprecated
     if (!missing(dich.val)) {
       .Deprecated("dich.by", old = "dich.val")
       dich.by <- dich.val
     }
     
     # check for correct dichotome types
     if (!is.numeric(dich.by) && dich.by != "median" && dich.by != "mean" && dich.by != "md" && dich.by != "m") {
       stop("argument `dich.by` must either be `median`, `mean` or a numerical value." , call. = FALSE)
     }
     
     if (is.matrix(x) || is.data.frame(x) || is.list(x)) {
       # get length of data frame or list, i.e.
       # determine number of variables
       if (is.data.frame(x) || is.matrix(x))
         nvars <- ncol(x)
       else
         nvars <- length(x)
       
       # dichotomize all
       for (i in 1:nvars) {
         x[[i]] <- dicho_helperMM(x[[i]], dich.by, as.num, var.label, val.labels)
       }
       return(x)
     } else {
       return(dicho_helperMM(x, dich.by, as.num, var.label, val.labels))
     }
   }
   
   
   #' @importFrom stats median
   dicho_helperMM <- function(x, dich.by, as.num, var.label, val.labels) {
     # do we have labels? if not, try to
     # automatically get variable labels
     if (is.null(var.label))
       varlab <- get_label(x)
     else
       varlab <- var.label
     
     # check if factor. factors need conversion
     # to numeric before dichtomizing
     if (is.factor(x)) {
       # non-numeric-factor cannot be converted
       if (is_num_fac(x)) {
         # try to convert to numeric
         x <- as.numeric(as.character(x))
       } else {
         # convert non-numeric factor to numeric
         # factor levels are replaced by numeric values
         x <- to_value(x, keep.labels = FALSE)
         message("Trying to dichotomize non-numeric factor.")
       }
     }
     # split at specific value
     if (is.numeric(dich.by)) {
       x <- ifelse(x <= dich.by, 0, 1)
     } else if (dich.by == "median" || dich.by == "md") {
       x <- ifelse(x <= stats::median(x, na.rm = T), 0, 1)
       # split at mean
     } else if (dich.by == "mean" || dich.by == "m") {
       x <- ifelse(x <= mean(x, na.rm = T), 0, 1)
     }
     
     if (!as.num) x <- as.factor(x)
     # set back variable labels
     if (!is.null(varlab)) x <- set_label(x, varlab)
     # set value labels
     if (!is.null(val.labels)) x <- set_labels(x, val.labels)
     return(x)
   }
 }
