
# Ultimas Funciones añadidas de Psicometría

frmMM<-function(x,d=2) formatC(round(x,d),d,format="f")

# Computa variables dos a dos según la posición (nv) que ocupan en el fichero de datos.
mkIdx<-function(nv) {
  nLev=length(nv)
  k <- 1; il <- vector(); jl <- vector()
  for (i in (1:(nLev-1))) {
    for (j in ((i+1):(nLev))) {
      il[k]<-nv[i]
      jl[k]<-nv[j]
      k<- k+1
    }
  }
  return(list(il=il,jl=jl))
}

mkIdx2<-function(nv1,nv2) {
  nLev1=length(nv1)
  nLev2=length(nv2)
  k <- 1; il <- vector(); jl <- vector()
  for (i in (1:(nLev1))) {
    for (j in (1:(nLev2))) {
      il[k]<-nv1[i]
      jl[k]<-nv2[j]
      k<- k+1
    }
  }
  return(list(il=il,jl=jl))
}

# Lo que se necesita de {textreadr}, un paquete que ya no funciona
read_rtf <- function(file, skip = 0, remove.empty = TRUE, trim = TRUE, ...) {
  
  
  ## use striprtf to read in the document
  text <- striprtf::read_rtf(file, ...)
  
  ## formatting
  if (isTRUE(remove.empty)) text <- text[!grepl("^\\s*$", text)]
  if (skip > 0) text <- text[-seq(skip)]
  if (isTRUE(trim)) text <- trimws(text)
  if (length(text) == 0) text <- ''
  
  text
}


# Lo que se necesita de {CMC: Cronbach-Mesbah Curve}, un paquete que ya no funciona
alpha.curve = function(x){
  data = x	
  n = nrow(data) #n. of obs
  k = ncol(data) #n. of item
  max.vec 			= c()
  which.max.vec = c()
  
  #Compute alpha for the complete dataset
  complete.alpha = alpha.cronbach(data)#,n,k)
  
  #Compute alpha max removing j elements from the complete dataset
  j = 1 #(1 element to remove)
  elements.to.remove.1	= seq(1:k)
  alpha.1				 		= c()
  for(i in 1:length(elements.to.remove.1)){
    data.reduced 	= data[,-elements.to.remove.1[i]]
    #n.reduced 		= nrow(data.reduced)
    #k.reduced		= ncol(data.reduced)
    alpha.1[i]		= alpha.cronbach(data.reduced)#,n.reduced,k.reduced)
  }
  max.vec[j]	 			= max(alpha.1)
  which.max.vec[j]   	= which.max(alpha.1)
  
  for (j in 2:(k-2)){
    elements.to.remove	= matrix(0, nrow=(k-j+1), ncol=(j-1))
    
    for(r in 1:length(which.max.vec)){
      elements.to.remove[,r] = matrix(rep(which.max.vec[r], k-j+1), ncol=1)
    }
    elements.to.remove = cbind(elements.to.remove,seq(1:k)[-which.max.vec[1:(j-1)]])
    
    alpha				 	= c()
    for(i in 1:nrow(elements.to.remove)){
      data.reduced 	= data[,-elements.to.remove[i,]]
      #n.reduced 		= nrow(data.reduced)
      #k.reduced		= ncol(data.reduced)
      alpha[i]			= alpha.cronbach(data.reduced)#,n.reduced,k.reduced)
    }
    
    max.vec[j]			= max(alpha)
    which.max.vec[j]	= elements.to.remove[which.max(alpha),j]
  }
  
  output = data.frame("N.Item" = seq(2,k),
                      "Alpha Max" = c(rev(max.vec),complete.alpha), 
                      "Removed Item" = c(colnames(data)[rev(which.max.vec)],"--"))
  
  plot(output[,1],output[,2],t="b",xlab="N.Item", ylab="Alpha Max")
  text(seq(2,k),output[,2],c(colnames(data)[rev(which.max.vec)],""),pos=3,cex=0.6)
  
  return(output)
}

# Lo que se necesita de {CMC: Cronbach-Mesbah Curve}, un paquete que ya no funciona
alpha.cronbach = function(x){
  
  n = nrow(x) #n. of subject
  k = ncol(x) #n. of items
  
  var.cov.mat 	= cov(x) * (n-1)/n
  num 				= sum(diag(var.cov.mat))
  den 				= 2*sum(var.cov.mat[lower.tri(var.cov.mat)])+num
  alpha 			= k / (k-1) * (1 - num/den)
  return(alpha)
}

CFA_EFA<-function(ResIAllp,DirPas,NumFacp) {
  ReSem<-list()
  ColDefine<-c("yellow","lightblue","green","orange")
  ParaFac<-c(1:ResIAllp$NumI)
  for (i in 1:ResIAllp$NFac) ParaFac[ResIAllp$Factor[[i]]]<-ColDefine[i]
  LosDatos<-ResIAllp$DTOmit
  
  efaOrig<-psych::fa(LosDatos,nfactors = NumFacp)
  AjModMas <- try(cfa(efa_to_cfa(efaOrig, threshold = 0.3), 
                      data=LosDatos,auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE,
                      auto.cov.y = TRUE, estimator="MLF",missing="ml"),silent = TRUE)
  
  AjModMas2 <- try(cfa(efa_to_cfa(efaOrig, threshold = 0.3), 
                      data=LosDatos,auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE,
                      auto.cov.y = TRUE, orthogonal = TRUE,std.lv = TRUE,estimator="MLF",missing="ml"),silent = TRUE)
  
  
  
  
  frps<-semPaths(AjModMas,"std",rotation = 4, intercept = FALSE, edge.label.cex = 1, edge.color="black",exoVar = FALSE,
                 fade = F, residuals = F, curvature = 2,title = F,sizeMan = 8,sizeMan2 = 3, sizeLat2 = 8,DoNotPlot=T)
  ExtrNm<-frps$Arguments$labels[1:24]
  matches<-regmatches(ExtrNm, gregexpr("[[:digit:]]+", ExtrNm))
  matches2<-as.numeric(unlist(matches))
  LosCol<-ParaFac[matches2]
  elSem<-semPaths(AjModMas,"std",rotation = 4, intercept = FALSE, edge.label.cex = .8, edge.color="black",exoVar = FALSE,
                  fade = F, residuals = F, curvature = 2,title = F,sizeMan = 8,sizeMan2 = 3, sizeLat2 = 8,
                  color = list(
                    lat = c("white"), 
                    man = LosCol),DoNotPlot=T)
  
  frps2<-semPaths(AjModMas2,"std",rotation = 4, intercept = FALSE, edge.label.cex = 1, edge.color="black",exoVar = FALSE,
                 fade = F, residuals = F, curvature = 2,title = F,sizeMan = 8,sizeMan2 = 3, sizeLat2 = 8,DoNotPlot=T)
  ExtrNm2<-frps2$Arguments$labels[1:24]
  matches2<-regmatches(ExtrNm2, gregexpr("[[:digit:]]+", ExtrNm2))
  matches2b<-as.numeric(unlist(matches2))
  LosCol2<-ParaFac[matches2b]
  elSem2<-semPaths(AjModMas2,"std",rotation = 4, intercept = FALSE, edge.label.cex = .8, edge.color="black",exoVar = FALSE,
                  fade = F, residuals = F, curvature = 2,title = F,sizeMan = 8,sizeMan2 = 3, sizeLat2 = 8,
                  color = list(
                    lat = c("white"), 
                    man = LosCol2),DoNotPlot=T)
  
  
  
  NmFileGrp<-paste0(DirPas,ResIAllp$Task,"/EFAGraficos",NumFacp,"Fac.pdf")
  pdf(NmFileGrp)
    plot(elSem)
    plot(elSem2)
  dev.off()
  ReSem$NoOrtog<-elSem
  ReSem$Ortog<-elSem2
  ReSem
}

DensitMM <-function(DataP,TitleP="3 Distribución Dificultad a través ítems") {
  d <- density(DataP)
  hist(DataP, breaks = "FD",col = "lightblue",xlab="lbl",prob=T,main=TitleP)
  polygon(d, col=adjustcolor("gray",alpha.f=0.5))
}

ImprKbl <-function(mensa,varTo) {
  cat(mensa, "\n");
  print((kbl(as.data.table(cbind(Numero=1:nrow(varTo), Variables=varTo)), format = "markdown",longtable=T,booktabs=T)))
}

ImprKbl2 <-function(mensa,varTo) {
  cat(mensa, "\n");
  print(((as_tibble(cbind(Numero=1:nrow(varTo), Variables=varTo)))))
}
condisc <- function(x){
  std.loadings<- inspect(x, what="std")$lambda
  #std.loadings
  std.loadings[std.loadings==0] <- NA
  #std.loadings
  std.loadings <- std.loadings^2
  #std.loadings
  ave <- colMeans(std.loadings, na.rm=TRUE)
  #ave
  #factor correlation matrix
  fcor <- lavInspect(x, "cor.lv")
  #fcor
  sqfcor <- fcor^2
  #sqfcor
  list(Squared_Factor_Correlation=round(sqfcor, digits=3),
       Average_Variance_Extracted=round(ave, digits=3))
}

utf8Tortf <- function(x){
  
  stopifnot(length(x) == 1 & "character" %in% class(x) )
  
  x_char <- unlist(strsplit(x, ""))
  x_int <- utf8ToInt(x)
  x_rtf <- ifelse(x_int <= 255, x_char,
                  ifelse(x_int <= 32768, paste0("\\uc1\\u", x_int,"?"),
                         paste0("\\uc1\\u-", x_int - 65536, "?") ) )
  
  paste0(x_rtf, collapse = "")
  
}

InsCarga <- function(Required_Packages)
{
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];
  if(length(Remaining_Packages)) 
  {
    install.packages(Remaining_Packages, repos="https://cloud.r-project.org");
  }
  for(package_name in Required_Packages)
  {
    library(package_name,character.only=TRUE,quietly=TRUE);
  }
}

cor_to_p <- function(cor, n, method = "pearson") {
  
  # Statistic
  if (method == "kendall") {
    warning("Estimation for Kendall's correlation is not perfectly correct. Help us to improve it.")
    statistic <- (3 * cor * sqrt(n * (n - 1))) / sqrt(2 * (2 * n + 5))
  } else {
    statistic <- cor * sqrt((n - 2) / (1 - cor^2))
  }
  
  # p-value
  if (method == "kendall") {
    p <- 2 * stats::pnorm(-abs(statistic))
  } else {
    p <- 2 * stats::pt(-abs(statistic), df = n - 2)
  }
  
  list(p = p, statistic = statistic)
}

#Data=DTVal;Ordin=c(2:7);Number=1
CorrOrdinal <-function(Data,Ordin=0,Number) {
  require(parallel);require(data.table);require(polycor);require(psych)
  CorrF<-list()
  DTOmit=na.omit(Data)
  X<-mclapply(Ordin, function(i) {
    polycor::polyserial(x=c(DTOmit[,Number,with=F][[1]]), y=c(DTOmit[,i,with=F][[1]]))
  });
  Corr=do.call(rbind,setNames(X,names(DTOmit)[Ordin]))
  
  Corr2=cor_to_p(Corr,n=nrow(na.omit(DTVal)),method = "polychoric")$p
  CorrF$r<-Corr
  CorrF$p<-Corr2
  CorrF
}

#SelVal = ResSel2;DirPas = NmDirActual;ResIAllp = ResIAll;Wv = 2
EstimaVal<-function(SelVal,DirPas,ResIAllp,Wv=0) {
  Tasks3<-names(SelVal)
  co <- cor(SelVal,use='pairwise.complete.obs')
  row.names(co)<-Tasks3
  colnames(co)<-Tasks3
  cor2<-corstars2(SelVal, result="none",removeTriangle = "null")
  
  CorSel<-co[,1][-1]
  CorSel2<-cor2[,1][-1]
  CorSelN<-data.frame( Escalas=Tasks3[-1],Correlacion=CorSel)
  dotchart2(CorSel,xlim=c(-1,1),dotsize=1.5,auxdata=as.character(CorSel2),labels =CorSelN$Escalas)
  abline(v=-.4,lty=3,col="red");abline(v=.4,lty=3,col="red");abline(v=.0,lty=3,col="black")
  
  NmFic<-paste0(DirPas,"ValidezGraficos.pdf")
  pdf(NmFic, width=8, height=8)
  dotchart2(CorSel,xlim=c(-1,1),dotsize=1.5,auxdata=as.character(CorSel2),labels =CorSelN$Escalas)
  abline(v=-.4,lty=3,col="red");abline(v=.4,lty=3,col="red");abline(v=.0,lty=3,col="black")
  dev.off()
  ResF<-data.frame(Escalas=Tasks3[-1],Correlacion=round(CorSel,3),p=CorSel2)
  
  rtffile <- RTF(file = paste0(DirPas,"Validez.doc"),height=8.5, width=11)
  addParagraph(rtffile,"Resumen de Análisis Validez:"); addNewLine(rtffile,2);
  addTable(rtffile, ResF);
  done(rtffile)
  
  if (Wv>0) {
    DTVal<<-data.table(ResIAllp[[1]]$ResNewAllVal[,1],ResIAllp[[Wv]]$DTOmit)
    CorrItemsSd=CorrOrdinal(DTVal,Ordin=c(2:length(DTVal)),Number=1)
    mystars <- ifelse(CorrItemsSd$p < .01, "**", ifelse(CorrItemsSd$p < .05, "* ", "  "))    
    cor2It<-paste( frmMM(CorrItemsSd$r,2), mystars, sep="")
    
    CorSel<-c(t(CorrItemsSd$r),co[,1][-1])
    CorSel2<-c(cor2It,cor2[,1][-1])
    CorSelN<-data.frame( Escalas=c(names(DTVal)[-1],Tasks3[-1]),Correlacion=CorSel)
    dotchart2(CorSel,xlim=c(-1,1),dotsize=1.5,auxdata=as.character(CorSel2),labels =CorSelN$Escalas)
    abline(v=-.4,lty=3,col="red");abline(v=.4,lty=3,col="red");abline(v=.0,lty=3,col="black")
    
    NmFic<-paste0(DirPas,"ValidezMasGraficos.pdf")
    pdf(NmFic, width=8, height=8)
    dotchart2(CorSel,xlim=c(-1,1),dotsize=1.5,auxdata=as.character(CorSel2),labels =CorSelN$Escalas)
    abline(v=-.4,lty=3,col="red");abline(v=.4,lty=3,col="red");abline(v=.0,lty=3,col="black")
    dev.off()
    ResF<-data.frame(Escalas=c(names(DTVal)[-1],Tasks3[-1]),Correlacion=round(CorSel,3),p=CorSel2)
    rtffile <- RTF(file = paste0(DirPas,"ValidezMas.doc"),height=8.5, width=11)
    addParagraph(rtffile,"Resumen de Análisis Validez:"); addNewLine(rtffile,2);
    addTable(rtffile, ResF);
    done(rtffile)
  }
  ResF
}

IDnMM.21<-function(Data,Ordin=0,Number,Categ=0) {
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
  names(IDnF)<-c("Items","Corr.IDn","Corr.Pearson")
  return(IDnF)
} 

ProcItAlt.21<-function(Tk=1,Tasks,LosNm,DTNm,EscalaL=c("1 Nada","2 Algo","3 Medio","4 Bastante","5 Totalmente"),
                       EstrFac,NmEscalas,Incr1=T,Recodif=F,SigItems=ResIt$nm) {
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
  ResIt$nm<-names(DTNm[,TkO,with=F])
  ResIt$nm2<-as.vector(SigItems)
  ResIt$lbl<-paste0("Tarea ",Tk,"_",as.vector(DTNm[TkO2[1],3][[1]]),"_Factor ",as.vector(DTNm[TkO2[1],4][[1]]))
  ResIt$NFac<-length(EstrFac)
  if (ResIt$NFac>0) {ResIt$Factor<-EstrFac; ResIt$NmFactor<-NmEscalas}
  if (ResIt$NFac==0) {ResIt$NFac<-1}
  
  ResIt$Validez<-Tasks[-1]
  
  ResNewAll<-data.table(NA)
  ModLav<-list()
  ModLavT<-list()
  NumF<-ResIt$NFac
  if (NumF>1) FactoresT<-c(paste0(ResIt$Task,".F",1:ResIt$NFac))
  NmTT<-paste0(ResIt$Task,"TT")
  
  Sel<-ResIt$TkO
  ResfinalFi<-DTNm[,Sel,with=F]
  DTOmit<-data.table(na.omit(ResfinalFi))
  if (min(DTOmit)==0&Incr1) DTOmit<-DTOmit+1
  ResIt$EscalaVal<-c(min(DTOmit):max(DTOmit))
  if (Recodif) {
    DTOmit2<-copy(DTOmit);
    DTOmit2$Name<-paste0("s",c(1:nrow(DTOmit)))
    DTOmit = reshape2::dcast(
      dplyr::mutate(
        reshape2::melt(DTOmit2,id.var="Name"),
        value=plyr::mapvalues(
          value, c(min(DTOmit):max(DTOmit)),Recodif)),
      Name~variable)
    DTOmit<-as.data.table(DTOmit[-1])
  }
  
  
  ResNewF<-data.table(rowSums(DTOmit,na.rm = F))
  #ResNewF<-data.table(rowSums(DT[,grep(ResIAll[[Tkk]]$Task,names(DT),value=F),with=F],na.rm = F))
  #ResNewF<-data.table(ResfinalFi[ ,rowSums(.SD, na.rm = F)])
  #MSPSS.TT=rowSums(DT[,grep(Tasks[2],LosNm,value=F),with=F],na.rm = F)
  ModLavT<-paste0(ModLavT, "\n", paste0(NmTT," =~ ",paste0(as.vector(ResIt$nm),collapse = " + ")))
  
  if (NumF>1) {
    for (iF in c(1:NumF)) {
      Nmm<-FactoresT[iF]
      SelnM<-paste0(ResIt$Task,"_",ResIt$Factor[[iF]])
      ModLav<-paste0(ModLav, "\n", paste0(Nmm," =~ ",paste0(SelnM,collapse = " + ")))
      ResNew<-data.table(rowSums(DTOmit[,SelnM,with=F],na.rm = F))
      names(ResNew)<-Nmm
      ResNewF<-data.table(ResNewF,ResNew)
    }
    ModLavT<-paste0(ModLavT,ModLav)
    
    ModLavT2<-paste0(ModLav, "\n",paste0(NmTT," =~ ",paste0(FactoresT,collapse = " + ")))
  }
  if (NumF<=1) ModLav<-ModLavT
  
  names(ResNewF)<-c(paste0(ResIt$Task,".TT"),names(ResNewF)[-1])
  
  ResIt$ModLav<-ModLav
  ResIt$ModLavT<-ModLavT
  ResIt$ModLavT2<-ModLavT2
  
  ResIt$DTOmit<-DTOmit
  ResIt$ResNewAll<-ResNewF
  
  SelV<-grep(ResIt$Validez,LosNm,value=F)
  ResfinalFiV<-DTNm[,SelV,with=F]
  ResNewV<-data.table(rowSums(ResfinalFiV))
  names(ResNewV)<-ResIt$Validez
  ResIt$ResNewAllVal<-cbind(ResNewF,ResNewV)
  options(warn=0)
  ResIt
}


# ResIAllp = ResIAll[[1]];NFExtr=3;PathRespP=PathRes
# TyExI=2; TyRttI=3; TyExI2=2; TyRttI2=4;TypeCorrInic=5; TyMod=2
NumFac.21 <-function(ResIAllp, TyMod=1,NFExtr=1, TyExI=2, TyRttI=3, TyExI2=2, TyRttI2=4,TypeCorrInic=5, TypeCorrDT=4,PathRespP="") {
  
  # Preliminares
  # Por defecto
  # TyMod=1 es "Componentes"
  # NFExtr=6      # N?? Factores, lo marca todo ????Ojo!!
  # TyExI<-2 es "mle"
  # TyRttI<-3 es "oblimin"
  # TyExI2<-2  es "ml"
  # TyRttI2<-4 es "quartimin"
  # TypeCorrInic<-5 1 es Pearson y 5 es Heterog??nea
  
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
  TyEstrInAxis<-c("component","maxr","ginv","multiple") #Exclusivamente para los tipos ipa ?? nipa
  # Four different choices of initial communalities are given:
  # components, maximum correlation, generalized inverse and multiple correlation
  
  TypCorr2 = c("cor", "cov", "tet", "poly", "mixed")
  # How to find the correlations: "cor" is Pearson", "cov" is covariance, "tet" is tetrachoric, 
  # "poly" is polychoric, "mixed" 
  
  TypData=ResIAllp$Task
  NmDirActual<-paste0(PathRespP,TypData,"/")
  if (!dir.exists(NmDirActual)) {dir.create(NmDirActual)}
  DTOmit<-ResIAllp$DTOmit
  CrT=TypeCorrInic;CrTs=1;CrTCD=1
  coF<-OnlyCorr(DTOmitp = DTOmit,Ordin=c(length(DTOmit)),TypData=TypData,DirP=PathRespP)
  co<-coF$v
  CrTs=1;CrTCD=1 # Para fijar el tipo de Corr en las simulaciones
  QuanSim=0.50
  DelCas=TRUE # True: "Delete cases listwise"
  Repla=FALSE
  Cases<-TypCase[2]
  n <- ncol(DTOmit)
  ncase <- nrow(DTOmit)
  ncase2 <- ncase
  Seed=NULL
  repi=1000
  Model=TypMod[TyMod]
  Sim=TypSim[2]
  EstaSim=TypEsta[2]
  cont <- 0
  
  Data<-data.frame(DTOmit)
  mdata <- Data
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
  DelCasCD=TRUE # True: "Delete cases listwise"
  UntilSig=TRUE
  F.Max=20; N.Pop=10000;N.Samples=500; Alpha = 0.3
  if (Model==TypMod[1]) compPP=TRUE else compPP=FALSE
  Corr=TypCorr[CrT];CorrSim=TypCorr[CrTs];CorrCD=TypCorr[CrT]
  CorrDif=TypCorrDif[CrT]
  
  Rott=TypRot[TyRttI]   # Tipo de Rotaci??n Inicial
  Diago=FALSE
  ExtrMet=TypExtr[TyExI] # Tipo de Extracci??n Inicial
  #NFExtr=6
  
  # C.2.1) Cattel Scree Test en revisi??n pues aporta resultados extra??os para CFA
  # De momento almaceno las soluciones que aporta pero sin integrarlas en el
  # conjunto, las sustituyo por la versi??n simplificada que viene a continuaci??n
  ptm=proc.time()
  if (n >= 3) {
    set.seed(Seed)
    evpea <- matrix(NA, ncol = n, nrow = repi)
    evpea_b <- matrix(NA, ncol = n, nrow = repi)
    # Eigenvalues from components or factors
    # if (Model==TypMod[1]) {evt <- eigen(co, only.values = T) }
    # if (Model==TypMod[2]) {evt <- eigen(co - ginv(diag(diag(ginv(co)))), only.values=T) }
    evt <- coF$evt$values
    # Simulations
    
    # Simulation Type standardized normal
    if (Sim==TypSim[1]) {
      co_used <- "Pearson"
      for (k in 1:repi) { 
        y <- mvrnorm(ncase, rep(0, n), diag(1,n), empirical = FALSE)
        corY<-EstimaCorr(data.frame(y),scalP = scal,CasesP=Cases,TCor = 1,wT=F,CompP=compPP)
        if (Model==TypMod[2]) { corY <- corY - ginv(diag(diag(ginv(corY)))) }
        evpea[k, ] <- corY$evt$values
        # corY <- cor(y, use=Cases, method="pearson") 
        # if (Model==TypMod[2]) { corY <- corY - ginv(diag(diag(ginv(corY)))) }
        # ev <- eigen(corY, only.values = TRUE)
        # evpea[k, ] <- ev$values 
      }
    }
    
    # Simulation Type Permutation
    if (Sim==TypSim[2]) {
      sdata <- matrix (0,ncol = ncol(mdata), nrow =ncase_list)
      if (!DelCas) {sdata <- mdata}
      if (DelCas) {
        mdata2 <- as.matrix(mdata)
        lcor <- 0 
        for (i in 1:ncase2) {
          ind <- 0; j <- 0
          while (j<n && ind ==0) { j <- j+1; if (is.na(mdata[i,j])) { ind <- 1 } }
          if (ind==0) { lcor <- lcor+1; sdata[lcor,] <- mdata2[i,] } 
        } 
      }
      
      for (k in 1:repi) { 
        perm <- apply(sdata[,2:n], 2, sample, replace = Repla)
        perm <- data.frame(sdata[,1],perm)
        
        corYF<-EstimaCorr(perm,scalP = scal,CasesP=Cases,TCor = CrTs,wT=F,CompP=compPP)
        corY<-corYF$v
        co_used<-CorrSim
        evpea[k, ] <- corYF$evt$values
      }
    }
    
    mevpea <- sapply(as.data.frame(evpea), mean)
    qevpea <- sapply(as.data.frame(evpea), quant)
    ap <- list(eigen = data.frame(mevpea, qevpea) )  
    
    # parallel analysis and scree
    ss <- EstaSim
    if (EstaSim==TypEsta[1]) {nS <- nScree(evt, aparallel=ap$eigen$mevpea); s <- "ap$eigen$mevpea"}
    if (EstaSim==TypEsta[2]) {nS <- nScree(evt, aparallel=ap$eigen$qevpea); s <- "ap$eigen$qevpea"}
    if (Model==TypMod[2]) { 
      criteria <- mean(evt)
      nS$Components$nkaiser <- sum(evt >= rep(criteria, n))
      p.vec <- which(evt >= ap$eigen$qevpea) 
      nS$Components$nparallel <- sum(p.vec == (1:length(p.vec)))
    }
    
    title="Parameters for Parallel Analysis";print(title);
    junto <- data.frame(repi, Model, Sim, ss, Corr, co_used)
    names(junto) <- c("Number of Samples","Model","Data","Measure", "Correlation matrix","Matrix for simulations")
    print(junto)
    #kable(junto)
    title="Distribution of the eigenvalues computed";print(title);
    junto <- data.frame(paste("",1:n, rep=""), ap$eigen)
    names(junto) <- c("Order", "Mean", "Quantile selected")
    print(junto)
    NmFileGrp<-paste0(NmDirActual,"1 Scree1_Distrib of the eigenvalues computed_",TypData,"_",Corr,".txt")
    write.table(junto,file=NmFileGrp)
    #kable(junto,format = "markdown")
    title="Number of components/factors to retain according to different rules";print(title);
    junto <- data.frame(nS$Components)
    nmjunto <- c("Optimal coordinates","Acceleration factor", "Parallel analysis","Kaiser rule")
    names(junto)<-nmjunto
    print(junto)
    #kable(junto,format = "markdown")
    NmFileGrp<-paste0(NmDirActual,"1 Scree2_Number To Retain_",TypData,"_",Corr,".txt")
    write.table(junto,file=NmFileGrp)
    
    # Desactivado de momento
    # juntoF1<-junto 
    # Desactivado de momento
    
    #xtable(junto)
    title="Data linked to the different rules";print(title);
    junto <- data.frame(nS$Analysis)
    names(junto) <- c("Eigenvalues", "ProportionVariance", "Cumulative", "ParallelAnalysis",
                      "PredictedEigenvalues", "OC", "AccelerationFactor", "AF")
    print(junto)
    #kable(junto,format = "markdown")
    NmFileGrp<-paste0(NmDirActual,"1 Scree3_Data linked to the different rules_",TypData,"_",Corr,".txt")
    write.table(junto,file=NmFileGrp)
    
    # Desactivado de momento
    #juntoF1b<-junto
    # Desactivado de momento
    
    #quartz("Parallel analysis")
    if (Model==TypMod[1]) {
      if (Sim==TypSim[1]) {
        plotnScree(nS, ylab = "Eigenvalues", xlab = "Components",
                   main = "Parallel analysis on random uncorrelated standardized normal")
        
        NmFileGrp<-paste0(NmDirActual,"1 Scree1_",TypData,"_",Corr,".pdf")
        pdf(NmFileGrp)
        plotnScree(nS, ylab = "Eigenvalues", xlab = "Components",
                   main = "Parallel analysis on random uncorrelated standardized normal")
        dev.off()
      }
      if (Sim==TypSim[2]) {
        plotnScree(nS, ylab = "Eigenvalues", xlab = "Components",
                   main = "Parallel analysis on data permutation") 
        
        NmFileGrp<-paste0(NmDirActual,"1 Scree1_",TypData,"_",Corr,".pdf")
        pdf(NmFileGrp)
        plotnScree(nS, ylab = "Eigenvalues", xlab = "Components",
                   main = "Parallel analysis on data permutation") 
        dev.off()
        
      }
    }
    
    if (Model==TypMod[2]) {
      if (Sim==TypSim[1]) {
        plotnScree(nS, ylab = "Eigenvalues", xlab = "Factors", 
                   main = "Parallel analysis on random uncorrelated standardized normal") 
        
        NmFileGrp<-paste0(NmDirActual,"1 Scree1_",TypData,"_",Corr,".pdf")
        pdf(NmFileGrp)
        plotnScree(nS, ylab = "Eigenvalues", xlab = "Factors", 
                   main = "Parallel analysis on random uncorrelated standardized normal") 
        dev.off()
      }
      if (Sim==TypSim[2]) {
        plotnScree(nS, ylab = "Eigenvalues", xlab = "Factors", 
                   main = "Parallel analysis on data permutation") 
        
        NmFileGrp<-paste0(NmDirActual,"1 Scree1_",TypData,"_",Corr,".pdf")
        pdf(NmFileGrp)
        plotnScree(nS, ylab = "Eigenvalues", xlab = "Factors", 
                   main = "Parallel analysis on data permutation") 
        dev.off()
      }
    }
    
  }
  print(proc.time() - ptm)
  # Cattel Scree Test
  
  # C.2.1b) Cattel Scree Test simplificado q de momento sustituye al anterior
  
  evC <- eigen(co) # get eigenvalues
  apC <- parallel(subject=ncase_list,var=ncol(DTOmit),rep=100,cent=.05)
  if (Model==TypMod[2]) nS <- nScree(x=evC$values, aparallel=apC$eigen$qevpea,model = "factors")
  if (Model==TypMod[1]) nS <- nScree(x=evC$values, aparallel=apC$eigen$qevpea,model = "components")
  
  title="Number of components/factors to retain according to different rules";print(title);
  junto <- data.frame(nS$Components)
  nmjunto <- c("Optimal coordinates","Acceleration factor", "Parallel analysis","Kaiser rule")
  names(junto)<-nmjunto
  print(junto)
  #kable(junto,format = "markdown")
  NmFileGrp<-paste0(NmDirActual,"1new Scree2_Number To Retain_",TypData,"_",Corr,".txt")
  write.table(junto,file=NmFileGrp)
  juntoF1<-junto
  
  title="Data linked to the different rules";print(title);
  junto <- data.frame(nS$Analysis)
  names(junto) <- c("Eigenvalues", "ProportionVariance", "Cumulative", "ParallelAnalysis",
                    "PredictedEigenvalues", "OC", "AccelerationFactor", "AF")
  print(junto)
  #kable(junto,format = "markdown")
  NmFileGrp<-paste0(NmDirActual,"1new Scree3_Data linked to the different rules_",TypData,"_",Corr,".txt")
  write.table(junto,file=NmFileGrp)
  juntoF1b<-junto
  
  NmFileGrp<-paste0(NmDirActual,"1new Scree1_",TypData,"_",Corr,".pdf")
  pdf(NmFileGrp)
  nFactors::plotnScree(nS)
  dev.off()
  
  
  # C.2.2) Velicer Map  
  ptm=proc.time()
  #co<-data.frame(co)
  #as.matrix(co)
  ev <- eigen(co, symmetric=TRUE)
  load2 <- ev$vectors%*%sqrt(diag(ev$values))
  f <- rep(0,(n-1)); fq <- rep(0,(n-1))
  som <- sum(co^2); somq <- sum(co^4)
  m <- n*(n-1); f[1] <- (som-n)/m; fq[1] <- (somq-n)/m
  for (i in 1:(n-2)) { 
    b <- load2[,1:i]; cc <- co-(b%*%t(b))
    cc2<-as.matrix(cc)
    d <- diag(1/sqrt(diag(cc2))); ea <- d%*%cc2%*%d      #partial correlation matrix controlling components
    som <- sum(ea^2); somq <- sum(ea^4)
    f[i+1] <- (som-n)/m; fq[i+1] <- (somq-n)/m 
  }
  
  matt <- replace(f, f == "NaN", 2)
  matt2 <- replace(fq, fq == "NaN", 2)
  fm <- min(matt); fqm <- min(matt2)
  for (i in 1:(n-1)) { if (f[i]==fm) { fma <- i-1; break } }
  for (i in 1:(n-1)) { if (fq[i]==fqm) { fqma <- i-1; break } }
  
  junto <- data.frame("Squared average partial correlations"=f[1:(n-1)],
                      "4th average partial correlations"=fq[1:(n-1)],row.names=0:(n-2))
  title="Velicer's MAP values";print(title)
  print(junto)
  
  title="Velicer's Minimum Average Partial Test"
  junto1<-paste0("Squared MAP: ", "Velicer's Minimum= ",round(fm,6),"; Velicer's Components to retain: ",round(fma,6))
  junto2<-paste0("4th power MAP: ", "Velicer's Minimum= ",round(fqm,6),"; Velicer's Components to retain: ",round(fqma,6))
  print(c(junto1,junto2))
  juntoAl<-cbind(juntoF1,"Velicer's Squared MAP"=round(fma,6),"Velicer's 4th power MAP"=round(fqma,6))
  NmFileGrp<-paste0(NmDirActual,"2 Velicer Minimum APT_",TypData,"_",Corr,"txt")
  write.table(c(junto1,junto2),file=NmFileGrp)
  juntoF2<-c(junto1,junto2)
  print(proc.time() - ptm)
  # Velicer Map
  
  # C.2.3) Very Simple Structure
  ptm=proc.time()
  #quartz("VSS")   
  
  v <- VSS(co, n = NFExtr, rotate = Rott, diagonal = Diago, fm = ExtrMet, n.obs=ncase, plot=TRUE)
  
  NmFileGrp<-paste0(NmDirActual,"3 VSS Graph_",TypData,"_",Corr,".pdf")
  pdf(NmFileGrp)
  v <- VSS(co, n = NFExtr, rotate = Rott, diagonal = Diago, fm = ExtrMet, n.obs=ncase, plot=TRUE)
  dev.off()
  
  junto <- data.frame("Complexity 1"=v$cfit.1, "Complexity 2"=v$cfit.2)
  title="Very Simple Structure";print(title)
  print(junto)
  NmFileGrp<-paste0(NmDirActual,"3 VSS 1_Very Simple Structure_",TypData,"_",Corr,".txt")
  write.table(junto,file=NmFileGrp)
  nu <- NFExtr
  vss1 <- 0
  for (i in 1:nu) {vss1 <- vss1+1;if ( v$cfit.1[i]==max(v$cfit.1) ) { break }}
  vss2 <- 0
  for (i in 1:nu) {vss2 <- vss2+1;if ( v$cfit.2[i]==max(v$cfit.2) ) { break }}
  
  
  junto<-paste0("VSS Values: ", "\n",
                "Rotation= ","",Rott,"\n",
                "Factoring method= ",ExtrMet,"\n",
                "Max VSS complexity 1= ",round(max(v$cfit.1),6),"\n",
                "N factors complexity 1= ",round(vss1,6),"\n",
                "----------------","\n",
                "Max VSS complexity 2= ",round(max(v$cfit.2),6),"\n",
                "N factors complexity 2= ",round(vss2,6));
  
  juntoAl<-cbind(juntoAl,"VSS complexity 1"=round(vss1,6),"VSS complexity 2"=round(vss2,6))
  
  title="Very Simple Structure (Number of factors)";
  cat(title,junto);
  NmFileGrp<-paste0(NmDirActual,"3 VSS 2_Parameteres_",TypData,"_",Corr,".txt")
  sink(NmFileGrp); cat(title,junto); sink()
  juntoF3<-junto
  print(proc.time() - ptm)
  # Very Simple Structure
  
  # C.2.4) More Number of Factors Comparison Data
  ptm=proc.time()
  sdata <- matrix (0,ncol = ncol(mdata), nrow =ncase_list) 
  if (DelCasCD) {
    mdata2 <- as.matrix(mdata)
    lcor <- 0 
    for (i in 1:ncase2) {
      ind <- 0; j <- 0
      while (j<n && ind ==0) {j <- j+1; if (is.na(mdata[i,j])) { ind <- 1 } }
      if (ind==0) { lcor <- lcor+1; sdata[lcor,] <- mdata2[i,] } 
    } 
  }
  if (!DelCasCD) { sdata <- mdata } 
  #print(sdata)
  # Matrix to store each variable score distribution
  Distributions <- matrix(0, nrow = N.Pop, ncol = dim(sdata)[2])
  # Generate distribution for each variable
  b2 <- matrix(0, nrow = dim(sdata)[2], ncol = 1)
  for (i in 1:dim(sdata)[2]) {
    bb <- sort(sample(sdata[,i], size = N.Pop, replace = T) )
    b2[i] <- length(bb)
    ac <- N.Pop-b2[i]
    if (b2[i] < N.Pop) {for (j in 1:ac) {qw <- sample(1: (b2[i] + j),1); bb <- insert (bb, qw, NA) }} 
    Distributions[,i] <- as.matrix(bb) 
  }
  #print(Distributions)
  
  #DataP = sdata; F.Max =  20; N.Samples = 500; Alpha = 0.3; scalP=scal;CasesP =Cases;  TCor = CrTCD
  ysa<-EFA.Comp.Data(DataP = sdata, F.Max = F.Max, N.Pop=N.Pop,N.Samples = N.Samples, Alpha = Alpha,
                     scalP=scal ,CasesP =Cases,  TCor = CrTCD,UntilSig=UntilSig,Distributions=Distributions)
  
  NmFileGrp<-paste0(NmDirActual,"4 CD Graph_",TypData,"_",Corr,".pdf")
  pdf(NmFileGrp)
  plot(x = 1:ysa$x.max, y = ysa$ys, ylim = c(0, max(ysa$ys)), xlab = "Factor", 
       ylab = "RMSR Eigenvalue", type = "b", main = "Fit to Comparison Data")
  abline(v = ysa$nf, lty = 3)
  dev.off()
  
  
  title="Fit to Comparison Data";
  junto <- data.frame(paste("",1:ysa$x.max, rep = "factor"), round(ysa$ys,4), round(ysa$sig2,4))
  names(junto) <- c("Number of factors","RMSR Eigenvalue","p-value")
  print(title);print(junto) 
  NmFileGrp<-paste0(NmDirActual,"4 CD 1_Fit to Comparison Data_",TypData,"_",Corr,".txt")
  write.table(junto,file=NmFileGrp)
  
  title="Comparison Data";
  junto <- data.frame("Correlations"=TypCorr[CrTCD],"Number of factors to retain"=ysa$nf)
  juntoAl<-cbind(juntoAl,"Comparison Data"=ysa$nf)
  print(title);print(junto) 
  
  NmFileGrp<-paste0(NmDirActual,"4 CD 2_Comparison Data_",TypData,"_",Corr,".txt")
  write.table(junto,file=NmFileGrp)
  juntoF4<-junto
  print(proc.time() - ptm)
  # More Number of Factors Comparison Data
  
  # C.2.5) fa.parallel Revelle
  title="fa.parallel Revelle";
  # NF1<-fa.parallel(data.frame(DTOmit),cor = 'poly') No funciona
  NF1<-fa.parallel(DTOmit)
  juntoF1b<-cbind(juntoF1b,fa_parallelFA=NF1$fa.values,fa_parallelPC=NF1$pc.values)
  
  print(title);print(NF1) 
  if (TyMod==1) juntoAl<-cbind(juntoAl,"Parallel Analysis 2 Components"=NF1$ncomp)
  if (TyMod==2) juntoAl<-cbind(juntoAl,"Parallel Analysis 2 Factors"= NF1$nfact)
  
  NmFileGrp<-paste0(NmDirActual,"5 fa_parallel Revelle_",TypData,"_",Corr,".txt")
  #write.table(NF1,file=NmFileGrp)
  sink(NmFileGrp); print(NF1); sink()
  
  NmFileGrp<-paste0(NmDirActual,"5 fa_parallel Revelle.pdf")
  pdf(NmFileGrp)
  fa.parallel(DTOmit)
  dev.off()
  
  # C.2.6) VSS Revelle
  title="VSS/nfactor Revelle";
  NF2<-vss(DTOmit,n = ncol(DTOmit))
  print(title);print(NF2)
  
  NmFileGrp<-paste0(NmDirActual,"6 VSS Revelle_",TypData,"_",Corr,".txt")
  sink(NmFileGrp); print(NF2); sink()
  
  NmFileGrp<-paste0(NmDirActual,"6 VSS Revelle.pdf")
  pdf(NmFileGrp)
  nfactors(DTOmit,n = ncol(DTOmit))
  dev.off()
  
  juntoF1b<-cbind(juntoF1b,map=NF2$map,NF2$vss.stats)
  NF3<-nfactors(DTOmit,n = ncol(DTOmit))
  
  juntoAl<-cbind(juntoAl,
                 VSS1=which.max(NF3$vss.stats$cfit.1),
                 VSS2=which.max(NF3$vss.stats$cfit.2),
                 MAP2=which.min(NF3$map),
                 BIC=which.min(NF3$vss.stats$BIC),
                 eBIC=which.min(NF3$vss.stats$eBIC),
                 SABIC=which.min(NF3$vss.stats$SABIC)
  )
  
  # C.2.7 parameters library
  NF4<-parameters::n_factors(DTOmit,package='all')
  title="parameters Library";
  print(title);print(NF4)
  
  NmFileGrp<-paste0(NmDirActual,"7 Parameters Library_",TypData,"_",Corr,".txt")
  sink(NmFileGrp); print(NF4); sink()
  
  NmPrv<-names(juntoAl)
  juntoAl2<-data.frame(c(juntoAl, NF4$n_Factors))
  names(juntoAl2)<-c(NmPrv, NF4$Method)
  
  
  # C.2.8 N_FACTORS library
  NF5<-N_FACTORS(DTOmit)
  NF5.Sc<-SCREE(DTOmit)
  title="N_FACTORS Library";
  print(title);print(NF5)
  
  NmFileGrp<-paste0(NmDirActual,"8 N_FACTORS Library_",TypData,"_",Corr,".txt")
  sink(NmFileGrp); print(NF5); sink()
  
  juntoF1b<-cbind(juntoF1b, NF5.Sc$eigen_PCA, NF5.Sc$eigen_SMC, NF5.Sc$eigen_EFA)
  juntoAl3<-data.frame(c(juntoAl2,NF5$n_factors))
  
  NmFileGrp<-paste0(NmDirActual,"8 N_FACTORS LibraryGraph.pdf")
  pdf(NmFileGrp)
  print(NF5)
  SCREE(DTOmit)
  dev.off()
  
  # C.2.9 nFactors library
  nBar<-nFactors::nBartlett(co,N = ncase_list)
  nBen<-nFactors::nBentler(co,N = ncase_list)
  if (Model==TypMod[2]) {
    nCn<-nFactors::nCng(co,model = "factors")
    nM<-nFactors::nMreg(co,model = "factors")
    nSeS<-nFactors::nSeScree(co,model = "factors")
  }
  if (Model==TypMod[1]) {
    nCn<-nFactors::nCng(co,model = "components")
    nM<-nFactors::nMreg(co,model = "components")
    nSeS<-nFactors::nSeScree(co,model = "components")
  }
  
  title="nFactors library";
  print(title);print(nBar);print(nBen);print(nCn);print(nM);print(nSeS);
  juntoAl4<-data.frame(c(juntoAl3,nBar$nFactors,Bentler_Yuan=nBen$nFactors,
                         Cattell_Nelson_Gorsuch_CNG=nCn$nFactors,nM$nFactors,nSeS$nFactors))
  
  # C.2.10 Medida experimental de Revelle para analizar Unidimensionalidad 
  UnidimRev<-unidim(DTOmit,cor="poly")
  title="Medida experimental de Revelle para analizar Unidimensionalidad";
  print(title);print(UnidimRev);
  
  # Almac??n final de todos los resultados
  ResNF<-list()
  ResNF$parallel_analysis_and_scree<-juntoF1
  ResNF$Eigen<-juntoF1b
  ResNF$Velicer<-juntoF2
  ResNF$VSS<-juntoF3
  ResNF$More<-juntoF4
  ResNF$Unidim<-UnidimRev
  
  ResNF$All<-t(juntoAl4)
  FrqRes<-sort(table(ResNF$All),decreasing = T)
  mat<-data.table(c("Nº de Factores","Frecuencia"),t(as.data.frame.table(FrqRes)))
  colnames(mat) <- c("",1:length(FrqRes))
  ResNF$Frecuencias<-mat
  
  NumFacResP<-copy(ResNF)
  #names(NumFacResP$Eigen)
  NumFacResP$Eigen$Num<-1:nrow(NumFacResP$Eigen)
  
  LosP<-as.numeric(as.vector(as.data.frame(table(NumFacResP$All))[1]$Var1))
  LosF<-as.numeric(as.data.frame(table(NumFacResP$All))[2]$Freq)
  
  NumFacResP$Eigen$Size<-0
  NumFacResP$Eigen$Size[c(LosP)]<-LosF
  
  
  ForAnnot<-rep("",nrow(NumFacResP$Eigen))
  ForAnnot[c(LosP)]<-LosF
  #names(NumFacResP$Eigen)<-c("Eigenvalues", "ProportionVariance", "Cumulative", "ParallelAnalysis",
  #                      "PredictedEigenvalues", "OC", "AccelerationFactor", "AF", "Num", "Size")
  coeff=10
  #coeff <- round(max(NumFacResP$Eigen$Eigenvalues)/max(NumFacResP$Eigen$Cumulative))
  eig <- NumFacResP$Eigen$Eigenvalues
  nk <- length(eig)
  k <- 1:nk
  noc <- NumFacResP$parallel_analysis_and_scree$`Optimal coordinates`
  vp.p <- lm(eig[c(noc + 1, nk)] ~ k[c(noc + 1, nk)])
  
  ggMM<-ggplot(NumFacResP$Eigen, aes(Num)) +
    geom_line(aes(y = Eigenvalues), colour = "blue") +
    geom_line(aes(y = NF5.Sc$eigen_PCA), colour = "blue",linetype=2) +
    geom_point(aes(y = NF5.Sc$eigen_PCA), colour = "blue") +
    geom_line(aes(y = NF5.Sc$eigen_SMC), colour = "blue",linetype=2) +
    geom_point(aes(y = NF5.Sc$eigen_SMC), colour = "blue") +
    geom_line(aes(y = NF5.Sc$eigen_EFA), colour = "blue",linetype=2) +
    geom_point(aes(y = NF5.Sc$eigen_EFA), colour = "blue") +
    geom_point(aes(y = Eigenvalues, color=Size,size=Size)) +
    geom_line(aes(y = 1), colour = "Black",linetype=2) +
    geom_line(aes(y = Cumulative*coeff), colour = "red") +
    geom_point(aes(y = Cumulative*coeff), colour = "red") +
    geom_line(aes(y = ParallelAnalysis), colour = "green",linetype=2) +
    scale_y_continuous(sec.axis = sec_axis(~.*coeff, name="Cumulative Variance (%)")) +
    geom_abline(intercept = vp.p$coefficients[1], slope = vp.p$coefficients[2],color="orange",linetype=2) +
    annotate("text", x = k, y=NumFacResP$Eigen$Eigenvalues, label = ForAnnot,color="white",size=3) +
    theme_bw()
  
  NmFileGrp<-paste0(NmDirActual,"100 AllDataGraph.pdf")
  pdf(NmFileGrp)
  plot(ggMM)
  dev.off()
  
  plot(ggMM)
  
  NmFileGrp<-paste0(NmDirActual,"100 All Results_",TypData,"_",Corr,".txt")
  sink(NmFileGrp); print(ResNF); sink()
  NmFileGrp<-paste0(NmDirActual,"100 All Results Eigen_",TypData,"_",Corr,".txt")
  write.table(ResNF$Eigen,file=NmFileGrp)
  
  ResNF   
}

NumFac.21b <-function(ResIAllp, TyMod=1,NFExtr=1, TyExI=2, TyRttI=3, TyExI2=2, TyRttI2=4,TypeCorrInic=5, TypeCorrDT=4,PathRespP="",wFeed=F) {
  library(EFAtools)
  
  TypCorr = c("Pearson", "Polychoric (Two Step)", "Polychoric (Max. Lik.)", "Spearman", 
              "Heterogenous (Two Step)", "Heterogenous (Max. Lik.)")
  CrT=TypeCorrInic
  Corr=TypCorr[CrT]
  ResNF<-list()
  TypData=ResIAllp$Task
  NmDirActual<-paste0(PathRespP,TypData,"/")
  if (!dir.exists(NmDirActual)) {dir.create(NmDirActual)}
  DTOmit<-ResIAllp$DTOmit
  
  # C.2.8 N_FACTORS library
  NF5<-EFAtools::N_FACTORS(DTOmit)
  #NF5.Sc<-SCREE(DTOmit)
  title="N_FACTORS Library";
  if (wFeed) {print(title);print(NF5)}
  
  NmFileGrp<-paste0(NmDirActual,"EFA N_FACTORS LibraryOutput.txt")
  sink(NmFileGrp); print(NF5$outputs); print(NF5$n_factors);sink()
  #capture.output(NF5,NmFileGrp)
  #write.table(NF5,file=NmFileGrp)
  
  #juntoF1b<-cbind(NF5.Sc$eigen_PCA, NF5.Sc$eigen_SMC, NF5.Sc$eigen_EFA)
  juntoAl3<-data.frame(NF5$n_factors)
  
  NmFileGrp<-paste0(NmDirActual,"EFA N_FACTORS LibraryGraph.pdf")
  pdf(NmFileGrp)
    print(NF5)
    SCREE(DTOmit)
  dev.off()
  
  ResNF$Detall<-NF5
  FrqRes<-sort(table(t(juntoAl3)),decreasing = T)
  
  mat<-data.table(c("Nº de Factores","Frecuencia"),t(as.data.frame.table(FrqRes)))
  colnames(mat) <- c("",1:length(FrqRes))
  ResNF$Frecuencias<-mat
  ResNF
}

ExplorFac<- function(ResIAllp,NFExplora,PathRespP="Result") {
  options(show.error.messages = FALSE)
  MisDatos<<-ResIAllp$DTOmit
  TypData=ResIAllp$Task
  ResFit<-list()
  cnt=0
  
  for (i in NFExplora) {
    NumConf<-i
    cnt<-cnt+1
    ResFit[[cnt]]<-NA
    efa<-psych::fa(MisDatos,nfactors = NumConf)
    model1p <- efa_to_cfa(efa)
    model2p <- efa_to_cfa(efa, threshold = 0.3)
    aovp<-  try(anova(
      lavaan::cfa(model1p, data = MisDatos),
      lavaan::cfa(model2p, data = MisDatos)
    ),silent = TRUE)
    ElModelo<-model2p
    AjMod <- try(cfa(ElModelo, data=MisDatos,auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE,
                     auto.cov.y = TRUE, estimator="MLF",missing="ml"),silent = TRUE)
    ResTry<-try(fitlavPar19.2(AjMod)$Bond,silent = TRUE)
    if (class(ResTry) != "try-error") ResFit[[cnt]]<-ResTry
  }
  ResFitAll<-cbind(NFExplora,do.call(rbind,ResFit))
  NmFileGrp<-paste0(PathRespP,TypData,"/EFAResumen.txt")
  write.table(ResFitAll,NmFileGrp)
  rtffile <- RTF(file = paste0(PathRespP,TypData,"/EFADetalles.doc"),height=8.5, width=11)
  addParagraph(rtffile,"Resumen:"); addNewLine(rtffile,2);
  addTable(rtffile, (ResFitAll));
  LblBond<- read_rtf("NotasBondadAjuste.rtf");
  addText(rtffile,utf8Tortf(LblBond));
  done(rtffile)
  
  
  NmFileGrp<-paste0(PathRespP,TypData,"/EFAGraficos.pdf")
  pdf(NmFileGrp)
  for (i in NFExplora) {
    try(iclust(MisDatos,nclusters = i),silent = TRUE)
    try(omega(MisDatos,nfactors = i),silent = TRUE)
    try(bassAckward(MisDatos,nfactors = i),silent = TRUE)
    try(psych::fa.diagram(psych::fa(MisDatos,nfactors = i,rotate="oblimin")),silent = TRUE)
    try(psych::fa.diagram(psych::fa(MisDatos,nfactors = i,rotate="varimax")),silent = TRUE)
  }
  dev.off()
  
  ResFitAllF<- ResFitAll %>%
    kbl(caption=paste0("Bondad Ajuste comparativa de todas las posibles soluciones según Nº Factores"),
        digits=4,escape = F, align = "c")  %>%
    kable_paper("hover", full_width = F,font_size = 10) %>%
    kable_styling(fixed_thead = T) %>%
    footnote(general = LblBond) %>%
    scroll_box(width = "100%", height = "800px")
  
  options(show.error.messages = TRUE)
  ResFitAllF
}

# Tkkp=1;ResIAllp=ResIAll
# SelecIDn=T;SelDn=0.30; CritVal=.15;CritFiab=.30; CritCargas=.3;DirPas=NmDirActual
AnItemsMM.21<-function(Tkkp, ResIAllp, SelecIDn=T,SelDn=0.3, CritVal=.15,CritFiab=.30,CritCargas=.32,
                       DirPas="Result",wFeed=FALSE) {
  # require(psych)
  # require(parallel);require(data.table);require(psychometric);
  # require(GPArotation)
  # require(Rcsdp)
  # require(CMC)
  # require(knitr); require(kableExtra); require(stringr)
  options(knitr.kable.NA = '')
  
  ResFinal<-list()
  # Definiciones generales
  ResI<-ResIAllp[[Tkkp]]
  DTOmit<-ResI$DTOmit
  NumIt<-ResI$NumI
  LosIt<-paste0("Item ",1:NumIt)
  NumberO=1:NumIt
  NamesOrig<-names(DTOmit)
  Nfac<-ResI$NFac
  EtiqFactores<-ResI$NmFactor
  
  if (Nfac==0) Nfac=1
  Tkn<-Tkkp
  LaTask<-ResIAllp[[Tkkp]]$Task
  RtNmFile<-paste0("AnaItems_",LaTask)
  ElDir<-DirPas
  IDnF<-IDn<-list()
  
  # Cálculos básicos     
  if (Tkn ==2) { IDnF<- IDnMM.21(DTOmit,Number=0,Ordin=0,Categ=NumberO)}
  if (Tkn !=2) { IDnF<- IDnMM.21(DTOmit,Number=0,Ordin=NumberO,Categ=0)}
  #??IDnF$Items<-paste0(NumberO,IDnF$Items)
  
  
  # Ahora comparo mis cálculos con los del paquete alpha
  # r.drop. Item whole correlation for this item against the scale 
  # without this item
  ResAlpha<-psych::alpha(DTOmit)
  #summary(ResAlpha)
  # Con Omega (variante de Alfa)
  om.results<-psych::omega(DTOmit,nfactors=Nfac,poly = F)
  # Y con el Ind Discriminac clásico basado en la biserial-puntual
  ResPsych<-item.exam(DTOmit, discrim=TRUE,y = ResI$ResNewAllVal[,get(ResI$Validez)])
  ResItAna<-ItemAnalysis(DTOmit,criterion = ResI$ResNewAllVal[,get(ResI$Validez)])
  test.simple3 <-psych::fa(DTOmit,nfactors=1,rotate="varimax")
  colnames(test.simple3$loadings) <- c("General")
  rownames(test.simple3$loadings) <- LosIt
  ResCargas<-AnCargas.21(psych::fa(DTOmit,nfactors=1,rotate="varimax"),LosItnp = LosIt,NFa=1,SelCarg=CritCargas)
  ResAllAnaIt<-cbind(ResPsych,ResItAna,ResAlpha$item.stats,ResAlpha$alpha.drop,ResAlpha$response.freq,ResCargas$ResCarg[,c(5:6)])
  ic.out <- psych::iclust(DTOmit)
  
  # Añado las 2 estimaciones
  #ResAlpha$item.stats$r.drop
  IDnF[,rdrop:=ResAlpha$item.stats$r.drop]
  IDnF[,rbis:=ResPsych$Item.Tot.woi]
  IDnF[,Discrim:=ResPsych$Discrimination]
  IDnF[,Dific:=ResPsych$Difficulty]
  IDnF[,Fiab:=ResPsych$Item.Rel.woi]
  IDnF[,Fiab2:=((ResAlpha$item.stats$r.drop)^2)/ ResAlpha$alpha.drop$std.alpha]
  IDnF[,Validez:=ResPsych$Item.Validity]
  IDnF[,Cargas:=ResCargas$ValCargas]
  IDnF[,Clusters:=c(ic.out$loadings)]
  
  # Y Ordenamos
  IDfOrd<-copy(IDnF)
  IDfOrd<-IDfOrd[order(Corr.IDn)]
  VarSelect<-c('Items', 'Corr.IDn', 'Fiab','Fiab2','Validez','Cargas')
  
  # Creamos la representación gráfica con todos los resultados
  ggAnaIt<-IDnF  %>%
    dplyr::select(all_of(VarSelect))  %>% 
    arrange(Corr.IDn) %>%    # First sort by val. 
    mutate(Items=factor(Items, levels=Items)) %>% 
    reshape(direction = "long",varying = list(VarSelect[-1]),
            idvar = "Items", v.names = "Value",
            times=VarSelect[-1],timevar=c("Index")) %>%
    ggplot(aes(Value, Items)) +
    labs(title = "2 Comparativa de diferentes índices",
         caption = "Para más detalles consultar el fichero ResInDisMas.csv") +
    geom_point(size = 1) +
    facet_grid(cols = vars(Index)) +
    geom_vline(aes(xintercept = z),
               data.frame(z = c(SelDn,CritFiab,CritFiab,CritVal,CritCargas),
                          Index=VarSelect[-1]),lty=2) + xlim(0,1)
  
  
  # Asignaciones
  ResFinal$A_IndDiscrim <-IDfOrd
  ResFinal$B_Compara <-ggAnaIt
  ResFinal$A2_IndMas <-ResAllAnaIt
  ResFinal$D_PorcVar <-test.simple3$Vaccounted
  ResFinal$E_Dificultad<-DDplot(DTOmit, discrim = 'ULI', k = 3, l = 1, u = 3) 
  # Se imprimen los resultados en pantalla si se requiere
  
  if (wFeed)  {
    ElMensa<-"1 Índices de Discriminación \n";
    print(ElMensa)
    print(IDnF)
    IDnF[,dotchart(Corr.IDn , labels=Items,cex=0.75,main="1 Indice Discriminacion Cálculos Rutina Propia",xlim=c(0,1))]
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    abline(v=.3,lty=2);abline(v=-.3,lty=2)
    ElMensa<-"2 Comparativa de diferentes índices \n";
    print(ElMensa)
    plot(ggAnaIt)
    ElMensa<-"Para más detalles, se han añadido otros índices, que se 
      almacenarán en el fichero ResInDisMas.csv, que puede recuperar desde Excel\n";
    print(ElMensa)
    print(ResAllAnaIt)
    ElMensa<-"\n3 Índices de la Dificultad (ver el gráfico)\n";
    print(ElMensa)
    IDfOrd[,plot(density(Dific),main="3 Distribución Dificultad a través ítems")]
    IDfOrd[,hist(Dific, freq=FALSE,add=T)]
    # Kernel density plot
    ElMensa<-"\n3b Índices de la Dificultad y Discriminación (ver el gráfico)\n";
    print(ElMensa)
    DDplot(DTOmit, discrim = 'ULI', k = 3, l = 1, u = 3) 
    ElMensa<-"\n4 Análisis visual-exploratorio de la dimensionalidad de los Itmes\n";
    print(ElMensa)
    ElMensa<-"\nEl Porcentaje de Varianza Explicado por la dimensión general:\n";
    cat(ElMensa, paste0(round(test.simple3$Vaccounted[2]*100,2),"%"),"\n")
  
    #print(test.simple3$Vaccounted)
    # Para analizar la dimensionalidad de los items con AF cl??sico
    psych::fa.diagram(test.simple3,main ="4 Análisis gráfico de la dimensionalidad de los Itmes 1" )
    # Otra confirmaci??n del An??lisis de las dimensiones
    psych::omega.diagram(om.results,cex=1,main ="4b Análisis gráfico de la dimensionalidad de los Itmes 2")
    ElMensa<-"\n4c Análisis gráfico de la dimensionalidad de los Itmes 3\n";
    print(ElMensa)
    alpha.curve(DTOmit)
    ElMensa<-"n4c Análisis gráfico de la dimensionalidad de los Itmes 4\n Alternativa basado en Clusters: \n";print(ElMensa)
    psych::iclust.diagram(ic.out)
  }
  
  p_qq <- ggplot(IDfOrd, aes(sample = Dific))
  p_qq <-p_qq + stat_qq() + stat_qq_line() + theme_classic() +
      ggtitle("3b Normalidad Distribución Dificultad a través ítems") 
  
  # Almacén de los resultados
  #Almacena Resultados en el fichero IDn.pdf
  # NmFileGrp<-Mk.pdf(NmDirp =paste0(DirPas,"/",LaTask) ,NmFilep = paste0("IDn",RtNmFile))
  NmFileGrp<-paste0(ElDir,"AnaItemsGraficos.pdf")
  pdf(NmFileGrp,width=11.69,height=8.27,paper="a4r")
    IDfOrd[,dotchart(Corr.IDn , labels=Items,cex=0.75,main="1 Indice Discriminacion Cálculos Rutina Propia",xlim=c(0,1))]
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    abline(v=.3,lty=2);abline(v=-.3,lty=2)
    plot(ggAnaIt)
    IDfOrd[,plot(density(Dific),main="3 Distribución Dificultad a través ítems")]
    IDfOrd[,hist(Dific, freq=FALSE,add=T)]
    DDplot(DTOmit, discrim = 'ULI', k = 3, l = 1, u = 3)
    
    plot(p_qq)
    #IDfOrd[,plot(qqnorm(Dific),main="3b Normalidad Distribución Dificultad a través ítems")]
    #IDfOrd[,plot(qqline(Dific),col = 2)]
    
    psych::fa.diagram(test.simple3,main ="4 Análisis gráfico de la dimensionalidad de los Itmes 1" )
    # Otra confirmaci??n del An??lisis de las dimensiones
    psych::omega.diagram(om.results,cex=1,main ="5b Análisis gráfico de la dimensionalidad de los Itmes 2")
    alpha.curve(DTOmit)
    iclust.diagram(ic.out)
  dev.off()
  
  #Almacena tb los resultados en ficheros de texto o de word
  NmFileGrp<-paste0(ElDir,"AnaItems.csv")
  write.table(IDfOrd,NmFileGrp)
  NmFileGrp<-paste0(ElDir,"AnaItemsMas.csv")
  write.table(ResAllAnaIt,NmFileGrp)
  
  rtffile <- RTF(file = paste0(ElDir,"AnaItemsGlobal.doc"),width=11.69,height=8.27 ,font.size=9)
    ElMensa<-"1 Indices de Discriminacion\n";
    addText(rtffile,ElMensa)
    addNewLine(rtffile)
    addTable(rtffile, as.data.table(cbind(IDfOrd[,1],IDfOrd[,round(.SD,3), .SDcols=c(2:ncol(IDfOrd))])));
    addPageBreak(rtffile);
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      IDnF[,dotchart(Corr.IDn, labels=Items,cex=0.75,
                     main="1 Indice Discriminación Cálculos Rutina Propia",xlim=c(0,1))]
      abline(v=.2,lty=2);abline(v=-.2,lty=2)
      abline(v=.3,lty=2);abline(v=-.3,lty=2)}
    )
    addPageBreak(rtffile,width=11.69,height=8.27);
    ElMensa<-"2 Gráfico comparativo de todos los Indices\n";
    #addText(rtffile,ElMensa)
    addPlot(rtffile,plot.fun=print,width=10,height=6,res=600,ggAnaIt)
    addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      IDfOrd[,plot(density(Dific),main="3 Distribución Dificultad a través ítems")]
      IDfOrd[,hist(Dific, freq=FALSE,add=T)]
    })
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      DDplot(DTOmit, discrim = 'ULI', k = 3, l = 1, u = 3)
    })
    addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
      addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      plot(p_qq)
    })
    
    addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
    
    ElMensa<-"\nAnálisis de la dimensionalidad de los ítems:\n";
    ElMensa<-enc2utf8(as(ElMensa, "character"))
    addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      psych::fa.diagram(test.simple3,main ="4 Análisis gráfico de la dimensionalidad de los Itmes 1" )
    })
    addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      psych::omega.diagram(om.results,cex=1,main ="5b Análisis gráfico de la dimensionalidad de los Itmes 2")
    })
    addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      alpha.curve(DTOmit)
    })
    addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600,iclust.diagram(ic.out));
    addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
    ElMensa<-"\nEl Porcentaje de Varianza Explicado por la dimension general:\n";
    addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
    addParagraph(rtffile,paste0(round(test.simple3$Vaccounted[2]*100,2),"%")); addNewLine(rtffile,2);
    ElMensa<-"2 Detalles de calculo comparativo de todos los Indices \n Libreria item.exam";
    addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
    addTable(rtffile, as.data.table(round(ResPsych,3)));
    addPageBreak(rtffile,width=11.69,height=8.27);
    ElMensa<-"2 Detalles de calculo comparativo de todos los Indices \n Libreria ShinyItemAnalysis";
    addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
    addTable(rtffile, as.data.table(round(ResItAna,3)));
    addPageBreak(rtffile,width=11.69,height=8.27);
    ElMensa<-"2 Detalles de calculo comparativo de todos los Indices \n Libreria Psych::alpha";
    addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
    addTable(rtffile, as.data.table(round(ResAlpha$item.stats,3)));
    addPageBreak(rtffile,width=11.69,height=8.27);
    addTable(rtffile, as.data.table(round(ResAlpha$alpha.drop,3)));
    addPageBreak(rtffile,width=11.69,height=8.27);
    addTable(rtffile, as.data.table(round(ResAlpha$response.freq,3)));
    addPageBreak(rtffile,width=11.69,height=8.27);
    ElMensa<-"3 Detalles de calculo comparativo de todos los Indices \n EFA con funcion fa";
    addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
    addTable(rtffile, as.data.table(ResCargas$ResCarg));
    addPageBreak(rtffile,width=11.69,height=8.27);
    #ResCargas$ResCarg[,c(5:6)]
  done(rtffile)
  
  #La selección final:
  Todos<-names(DTOmit)
  Sobrantes<-IDnF[Corr.IDn<=SelDn,Items]
  ElMensa<-"Conclusión: ítems a eleminar según el Ind Discriminación: \n";
  if (wFeed) {cat(ElMensa, "\n"); print(data.table(Sobrantes))}
  ResFinal$ConclusSobranDiscr<-data.table(Sobrantes)
  SeQuedan<-IDnF[Corr.IDn>SelDn,Items]
  TkOcF <- unique (grep(paste(SeQuedan,collapse="|"), 
                        NamesOrig, value=F))
  ElMensa<-"Conclusión: Y los ítems que no se eliminarían según el Ind Discriminación: \n";
  if (wFeed) {
    cat(ElMensa, "\n");
    print(data.table(names(DTOmit[,TkOcF,with=F])))}
  
  ResfinalSel<-DTOmit[,TkOcF,with=F]
  
  # Ahora se aplica este 2º filtro basado en LAS CARGAS, tras el filtro 1º basado en el InDn
  Sobrantes2<-IDnF[Cargas<=CritCargas,Items]
  ElMensa<-"Conclusión: ítems a eleminar según La Carga Factorial: \n";
  if (wFeed) {cat(ElMensa, "\n"); print(data.table(Sobrantes2))}
  ResFinal$ConclusSobranCargas<-data.table(Sobrantes2)
  SeQuedan2<-IDnF[Cargas>CritCargas,Items]
  TkOcF2 <- unique (grep(paste(SeQuedan2,collapse="|"), 
                         NamesOrig, value=F))
  ElMensa<-"Conclusión: Y los ítems que no se eliminaríaan según La Carga Factorial: \n";
  if (wFeed) {
    cat(ElMensa, "\n")
    print(data.table(names(DTOmit[,TkOcF2,with=F])))
  }
  TkOcFinal <- unique (grep(paste(SeQuedan2,collapse="|"), 
                            names(ResfinalSel), value=T))
  ResfinalSel<-DTOmit[,TkOcFinal,with=F]
  if (SelecIDn) Resfinal<-ResfinalSel
  ResFinal$SeQuedanTrasFiltros <-ResfinalSel
  
  ResFinal$ZkntA<- ResFinal$A_IndDiscrim %>%
    kbl(caption=paste0("1 Índices de Discriminación"),
        digits=4,escape = F, align = "c")  %>%
    kable_paper("hover", full_width = F,font_size = 10) %>%
    kable_styling(fixed_thead = T) %>%
    footnote(general = paste0("Porcentaje de Varianza Explicado por la dimensión general: ",
                              paste0(round(test.simple3$Vaccounted[2]*100,2),"%"))) %>%
    scroll_box(width = "100%", height = "800px")

  ResFinal$ZkntB<-ResFinal$A2_IndMas %>%
    kbl(caption=paste0("2. Más sobre Índices de Discriminación"),
        digits=4,escape = F, align = "c")  %>%
    kable_paper("hover", full_width = F,font_size = 10) %>%
    kable_styling(fixed_thead = T) %>%
    footnote(general = paste0("Porcentaje de Varianza Explicado por la dimensión general: ",
                              paste0(round(test.simple3$Vaccounted[2]*100,2),"%"))) %>%
    scroll_box(width = "100%", height = "800px")
  
  ResFinal$ZkntC<-ResFinal$SeQuedanTrasFiltros %>%
    kbl(caption=paste0("Conclusión. Cómo quedarían los datos tras los filtros sucesivos"),
        digits=4,escape = F, align = "c")  %>%
    kable_paper("hover", full_width = F,font_size = 10) %>%
    kable_styling(fixed_thead = T) %>%
    footnote(general = paste0("Sobran Por el IDriscriminac: ",ResFinal$ConclusSobranDiscr, "\n",
                              "Sobran Por Las Cargas Factoriales: ", ResFinal$ConclusSobranCargas)) %>%
    scroll_box(width = "100%", height = "800px")
  
  
  ResFinal
}

AnItemsMM.23<-function(Tkkp, ResIAllp, SelecIDn=T,SelDn=0.3, CritVal=.15,CritFiab=.30,CritCargas=.32,
                       DirPas="Result",wFeed=FALSE) {
  options(knitr.kable.NA = '')
  
  ResFinal<-list()
  # Definiciones generales
  ResI<-ResIAllp[[Tkkp]]
  DTOmit<-ResI$DTOmit
  NumIt<-ResI$NumI
  LosIt<-paste0("Item ",1:NumIt)
  NumberO=1:NumIt
  NamesOrig<-names(DTOmit)
  Nfac<-ResI$NFac
  EtiqFactores<-ResI$NmFactor
  
  if (Nfac==0) Nfac=1
  Tkn<-Tkkp
  LaTask<-ResIAllp[[Tkkp]]$Task
  RtNmFile<-paste0("AnaItems_",LaTask)
  ElDir<-DirPas
  IDnF<-IDn<-list()
  
  # Cálculos básicos     
  if (Tkn ==2) { IDnF<- IDnMM.21(DTOmit,Number=0,Ordin=0,Categ=NumberO)}
  if (Tkn !=2) { IDnF<- IDnMM.21(DTOmit,Number=0,Ordin=NumberO,Categ=0)}
  #??IDnF$Items<-paste0(NumberO,IDnF$Items)
  
  
  # Ahora comparo mis cálculos con los del paquete alpha
  # r.drop. Item whole correlation for this item against the scale 
  # without this item
  ResAlpha<-psych::alpha(DTOmit)
  #summary(ResAlpha)
  # Con Omega (variante de Alfa)
  om.results<-psych::omega(DTOmit,nfactors=Nfac,poly = F)
  # Y con el Ind Discriminac clásico basado en la biserial-puntual
  ResPsych<-item.exam(DTOmit, discrim=TRUE,y = ResI$ResNewAllVal[,get(ResI$Validez)])
  ResItAna<-ItemAnalysis(DTOmit,criterion = ResI$ResNewAllVal[,get(ResI$Validez)])
  test.simple3 <-psych::fa(DTOmit,nfactors=1,rotate="varimax")
  colnames(test.simple3$loadings) <- c("General")
  rownames(test.simple3$loadings) <- LosIt
  ResCargas<-AnCargas.21(psych::fa(DTOmit,nfactors=1,rotate="varimax"),LosItnp = LosIt,NFa=1,SelCarg=CritCargas)
  ResAllAnaIt<-cbind(ResPsych,ResItAna,ResAlpha$item.stats,ResAlpha$alpha.drop,ResAlpha$response.freq,ResCargas$ResCarg[,c(5:6)])
  ic.out <- psych::iclust(DTOmit)
  
  # Añado las 2 estimaciones
  #ResAlpha$item.stats$r.drop
  IDnF[,rdrop:=ResAlpha$item.stats$r.drop]
  IDnF[,rbis:=ResPsych$Item.Tot.woi]
  IDnF[,Discrim:=ResPsych$Discrimination]
  IDnF[,Dific:=ResPsych$Difficulty]
  IDnF[,Fiab:=ResPsych$Item.Rel.woi]
  IDnF[,Fiab2:=((ResAlpha$item.stats$r.drop)^2)/ ResAlpha$alpha.drop$std.alpha]
  IDnF[,Validez:=ResPsych$Item.Validity]
  IDnF[,Cargas:=ResCargas$ValCargas]
  IDnF[,Clusters:=c(ic.out$loadings[1])]
  
  # Y Ordenamos
  IDfOrd<-copy(IDnF)
  IDfOrd<-IDfOrd[order(Corr.IDn)]
  VarSelect<-c('Items', 'Corr.IDn', 'Fiab','Fiab2','Validez','Cargas')
  
  # Creamos la representación gráfica con todos los resultados
  ggAnaIt<-IDnF  %>%
    dplyr::select(all_of(VarSelect))  %>% 
    arrange(Corr.IDn) %>%    # First sort by val. 
    mutate(Items=factor(Items, levels=Items)) %>% 
    reshape(direction = "long",varying = list(VarSelect[-1]),
            idvar = "Items", v.names = "Value",
            times=VarSelect[-1],timevar=c("Index")) %>%
    ggplot(aes(Value, Items)) +
    labs(title = "2 Comparativa de diferentes índices",
         caption = "Para más detalles consultar el fichero ResInDisMas.csv", x="Magnitud Índice") +
    geom_point(size = 1) +
    facet_grid(cols = vars(Index)) +
    geom_vline(aes(xintercept = z),
               data.frame(z = c(SelDn,CritFiab,CritFiab,CritVal,CritCargas),
                          Index=VarSelect[-1]),lty=2) + xlim(0,1) +
    theme(axis.text.y = element_text(size = 6))
  
  
  # Asignaciones
  ResFinal$A_IndDiscrim <-IDfOrd
  ResFinal$B_Compara <-ggAnaIt
  ResFinal$A2_IndMas <-ResAllAnaIt
  ResFinal$D_PorcVar <-test.simple3$Vaccounted
  ResFinal$E_Dificultad<-DDplot(DTOmit, discrim = 'ULI', k = 3, l = 1, u = 3) 
  # Se imprimen los resultados en pantalla si se requiere
  
  if (wFeed)  {
    ElMensa<-"1 Índices de Discriminación \n";
    print(ElMensa)
    print(IDnF)
    IDnF[,dotchart(Corr.IDn , labels=Items,cex=0.75,main="1 Indice Discriminacion Cálculos Rutina Propia",xlim=c(0,1))]
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    abline(v=.3,lty=2);abline(v=-.3,lty=2)
    ElMensa<-"2 Comparativa de diferentes índices \n";
    print(ElMensa)
    plot(ggAnaIt)
    ElMensa<-"Para más detalles, se han añadido otros índices, que se 
      almacenarán en el fichero ResInDisMas.csv, que puede recuperar desde Excel\n";
    print(ElMensa)
    print(ResAllAnaIt)
    ElMensa<-"\n3 Índices de la Dificultad (ver el gráfico)\n";
    print(ElMensa)
    IDfOrd[,plot(density(Dific),main="3 Distribución Dificultad a través ítems")]
    IDfOrd[,hist(Dific, freq=FALSE,add=T)]
    # Kernel density plot
    ElMensa<-"\n3b Índices de la Dificultad y Discriminación (ver el gráfico)\n";
    print(ElMensa)
    DDplot(DTOmit, discrim = 'ULI', k = 3, l = 1, u = 3) 
    ElMensa<-"\n4 Análisis visual-exploratorio de la dimensionalidad de los Itmes\n";
    print(ElMensa)
    ElMensa<-"\nEl Porcentaje de Varianza Explicado por la dimensión general:\n";
    cat(ElMensa, paste0(round(test.simple3$Vaccounted[2]*100,2),"%"),"\n")
    
    #print(test.simple3$Vaccounted)
    # Para analizar la dimensionalidad de los items con AF cl??sico
    psych::fa.diagram(test.simple3,main ="4 Análisis gráfico de la dimensionalidad de los Itmes 1" )
    # Otra confirmaci??n del An??lisis de las dimensiones
    psych::omega.diagram(om.results,cex=1,main ="4b Análisis gráfico de la dimensionalidad de los Itmes 2")
    ElMensa<-"\n4c Análisis gráfico de la dimensionalidad de los Itmes 3\n";
    print(ElMensa)
    CMC::alpha.curve(DTOmit)
    ElMensa<-"n4c Análisis gráfico de la dimensionalidad de los Itmes 4\n Alternativa basado en Clusters: \n";print(ElMensa)
    psych::iclust.diagram(ic.out)
  }
  
  p_qq <- ggplot(IDfOrd, aes(sample = Dific))
  p_qq <-p_qq + stat_qq() + stat_qq_line() + theme_classic() +
    ggtitle("3b Normalidad Distribución Dificultad a través ítems") 
  
  # Almacén de los resultados
  #Almacena Resultados en el fichero IDn.pdf
  # NmFileGrp<-Mk.pdf(NmDirp =paste0(DirPas,"/",LaTask) ,NmFilep = paste0("IDn",RtNmFile))
  NmFileGrp<-paste0(ElDir,"AnaItemsGraficos.pdf")
  pdf(NmFileGrp,width=11.69,height=8.27,paper="a4r")
  IDfOrd[,dotchart(Corr.IDn , labels=Items,cex=0.75,main="1 Indice Discriminacion Cálculos Rutina Propia",xlim=c(0,1))]
  abline(v=.2,lty=2);abline(v=-.2,lty=2)
  abline(v=.3,lty=2);abline(v=-.3,lty=2)
  plot(ggAnaIt)
  IDfOrd[,plot(density(Dific),main="3 Distribución Dificultad a través ítems")]
  IDfOrd[,hist(Dific, freq=FALSE,add=T)]
  DDplot(DTOmit, discrim = 'ULI', k = 3, l = 1, u = 3)
  
  plot(p_qq)
  #IDfOrd[,plot(qqnorm(Dific),main="3b Normalidad Distribución Dificultad a través ítems")]
  #IDfOrd[,plot(qqline(Dific),col = 2)]
  
  psych::fa.diagram(test.simple3,main ="4 Análisis gráfico de la dimensionalidad de los Itmes 1" )
  # Otra confirmaci??n del An??lisis de las dimensiones
  psych::omega.diagram(om.results,cex=1,main ="5b Análisis gráfico de la dimensionalidad de los Itmes 2")
  CMC::alpha.curve(DTOmit)
  iclust.diagram(ic.out)
  dev.off()
  
  #Almacena tb los resultados en ficheros de texto o de word
  NmFileGrp<-paste0(ElDir,"AnaItems.csv")
  write.table(IDfOrd,NmFileGrp)
  NmFileGrp<-paste0(ElDir,"AnaItemsMas.csv")
  write.table(ResAllAnaIt,NmFileGrp)
  
  
  
  rtffile <- RTF(file = paste0(ElDir,"AnaItemsGlobal.doc"),width=11.69,height=8.27 ,font.size=9)
  ElMensa<-"1 Indices de Discriminacion\n";
  addText(rtffile,ElMensa)
  addNewLine(rtffile)
  addTable(rtffile, as.data.table(cbind(IDfOrd[,1],IDfOrd[,round(.SD,3), .SDcols=c(2:ncol(IDfOrd))])));
  addPageBreak(rtffile);
  addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
    IDnF[,dotchart(Corr.IDn, labels=Items,cex=0.75,
                   main="1 Indice Discriminación Cálculos Rutina Propia",xlim=c(0,1))]
    abline(v=.2,lty=2);abline(v=-.2,lty=2)
    abline(v=.3,lty=2);abline(v=-.3,lty=2)}
  )
  addPageBreak(rtffile,width=11.69,height=8.27);
  ElMensa<-"2 Gráfico comparativo de todos los Indices\n";
  #addText(rtffile,ElMensa)
  addPlot(rtffile,plot.fun=print,width=10,height=6,res=600,ggAnaIt)
  addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
  addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
    IDfOrd[,plot(density(Dific),main="3 Distribución Dificultad a través ítems")]
    IDfOrd[,hist(Dific, freq=FALSE,add=T)]
  })
  addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
    DDplot(DTOmit, discrim = 'ULI', k = 3, l = 1, u = 3)
  })
  addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
  addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
    plot(p_qq)
  })
  
  addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
  
  ElMensa<-"\nAnálisis de la dimensionalidad de los ítems:\n";
  ElMensa<-enc2utf8(as(ElMensa, "character"))
  addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
  addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
    psych::fa.diagram(test.simple3,main ="4 Análisis gráfico de la dimensionalidad de los Itmes 1" )
  })
  addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
  addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
    psych::omega.diagram(om.results,cex=1,main ="5b Análisis gráfico de la dimensionalidad de los Itmes 2")
  })
  addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
  addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
    CMC::alpha.curve(DTOmit)
  })
  addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
  addPlot(rtffile,plot.fun=print,width=6,height=6,res=600,iclust.diagram(ic.out));
  addPageBreak(rtffile,width=11.69,height=8.27,font.size=9);
  ElMensa<-"\nEl Porcentaje de Varianza Explicado por la dimension general:\n";
  addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
  addParagraph(rtffile,paste0(round(test.simple3$Vaccounted[2]*100,2),"%")); addNewLine(rtffile,2);
  ElMensa<-"2 Detalles de calculo comparativo de todos los Indices \n Libreria item.exam";
  addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(round(ResPsych,3)));
  addPageBreak(rtffile,width=11.69,height=8.27);
  ElMensa<-"2 Detalles de calculo comparativo de todos los Indices \n Libreria ShinyItemAnalysis";
  addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(round(ResItAna,3)));
  addPageBreak(rtffile,width=11.69,height=8.27);
  ElMensa<-"2 Detalles de calculo comparativo de todos los Indices \n Libreria Psych::alpha";
  addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(round(ResAlpha$item.stats,3)));
  addPageBreak(rtffile,width=11.69,height=8.27);
  addTable(rtffile, as.data.table(round(ResAlpha$alpha.drop,3)));
  addPageBreak(rtffile,width=11.69,height=8.27);
  addTable(rtffile, as.data.table(round(ResAlpha$response.freq,3)));
  addPageBreak(rtffile,width=11.69,height=8.27);
  ElMensa<-"3 Detalles de calculo comparativo de todos los Indices \n EFA con funcion fa";
  addParagraph(rtffile,ElMensa); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(ResCargas$ResCarg));
  addPageBreak(rtffile,width=11.69,height=8.27);
  #ResCargas$ResCarg[,c(5:6)]
  done(rtffile)
  
  #La selección final (nuevo 2023:
  Todos<-names(DTOmit)
  Sobrantes<-IDnF[Corr.IDn<=SelDn,Items]
  Sobrantes2<-IDnF[Cargas<=CritCargas,Items]
  Sobrantes3<-IDnF[Fiab2<=CritFiab,Items]
  Sobrantes4<-IDnF[Validez<=CritVal,Items]
  SobrantesAll<-c(Sobrantes,Sobrantes2,Sobrantes3,Sobrantes4)
  SobrantesAll<-unique(SobrantesAll)
  SobrantesAll<-  sort(SobrantesAll)
  
  SeQuedanDef<-setdiff(NamesOrig,SobrantesAll)
  SeQuePos=which(NamesOrig %in% SeQuedanDef)
  LosNmReten<-data.table(Item=paste0("EIC_",c(1:99)),Significado=NmItems[2])
  ParaAlmacen<-LosNmReten[(SeQuePos)]
  LosNmReten[(SeQuePos),1]<-paste0(c(LosNmReten[(SeQuePos),1]$Item),"***")
  
  if (wFeed)  {
    ElMensa<-"Conclusión: ítems a eleminar según Las Cargas: \n";
    cat(ElMensa, "\n"); print(data.table(Sobrantes2))
    ElMensa<-"Conclusión: ítems a eleminar según el Ind Discriminación: \n";
    cat(ElMensa, "\n"); print(data.table(Sobrantes))
    ElMensa<-"Conclusión: ítems a eleminar según el Ind Fiabilidad: \n";
    cat(ElMensa, "\n"); print(data.table(Sobrantes3))
    ElMensa<-"Conclusión: ítems a eleminar según el Ind Validez: \n";
    cat(ElMensa, "\n"); print(data.table(Sobrantes4))
    ElMensa<-"Conclusión: ítems a eleminar según la convergencia de todos los Índices: \n";
    cat(ElMensa, "\n"); print(data.table(SobrantesAll))
    ElMensa<-"Conclusión: Y los ítems que no se eliminarían según el análisis: \n";
    cat(ElMensa, "\n"); print(data.table(SeQuedanDef))
  }
  
  ResFinal$ConclusSobran$Cargas<-data.table(Sobrantes2)
  ResFinal$ConclusSobran$Discr<-data.table(Sobrantes)
  ResFinal$ConclusSobran$Fiab<-data.table(Sobrantes3)
  ResFinal$ConclusSobran$Val<-data.table(Sobrantes4)
  ResFinal$ConclusSobran$All<-data.table(SobrantesAll)
  ResFinal$SeQuedanTrasFiltros <-data.table(SeQuedanDef)
  
  ResFinal$ZkntA<- ResFinal$A_IndDiscrim %>%
    kbl(caption=paste0("1 Índices de Discriminación"),
        digits=4,escape = F, align = "c")  %>%
    kable_paper("hover", full_width = F,font_size = 10) %>%
    kable_styling(fixed_thead = T) %>%
    footnote(general = paste0("Porcentaje de Varianza Explicado por la dimensión general: ",
                              paste0(round(test.simple3$Vaccounted[2]*100,2),"%"))) %>%
    scroll_box(width = "100%", height = "800px")
  
  ResFinal$ZkntB<-ResFinal$A2_IndMas %>%
    kbl(caption=paste0("2. Más sobre Índices de Discriminación"),
        digits=4,escape = F, align = "c")  %>%
    kable_paper("hover", full_width = F,font_size = 10) %>%
    kable_styling(fixed_thead = T) %>%
    footnote(general = paste0("Porcentaje de Varianza Explicado por la dimensión general: ",
                              paste0(round(test.simple3$Vaccounted[2]*100,2),"%"))) %>%
    scroll_box(width = "100%", height = "800px")
  
  
  ResFinal$ZkntC<- DTOmit[,..SeQuedanDef] %>%
    kbl(caption=paste0("Conclusión. Cómo quedarían los datos tras los filtros sucesivos"),
        digits=4,escape = F, align = "c")  %>%
    kable_paper("hover", full_width = F,font_size = 10) %>%
    kable_styling(fixed_thead = T) %>%
    footnote(general = paste0("Sobran Por Las Cargas Factoriales: ", ResFinal$ConclusSobran$Cargas, "\n",
                              "Sobran Por el IDriscriminac: ",ResFinal$ConclusSobran$Discr, "\n",
                              "Sobran Por el IFiabilidad: ",  ResFinal$ConclusSobran$Fiab, "\n",
                              "Sobran Por el IValidez: ",  ResFinal$ConclusSobran$Val, "\n",
                              "Sobran en general: ", ResFinal$ConclusSobran$All)) %>%
    scroll_box(width = "100%", height = "800px")
  
  ResFinal$ZkntA %>% save_kable(paste0(ElDir,"AnalisiItems1.html"))
  ResFinal$ZkntB %>% save_kable(paste0(ElDir,"AnalisiItems2.html"))
  ResFinal$ZkntC %>% save_kable(paste0(ElDir,"AnalisiItems3.html"))
  
  write.csv2(ResFinal$ConclusSobran$All,paste0(ElDir,"ItemsSobrantes.csv"))
  write.csv2(ParaAlmacen,paste0(ElDir,"ItemsEIC_Filtados.csv"))
  
  rtffile <- RTF(file = paste0(ElDir,"ItemsSeleccionados.doc"),width=11.69,height=8.27 ,font.size=9)
  ElMensa<-"Ítems seleccionados marcados con asteriscon\n";
  addText(rtffile,ElMensa)
  addNewLine(rtffile)
  addTable(rtffile, LosNmReten);
  done(rtffile)
  
  ResFinal
}

EstimaFiab.21<-function(ResIAllp,ElDir) {
  ResFia<-list()
  MisDatos<<-ResIAllp$DTOmit
  ElModelo<-ResIAllp$ModLav
  AjMod <- lavaan::cfa(ElModelo, data=MisDatos,auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE,
                       auto.cov.y = TRUE, estimator="MLF",missing="ml")
  Alf<-psych::alpha(MisDatos)
  Alf2<-psych::alpha.ci(Alf$total$raw_alpha,nrow(MisDatos))
  Fiab<-t(round(reliability(AjMod),3))
  Om<-omega(MisDatos)
  cond<-condisc(AjMod)
  tt<-rbind(Fiab[,-3],Total=with(Om, round(c(alpha,omega_h,omega.tot,mean(cond$Average_Variance_Extracted)),3)))
  colnames(tt)<-c("Alfa","OmegaH","OmegaT","AVE")
  tt<-data.frame(Dim=rownames(tt),tt)
  ResFia$M1<-list()
  ResFia$M1$Mod<-paste0("No Ortogonal",ElModelo)
  ResFia$M1$General<-tt
  ResFia$M1$AlfaCI<-with(Alf2, paste0(round(alpha,3)," [",round(lower.ci,3),",",round(upper.ci,3),"]"))
  tt2<-cond$Squared_Factor_Correlation
  tt2<-data.frame(Dim=rownames(tt2),tt2)
  ResFia$M1$Squared_Factor_Correlation<-tt2
  
  
  AjMod2 <- lavaan::cfa(ElModelo, data=MisDatos,auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE,
                        auto.cov.y = TRUE, orthogonal = TRUE,std.lv = TRUE,estimator="MLF",missing="ml")
  Fiab<-t(round(reliability(AjMod2),3))
  cond<-condisc(AjMod2)
  tt<-rbind(Fiab[,-3],Total=with(Om, round(c(alpha,omega_h,omega.tot,mean(cond$Average_Variance_Extracted)),3)))
  colnames(tt)<-c("Alfa","OmegaH","OmegaT","AVE")
  tt<-data.frame(Dim=rownames(tt),tt)
  ResFia$M2<-list()
  ResFia$M2$Mod<-paste0("Ortogonal",ElModelo)
  ResFia$M2$General<-tt
  ResFia$M2$AlfaCI<-with(Alf2, paste0(round(alpha,3)," [",round(lower.ci,3),",",round(upper.ci,3),"]"))
  tt2<-cond$Squared_Factor_Correlation
  tt2<-data.frame(Dim=rownames(tt2),tt2)
  ResFia$M2$Squared_Factor_Correlation<-tt2
  
  rtffile <- RTF(file = paste0(ElDir,"Fiabilidad.doc"),width=11.69,height=8.27 ,font.size=9)
  addParagraph(rtffile,ResFia$M1$Mod); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(ResFia$M1$General)); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(ResFia$M1$AlfaCI)); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(ResFia$M1$Squared_Factor_Correlation)); addNewLine(rtffile,2);
  
  addParagraph(rtffile,ResFia$M2$Mod); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(ResFia$M2$General)); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(ResFia$M2$AlfaCI)); addNewLine(rtffile,2);
  addTable(rtffile, as.data.table(ResFia$M2$Squared_Factor_Correlation)); addNewLine(rtffile,2);
  
  done(rtffile)
  
  ResFia
}

AnCargas.21<-function(tespas,LosItnp,NFa=1,SelCarg=.32) {
  Res<-list()
  LosFactores<-tespas$loadings[,order(colnames(tespas$loadings))]
  if (NFa>1)    {
    rowmax <- apply(abs(LosFactores), 1, max)
    LosFactores[abs(LosFactores) < rowmax] <- 0
    conti=0;cutP=0
    factores<-NA;Variables<-NA;Variables2<-NA;cargasP<-NA
    for (nf in 1:ncol(LosFactores)) {
      for (i in 1:nrow(LosFactores)) {
        i2=LosItnp[i]
        if (abs(LosFactores[i, nf]) > cutP) {
          conti<-conti+1
          factores[conti]<-nf
          Variables[conti]<-i2
          Variables2[conti]<-row.names(LosFactores)[i]
          cargasP[conti]<-round(LosFactores[i, nf], 4)
          #print(paste(colnames(factors)[nf], "-> V", i, " [ label = ", round(factors[i, nf], 4), sep = ""))
        }
      }
    }
  }
  
  if (NFa==1) {
    conti=0;cutP=0
    factores<-NA;Variables<-NA;Variables2<-NA;cargasP<-NA   
    for (i in 1:length(LosFactores)) {
      i2=LosItnp[i]
      if (abs(LosFactores[i]) > cutP) {
        conti<-conti+1
        factores[conti]<-1
        Variables[conti]<-i2
        Variables2[conti]<-names(LosFactores)[i]
        cargasP[conti]<-round(LosFactores[i], 4)
      }
    }
  }
  
  ResCarg<-data.table(Factor=factores,Variable=Variables,Nombres=paste0(Variables,"_",Variables2),NombresOrig=Variables2,
                      Cargas=cargasP, Tipif=scale(cargasP,T,T)[,1])
  CargOrd<-ResCarg[order(Cargas)]
  SobrantesC<-CargOrd[Cargas<=SelCarg,Variable]
  SobrantesC2<-CargOrd[Cargas<=SelCarg,NombresOrig]
  
  Res$ResCarg<-ResCarg
  Res$Variables<-Variables
  Res$SobrantesNum<-SobrantesC
  Res$SobrantesName<-SobrantesC2
  Res$ValCargas<-cargasP
  Res
}

ProcPrevCom.21<-function(VarG,Tk=1,ResI,DTpas,Acum=FALSE,NumCut=5,TypCut=1) {
  require(data.table)
  if (DTpas[,is.numeric(get(VarG))]) {
    if (TypCut==1) DTpas[,New:=cut(get(VarG),NumCut)]
    if (TypCut==2) DTpas[,New:=cut2(x=get(VarG),g=NumCut)]
    VarG<-c("New")
  }
  lv<-levels(DTpas[,get(VarG)])
  keycols = c(VarG)
  setkeyv(DTpas,keycols)
  #ResExpA<-ResExp2A<-list()
  ValLikNew<-ResI$EscalaVal
  if (min(ValLikNew)==0) ValLikNew<- ValLikNew+1
  DTF<-data.table()
  
  #ilv=lv[2]
  #ilv="Hombre"
  for (ilv in lv) {
    sel<-DTpas[ilv][,ResI$TkO,with=F]
    #if (Tk==12) {sel<-sel+1}
    lE<-length(sel)
    ResExp2<-ResExp<-matrix(NA,nrow =lE,ncol =ResI$NLik)
    for (i in c(1:lE))  {
      RR<-sel[,.N,by=eval(ResI$nm[i])]
      RR<-na.omit(RR)
      setkeyv(RR,eval(ResI$nm[i]))
      cont<-0
      for (j in ValLikNew) {
        cont<-cont+1
        ResExp[i,j]<-RR$N[cont]
        ResExp2[i,j]<-RR$N[cont]/sum(RR$N)
      }
      for (j in ValLikNew) {
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

ProcPrevBas.21<-function(Tk=1,ResI,DTpp,Acum=FALSE) {
  require(data.table)
  ValLikNew<-ResI$EscalaVal
  if (min(ValLikNew)==0) ValLikNew<- ValLikNew+1
  DTF<-data.table()
  
  sel<-DTpp[,ResI$TkO,with=F]
  #if (Tk==12) {sel<-sel+1}
  lE<-length(sel)
  ResExp2<-ResExp<-matrix(NA,nrow =lE,ncol =ResI$NLik)
  for (i in c(1:lE))  { #i=1
    RR<-sel[,.N,by=eval(ResI$nm[i])]
    RR<-na.omit(RR)
    setkeyv(RR,eval(ResI$nm[i]))
    cont<-0
    for (j in ValLikNew) {
      cont<-cont+1
      ResExp[i,j]<-RR$N[cont]
      ResExp2[i,j]<-RR$N[cont]/sum(RR$N)
    }
    for (j in ValLikNew) {
      if (is.na(ResExp[i,j])) ResExp[i,j]<-0
      if (is.na(ResExp2[i,j])) ResExp2[i,j]<-0
    }
  }
  
  if (Acum) {DTF<-data.table(Question=ResI$nm2,ResExp2)}
  if (!Acum){DTF<-data.table(Question=ResI$nm2,ResExp)}
  names(DTF)<-c("Question",ResI$EscalaL)
  DTF
}

ProcGrp.21<-function(DTFa,lbl,wGrp=FALSE,Path,Wfeedpp=TRUE) {
  
  LikCou3 <- ## 8in x 9in  ## ProfChal.pdf
    HH::likert(Question ~ . , DTFa, 
               as.percent=TRUE,    ## implies display Row Count Totals
               ylab=NULL,
               main=list(lbl, x=unit(.65, "npc")),
               positive.order=TRUE
    )
  if (Wfeedpp) print(LikCou3)
  LikCou4 <- ## 8in x 9in  ## ProfChal.pdf
    HH::likert(Question ~ . , DTFa, 
               as.percent=TRUE,    ## implies display Row Count Totals
               ylab=NULL,
               main=list(lbl, x=unit(.65, "npc"))
    )
  if (Wfeedpp) print(LikCou4)
  
  if (wGrp) {
    NmFic<-paste0(Path,".pdf")
    pdf(NmFic, width=14, height=14)
    print(LikCou4)
    print(LikCou3)
    dev.off()
  }
}

ProcGrp.21b<-function(DTFa,DTF,lbl,wGrp=FALSE,Path,VarGP,NumCut=5,TypCut=1,Wfeed=TRUE) {
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
  if (Wfeed) print(LikCou)
  
  #Sin Ordenar
  LikCou2 <- ## 8in x 9in  ## ProfChal.pdf
    HH::likert(Question ~ . | Subtable, DTF,
               as.percent=TRUE,    ## implies display Row Count Totals
               ylab=NULL,
               main=list(lbl, x=unit(.65, "npc")),
               strip.left=strip.custom(bg="gray85"),
               strip=FALSE
    )
  if (Wfeed) print(LikCou2)
  
  LikCou3 <- ## 8in x 9in  ## ProfChal.pdf
    HH::likert(Question ~ . , DTFa, 
               as.percent=TRUE,    ## implies display Row Count Totals
               ylab=NULL,
               main=list(lbl, x=unit(.65, "npc")),
               positive.order=TRUE
    )
  if (Wfeed) print(LikCou3)
  LikCou4 <- ## 8in x 9in  ## ProfChal.pdf
    HH::likert(Question ~ . , DTFa, 
               as.percent=TRUE,    ## implies display Row Count Totals
               ylab=NULL,
               main=list(lbl, x=unit(.65, "npc"))
    )
  if (Wfeed) print(LikCou4)
  
  if (TypCut==1) DePaso=data.frame(Var=cut(VarGP,NumCut))
  if (TypCut==2) DePaso=data.frame(Var=cut2(x=VarGP,g=NumCut))
  if (TypCut==3) DePaso=data.frame(VarGP)
  ttab<-prop.table(table(DePaso))
  if (Wfeed) {
    bp<-barplot(ttab)
    text(bp,ttab/2,labels=round(ttab,3)*100)
    }
  VarGP<-na.omit(VarGP)
  if (is.numeric(VarGP)) {
    d <- density(VarGP)
    GenVar<-as.data.frame(VarGP)
    #print(plot(likert::likert(DePaso),include.histogram=TRUE))
    if (Wfeed) {
      hist(VarGP, breaks = "FD",col = "lightblue",xlab=lbl,prob=T)
      polygon(d, col=adjustcolor("gray",alpha.f=0.5))
      qqnorm(VarGP); qqline(VarGP, col = 2)
      }
    }
  
  
  if (wGrp) {
    #NmFic<-paste0(Path,lbl,".pdf")
    NmFic<-paste0(Path,".pdf")
    pdf(NmFic, width=14, height=14)
    print(LikCou2)
    print(LikCou)
    #print(LikCou4)
    #print(LikCou3)
    bp<-barplot(ttab)
    text(bp,ttab/2,labels=round(ttab,3)*100)
    if (is.numeric(VarGP)) {
      #print(plot(likert::likert(DePaso),include.histogram=TRUE))
      hist(VarGP, breaks = "FD",col = "lightblue",xlab=lbl,prob=T)
      polygon(d, col=adjustcolor("gray",alpha.f=0.5))
      #lines(d,col="red")
      qqnorm(VarGP); qqline(VarGP, col = 2)
    }
    dev.off()
    
    # rtffile <- RTF(file=paste0(Path,".doc"))
    #   addNewLine(rtffile,2);
    #   addPlot(rtffile,plot.fun=print,width=10, height=10,res=600, {
    #     LikCou2
    #     })
    #   addNewLine(rtffile,2);addPageBreak(rtffile)
    #   addNewLine(rtffile,2);
    #   addPlot(rtffile,plot.fun=print,width=10, height=10,res=600, {
    #     LikCou
    #   })
    #   addNewLine(rtffile,2);addPageBreak(rtffile)
    #   addPlot(rtffile,plot.fun=print,width=10, height=10,res=600, {
    #     bp<-barplot(ttab)
    #     text(bp,ttab/2,labels=round(ttab,3)*100)
    #   })
    #   addNewLine(rtffile,2);addPageBreak(rtffile)
    #   addPlot(rtffile,plot.fun=print,width=10, height=10,res=600, {
    #     if (is.numeric(VarGP)) {
    #       #print(plot(likert::likert(DePaso),include.histogram=TRUE))
    #       hist(VarGP, breaks = "FD",col = "lightblue",xlab=lbl,prob=T)
    #       polygon(d, col=adjustcolor("gray",alpha.f=0.5))
    #       #lines(d,col="red")
    #     }
    #   })
    #     addNewLine(rtffile,2);addPageBreak(rtffile)
    #     addPlot(rtffile,plot.fun=print,width=10, height=10,res=600, {
    #       if (is.numeric(VarGP)) {
    #         qqnorm(VarGP); qqline(VarGP, col = 2)
    #       }
    #   })
    # addNewLine(rtffile,2);
    # done(rtffile)
  }
}

#DTFa = DTpA;DTF = DTp;lbl = paste0(ResI$Task,"_",vs);wGrp = T;
#Path = paste0(NmDirActualp,ResI$Task,"_",vs,"_Cut",NumCutP,TypCutP);
#VarGP =DTpp[,get(vs)];NumCut=NumCutP;TypCut=3

#Tkpp=1;SociodemV=c(2,3,7:16);DTpp=DT;ResIp=ResIAll;NmDirActualp=NmDirActual
RegOrd<-function(datPas,NmPas,NmChange) {
  ResF<-list()
  Res2<-lapply(NmPas, function(i)
  {
    ElMod<-clm(factor(get(i),order=T)~New,data = datPas, link = "logit",Hess=T)
    ResPrv<-summary(ElMod)$coefficients[,4]
    OmnibusLev=NA; ResAOV<-NA;
    if (!is.na(sum(ResPrv))) {
      ResAOV<-try(anova(ElMod))
      OmnibusLev=ResAOV$"Pr(>Chisq)"
    }
    ResPrv<-c(ResPrv,OmnibusLev =OmnibusLev)
    ResPrv
  })
  dtr<-(round(do.call(rbind, setNames(Res2,NmPas)),4)<=0.05)*1
  dtr2<-as.data.table(dtr)
  dtr2<-data.table(Nm=NmChange,Test=dtr2$OmnibusLev)
  dtr2[,NmSig:=Nm]
  dtr2[Test=="1",NmSig:=paste0(Nm,"*")]
  NmCambiados<-dtr2$NmSig
  ResF$dtr<-dtr
  ResF$NmCambiados<-NmCambiados
  ResF
}

RegOrd2<-function(datPas,NmPas,NmChange) {
  ResF<-list()
  Res2<-lapply(NmPas, function(i)
  {
    ElMod<-glm(get(i)~New,data = datPas, family = poisson)
    aovr<-summary(ElMod)$coefficients
    ResPrv<-aovr[nrow(aovr),ncol(aovr)]
    ResPrv
  })
  dtr<-c(round(do.call(rbind, setNames(Res2,NmPas)),4)<=0.05)*1
  dtr2<-data.table(Nm=NmChange,Test=dtr)
  dtr2[,NmSig:=Nm]
  dtr2[Test=="1",NmSig:=paste0(Nm,"+")]
  NmCambiados<-dtr2$NmSig
  ResF$dtr<-dtr
  ResF$NmCambiados<-NmCambiados
  ResF
}


AnDescrip<-function(Tkpp=1,SociodemV,DTpp,ResIp,NmDirActualp,Wfeedp=TRUE) {
  options(show.error.messages = FALSE)
  require(rtf)
  ResDes<-list()
  Socio<-list()
  SocioRegOrd<-list()
  LosNm<-names(DTpp)
  ResI<-ResIp[[Tkpp]]
  DTpA<-ProcPrevBas.21(Tkpp,ResI,DTpp,Acum = F)
  ResDes$LikG<-data.table(DTpA)
  ProcGrp.21(DTFa = DTpA,lbl = paste0(ResI$Task,"_General"),wGrp = T,
             Path = paste0(NmDirActualp,ResI$Task,"_General"),Wfeedpp=T)
  DTpf<-as.data.table(DTpp)
  for (lik in SociodemV) {
    #lik=3 #DT$FqDIAent
    vs<-LosNm[lik]
    SocioRegOrd[[lik]]<-vs
    Socio[[lik]]<-vs
    if (DTpp[,is.numeric(get(vs))]) {
      
      NumCutP=5; TypCutP=1;
      ResINm<-copy(ResI)
      DTppp<-as.data.table(DTpp)
      DTppp[,New:=cut(get(vs),NumCutP)]
      DTpf[,New:=cut(get(vs),NumCutP)]
      names(DTpf)[length(names(DTpf))]<-paste0(vs,"t51")
      AplicaRegOrd<-RegOrd(datPas = DTppp, NmPas = ResI$nm, NmChange = ResI$nm2)
      dtr<-AplicaRegOrd$dtr
      ResINm$nm2<-AplicaRegOrd$NmCambiados
      
      AplicaRegOrd2<-RegOrd2(datPas = DTppp, NmPas = ResI$nm, NmChange = ResINm$nm2)
      dtr2<-AplicaRegOrd2$dtr
      ResINm$nm2<-AplicaRegOrd2$NmCambiados
      
      DTp<-ProcPrevCom.21(VarG =vs, Tkpp,ResINm,copy(DTpp),Acum = F,NumCut=NumCutP,TypCut=TypCutP)
      Socio[[lik]]$t51<-as.data.table(DTp)
      SocioRegOrd[[lik]]$t51<-as.data.table(dtr)
      ProcGrp.21b(DTFa = DTpA,DTF = DTp,lbl = paste0(ResI$Task,"_",vs),wGrp = T,
                  paste0(NmDirActualp,vs,"_Cut",NumCutP,TypCutP),
                  VarGP =DTpp[,get(vs)],NumCutP,TypCutP,Wfeed=Wfeedp)
      
      NumCutP=2; TypCutP=1;
      ResINm<-copy(ResI)
      DTppp<-as.data.table(DTpp)
      DTppp[,New:=cut(get(vs),NumCutP)]
      DTpf[,New:=cut(get(vs),NumCutP)]
      names(DTpf)[length(names(DTpf))]<-paste0(vs,"t21")
      AplicaRegOrd<-RegOrd(datPas = DTppp, NmPas = ResI$nm, NmChange = ResI$nm2)
      dtr<-AplicaRegOrd$dtr
      ResINm$nm2<-AplicaRegOrd$NmCambiados
      
      AplicaRegOrd2<-RegOrd2(datPas = DTppp, NmPas = ResI$nm, NmChange = ResINm$nm2)
      dtr2<-AplicaRegOrd2$dtr
      ResINm$nm2<-AplicaRegOrd2$NmCambiados
      
      DTp<-ProcPrevCom.21(VarG =vs, Tkpp,ResINm,copy(DTpp),Acum = F,NumCut=NumCutP,TypCut=TypCutP)
      Socio[[lik]]$t21<-as.data.table(DTp)
      SocioRegOrd[[lik]]$t21<-as.data.table(dtr)
      ProcGrp.21b(DTFa = DTpA,DTF = DTp,lbl = paste0(ResI$Task,"_",vs),wGrp = T,
                  Path = paste0(NmDirActualp,vs,"_Cut",NumCutP,TypCutP),
                  VarGP =DTpp[,get(vs)],NumCutP,TypCutP,Wfeed=Wfeedp)
      
      NumCutP=2; TypCutP=2;
      ResINm<-copy(ResI)
      DTppp<-as.data.table(DTpp)
      DTppp[,New:=cut2(x=get(vs),g=NumCutP)]
      DTpf[,New:=cut2(x=get(vs),g=NumCutP)]
      names(DTpf)[length(names(DTpf))]<-paste0(vs,"t22")
      AplicaRegOrd<-RegOrd(datPas = DTppp, NmPas = ResI$nm, NmChange = ResI$nm2)
      dtr<-AplicaRegOrd$dtr
      ResINm$nm2<-AplicaRegOrd$NmCambiados
      
      AplicaRegOrd2<-RegOrd2(datPas = DTppp, NmPas = ResI$nm, NmChange = ResINm$nm2)
      dtr2<-AplicaRegOrd2$dtr
      ResINm$nm2<-AplicaRegOrd2$NmCambiados
      
      DTp<-ProcPrevCom.21(VarG =vs, Tkpp,ResINm,copy(DTpp),Acum = F,NumCut=NumCutP,TypCut=TypCutP)
      Socio[[lik]]$t22<-as.data.table(DTp)
      SocioRegOrd[[lik]]$t22<-as.data.table(dtr)
      ProcGrp.21b(DTFa = DTpA,DTF = DTp,lbl = paste0(ResI$Task,"_",vs),wGrp = T,
                  Path = paste0(NmDirActualp,vs,"_Cut",NumCutP,TypCutP),
                  VarGP =DTpp[,get(vs)],NumCutP,TypCutP,Wfeed=Wfeedp)
      
      # Opción 4, la variable queda como cuantt en Regres Ordinal, pero el gráfico es como
      # el del tipo 3 (división por las Medianas)
      NumCutP=2; TypCutP=2;
      ResINm<-copy(ResI)
      DTppp<-as.data.table(DTpp)
      DTppp[,New:=get(vs)]
      DTpf[,New:=get(vs)]
      names(DTpf)[length(names(DTpf))]<-paste0(vs,"t22Cuantt")
      AplicaRegOrd<-RegOrd(datPas = DTppp, NmPas = ResI$nm, NmChange = ResI$nm2)
      dtr<-AplicaRegOrd$dtr
      ResINm$nm2<-AplicaRegOrd$NmCambiados
      
      AplicaRegOrd2<-RegOrd2(datPas = DTppp, NmPas = ResI$nm, NmChange = ResINm$nm2)
      dtr2<-AplicaRegOrd2$dtr
      ResINm$nm2<-AplicaRegOrd2$NmCambiados
      
      DTp<-ProcPrevCom.21(VarG =vs, Tkpp,ResINm,copy(DTpp),Acum = F,NumCut=NumCutP,TypCut=TypCutP)
      Socio[[lik]]$t22Cuantt<-as.data.table(DTp)
      SocioRegOrd[[lik]]$t22Cuantt<-as.data.table(dtr)
      ProcGrp.21b(DTFa = DTpA,DTF = DTp,lbl = paste0(ResI$Task,"_",vs),wGrp = T,
                  Path = paste0(NmDirActualp,vs,"_Cut_Cuantt",NumCutP,TypCutP),
                  VarGP =DTpp[,get(vs)],NumCutP,TypCutP,Wfeed=Wfeedp)
      
    }
    if (DTpp[,!is.numeric(get(vs))]) {
      NumCutP=0; TypCutP=3;
      ResINm<-copy(ResI)
      DTppp<-as.data.table(DTpp)
      DTppp[,New:=get(vs)]
      DTpf[,New:=get(vs)]
      names(DTpf)[length(names(DTpf))]<-paste0(vs,"t0")
      AplicaRegOrd<-RegOrd(datPas = DTppp, NmPas = ResI$nm, NmChange = ResI$nm2)
      dtr<-AplicaRegOrd$dtr
      ResINm$nm2<-AplicaRegOrd$NmCambiados
      
      AplicaRegOrd2<-RegOrd2(datPas = DTppp, NmPas = ResI$nm, NmChange = ResINm$nm2)
      dtr2<-AplicaRegOrd2$dtr
      ResINm$nm2<-AplicaRegOrd2$NmCambiados
      
      DTp<-ProcPrevCom.21(VarG =vs, Tkpp,ResINm,copy(DTpp),Acum = F,NumCut=NumCutP,TypCut=TypCutP)
      Socio[[lik]]$t0<-as.data.table(DTp)
      SocioRegOrd[[lik]]$t0<-as.data.table(dtr)
      ProcGrp.21b(DTFa = DTpA,DTF = DTp,lbl = paste0(ResI$Task,"_",vs),wGrp = T,
                  Path = paste0(NmDirActualp,vs,"_Cut",NumCutP),
                  VarGP =DTpp[,get(vs)],NumCut = NumCutP,TypCut = 3,Wfeed=Wfeedp)
    }
  }
  # Likert General
  bla1 <- capture.output(
  stargazer(ResDes$LikG, title="Likert General", 
            type = 'html', summary=F,
            initial.zero = T, 
            digits=2,align = T,out=paste0(NmDirActualp,"AnalisisLikertGeneral.doc"))
  )
  # Freq Sociodemog
  ByFreq<-lapply(DTpp[,..SociodemV],function(x) cbind(Freq=table(x,exclude = NULL),
                                                      Perc=round(prop.table(table(x,exclude = NULL))*100,1)))
  NmByFre<-names(ByFreq)
  NmByFre<-str_replace_all(NmByFre, "_", ".")
  bla2 <- capture.output(stargazer(ByFreq, title=NmByFre, 
            type = 'html', 
            initial.zero = T, 
            digits=2,align = T,out=paste0(NmDirActualp,"AnalisisSociodemog.doc"))
  )
  # Ordinal Reg Sociodemog
  LsNewNm<-setdiff(names(DTpf),names(DTpp))
  Nmm<-ResI$nm
  ByRegOrd <-lapply(DTpf[,..LsNewNm],function(x) {
    Res2<-lapply(Nmm, function(i)
    {
      ElMod<-clm(factor(get(i),order=T)~x,data = DTpf, link = "logit",Hess=T)
      ResPrv<-summary(ElMod)$coefficients[,4]
      OmnibusLev=NA; ResAOV<-NA;      
      if (!is.na(sum(ResPrv))) {
        ResAOV<-try(anova(ElMod))
        OmnibusLev=ResAOV$"Pr(>Chisq)"
      }
      ResPrv<-c(ResPrv,OmnibusLev =OmnibusLev)
      ResPrv
    })
    (round(do.call(rbind, setNames(Res2,Nmm)),4)<=0.05)*1
  })
  
  LsNewNm2<-LsNewNm
  LsNewNm2<-str_replace_all(LsNewNm2, "_", ".")
  
  bla3 <-capture.output(stargazer(ByRegOrd, title=LsNewNm2, 
            type = 'html', 
            initial.zero = T, 
            digits=2,align = T,out=paste0(NmDirActualp,"AnalisisSociodemogOrdRegr.doc"))
  )
  ByRegOrd2 <-lapply(DTpf[,..LsNewNm],function(x) {
    Res2<-lapply(Nmm, function(i)
    {
      ElMod<-glm(get(i)~x,data = DTpf, family = poisson)
      aovr<-summary(ElMod)$coefficients
      ResPrv<-aovr[nrow(aovr),ncol(aovr)]
      ResPrv
    })
    round(do.call(rbind, setNames(Res2,Nmm)),4)
  })
  write_csv2(as.data.table(ByRegOrd2),paste0(NmDirActualp,"AnalisisSociodemogOrdRegrGLM.csv"))
  # Descriptive
  Medias<-lapply(DTpf[,.SD],function(x) mean(x))
  bla4 <-capture.output(stargazer(DTpf,type = 'html',digits=2,out=paste0(NmDirActualp,"AnalisisTodasVar.doc")))
  
  rtffile <- RTF(file=paste0(NmDirActualp,"AnalisisTodasVarAll.doc"))
  addNewLine(rtffile,2);
  addParagraph(rtffile,Medias); 
  addNewLine(rtffile,2);
  done(rtffile)
  cat(capture.output(Hmisc::describe(DTpp)), file = paste0(NmDirActualp,"AnalisisTodasVarAll.txt"), sep = '\n')
  
  ResDes$Socio<-Socio
  ResDes$SocioRegOrd<-SocioRegOrd
  ResDes$DiffByRegOrd<-as.data.table(ByRegOrd)
  ResDes$DiffByRegGLM<-as.data.table(ByRegOrd2)
  ResDes$Medias<-Medias
  ResDes$Freq<-ByFreq
  options(show.error.messages = TRUE)
  ResDes
}

#ResIAllp=ResIAll[[Tk]];DirPas=NmDirActual
CFA.21 <-function(ResIAllp,DirPas) {
  options(show.error.messages = FALSE)
  ResFit<-list()
  ElModelo<-ResIAllp$ModLav
  ElModelo2<-ResIAllp$ModLavT2
  LosDatos<<-ResIAllp$DTOmit
  AjMod <- lavaan::cfa(model = ElModelo, data=LosDatos,auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE,
               auto.cov.y = TRUE, estimator="MLF",missing="ml")
  ResFit$M1$Type<-paste0("No Ortogonal",ElModelo)
  FitLavMod<-fitlavPar19.2(fitp = AjMod)
  ResFit$M1$fit<-FitLavMod$Inter
  ResFit$M1$fit2<-FitLavMod$Bond
  sink("sink-temp.txt")
  ResFit$M1$Detall<-summary(AjMod,standardized=TRUE,fit.measures=TRUE)
  sink()
  ResFit$M1$Plot<-semPlot::semPaths(AjMod, style="lisrel", 
                                    whatLabels = "std", edge.label.cex = .6, 
                                    label.prop=0.9, edge.label.color = "black", rotation = 4, 
                                    equalizeManifests = FALSE, optimizeLatRes = TRUE, node.width = 1.5, 
                                    edge.width = 0.5, shapeMan = "rectangle", shapeLat = "ellipse", 
                                    shapeInt = "triangle", sizeMan = 4, sizeInt = 2, sizeLat = 4, 
                                    curve=2, unCol = "#070b8c",DoNotPlot=T)
  
  AjMod2 <- lavaan::cfa(ElModelo, data=LosDatos,auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE,
                auto.cov.y = TRUE, orthogonal = TRUE,std.lv = TRUE,estimator="MLF",missing="ml")
  ResFit$M2$Type<-paste0("Ortogonal",ElModelo)
  FitLavMod2<-fitlavPar19.2(fitp = AjMod2)
  ResFit$M2$fit<-FitLavMod2$Inter
  ResFit$M2$fit2<-FitLavMod2$Bond
  sink("sink-temp.txt")
  ResFit$M2$Detall<-summary(AjMod2,standardized=TRUE,fit.measures=TRUE)
  sink()
  ResFit$M2$Plot<-semPlot::semPaths(AjMod2, style="lisrel", 
                                    whatLabels = "std", edge.label.cex = .6, 
                                    label.prop=0.9, edge.label.color = "black", rotation = 4, 
                                    equalizeManifests = FALSE, optimizeLatRes = TRUE, node.width = 1.5, 
                                    edge.width = 0.5, shapeMan = "rectangle", shapeLat = "ellipse", 
                                    shapeInt = "triangle", sizeMan = 4, sizeInt = 2, sizeLat = 4, 
                                    curve=2, unCol = "#070b8c",DoNotPlot=T)
  
  AjMod3 <- lavaan::cfa(ElModelo2, data=LosDatos,auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE,
                auto.cov.y = TRUE, estimator="MLF",missing="ml")
  ResFit$M3$Type<-paste0("No Ha lugar la Ortogonalidad",ElModelo2)
  FitLavMod3<-fitlavPar19.2(fitp = AjMod3)
  ResFit$M3$fit<-FitLavMod3$Inter
  ResFit$M3$fit2<-FitLavMod3$Bond
  sink("sink-temp.txt")
  ResFit$M3$Detall<-summary(AjMod3,standardized=TRUE,fit.measures=TRUE)
  sink() 
  ResFit$M3$Plot<-semPlot::semPaths(AjMod3, style="lisrel", 
                                    whatLabels = "std", edge.label.cex = .6, 
                                    label.prop=0.9, edge.label.color = "black", rotation = 4, 
                                    equalizeManifests = FALSE, optimizeLatRes = TRUE, node.width = 1.5, 
                                    edge.width = 0.5, shapeMan = "rectangle", shapeLat = "ellipse", 
                                    shapeInt = "triangle", sizeMan = 4, sizeInt = 2, sizeLat = 4, 
                                    curve=2, unCol = "#070b8c",DoNotPlot=T)
  # El Modelo tipo 2 no puede ser ortogonal por definición
  Variantes<-c("No Ortogonal","Ortogonal","Factores y Total")
  VarUnite<-data.table(cbind(Variantes,rbind(ResFit$M1$fit2,ResFit$M2$fit2,ResFit$M3$fit2)))
  
  ResFit$CFA<-VarUnite
  ResFit$ZkntCFA<- VarUnite %>%
    kbl(caption=paste0("Bondad Ajuste comparativa de las tres variantes"),
        digits=4,escape = F, align = "c")  %>%
    kable_paper("hover", full_width = F,font_size = 10) %>%
    kable_styling(fixed_thead = T) %>%
    footnote(general = "Para más detalles puede consultar el fichero CFADetalles.doc") %>%
    scroll_box(width = "100%", height = "800px")
  
  # Almacena los resultados
  NmFileGrp<-paste0(DirPas,"CFAResumen.txt")
  write.table(ResFit$CFA,NmFileGrp)
  
  NmFileGrp<-paste0(DirPas,"CFAGraficos.pdf")
  pdf(NmFileGrp,width=600,height=768,paper="a4")
    plot(ResFit$M1$Plot);title("Variante tipo No Ortogonal", line = 3)
    plot(ResFit$M2$Plot);title("Variante tipo Ortogonal", line = 3)
    plot(ResFit$M3$Plot);title("Variante con Factores y puntuación Total", line = 3)
  dev.off()
  
  
  rtffile <- RTF(file = paste0(DirPas,"CFADetalles.doc"))
    addParagraph(rtffile,ResFit$M1$Type); addNewLine(rtffile,2);
    addParagraph(rtffile,ResFit$M1$fit); addNewLine(rtffile,2);
    addTable(rtffile, as.data.table(cbind(ResFit$M1$Detall$PE[1:3],round(ResFit$M1$Detall$PE[,4:11],3))));
    addPageBreak(rtffile);
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      plot(ResFit$M1$Plot);title("Variante tipo No Ortogonal", line = 3)
    })
    addPageBreak(rtffile);
    addParagraph(rtffile,ResFit$M2$Type); addNewLine(rtffile,2);
    addParagraph(rtffile,ResFit$M2$fit); addNewLine(rtffile,2);
    addTable(rtffile, as.data.table(cbind(ResFit$M2$Detall$PE[1:3],round(ResFit$M2$Detall$PE[,4:11],3))));
    addPageBreak(rtffile);
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      plot(ResFit$M2$Plot);title("Variante tipo Ortogonal", line = 3)
    })
    addPageBreak(rtffile);
    addParagraph(rtffile,ResFit$M3$Type); addNewLine(rtffile,2);
    addParagraph(rtffile,ResFit$M3$fit); addNewLine(rtffile,2);
    addTable(rtffile, as.data.table(cbind(ResFit$M3$Detall$PE[1:3],round(ResFit$M3$Detall$PE[,4:11],3))));
    addPageBreak(rtffile);
    addPlot(rtffile,plot.fun=print,width=6,height=6,res=600, {
      plot(ResFit$M3$Plot);title("Variante con Factores y puntuación Total", line = 3)
    })
    addPageBreak(rtffile);
    addParagraph(rtffile,"Resumen:"); addNewLine(rtffile,2);
    addTable(rtffile, (ResFit$CFA));
    LblBond<- read_rtf("NotasBondadAjuste.rtf");
    addText(rtffile,utf8Tortf(LblBond));
  done(rtffile)
  options(show.error.messages = TRUE)
  ResFit
}

# Funciones b??sicas de an??lisis estad??stico para apoyo a todos los Programas de Manuel Miguel
# Parte II. Versi??n 23 marzo de 2019
# Aspectos Psicom??tricos

QuanSim<-0.50
F.Max=20; N.Pop=10000;N.Samples=500; Alpha = 0.3

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

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
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
  
  Rnew <- as.matrix(Rnew)
  Rnew <- as.data.frame(Rnew)
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

quant <- function(x, sprobs = sprobs) { return(as.vector(quantile(x, probs = c(QuanSim)))) }

### Function 1/3: EFA.Comp.Data

EFA.Comp.Data <- function(DataP, F.Max=7, N.Pop=10000,N.Samples=500, Alpha = 0.3,scalP ,CasesP,TCor,UntilSig=T,Distributions) {
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
    Pop <- GenData(DataP, N.Factors = F.CD, N = N.Pop, Target.Corr = cor.Data,scalP = scalP,CasesP=CasesP,TCor = TCor,Distributions=Distributions)
    
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
GenData <- function(DataP, N.Factors, N, Max.Trials = 5, Initial.Multiplier = 1, Target.Corr,
                    scalP = scalP,CasesP=CasesP,TCor = TCor,Distributions) {
  # Ruscio, J., & Kaczetow, W. (2008). 
  # Simulating multivariate nonnormal data using an iterative algorithm. 
  # Multivariate Behavioral Research, 43(3), 355-381.
  k <- dim(DataP)[2]
  Iteration <- 0   # Iteration counter
  Best.RMSR <- 1   # Lowest RMSR correlation
  Trials.Without.Improvement <- 0   # Trial counter
  Data <- matrix(0, nrow = N, ncol = k)   # Matrix to store the simulated data
  # Calculate and store a copy of the target correlation matrix
  Intermediate.Corr <- Target.Corr
  # Generate random normal data, initialize factor loadings
  Shared.Comp <- matrix(rnorm(N * N.Factors, 0, 1), nrow = N, ncol = N.Factors)
  Unique.Comp <- matrix(rnorm(N.Pop * dim(DataP)[2], 0, 1), nrow = N.Pop, ncol =  dim(DataP)[2])
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

## Ya las he actualizado en 2019. Est??n preparadas para engarzarlas
TypCorrF <- function(x) {
  Typ<-c("Pearson", "Polychoric (Two Step)", "Polychoric (Max. Lik.)", "Spearman", 
         "Heterogenous (Two Step)", "Heterogenous (Max. Lik.)")
  if (x=="l") {return(length(Typ));break}
  if (x>length(Typ) || x<0) {stop("Ha sobrepasado el n??mero de Tipos contemplados")}
  Typ[x]
}

TypCorrDifF <- function(x) {
  Typ<-c("Pearson - Polychoric (Two Step)", "Pearson - Polychoric (Max. Lik.)",
         "Polychoric (Two Step) - Polychoric (Max. Lik.)", "Pearson - Spearman",
         "Polychoric (Two Step) - Spearman", "Polychoric (Max. Lik.) - Spearman")
  if (x=="l") {return(length(Typ));break}
  if (x>length(Typ) || x<0) {stop("Ha sobrepasado el n??mero de Tipos contemplados")}
  Typ[x]
}

TypExtrF <- function(x) {
  Typ<-c("pc","mle","fa","minres","wls","gls");
  # Que simboliza:
  # c("Principal Components","Maximum Lielihood","Principal Axis Factor","Minimum Residual",
  #    "Weighted Least Sqared","Generalized Least Squares")
  if (x=="l") {return(length(Typ));break}
  if (x>length(Typ) || x<0) {stop("Ha sobrepasado el n??mero de Tipos contemplados")}
  Typ[x]
}

TypExtr2F <- function(x) {
  Typ<-c("pca","ml","pa","minres","wls","gls","ipa","nipa")
  if (x=="l") {return(length(Typ));break}
  if (x>length(Typ) || x<0) {stop("Ha sobrepasado el n??mero de Tipos contemplados")}
  Typ[x]
}

TypRotF <- function(x) {
  Typ<-c("none", "varimax", "oblimin","promax")
  if (x=="l") {return(length(Typ));break}
  if (x>length(Typ) || x<0) {stop("Ha sobrepasado el n??mero de Tipos contemplados")}
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
  if (x=="l") {return(length(Typ));break}
  if (x>length(Typ) || x<0) {stop("Ha sobrepasado el n??mero de Tipos contemplados")}
  Typ[x]
}

OrtogF <- c("varimax","quartimax","equamax","parsimax","factor.parsimony","entropy","bentlerT","tandemI","tandemII","geominT","infomaxT","mccammon")

TypCase<-c("pairwise.complete.obs","complete.obs")
TypMod=c("Components","Factors")
TypSim<-c("Normal Distribution","Permutation")
TypEsta<-c("Mean","Quantile")  

TyEstrInAxisF <- function(x) {
  Typ<-c("component","maxr","ginv","multiple") 
  # Exclusivamente para los tipos de extracci??n "ipa" ?? "nipa"
  # Four different choices of initial communalities are given:
  # components, maximum correlation, generalized inverse and multiple correlation
  if (x=="l") {return(length(Typ));break}
  if (x>length(Typ) || x<0) {stop("Ha sobrepasado el n??mero de Tipos contemplados")}
  Typ[x]
}
## Ya las he actualizado en 2019. Est??n preparadas para engarzarlas


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
 	# Yo he a??adido los tests del paquete MVN {Mardia, Henze-Zirkler, & Royston}
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
   # Mis c??lculos propios
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
             MSA <- matrix(MSA[-1], 1)
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
                                 length(eigs))
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
   MSA <- matrix(MSA[-1], 1)
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
     eigvecr <- matrix(eigen(x)$vector[, 0:length(eigs)], length(eigs))
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
   #Servir?? para eleiminar m??s items y actualizr todo adecuadamente.
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

 ###### Añadidas en marzo de 2019

 EstimaFiab<-function(DTPas,NmFileGrp,Nfac=3) {
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
                    "Análisis Ind Discriminac cl??sico biserial-puntual de Fiabilidad: \n",
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
   ResIt$nm<-names(DTNm[,TkO,with=F])
   ResIt$nm2<-as.vector(DTNm[TkO2,2][[1]])
   ResIt$lbl<-paste0("Tarea ",Tk,"_",as.vector(DTNm[TkO2[1],3][[1]]),"_Factor ",as.vector(DTNm[TkO2[1],4][[1]]))
   ResIt$NFac<-length(EstrFac)
   if (ResIt$NFac>0) ResIt$Factor<-EstrFac
   options(warn=0)
   
   ResIt
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
   ResIt$nm<-names(DTNm[,TkO,with=F])
   ResIt$nm2<-as.vector(DTNm[TkO2,2][[1]])
   ResIt$lbl<-paste0("Tarea ",Tk,"_",as.vector(DTNm[TkO2[1],3][[1]]),"_Factor ",as.vector(DTNm[TkO2[1],4][[1]]))
   ResIt$NFac<-length(EstrFac)
   if (ResIt$NFac>0) ResIt$Factor<-EstrFac
   if (ResIt$NFac==0) ResIt$NFac<-1
   options(warn=0)
   
   ResIt
 }
 
 ProcItAlt19Sel<-function(Tk=1,Tasks,LosNm,DTNm,EscalaL=c("1 Nada","2 Algo","3 Medio","4 Bastante","5 Totalmente"),EstrFac,ItQuitarp) {
   options(warn=-1)
   ResIt<-list()
   NumL<-length(EscalaL)
   TkO<-grep(Tasks[Tk],LosNm,value=F)
   TkO0<-TkO[-ItQuitarp]
   TkO2<-grep(Tasks[Tk],DTNm$Item,value=F)
   TkO20<-TkO2[-ItQuitarp]
   stre<-LosNm[last(TkO0)]
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
   NumIt<-length(TkO0)
   #Conservar para escala de MSPSS precisamente
   #if (Tk==2) {NumIt<-which(last(PattID) == letters);NumAlt=1;NumL=2}
   ResIt$Task<-Tasks[[Tk]]
   ResIt$NumI<-NumIt
   ResIt$NumOpc<-NumAlt
   ResIt$NLik<-NumL
   ResIt$EscalaL<-EscalaL
   ResIt$TkO<-TkO0
   ResIt$TkO2<-TkO20
   ResIt$SQ<-seq(from=0, to=(NumIt*NumAlt),by=NumAlt)
   ResIt$nm<-names(DTNm[,TkO0,with=F])
   ResIt$nm2<-as.vector(DTNm[TkO20,2][[1]])
   ResIt$lbl<-paste0("Tarea ",Tk,"_",as.vector(DTNm[TkO20[1],3][[1]]),"_Factor ",as.vector(DTNm[TkO20[1],4][[1]]))
   ResIt$NFac<-length(EstrFac)
   if (ResIt$NFac>0) ResIt$Factor<-EstrFac
   if (ResIt$NFac==0) ResIt$NFac<-1
   options(warn=0)
   
   ModLav<-list()
   ModLavT<-list()
   NumF<-ResIt$NFac
   if (NumF>1) FactoresT<-c(paste0(ResIt$Task,".F",1:ResIt$NFac))
   NmTT<-paste0(ResIt$Task,"TT")
   ModLavT<-paste0(ModLavT, "\n", paste0(NmTT," =~ ",paste0(as.vector(ResIt$nm2),collapse = " + ")))
   if (NumF>1) {
     for (iF in c(1:ResIt$NFac)) {
       Nmm<-FactoresT[iF]
       SelnM<-paste0(ResIt$Task,"_",ResIt$Factor[[iF]])
       ModLav<-paste0(ModLav, "\n", paste0(Nmm," =~ ",paste0(SelnM,collapse = " + ")))
     }
     ModLavT<-paste0(ModLavT,ModLav)
   }
   if (NumF<=1) ModLav<-ModLavT
   ResIt$ModLav<-ModLav
   ResIt$ModLavT<-ModLavT
   ResIt
 }
 
 ProcPrev<-function(Tk=1,ResI,DTx,Acum=FALSE) {
   #require(parallel)
   #Expert<-DT[Grupo==1]
   #Lego<-DT[Grupo==2]
   
   #DT1<-DT[,ResI$TkO,with=F]
   ExpDT<-DTx[Sexo=="Hombre"][,ResI$TkO,with=F]
   LegoDT<-DTx[Sexo=="Mujer"][,ResI$TkO,with=F]
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
   if (DTpp[,is.numeric(get(VarG))]) {
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
 
 # fitlavPar19<-function(fit,Prec=2) {
 #   result<-list()
 #   
 #   fitSt<-fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea","rmsea.ci.lower", "rmsea.ci.upper" ,"SRMR","ntotal","gfi","nnfi"))
 #   
 #   result$Par<-parameterEstimates(fit,standardized=T)
 #   #CHI
 #   Val1=round(fitSt[[1]],Prec)
 #   Val2=fitSt[[2]]
 #   ValDiv=as.character(round((fitSt[[1]]/fitSt[[2]]),Prec))
 #   if ((Val1/Val2)<2) ValDiv=paste0(ValDiv,"*")
 #   Val3=as.character(round(fitSt[[3]],Prec))
 #   Lap=fitSt[[3]];
 #   if (Lap>=0.05) Val3=paste0(Val3,"*")
 #   #CFI
 #   Val4=as.character(round(fitSt[[4]],Prec))
 #   Lap=fitSt[[4]]; if (Lap>=0.95) Val4=paste0(Val4,"*")
 #   #RMSEA
 #   Val5=as.character(round(fitSt[[5]],Prec))
 #   Lap=fitSt[[5]];if (Lap<=0.06) Val5=paste0(Val5,"*");
 #   Val5b=paste0(Val5," (",round(fitSt[[6]],Prec),",",round(fitSt[[7]],Prec),")")
 #   #SRMR
 #   Val6=as.character(round(fitSt[[8]],Prec))
 #   Lap=fitSt[[8]];if (Lap<=0.08) Val6=paste0(Val6,"*")
 #   #N
 #   Val7=fitSt[[9]]
 #   
 #   #GFI
 #   Val8=as.character(round(fitSt[[10]],Prec))
 #   Lap=fitSt[[10]]; if (Lap>=0.95) Val8=paste0(Val8,"*");
 #   #NNFI
 #   Val9=as.character(round(fitSt[[11]],Prec))
 #   Lap=fitSt[[11]]; if (Lap>=0.95) Val9=paste0(Val9,"*");
 #   
 #   #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
 #   result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=paste0(Val3," {>=.05}"),GFI=paste0(Val8," {>=.95}"),CFI=paste0(Val4," {>=.95}"),
 #                           NNFI=paste0(Val9," {>=.95}"), RMSE=paste0(Val5b," {<=.06}"),SRMR=paste0(Val6," {<=.08}"),N=Val7,Ratio=paste0(ValDiv," {<2}"))
 #   result$All<-fitMeasures(fit)
 #   result
 # }
 

 
 
 fitlavPar19.2<-function(fitp,Prec=2) {
   #library(semTools)
   result<-list()
   
   fitSt<-fitMeasures(fitp, c("chisq", "df", "pvalue", "cfi","tli", "nnfi","SRMR","rmsea","rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "CN_05", "ntotal","ifi","AIC","BIC2"))
   
   result$Par<-parameterEstimates(fitp,standardized=T)
   
   #CHI
   Val1=round(fitSt[[1]],Prec)
   Val2=fitSt[[2]]
   ValDiv=as.character(round((fitSt[[1]]/fitSt[[2]]),Prec))
   if ((Val1/Val2)<2) ValDiv=paste0(ValDiv,"+")
   Val3=as.character(round(fitSt[[3]],Prec))
   Lap=fitSt[[3]];
   if (Lap>=0.05) Val3=paste0(Val3,"*")
   
   #CFI
   Val4=as.character(round(fitSt[[4]],Prec))
   Lap=fitSt[[4]]; if (Lap>=0.95) Val4=paste0(Val4,"+")
   #TLI
   Val5=as.character(round(fitSt[[5]],Prec))
   Lap=fitSt[[5]]; if (Lap>=0.95) Val5=paste0(Val5,"+")
   #NNFI
   Val6=as.character(round(fitSt[[6]],Prec))
   Lap=fitSt[[6]]; if (Lap>=0.95) Val6=paste0(Val6,"+");
   #SRMR
   Val7=as.character(round(fitSt[[7]],Prec))
   Lap=fitSt[[7]];if (Lap<=0.08) Val7=paste0(Val7,"+")
   
   #RMSEA
   Val8=as.character(round(fitSt[[8]],Prec))
   Val8.b=fitSt[[11]]
   if (Val8.b>0.05) Val8=paste0(Val8,"*");
   Val8c=paste0(Val8," (95% CI ",round(fitSt[[9]],Prec),"-",round(fitSt[[10]],Prec),")")
   
   #Hoelter CN
   Val9=as.character(round(fitSt[[12]],0))
   Lap=fitSt[[12]];if (Lap>200) Val9=paste0(Val9,"+")
   
   #N
   Val10=fitSt[[13]]
   
   #IFI Bollen
   Val11=as.character(round(fitSt[[14]],Prec))
   Lap=fitSt[[14]]; if (Lap>=0.95) Val11=paste0(Val11,"+")
   
   
   #AIC
   Val12=round(fitSt[[15]],0)
   #BIC Adj
   Val13=round(fitSt[[16]],0)
   
   MoreIdx<-round(moreFitIndices(fitp),4)
   #print("Hasta aquí Mas 2")
   Val14=as.character(round(MoreIdx[[1]],Prec))
   Lap=MoreIdx[[1]]; if (Lap>=0.95) Val14=paste0(Val14,"+")
   Val15=as.character(round(MoreIdx[[2]],Prec))
   Lap=MoreIdx[[2]]; if (Lap>=0.95) Val15=paste0(Val15,"+")
   Val16=as.character(MoreIdx[[3]])
   Lap=MoreIdx[[3]]; if (Lap>0.158) Val16=paste0(Val16,"+")
     
      #result$Bond<-data.frame(CHI=round(fitSt[[2]],4),df=fitSt[[3]],p.CHI=round(fitSt[[4]],4),CFI=round(fitSt[[8]],4),RMSE=round(fitSt[[23]],4),SRMR=round(fitSt[[29]],4))
   #result$Bond<-data.frame(CHI=Val1,df=Val2,p.CHI=paste0(Val3," {>=.05}"),GFI=paste0(Val8," {>=.95}"),CFI=paste0(Val4," {>=.95}"),
   #                       NNFI=paste0(Val9," {>=.95}"), RMSE=paste0(Val5b," {<=.06}"),SRMR=paste0(Val6," {<=.08}"),N=Val7,Ratio=paste0(ValDiv," {<2}"))
   
   result$Bond<-data.frame(CHI=Val1, df=Val2, p.CHI= Val3, CFI=Val4, IFI=Val11, TLI= Val5, NNFI=Val6, SRMR= Val7, RMSEA = Val8c, CN= Val9, N= Val10,
                           Ratio=ValDiv,AIC=Val12,BIC=Val13)
   names(result$Bond)<-c( "CHI",  "df", "p.CHI {>=05}", "CFI {>=0.95}","IFI {>=0.95}","TLI {>=0.95}", "NNFI {>=0.95}", "SRMR {<=.08}", 
                          "RMSEA {<=.06}", "Hoelter CN {>200}","N","Ratio {<2}","AIC","BIC Adj")
   result$All<-fitMeasures(fitp)
  
   result$Inter<- paste0("X2(", Val2, ") = ", Val1, ", p < ", Val3, ", Ratio {<2} = ", ValDiv, ", Hoelter CN {>200} = ", Val9, 
          ", CFI {>=0.95} = ", Val4, ", IFI {>=0.95} = ", Val11, ", TLI {>=0.95} = ", Val5, ", NNFI {>=0.95} = ", Val6,
          ", SRMR {<=.08} = ", Val7, ", RMSEA {<=.06} = ", Val8c, ", baseline RMSEA {>0.158} = ", Val16, 
          ", AIC = ", Val12, ", Adj BIC = ", Val13,
          ", gammaHat {>=0.95} = ", Val14, ", adjGammaHat {>=0.95} = ", Val15)
   
    result
 }
 
 # para llevar al programa General pues no son expresamente de psicometr??a
 AlmS<-function(PCat,PPrint,NmF,App=F) {
   Lcd<-"\n-------------------------------------------\n";
   capture.output(cat(PCat),print(PPrint),cat(Lcd),file=NmF,append = App);
 }
 
 # Estima la correlaci??n adecuada en funci??n del tipo de item (Ordinal, Num??rico o Categ??rico)
 # y aporta de manera compartativa Pearson
 # puede sustituir a las dos anteriores: IDnMM, IDnMMOrd
 # Los par??metros hacen referencia a la posici??n ocupada por cada tipo en Datos.
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

 # An??lisis de las Cargas Factoriales
 AnCargas<-function(tespas,LosItnp) {
   Res<-list()
   LosFactores<-tespas$loadings[,order(colnames(tespas$loadings))]
   rowmax <- apply(abs(LosFactores), 1, max)
   LosFactores[abs(LosFactores) < rowmax] <- 0
   conti=0;cutP=0
   factores<-NA;Variables<-NA;Variables2<-NA;cargasP<-NA
   
   for (nf in 1:ncol(LosFactores)) {
     for (i in 1:nrow(LosFactores)) {
       i2=LosItnp[i]
       if (abs(LosFactores[i, nf]) > cutP) {
         conti<-conti+1
         factores[conti]<-nf
         Variables[conti]<-i2
         Variables2[conti]<-row.names(LosFactores)[i]
         cargasP[conti]<-round(LosFactores[i, nf], 4)
         #print(paste(colnames(factors)[nf], "-> V", i, " [ label = ", round(factors[i, nf], 4), sep = ""))
       }
     }
   }
   
   ResCarg<-data.table(Factor=factores,Variable=Variables,Nombres=paste0(Variables,"_",Variables2),NombresOrig=Variables2,
                       Cargas=cargasP, Tipif=scale(cargasP,T,T)[,1])
   CargOrd<-ResCarg[order(Cargas)]
   SobrantesC<-CargOrd[Cargas<=SelCarg,Variable]
   SobrantesC2<-CargOrd[Cargas<=SelCarg,NombresOrig]
   
  Res$ResCarg<-ResCarg
  Res$Variables<-Variables
  Res$SobrantesNum<-SobrantesC
  Res$SobrantesName<-SobrantesC2
  Res
 }
 
 AnCargas2<-function(tespas,LosItnp,NFa=1) {
   Res<-list()
   LosFactores<-tespas$loadings[,order(colnames(tespas$loadings))]
   if (NFa>1)    {
     rowmax <- apply(abs(LosFactores), 1, max)
   LosFactores[abs(LosFactores) < rowmax] <- 0
   conti=0;cutP=0
   factores<-NA;Variables<-NA;Variables2<-NA;cargasP<-NA
   for (nf in 1:ncol(LosFactores)) {
     for (i in 1:nrow(LosFactores)) {
       i2=LosItnp[i]
       if (abs(LosFactores[i, nf]) > cutP) {
         conti<-conti+1
         factores[conti]<-nf
         Variables[conti]<-i2
         Variables2[conti]<-row.names(LosFactores)[i]
         cargasP[conti]<-round(LosFactores[i, nf], 4)
         #print(paste(colnames(factors)[nf], "-> V", i, " [ label = ", round(factors[i, nf], 4), sep = ""))
       }
     }
   }
   }
   
   if (NFa==1) {
     conti=0;cutP=0
     factores<-NA;Variables<-NA;Variables2<-NA;cargasP<-NA   
   for (i in 1:length(LosFactores)) {
     i2=LosItnp[i]
     if (abs(LosFactores[i]) > cutP) {
       conti<-conti+1
       factores[conti]<-1
       Variables[conti]<-i2
       Variables2[conti]<-names(LosFactores)[i]
       cargasP[conti]<-round(LosFactores[i], 4)
     }
   }
   }
   
   ResCarg<-data.table(Factor=factores,Variable=Variables,Nombres=paste0(Variables,"_",Variables2),NombresOrig=Variables2,
                       Cargas=cargasP, Tipif=scale(cargasP,T,T)[,1])
   CargOrd<-ResCarg[order(Cargas)]
   SobrantesC<-CargOrd[Cargas<=SelCarg,Variable]
   SobrantesC2<-CargOrd[Cargas<=SelCarg,NombresOrig]
   
   Res$ResCarg<-ResCarg
   Res$Variables<-Variables
   Res$SobrantesNum<-SobrantesC
   Res$SobrantesName<-SobrantesC2
   Res
 }
 
# Tkkp=2; ResIAllp=ResIAll;DirPas="Result"
 #DTp =DT ; ResIAllp =ResIAll;LosNmp=LosNm;SelecIDn=T;SelDn=0.25; CritVal=.15
 # AnItemsMM<-function(DTp,Tkkp, ResIAllp, LosNmp, SelecIDn=T,SelDn=0.25, CritVal=.15,DirPas="Result") {
 #   require(psych)
 #   require(parallel);require(data.table);require(psychometric);
 #   require(GPArotation)
 #   require(Rcsdp)
 #   require(CMC)
 #   
 #     Nfac<-ResIAllp[[Tkkp]]$NFac # N?? Factores impuesto para las estimaciones de CFA
 #     if (Nfac==0) Nfac=1
 #     Tkn<-Tkkp
 #     LaTask<-ResIAll[[Tkkp]]$Task
 #     RtNmFile<-paste0("IndiceDiscrim_",LaTask)
 #     ElDir<-Mk.dir(paste0(DirPas,"/",LaTask))
 #     
 #     IDn<-list()
 #     Sel<-as.vector(DTp[,grep(LaTask,LosNmp,value=F)])
 #     Resfinal<-DTp[,Sel,with=F]
 #     
 #     DTOmit<-data.table(na.omit(Resfinal))
 #     Number=1:length(Resfinal)
 #     
 #     if (Tkn ==2) { IDnF<- IDnMM_19(DTOmit,Number=0,Ordin=0,Categ=Number)}
 #     if (Tkn !=2) { IDnF<- IDnMM_19(DTOmit,Number=0,Ordin=Number,Categ=0)}
 #     
 #     #Antiguo Formato
 #     #if (Tkn ==2) {Y<-lapply(Number, function(i) {biserial.cor(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]],level=2)[[1]]})}
 #     #if (Tkn !=2) { Y<-lapply(Number, function(i) {cor(DTOmit[,rowSums(.SD)-DTOmit[,i,with=F]][[1]],DTOmit[,i,with=F][[1]])[[1]]})}
 #     #IDn<-c(rep(NA,length(DTOmit)))
 #     #IDn[Number]<-Y
 #     #IDnF<- data.table(Names=names(DTOmit),Corr=do.call(rbind,setNames(IDn,names(DTOmit))))
 #     
 #     ElMensa<-"??ndices de Discriminaci??n \n";
 #     print(ElMensa)
 #     print(IDnF)
 #     IDnF[,dotchart(Corr.IDn , labels=Names,cex=0.75,main="Indice Discriminacion C??lculos Rutina Propia")]
 #     abline(v=.2,lty=2);abline(v=-.2,lty=2)
 #     abline(v=.3,lty=2);abline(v=-.3,lty=2)
 #     
 #     # Ahora comparo mis c??lculos con los del paquete alpha
 #     
 #     ResAlpha<-psych::alpha(DTOmit)
 #     #summary(ResAlpha)
 #     # Con Omega (variante de Alfa)
 #     om.results<-psych::omega(DTOmit,nfactors=Nfac)
 #     # Y con el Ind Discriminac cl??sico basado en la biserial-puntual
 #     ResPsych<-item.exam(DTOmit, discrim=TRUE)
 #     
 #     # A??ado las 2 estimaciones
 #     #ResAlpha$item.stats$r.drop
 #     IDnF[,rdrop:=ResAlpha$item.stats$r.drop]
 #     IDnF[,rbis:=ResPsych$Item.Tot.woi]
 #     IDnF[,Discrim:=ResPsych$Discrimination]
 #     IDnF[,Dific:=ResPsych$Difficulty]
 #     IDnF[,Fiab:=ResPsych$Item.Rel.woi]
 #     
 #     # Vuelvo a ordenar sobre la base del primero de los 3 c??lculos
 #     IDfOrd<-IDnF[order(Corr.IDn)]
 #     ElMensa<-"\n??ndices de Discriminaci??n Ordenados \n";
 #     print(ElMensa)
 #     print(IDfOrd)
 #     
 #     #quartz("Indice Discriminacion Alpha")
 #     IDfOrd[,dotchart(rdrop , labels=Names,cex=0.75,main="Indice Discriminacion Alpha")]
 #     abline(v=.2,lty=2);abline(v=-.2,lty=2)
 #     abline(v=.3,lty=2);abline(v=-.3,lty=2)
 #     
 #     ElMensa<-"\n??ndices de la Dificultad \n";
 #     print(ElMensa)
 #     IDfOrd[,plot(density(Dific),main="Distribuci??n Dificultad a trav??s ??tems")]
 #     IDfOrd[,hist(Dific, freq=FALSE,add=T)]
 #     # Kernel density plot
 #     
 #     #Almacena Resultados en el fichero IDn.pdf
 #     NmFileGrp<-Mk.pdf(NmDirp =paste0(DirPas,"/",LaTask) ,NmFilep = paste0("IDn",RtNmFile))
 #     #NmFileGrp<-paste0(PathRes,"IDn",RtNmFile,".pdf")
 #     #pdf(NmFileGrp,width=8.27,height=11.69)
 #     IDnF[,dotchart(Corr.IDn , labels=Names,cex=0.75,
 #                    main="Indice Discriminacion C??lculos Rutina Propia")]
 #     abline(v=.2,lty=2);abline(v=-.2,lty=2)
 #     abline(v=.3,lty=2);abline(v=-.3,lty=2)
 #     
 #     IDfOrd[,dotchart(rdrop , labels=Names,cex=0.75,
 #                      main="Indice Discriminacion Alpha Ordenado")]
 #     abline(v=.2,lty=2);abline(v=-.2,lty=2)
 #     abline(v=.3,lty=2);abline(v=-.3,lty=2)
 #     
 #     IDfOrd[,dotchart(rbis , labels=Names,cex=0.75,
 #                      main="Indice Discriminacion C??lculos B??sicos Ordenado")]
 #     abline(v=.2,lty=2);abline(v=-.2,lty=2)
 #     abline(v=.3,lty=2);abline(v=-.3,lty=2)
 #     
 #     IDnF[,dotchart(Dific , labels=Names,cex=0.75,
 #                    main="Indice Dificultad C??lculos B??sicos")]
 #     abline(v=.1,lty=2);abline(v=-.1,lty=2)
 #     abline(v=.2,lty=2);abline(v=-.2,lty=2)
 #     
 #     IDfOrd[,dotchart(Dific , labels=Names,cex=0.75,
 #                      main="Indice Dificultad C??lculos B??sicos Ordenado seg??n Discriminaci??n")]
 #     abline(v=.1,lty=2);abline(v=-.1,lty=2)
 #     abline(v=.2,lty=2);abline(v=-.2,lty=2)
 #     
 #     IDnF[,dotchart(Fiab , labels=Names,cex=0.75,main="Indice Fiabilidad C??lculos B??sicos")]
 #     IDfOrd[,dotchart(Fiab , labels=Names,cex=0.75,
 #                      main="Indice Fiabilidad C??lculos B??sicos Ordenado seg??n Discriminaci??n")]
 #     
 #     IDfOrd[,plot(density(Dific),main="Distribuci??n Dificultad a trav??s ??tems")]
 #     IDfOrd[,hist(Dific, freq=FALSE,add=T)]
 #     alpha.curve(DTOmit)
 #     dev.off()
 #     
 #     #Almacena tb los resultados
 #     NmFileGrp<-paste0(ElDir,"ResInDis",RtNmFile,".csv")
 #     write.table(IDfOrd,NmFileGrp)
 #     
 #     # Otros ??ndices para probar
 #     ElMensa<-"An??lisis Fiabilidad basado en Alpha: \n";tags$b(ElMensa)
 #     print(summary(ResAlpha))
 #     ElMensa<-"An??lisis Fiabilidad basado en Omega: \n";tags$b(ElMensa)
 #     print(om.results);
 #     print(psych::omega.diagram(om.results,cex=2))
 #     ElMensa<-"An??lisis Fiabilidad basado en Ind Discriminac cl??sico seg??n la biserial-puntual: \n";tags$b(ElMensa)
 #     print(ResPsych)
 #     
 #     test.simple1 <-psych::fa(DTOmit,Nfac,rotate="oblimin")
 #     test.simple2 <-psych::fa(fm="minrank",DTOmit,Nfac,rotate="oblimin")
 #     #if(require(Rgraphviz)) {fa.graph(test.simple) } 
 #     ElMensa<-"An??lisis Factoral por defecto y Oblimin: \n";tags$b(ElMensa)
 #     psych::fa.diagram(test.simple1)
 #     ElMensa<-"An??lisis Factoral por defecto y Oblimin Varianza Explicada: \n";tags$em(ElMensa)
 #     test.simple1$Vaccounted
 #     ElMensa<-"An??lisis Factoral minrank-Oblimin: \n";tags$b(ElMensa)
 #     psych::fa.diagram(test.simple2)
 #     ElMensa<-"An??lisis Factoral minrank-Oblimin Varianza Explicada: \n";tags$em(ElMensa)
 #     test.simple2$Vaccounted
 #     ElMensa<-"An??lisis Alternativo basado en Clusters: \n";tags$em(ElMensa)
 #     ic.out <- psych::iclust(DTOmit)
 #     #psych::iclust.diagram(ic.out,cex=.4)
 #     
 #     NmFileGrp<-Mk.pdf(NmDirp =paste0(DirPas,"/",LaTask) ,NmFilep = paste0("Omega_SinFiltro",RtNmFile))
 #     #NmFileGrp<-paste0(ElDir,"Omega_SinFiltro",RtNmFile,".pdf")
 #     #pdf(NmFileGrp,width=8.27,height=11.69)
 #     psych::omega.diagram(om.results,cex=5)
 #     psych::fa.diagram(test.simple1,rsize=.5)
 #     psych::fa.diagram(test.simple2,rsize=.5)
 #     psych::iclust.diagram(ic.out,cex=.4)
 #     dev.off()
 #     
 #     # NmFileGrp<-paste0(PathData,"ResFact1_SinFil",RtNmFile,".txt")
 #     # NmFileGrp2<-paste0(PathData,"ResFact2_SinFil",RtNmFile,".txt")
 #     # NmFileGrp3<-paste0(PathData,"ResConsis_SinFil",RtNmFile,".txt")
 #     # 
 #     # sink(NmFileGrp); print(test.simple1$Vaccounted); print(test.simple1);sink()
 #     # sink(NmFileGrp2); print(test.simple2$Vaccounted); print(test.simple2);sink()
 #     # sink(NmFileGrp3); print(summary(ResAlpha)); print(om.results);sink()
 #     
 #     PCatP<-"\n F.1) An??lisis Factoral por defecto y Oblimin sin filtrado por IDn: \n";AlmS(PCatP,test.simple1$Vaccounted,NmAlmacen,T);
 #     PCatP<-"\n Detalles: \n";AlmS(PCatP,test.simple1,NmAlmacen,T);
 #     PCatP<-"\n F.2) An??lisis Factoral minrank-Oblimin sin filtrado por IDn: \n";AlmS(PCatP,test.simple2$Vaccounted,NmAlmacen,T);
 #     PCatP<-"\n Detalles: \n"; AlmS(PCatP,test.simple2,NmAlmacen,T);
 #     PCatP<-"\n F.3) An??lisis Alternativo basado en Alpha sin filtrado por IDn: \n";AlmS(PCatP,summary(ResAlpha),NmAlmacen,T);
 #     PCatP<-"\n Detalles: \n";AlmS(PCatP,om.results,NmAlmacen,T);
 #     PCatP<-"\n An??lisis Unidimensionalidad Experimental Revelle: \n";AlmS(PCatP,unidim(DTOmit),NmAlmacen,T);
 #     
 #     #sink(NmAlmacen,append = T); print("F.1) An??lisis Factoral por defecto y Oblimin sin filtrado por IDn: \n");print(test.simple1$Vaccounted); print(test.simple1);print(LaLinea);sink()
 #     #sink(NmAlmacen,append = T); print("F.2) An??lisis Factoral minrank-Oblimin sin filtrado por IDn:  \n ");print(test.simple2$Vaccounted); print(test.simple2);print(LaLinea);sink()
 #     #sink(NmAlmacen,append = T); print("F.3) An??lisis Alternativo basado en Alpha sin filtrado por IDn: \n ");print(summary(ResAlpha));print(om.results);print(LaLinea);sink()
 #     
 #     #La selecci??n final:
 #     #IDfOrd[Corr.V1<0.3,Names]
 #     Todos<-names(Resfinal)
 #     Sobrantes<-IDnF[Corr.IDn<=SelDn,Names]
 #     ElMensa<-"Conclusi??n: ??tems a eleminar seg??n el Ind Discriminaci??n: \n";tags$p(ElMensa)
 #     print(data.table(Sobrantes))
 #     
 #     SeQuedan<-IDnF[Corr.IDn>SelDn,Names]
 #     TkOcF <- unique (grep(paste(SeQuedan,collapse="|"), 
 #                           names(Resfinal), value=F))
 #     ElMensa<-"Conclusi??n: Y los ??tems que no se eliminar??an seg??n el Ind Discriminaci??n: \n";tags$p(ElMensa)
 #     print(data.table(names(Resfinal[,TkOcF,with=F])))
 #     tags$hr();
 #     
 #     PCatP<-"\n G) Conclusi??n Filtrados: \n";AlmS(PCatP,"",paste0(ElDir,Tasks[Tkk],".txt"),F);
 #     PCatP<-"\n ??tems a eleminar seg??n el Ind Discriminaci??n: \n";AlmS(PCatP,data.table(Sobrantes),paste0(ElDir,Tasks[Tkk],".txt"),T);
 #     PCatP<-"\n Y los ??tems que no se eliminar??an seg??n el Ind Discriminaci??n: \n";AlmS(PCatP,data.table(names(Resfinal[,TkOcF,with=F])),paste0(ElDir,Tasks[Tkk],".txt"),T);
 #     
 #     #sink(NmAlmacen,append = T); print("G) Conclusi??n Filtrados: \n");print("??tems a eleminar seg??n el Ind Discriminaci??n: \n");print(data.table(Sobrantes));print("Y los ??tems que no se eliminar??an seg??n el Ind Discriminaci??n: \n"); print(data.table(names(Resfinal[,TkOcF,with=F])));print(LaLinea);sink();
 #     
 #     #Aqu?? se aplica este 2?? filtro basado en el ??nd Discirminaci??n, tras el filtro 1?? basado en el Ind Conc
 #     ResfinalSel<-Resfinal[,TkOcF,with=F]
 #     if (SelecIDn) Resfinal<-ResfinalSel
 #     Resfinal
 # }
 
 #Simula Todos AF
 # DTOmitp=DTOmit;Number=NA;Categ=NA
 # Ordin=c(length(DTOmit));TypData=Tasks[TskSel]
 #DirP=PathRes
 # iEx<-1;iRot<-2
 # PromaxMake = F
 #DTOmitp = DTOmit;Ordin=c(length(DTOmit));TypData=Tasks[Tks];PromaxMake = T;DirP=PathRes; Nfac=3
 SimulFA<-function(DTOmitp,Ordin,Number=NA,Categ=NA,TypData,PromaxMake = T,DirP,Nfac=3) {
   require(polycor)
   require(psych)
   require(parallel);require(data.table);require(psychometric);
   require(GPArotation)
   require(Rcsdp)
   require(nFactors)
   require(corpcor)
   
   # DTOmitp: De donde partimos en esa escala
   
   NmDir<-paste0(DirP,TypData,"/")
   #NmDir<-paste0(PathData,TypData,"/",Corr,"_",ExtrMet2)
   if (!dir.exists(NmDir)) {try(dir.create(NmDir))}
   if (PromaxMake) NmDir2<-paste0(NmDir,"ConProMax","/")
   if (!PromaxMake) NmDir2<-paste0(NmDir,"SinProMax","/")
   if (!dir.exists(NmDir2)) {try(dir.create(NmDir2))}
   
   #Ordin=c(length(DTOmitp))
   #Number=NA
   #Categ=NA
   Data<-data.frame(DTOmitp)
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
   TyEstrInAxis<-c("component","maxr","ginv","multiple") #Exclusivamente para los tipos ipa ?? nipa
   # Four different choices of initial communalities are given:
   # components, maximum correlation, generalized inverse and multiple correlation
   Ortog<- c("varimax","quartimax","equamax","parsimax","factor.parsimony","entropy","bentlerT","tandemI","tandemII","geominT","infomaxT","mccammon")
   
   TypeCorrInic<-5 ; #1 es Pearson y 5 es Heterog??nea;
   CrTs=1;CrTCD=1 # Para fijar el tipo de Corr en las simulaciones
   Model=TypMod[1] # Se establece el modelo de 1: Componentes o 2: Factores
   if (Model==TypMod[1]) compPP=TRUE else compPP=FALSE
   DelCas=TRUE # True: "Delete cases listwise"
   Cases<-TypCase[2]
   suprNA<-0
   CrT=TypeCorrInic; # Tengo que fijar el tipo de Correlaci??n desde el principio
   
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
   
   
   ###### B. Para las simulaciones de todos los tipos de an??lisis factoriales.
   conti2=0
   ResCargAll<-list();
   BondAj<-list()
   
   for (iEx in 1:length(TypExtr2)) {
     for (iRot in 2:length(TypRot2)) {
       conti2=conti2+1    
       NFExtr=Nfac      # N?? Factores, lo marca todo ????Ojo!!
       TyExI<-1 # TypExtr<-c("pc","mle","fa","minres","wls","gls");
       TyRttI<-2 #  TypRot<- c("none", "varimax", "oblimin","promax")
       TyExI2<-iEx  #TypExtr2<-c("pca","ml","pa","minres","wls","gls","ipa","nipa")
       TyRttI2<-iRot # c("none","varimax","quartimax","quartimin","equamax","parsimax","factor.parsimony",
       #  "biquartimin","covarimin","oblimax","entropy","simplimax","bentlerT",
       #  "bentlerQ","tandemI","tandemII","geominT","geominQ","infomaxT",
       #  "infomaxQ","mccammon")
       #PromaxMake=T; # Promax despu??s de una que sea ortogonal
       TyItAx=1 # TyEstrInAxis<-c("component","maxr","ginv","multiple") #Exclusivamente para los tipos ipa ?? nipa
       uFCut2=.25 #2?? punto de corte ??nicamente para limpiar un poco los gr??ficos finales de n??meros
       uSimMax=.3;uSimMin=.1 # Criterios Soluci??n ??nica
       
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
       Rott=TypRot[TyRttI]   # Tipo de Rotaci??n Inicial
       Diago=FALSE
       ExtrMet=TypExtr[TyExI] # Tipo de Extracci??n Inicial
       #CD Method
       DelCasCD=TRUE # True: "Delete cases listwise"
       UntilSig=TRUE
       F.Max=20; N.Pop=10000;N.Samples=500; Alpha = 0.3
       
       Rtt2=TyRttI2
       Rott2<-TypRot2[Rtt2]; #Tipo Rotaci??n Importante
       
       ExtrMet2=TypExtr2[TyExI2]; #Tipo Extracci??n Importante
       Sorting=FALSE
       UFacPt=c(1,2)
       RFacPt=c(1,2)
       StgUFD=FALSE
       StgRFD=FALSE
       uFCut=0.30     # Umbral asignaci??n cargas factoriales
       KaissNorm=T
       KppPromax<-4
       kSimpliMax<-0
       InitExtrAx<-TyEstrInAxis[TyItAx]
       
       #quant <- function(x, sprobs = sprobs) { return(as.vector(quantile(x, probs = c(QuanSim)))) }
       pr <- NFExtr
       
       #if (PromaxMake) NmDir0<-paste0(NmDir,"/","ConProMax","/")
       #if (!PromaxMake) NmDir0<-paste0(NmDir,"/","SinProMax","/")
       
       #if (PromaxMake) NmDir0<-paste0(PathData,TypData,"/","ConProMax","/")
       #if (!PromaxMake) NmDir0<-paste0(PathData,TypData,"/","SinProMax","/")
       NmDirN<-paste0(NmDir2,Corr,"_",ExtrMet2)
       if (!dir.exists(NmDirN)) {try(dir.create(NmDirN))}
       
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
       NmFileGrp<-paste0(NmDirN,"/AF1 CommPequeñas_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
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
       
       NmFileGrp<-paste0(NmDirN,"/AFGrp1 Scree Plot_",TypData,"_",Corr,"_",ExtrMet2,".pdf")
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
       NmFileGrp<-paste0(NmDirN,"/AF2_Var.Expl (initial)_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
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
       NmFileGrp<-paste0(NmDirN,"/AF3_Var.Expl Extracted_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
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
         
         NmFileGrp<-paste0(NmDirN,"/AFGrp2 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,".pdf")
         pdf(NmFileGrp)
         factor.plot(x, cut = uFCut, labels=nam, xlim = c(-1, 1), ylim = c(-1, 1), xlab=x1,
                     ylab=y1, title = "Factor Plot (initial solution)")
         dev.off()
       } 
       
       # Factor diagram unrotated
       #quartz("Factor Diagram")
       # fa.diagram(ad, sort=StgUFD, labels=NULL, cut=uFCut, simple=FALSE, errors=TRUE, digits=2,
       #            e.size=0.05, rsize=0.15, side=2, main="Factor Diagram (initial solution)", cex=NULL) 
       
       NmFileGrp<-paste0(NmDirN,"/AFGrp3 Factor Diagram_",TypData,"_",Corr,"_",ExtrMet2,".pdf")
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
         NmFileGrp<-paste0(NmDirN,"/AF3_Var.Expl Extracted_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
         write.table(VarExplica,file=NmFileGrp)
         #VarExplica<-junto
         
         title="Rotation Matrix";
         colnames(ad2$Th)<-paste ("F",1:pr, sep="");
         row.names(ad2$Th)<-paste ("F",1:pr, sep="")
         #print(title);print(ad2$Th)
         NmFileGrp<-paste0(NmDirN,"/AF4_Rott Matrx_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
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
           NmFileGrp<-paste0(NmDirN,"/AF5_Corr Matrx Rott Factors_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
           write.table(ad2$Phi,file=NmFileGrp)
           NmFileGrp<-paste0(NmDirN,"/AF5b_Corr Matrx Rott Factors (0.4)_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
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
           
           NmFileGrp<-paste0(NmDirN,"/AFGrp4 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
           pdf(NmFileGrp,width=8.27,height=11.69)
           factor.plot(x, cut = uFCut, labels=nam,  xlim = c(-1, 1), ylim = c(-1, 1), xlab=x1, ylab=y1, title = "Factor Plot (rotated solution)")  
           dev.off()
         } 
         
         # Factor diagram after rotation
         ad$loadings <- ad2$loadings
         colnames(ad$loadings) <- paste("F", 1:pr, sep = "")
         
         #quartz("Factor Diagram")
         #fa.diagram(ad, sort=T, labels=NULL, cut=uFCut, simple=, errors=TRUE, digits=2, e.size=0.05, rsize=0.15, side=2, main="Factor Diagram (rotated solution)", cex=1)
         
         NmFileGrp<-paste0(NmDirN,"/AFGrp5 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
         pdf(NmFileGrp,width=8.27,height=11.69)
         fa.diagram(ad, sort=T, labels=NULL, cut=uFCut, simple=, errors=TRUE, digits=2, e.size=0.05, rsize=0.15, side=2, main="Factor Diagram (rotated solution)", cex=1)
         dev.off()
         
         #quartz("Factor Diagram Sin Ordenar")
         #fa.diagram(ad, sort=F, labels=NULL, cut=uFCut, simple=T, errors=TRUE, digits=2, e.size=0.05, rsize=0.2, side=2, main="Factor Diagram (rotated solution) II", cex=1) 
         
         NmFileGrp<-paste0(NmDirN,"/AFGrp6 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
         pdf(NmFileGrp,width=8.27,height=11.69)
         fa.diagram(ad, sort=F, labels=NULL, cut=uFCut, simple=T, errors=TRUE, digits=2, e.size=0.05, rsize=0.2, side=2, main="Factor Diagram (rotated solution) II", cex=1) 
         dev.off()
         
         #quartz("Factor Diagram Sin Ordenar y Sin Simplificar")
         #fa.diagram(ad, sort=F, labels=NULL, cut=uFCut, simple=F, errors=TRUE, digits=2, e.size=0.05, rsize=0.2, side=2, main="Factor Diagram (rotated solution) III", cex=1) 
         
         NmFileGrp<-paste0(NmDirN,"/AFGrp7 Factor Plot_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
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
         NmFileGrp<-paste0(NmDirN,"/AFGrp8 Factor Plot Def1_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
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
         NmFileGrp<-paste0(NmDirN,"/AFGrp9 Factor Plot Def2_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".pdf")
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
       ResFaAll$Complex<-ad$complexity # Aqui a??ado la Complejidad (??Ondice de Hoffman's)
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
       NmFileGrp<-paste0(NmDirN,"/AF4_PartCorr Control Var",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
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
       NmFileGrp<-paste0(NmDirN,"/AF5_PartCorr Control Fac",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
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
       NmFileGrp<-paste0(NmDirN,"/AFResum1_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
       write.table(ResfMM1$Basic,file=NmFileGrp)
       NmFileGrp<-paste0(NmDirN,"/AFResum2_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
       write.table(ResfMM1$MM,file=NmFileGrp)
       NmFileGrp<-paste0(NmDirN,"/AFResum3_",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
       write.table(ResfMM1$Corr,file=NmFileGrp)
       NmFileGrp<-paste0(NmDirN,"/AFResum4_Structr",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
       write.table(ResFaAll,file=NmFileGrp)
       NmFileGrp<-paste0(NmDirN,"/AFResum5_Structr2",TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt")
       write.table(ResCarg,file=NmFileGrp)
       ResCargAll[[conti2]]<-ResCarg
       print(paste0(round(100*(conti2/(length(TypExtr2)*length(TypRot2))),2),"%: ", TypData,"_",Corr,"_",ExtrMet2,"_",Rott2,".txt"))
       if (is.null(ad2$Phi)) {MaxCorObliqP=NA;MeanCorObliqP=NA}
       if (!is.null(ad2$Phi)) {MaxCorObliqP=max(abs(ad2$Phi[lower.tri(ad2$Phi)])); MeanCorObliqP= mean(abs(ad2$Phi[lower.tri(ad2$Phi)]))}
  
       BondAj[[conti2]]<-data.table(KMOClass=KMO2[[1]],ResfMM2,fit=round(ad$fit,4),fit.off=round(ad$fit.off,4),
                                    Chi=round(ad$STATISTIC,4),ChiEmp=round(ad$chi,4), df=ad$dof,pVal=round(ad$PVAL,5),Ratio=round(ad$STATISTIC,4)/ad$dof, 
                                    rms=round(ad$rms,4), t(round(ResfMM,4)), (ResFA[[11]][1,2:4]), (ResFA[[11]][3,2:4]),
                                    TypeExtrac=ExtrMet2,TypeRotat=Rott2,Ortog=ad2$orthogonal,HypCatt=HyperplaneCatt,CmplxHoff=CompleHoff,
                                    MaxCorObliq=MaxCorObliqP, MeanCorObliq= MeanCorObliqP)
       
       
     }
   }
   
   
   NmBdAj<-c("KMO {>.6}", "KMO(MSA) {>.6}", "GFI(ULS) {>.9}", "GFI(ML) {>.9}", "RMSR {<.06.08}", "RMSP {<.06.08}", "Residuals>0.05", "%Residuals>0.05",
             "Determinant {0}", "fit", "fit.off", "Chi", "ChiEmp", "df", "pVal","RatioCh_df {<2}", "rms {<.06.08}",  
             "Mean {>.7}", "Min", "Q1", "Md", "Q3", "Max",
             "Menor0.7", "%Menor0.7", "Menor0.5", "%Menor0.5", "Var.Expl.Init", "Var.Expl.Extrac", 
             "Bartlett Test", "p.Bartlett", "Steiger Test", "p.Steiger", "Jennrich Test", "p.Jennrich",
             "TypeExtrac", "TypeRotat", "Ortog", "HypCatt", "CmplxHoff","MaxCorObliq {.32}", "MeanCorObliq {.32}")
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
   setorder(ResBond2,Ortog,-HypCatt,CmplxHoff) #Estudio ??ndices Complejidad
   
   ResBond3<-ResBond2[TypeRotat=="varimax"]
   ResBond3[,Ortog:=NULL];ResBond3[,TypeRotat:=NULL]
   NmDirG<-paste0(DirP,TypData,"/")
   #NmDirG<-paste0(PathData,TypData,"/")
   if (!PromaxMake) {
       # Antes de Promax
       NmFileGrp<-paste0(NmDirG,"AFResumAll2_Compara Extraccion_",TypData,"_",Corr,"_",".txt")
       write.table(ResBond3,file=NmFileGrp)
       NmFileGrp<-paste0(NmDirG,"AFResumAll1_Compara Extracc&Rott_",TypData,"_",Corr,"_",".txt")
       write.table(ResBond2,file=NmFileGrp)
       NmFileGrp<-paste0(NmDirG,"AFResumAll3_Compara Todo_",TypData,"_",Corr,"_",".txt")
       write.table(ResCar2,file=NmFileGrp)
       }
      if (PromaxMake) {
        NmFileGrp<-paste0(NmDirG,"AFResumAll2_Compara Extraccion (incl Promax)_",TypData,"_",Corr,"_",".txt")
        write.table(ResBond3,file=NmFileGrp)
        NmFileGrp<-paste0(NmDirG,"AFResumAll1_Compara Extracc&Rott (incl Promax)_",TypData,"_",Corr,"_",".txt")
        write.table(ResBond2,file=NmFileGrp)
        NmFileGrp<-paste0(NmDirG,"AFResumAll3_Compara Todo (incl Promax)_",TypData,"_",Corr,"_",".txt")
        write.table(ResCar2,file=NmFileGrp)
      }
   ResFinal<-list()
   ResFinal$RCargas<-ResCar2
   ResFinal$RBond <- ResBond2
   ResFinal$RBondExtr<-ResBond3
   ResFinal
 }
 
 OnlyCorr<-function(DTOmitp,Ordin,Number=NA,Categ=NA,TypData,DirP) {
   require(polycor)
   require(psych)
   require(parallel);require(data.table);require(psychometric);
   require(GPArotation)
   require(Rcsdp)
   require(nFactors)
   require(corpcor)
   
   # DTOmitp: De donde partimos en esa escala
   
   NmDir<-paste0(DirP,TypData,"/")
   #NmDir<-paste0(PathData,TypData,"/",Corr,"_",ExtrMet2)
   if (!dir.exists(NmDir)) {try(dir.create(NmDir))}
   
   #Ordin=c(length(DTOmitp))
   #Number=NA
   #Categ=NA
   Data<-data.frame(DTOmitp)
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
   TyEstrInAxis<-c("component","maxr","ginv","multiple") #Exclusivamente para los tipos ipa ?? nipa
   # Four different choices of initial communalities are given:
   # components, maximum correlation, generalized inverse and multiple correlation
   Ortog<- c("varimax","quartimax","equamax","parsimax","factor.parsimony","entropy","bentlerT","tandemI","tandemII","geominT","infomaxT","mccammon")
   
   TypeCorrInic<-5 ; #1 es Pearson y 5 es Heterog??nea;
   CrTs=1;CrTCD=1 # Para fijar el tipo de Corr en las simulaciones
   Model=TypMod[1] # Se establece el modelo de 1: Componentes o 2: Factores
   if (Model==TypMod[1]) compPP=TRUE else compPP=FALSE
   DelCas=TRUE # True: "Delete cases listwise"
   Cases<-TypCase[2]
   suprNA<-0
   CrT=TypeCorrInic; # Tengo que fijar el tipo de Correlaci??n desde el principio
   
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
   corF
 }
 
 ExtrIntesecFA<-function(ResFrecPas,nmData,ResIAllp,CrPesos=.6) {
   # Para pruebas
   # ResFrecPas=ResFrec2Omn;nmData=names(DTOmit);ResIAllp=ResIAll
   rl<-levels(data.table(ResFrecPas)$Nombres)
   patt=paste0("_",substring(strsplit(rl[1],"_")[[1]][2],1,1))
   DTpr<-as.data.table(as.matrix(ResFrecPas))
   DTpr2<-data.table(Nombres=rl, Factor=DTpr[,max.col(.SD)])
   nFa<-1:ncol(DTpr)
   #lapply(DTpr2[,list(Nombres),by=Factor])
   #DTpr2[Factor==1,Nombres]
   Y<-lapply(nFa, function(i) {DTpr2[Factor==i,Nombres]})
   #names(DTOmit)
   Z<-lapply(nFa, function(i) {nmData[ResIAllp$Factor[[i]]]})
   L<-lapply(nFa, function(i) {
     NewOr<-as.integer(substr(Y[[i]],start = rep(1,length(Y[[i]])),stop=c(regexpr(patt,Y[[i]])-1)))
     Y[[i]]<-Y[[i]][order(NewOr)]
     Y[[i]]<-substring(Y[[i]],c(regexpr(patt,Y[[i]])+1))})
   VecInt<-c(do.call(rbind,lapply(nFa, function(j) {
     which.max(do.call(rbind,lapply(nFa, function(i) {length(intersect(Z[[j]],L[[i]]))})))})))
   ResFin<-lapply(nFa, function(i) {intersect(Z[[i]],L[[VecInt[i]]])})
   ResDif<-lapply(nFa, function(i) {outersect(Z[[i]],ResFin[[i]])})
   ResFinAll <-list()
   # ResFinAll$Original<-as.character(Z)
   # ResFinAll$OptimaSinFiltros<-as.character(L)
   # ResFinAll$Interseccion<-as.character(ResFin)
   estan<-intersect(VecInt,nFa)
   nestan<-setdiff(nFa,VecInt)
   union<-union(estan,nestan)
   # ResFinAll$OptimaSinFiltrosOrd<-as.character(L[union])
   # ResFinAll
   
   
   Ls<-lapply(nFa, function(i) {
     NewOr<-as.integer(substr(Y[[i]],start = rep(1,length(Y[[i]])),stop=c(regexpr(patt,Y[[i]])-1)))
     Y[[i]]<-Y[[i]][order(NewOr)]
     Y[[i]]<-toString(substring(Y[[i]],c(regexpr(patt,Y[[i]])+1)))
   })
   ResFinAll$Original<-lapply(nFa, function(i) {toString(nmData[ResIAllp$Factor[[i]]])})
   ResFinAll$Interseccion<-lapply(nFa, function(i) {toString(intersect(Z[[i]],L[[VecInt[i]]]))})
   ResFinAll$OptimaSinFiltros<-Ls
   ResFinAll$OptimaSinFiltrosOrd<-Ls[union]
   ResFinAll$Sobrantes<-lapply(nFa, function(i) {toString(outersect(Z[[i]],ResFin[[i]]))})
   ResFinAll
 }
 
 ExtrPesosFA<-function(ResFrecPas,nmData,ResIAllp,CrPesos=.6) {
   require(matrixStats)
   # Para pruebas
   # ResFrecPas=ResFrec2Omn;nmData=names(DTOmit);ResIAllp=ResIAll
   rl<-levels(data.table(ResFrecPas)$Nombres)
   DTpr<-as.data.table(as.matrix(ResFrecPas))
   patt=paste0("_",substring(strsplit(rl[1],"_")[[1]][2],1,1))
   nFa<-1:ncol(DTpr)
   NewNm<-substring(rl,c(regexpr(patt,rl)+1))
   DTpr3<-data.table(Nombres=NewNm,Pesos=DTpr[,round(rowMaxs(as.matrix(.SD))/rowSums(.SD),3)])
   DTpr3[Pesos<CrPesos,Rev:="**"]
   DTpr3[Pesos>=CrPesos,Rev:=""]
   Soluc<-DTpr3
   return(Soluc)
}
 
 outersect <- function(x, y) {
   sort(c(setdiff(x, y),
          setdiff(y, x)))
 }
 
 ExtrFreq<-function(Rp,Ortg=c(1:3)) {
   ROrt<-Rp;setkey(ROrt,Ortog)
   if (Ortg==1) {
     ROrt<-ROrt[Ortog==T]
     R1<-ROrt[,.N, by=list(Nombres,Factor)]
     ResR<-ftable(xtabs(N~Nombres+Factor,data=R1))
     lBl<-"Ortogonales"
   }
   if (Ortg==2) {
     ROrt<-ROrt[Ortog==F]
     R1<-ROrt[,.N, by=list(Nombres,Factor)]
     ResR<-ftable(xtabs(N~Nombres+Factor,data=R1))
     lBl<-"Oblicuos"
   }
   if (Ortg==3) {
     R1<-Rp[,.N, by=list(Nombres,Factor)]
     ResR<-ftable(xtabs(N~Nombres+Factor,data=R1))
     lBl<-"Todos los Tipos (Ortogonales + Oblicuos)"
   }
   mosaicplot(N~Nombres+Factor,data=ResR,shade=T,main=lBl)
   ResR
 }
 
 #ResIAllp=ResIAll[[9]]; DTp=DT;PathResp=PathRes
 NumFac<-function(ResIAllp, DTp,PathResp) {
  
   
   #Le paso DT
   TypData=ResIAllp$Task
   NmDirActual<-paste0(PathResp,TypData,"/")
   
   
   Nfac<-ResIAllp$NFac # N?? Factores impuesto para las estimaciones de CFA
   if (Nfac==0) Nfac=1
   Sel<-as.vector(ResIAllp$TkO)
   Resfinal<-DTp[,Sel,with=F]
   DTOmit<-data.table(na.omit(Resfinal))
   
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
   TyEstrInAxis<-c("component","maxr","ginv","multiple") #Exclusivamente para los tipos ipa ?? nipa
   # Four different choices of initial communalities are given:
   # components, maximum correlation, generalized inverse and multiple correlation
   
   NFExtr=6      # N?? Factores, lo marca todo ????Ojo!!
   TyExI<-2 # TypExtr<-c("pc","mle","fa","minres","wls","gls");
   TyRttI<-3 #  TypRot<- c("none", "varimax", "oblimin","promax")
   TyExI2<-2  #TypExtr2<-c("pca","ml","pa","minres","wls","gls","ipa","nipa")
   TyRttI2<-4 # c("none","varimax","quartimax","quartimin","equamax","parsimax","factor.parsimony",
   #  "biquartimin","covarimin","oblimax","entropy","simplimax","bentlerT",
   #  "bentlerQ","tandemI","tandemII","geominT","geominQ","infomaxT",
   #  "infomaxQ","mccammon")
   TypeCorrInic<-5 ; #1 es Pearson y 5 es Heterog??nea
   CrT=TypeCorrInic;CrTs=1;CrTCD=1
   coF<-OnlyCorr(DTOmitp = DTOmit,Ordin=c(length(DTOmit)),TypData=TypData,DirP=PathRes)
   co<-coF$v
   #exportJSON <- toJSON(coF)
   #write(exportJSON,paste0(PathData,NmFileCrr2))
   #write.table(x=co,file=paste0(PathData,NmFileCrr),na="NA", dec=",",sep=";")
   CrTs=1;CrTCD=1 # Para fijar el tipo de Corr en las simulaciones
   QuanSim=0.50
   DelCas=TRUE # True: "Delete cases listwise"
   Repla=FALSE
   Cases<-TypCase[2]
   n <- ncol(DTOmit)
   ncase <- nrow(DTOmit)
   ncase2 <- ncase
   Seed=NULL
   repi=1000
   Model=TypMod[1]
   Sim=TypSim[2]
   EstaSim=TypEsta[2]
   cont <- 0
   
   Data<-data.frame(DTOmit)
   mdata <- Data
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
   DelCasCD=TRUE # True: "Delete cases listwise"
   UntilSig=TRUE
   F.Max=20; N.Pop=10000;N.Samples=500; Alpha = 0.3
   if (Model==TypMod[1]) compPP=TRUE else compPP=FALSE
   Corr=TypCorr[CrT];CorrSim=TypCorr[CrTs];CorrCD=TypCorr[CrT]
   CorrDif=TypCorrDif[CrT]
   
   Rott=TypRot[TyRttI]   # Tipo de Rotaci??n Inicial
   Diago=FALSE
   ExtrMet=TypExtr[TyExI] # Tipo de Extracci??n Inicial
   NFExtr=6
   
   
   # Cattel Scree Test
   require(knitr)
   
   ptm=proc.time()
   #kable(head(mtcars[, 1:5]), format = "markdown")
   if (n >= 3) {
     set.seed(Seed)
     evpea <- matrix(NA, ncol = n, nrow = repi)
     evpea_b <- matrix(NA, ncol = n, nrow = repi)
     # Eigenvalues from components or factors
     # if (Model==TypMod[1]) {evt <- eigen(co, only.values = T) }
     # if (Model==TypMod[2]) {evt <- eigen(co - ginv(diag(diag(ginv(co)))), only.values=T) }
     evt <- coF$evt$values
     # Simulations
     
     # Simulation Type standardized normal
     if (Sim==TypSim[1]) {
       co_used <- "Pearson"
       for (k in 1:repi) { 
         y <- mvrnorm(ncase, rep(0, n), diag(1,n), empirical = FALSE)
         corY<-EstimaCorr(data.frame(y),scalP = scal,CasesP=Cases,TCor = 1,wT=F,CompP=compPP)
         evpea[k, ] <- corY$evt$values
         # corY <- cor(y, use=Cases, method="pearson") 
         # if (Model==TypMod[2]) { corY <- corY - ginv(diag(diag(ginv(corY)))) }
         # ev <- eigen(corY, only.values = TRUE)
         # evpea[k, ] <- ev$values 
       }
     }
     
     # Simulation Type Permutation
     if (Sim==TypSim[2]) {
       sdata <- matrix (0,ncol = ncol(mdata), nrow =ncase_list)
       if (!DelCas) {sdata <- mdata}
       if (DelCas) {
         mdata2 <- as.matrix(mdata)
         lcor <- 0 
         for (i in 1:ncase2) {
           ind <- 0; j <- 0
           while (j<n && ind ==0) { j <- j+1; if (is.na(mdata[i,j])) { ind <- 1 } }
           if (ind==0) { lcor <- lcor+1; sdata[lcor,] <- mdata2[i,] } 
         } 
       }
       
       for (k in 1:repi) { 
         perm <- apply(sdata[,2:n], 2, sample, replace = Repla)
         perm <- data.frame(sdata[,1],perm)
         
         corYF<-EstimaCorr(perm,scalP = scal,CasesP=Cases,TCor = CrTs,wT=F,CompP=compPP)
         corY<-corYF$v
         co_used<-CorrSim
         evpea[k, ] <- corYF$evt$values
       }
     }
     
     mevpea <- sapply(as.data.frame(evpea), mean)
     qevpea <- sapply(as.data.frame(evpea), quant)
     ap <- list(eigen = data.frame(mevpea, qevpea) )  
     
     # parallel analysis and scree
     ss <- EstaSim
     if (EstaSim==TypEsta[1]) {nS <- nScree(evt, aparallel=ap$eigen$mevpea); s <- "ap$eigen$mevpea"}
     if (EstaSim==TypEsta[2]) {nS <- nScree(evt, aparallel=ap$eigen$qevpea); s <- "ap$eigen$qevpea"}
     if (Model==TypMod[2]) { 
       criteria <- mean(evt)
       nS$Components$nkaiser <- sum(evt >= rep(criteria, n))
       p.vec <- which(evt >= ap$eigen$qevpea) 
       nS$Components$nparallel <- sum(p.vec == (1:length(p.vec)))
     }
     
     title="Parameters for Parallel Analysis";print(title);
     junto <- data.frame(repi, Model, Sim, ss, Corr, co_used)
     names(junto) <- c("Number of Samples","Model","Data","Measure", "Correlation matrix","Matrix for simulations")
     print(junto)
     #kable(head(mtcars[, 1:5]), format = "markdown")
     kable(junto)
     title="Distribution of the eigenvalues computed";print(title);
     junto <- data.frame(paste("",1:n, rep=""), ap$eigen)
     names(junto) <- c("Order", "Mean", "Quantile selected")
     print(junto)
     NmFileGrp<-paste0(NmDirActual,"Scree1_Distrib of the eigenvalues computed_",TypData,"_",Corr,".txt")
     write.table(junto,file=NmFileGrp)
     kable(junto,format = "markdown")
     title="Number of components/factors to retain according to different rules";print(title);
     junto <- data.frame(nS$Components)
     nmjunto <- c("Optimal coordinates","Acceleration factor", "Parallel analysis","Kaiser rule")
     names(junto)<-nmjunto
     print(junto)
     kable(junto,format = "markdown")
     NmFileGrp<-paste0(NmDirActual,"Scree2_Number To Retain_",TypData,"_",Corr,".txt")
     write.table(junto,file=NmFileGrp)
     juntoF1<-junto
     #xtable(junto)
     title="Data linked to the different rules";print(title);
     junto <- data.frame(nS$Analysis)
     names(junto) <- c("Eigenvalues","Proportion of variance", "Cumulative", "Parallel analysis", 
                       "Predicted eigenvalues", "OC", "Acceleration factor", "AF")
     print(junto)
     kable(junto,format = "markdown")
     NmFileGrp<-paste0(NmDirActual,"Scree3_Data linked to the different rules_",TypData,"_",Corr,".txt")
     write.table(junto,file=NmFileGrp)
     
     #quartz("Parallel analysis")
     if (Model==TypMod[1]) {
       if (Sim==TypSim[1]) {
         plotnScree(nS, ylab = "Eigenvalues", xlab = "Components",
                    main = "Parallel analysis on random uncorrelated standardized normal")
         
         NmFileGrp<-paste0(NmDirActual,"Scree1_",TypData,"_",Corr,".pdf")
         pdf(NmFileGrp)
         plotnScree(nS, ylab = "Eigenvalues", xlab = "Components",
                    main = "Parallel analysis on random uncorrelated standardized normal")
         dev.off()
       }
       if (Sim==TypSim[2]) {
         plotnScree(nS, ylab = "Eigenvalues", xlab = "Components",
                    main = "Parallel analysis on data permutation") 
         
         NmFileGrp<-paste0(NmDirActual,"Scree1_",TypData,"_",Corr,".pdf")
         pdf(NmFileGrp)
         plotnScree(nS, ylab = "Eigenvalues", xlab = "Components",
                    main = "Parallel analysis on data permutation") 
         dev.off()
         
       }
     }
     
     if (Model==TypMod[2]) {
       if (Sim==TypSim[1]) {
         plotnScree(nS, ylab = "Eigenvalues", xlab = "Factors", 
                    main = "Parallel analysis on random uncorrelated standardized normal") 
         
         NmFileGrp<-paste0(NmDirActual,"Scree1_",TypData,"_",Corr,".pdf")
         pdf(NmFileGrp)
         plotnScree(nS, ylab = "Eigenvalues", xlab = "Factors", 
                    main = "Parallel analysis on random uncorrelated standardized normal") 
         dev.off()
       }
       if (Sim==TypSim[2]) {
         plotnScree(nS, ylab = "Eigenvalues", xlab = "Factors", 
                    main = "Parallel analysis on data permutation") 
         
         NmFileGrp<-paste0(NmDirActual,"Scree1_",TypData,"_",Corr,".pdf")
         pdf(NmFileGrp)
         plotnScree(nS, ylab = "Eigenvalues", xlab = "Factors", 
                    main = "Parallel analysis on data permutation") 
         dev.off()
       }
     }
     
   } 
   print(proc.time() - ptm)
   # Cattel Scree Test
   
   #Velicer Map
   # Velicer MAP
   ptm=proc.time()
   #co<-data.frame(co)
   #as.matrix(co)
   ev <- eigen(co, symmetric=TRUE)
   load2 <- ev$vectors%*%sqrt(diag(ev$values))
   f <- rep(0,(n-1)); fq <- rep(0,(n-1))
   som <- sum(co^2); somq <- sum(co^4)
   m <- n*(n-1); f[1] <- (som-n)/m; fq[1] <- (somq-n)/m
   for (i in 1:(n-2)) { 
     b <- load2[,1:i]; cc <- co-(b%*%t(b))
     cc2<-as.matrix(cc)
     d <- diag(1/sqrt(diag(cc2))); ea <- d%*%cc2%*%d      #partial correlation matrix controlling components
     som <- sum(ea^2); somq <- sum(ea^4)
     f[i+1] <- (som-n)/m; fq[i+1] <- (somq-n)/m 
   }
   
   matt <- replace(f, f == "NaN", 2)
   matt2 <- replace(fq, fq == "NaN", 2)
   fm <- min(matt); fqm <- min(matt2)
   for (i in 1:(n-1)) { if (f[i]==fm) { fma <- i-1; break } }
   for (i in 1:(n-1)) { if (fq[i]==fqm) { fqma <- i-1; break } }
   
   junto <- data.frame("Squared average partial correlations"=f[1:(n-1)],
                       "4th average partial correlations"=fq[1:(n-1)],row.names=0:(n-2))
   title="Velicer's MAP values";print(title)
   print(junto)
   
   title="Velicer's Minimum Average Partial Test"
   junto1<-paste0("Squared MAP: ", "Velicer's Minimum= ",round(fm,6),"; Velicer's Components to retain: ",round(fma,6))
   junto2<-paste0("4th power MAP: ", "Velicer's Minimum= ",round(fqm,6),"; Velicer's Components to retain: ",round(fqma,6))
   print(c(junto1,junto2))
   NmFileGrp<-paste0(NmDirActual,"Velicer Minimum APT_",TypData,"_",Corr,"txt")
   write.table(c(junto1,junto2),file=NmFileGrp)
   juntoF2<-c(junto1,junto2)
   # table <- spss.BasePivotTable("Velicer's Minimum Average Partial Test","OMS")
   # rowdim <- BasePivotTable.Append(table,Dimension.Place.row,"")
   # coldim <- BasePivotTable.Append(table,Dimension.Place.column,"Velicer's Minimum")
   # row1 <- spss.CellText.String("Squared MAP")
   # row2 <- spss.CellText.String("4th power MAP")
   # col1 <- spss.CellText.String("Minimum")
   # col2 <- spss.CellText.String("Components to retain")
   # BasePivotTable.SetCategories(table,rowdim,list(row1, row2))
   # BasePivotTable.SetCategories(table,coldim,list(col1,col2))
   # BasePivotTable.SetCellsByRow(table,row1,list(spss.CellText.Number(fm), spss.CellText.Number(fma,6)))
   # BasePivotTable.SetCellsByRow(table,row2,list(spss.CellText.Number(fqm), spss.CellText.Number(fqma,6))) 
   print(proc.time() - ptm)
   
   
   #Very Simple Structure
   # Very Simple Structure
   ptm=proc.time()
   #quartz("VSS")
   
   
   v <- VSS(co, n = NFExtr, rotate = Rott, diagonal = Diago, fm = ExtrMet, n.obs=ncase, plot=TRUE)
   
   NmFileGrp<-paste0(NmDirActual,"VSS Graph_",TypData,"_",Corr,".pdf")
   pdf(NmFileGrp)
   v <- VSS(co, n = NFExtr, rotate = Rott, diagonal = Diago, fm = ExtrMet, n.obs=ncase, plot=TRUE)
   dev.off()
   
   junto <- data.frame("Complexity 1"=v$cfit.1, "Complexity 2"=v$cfit.2)
   title="Very Simple Structure";print(title)
   print(junto)
   NmFileGrp<-paste0(NmDirActual,"VSS 1_Very Simple Structure_",TypData,"_",Corr,".txt")
   write.table(junto,file=NmFileGrp)
   nu <- NFExtr
   vss1 <- 0
   for (i in 1:nu) {vss1 <- vss1+1;if ( v$cfit.1[i]==max(v$cfit.1) ) { break }}
   vss2 <- 0
   for (i in 1:nu) {vss2 <- vss2+1;if ( v$cfit.2[i]==max(v$cfit.2) ) { break }}
   
   
   junto<-paste0("VSS Values: ", "\n",
                 "Rotation= ","",Rott,"\n",
                 "Factoring method= ",ExtrMet,"\n",
                 "Max VSS complexity 1= ",round(max(v$cfit.1),6),"\n",
                 "N factors complexity 1= ",round(vss1,6),"\n",
                 "----------------","\n",
                 "Max VSS complexity 2= ",round(max(v$cfit.2),6),"\n",
                 "N factors complexity 2= ",round(vss2,6));
   title="Very Simple Structure (Number of factors)";
   cat(title,junto);
   NmFileGrp<-paste0(NmDirActual,"VSS 2_Parameteres_",TypData,"_",Corr,".txt")
   write.table(junto,file=NmFileGrp)
   juntoF3<-junto
   # table <- spss.BasePivotTable("Very Simple Structure (Number of factors)","OMS")
   # rowdim <- BasePivotTable.Append(table,Dimension.Place.row,"")
   # coldim <- BasePivotTable.Append(table,Dimension.Place.column,"Very Simple Structure (VSS)")
   # row1 <- spss.CellText.String("Values")
   # col1 <- spss.CellText.String("Rotation")
   # col2 <- spss.CellText.String("Factoring method")
   # col3 <- spss.CellText.String("Max VSS complexity 1")
   # col4 <- spss.CellText.String("N factors complexity 1")
   # col5 <- spss.CellText.String("Max VSS complexity 2")
   # col6 <- spss.CellText.String("N factors complexity 2")
   # BasePivotTable.SetCategories(table,rowdim,list(row1))
   # BasePivotTable.SetCategories(table,coldim,list(col1,col2,col3,col4,col5,col6))
   # BasePivotTable.SetCellsByColumn(table,col1,list(spss.CellText.String("")))
   # BasePivotTable.SetCellsByColumn(table,col2,list(spss.CellText.String("")))
   # BasePivotTable.SetCellsByColumn(table,col3,list(spss.CellText.Number(max(v$cfit.1))))
   # BasePivotTable.SetCellsByColumn(table,col4,list(spss.CellText.Number(vss1,6)))
   # BasePivotTable.SetCellsByColumn(table,col5,list(spss.CellText.Number(max(v$cfit.2))))
   # BasePivotTable.SetCellsByColumn(table,col6,list(spss.CellText.Number(vss2,6)))
   print(proc.time() - ptm)
   
   ###### More Number of Factors
   ptm=proc.time()
   sdata <- matrix (0,ncol = ncol(mdata), nrow =ncase_list) 
   if (DelCasCD) {
     mdata2 <- as.matrix(mdata)
     lcor <- 0 
     for (i in 1:ncase2) {
       ind <- 0; j <- 0
       while (j<n && ind ==0) {j <- j+1; if (is.na(mdata[i,j])) { ind <- 1 } }
       if (ind==0) { lcor <- lcor+1; sdata[lcor,] <- mdata2[i,] } 
     } 
   }
   if (!DelCasCD) { sdata <- mdata } 
   
   # Matrix to store each variable score distribution
   Distributions <- matrix(0, nrow = N.Pop, ncol = dim(sdata)[2])
   # Generate distribution for each variable
   b2 <- matrix(0, nrow = dim(sdata)[2], ncol = 1)
   for (i in 1:dim(sdata)[2]) {
     bb <- sort(sample(sdata[,i], size = N.Pop, replace = T) )
     b2[i] <- length(bb)
     ac <- N.Pop-b2[i]
     if (b2[i] < N.Pop) {for (j in 1:ac) {qw <- sample(1: (b2[i] + j),1); bb <- insert (bb, qw, NA) }} 
     Distributions[,i] <- as.matrix(bb) 
   }
   
   
   #DataP = sdata; F.Max =  20; N.Samples = 500; Alpha = 0.3; scalP=scal;CasesP =Cases;  TCor = CrTCD
   ysa<-EFA.Comp.Data(DataP = sdata, F.Max = F.Max, N.Samples = N.Samples, Alpha = Alpha,
                      scalP=scal ,CasesP =Cases,  TCor = CrTCD,UntilSig=UntilSig,Distributions)
   
   title="Fit to Comparison Data";
   junto <- data.frame(paste("",1:ysa$x.max, rep = "factor"), round(ysa$ys,4), round(ysa$sig2,4))
   names(junto) <- c("Number of factors","RMSR Eigenvalue","p-value")
   print(title);print(junto) 
   NmFileGrp<-paste0(NmDirActual,"CD 1_Fit to Comparison Data_",TypData,"_",Corr,".txt")
   write.table(junto,file=NmFileGrp)
   
   title="Comparison Data";
   junto <- data.frame("Correlations"=TypCorr[CrTCD],"Number of factors to retain"=ysa$nf)
   print(title);print(junto) 
   
   NmFileGrp<-paste0(NmDirActual,"CD 2_Comparison Data_",TypData,"_",Corr,".txt")
   write.table(junto,file=NmFileGrp)
   juntoF4<-junto
   print(proc.time() - ptm)
   ResNF<-list()
   ResNF$parallel_analysis_and_scree<-juntoF1
   ResNF$Velicer<-juntoF2
   ResNF$VSS<-juntoF3
   ResNF$More<-juntoF4
   ResNF
   
 }
 
 
 # SemTools completo
 ### Title: Compute more fit indices
 ### Authors: Terrence D. Jorgensen <TJorgensen314@gmail.com>
 ###          Sunthud Pornprasertmanit <psunthud@ku.edu>,
 ###          Aaron Boulton <aboulton@ku.edu>,
 ###          Ruben Arslan <rubenarslan@gmail.com>
 ### Last updated: 10 January 2021
 ### Description: Calculations for promising alternative fit indices
 
 
 
 ##' Calculate more fit indices
 ##'
 ##' Calculate more fit indices that are not already provided in lavaan.
 ##'
 ##' Gamma Hat (gammaHat; West, Taylor, & Wu, 2012) is a global fit index which
 ##' can be computed (assuming equal number of indicators across groups) by
 ##'
 ##' \deqn{ gammaHat =\frac{p}{p + 2 \times \frac{\chi^{2}_{k} - df_{k}}{N}} ,}
 ##'
 ##' where \eqn{p} is the number of variables in the model, \eqn{\chi^{2}_{k}} is
 ##' the \eqn{\chi^2} test statistic value of the target model, \eqn{df_{k}} is
 ##' the degree of freedom when fitting the target model, and \eqn{N} is the
 ##' sample size (or sample size minus the number of groups if \code{mimic} is
 ##' set to \code{"EQS"}).
 ##'
 ##' Adjusted Gamma Hat (adjGammaHat; West, Taylor, & Wu, 2012) is a global fit
 ##' index which can be computed by
 ##'
 ##' \deqn{ adjGammaHat = \left(1 - \frac{K \times p \times (p + 1)}{2 \times
 ##' df_{k}} \right) \times \left( 1 - gammaHat \right) ,}
 ##'
 ##' where \eqn{K} is the number of groups (please refer to Dudgeon, 2004 for the
 ##' multiple-group adjustment for agfi*).
 ##'
 ##' Corrected Akaike Information Criterion (aic.smallN; Burnham & Anderson,
 ##' 2003) is a corrected version of AIC for small sample size, often abbreviated
 ##' AICc:
 ##'
 ##' \deqn{ aic.smallN = AIC + \frac{2k(k + 1)}{N - k - 1},}
 ##'
 ##' where \eqn{AIC} is the original AIC: \eqn{-2 \times LL + 2k} (where \eqn{k}
 ##' = the number of estimated parameters in the target model). Note that AICc is
 ##' a small-sample correction derived for univariate regression models, so it is
 ##' probably \emph{not} appropriate for comparing SEMs.
 ##'
 ##' Corrected Bayesian Information Criterion (bic.priorN; Kuha, 2004) is similar
 ##' to BIC but explicitly specifying the sample size on which the prior is based
 ##' (\eqn{N_{prior}}).
 ##'
 ##' \deqn{ bic.priorN = f + k\log{(1 + N/N_{prior})},}
 ##'
 ##' Stochastic information criterion (SIC; Preacher, 2006) is similar to AIC or
 ##' BIC. This index will account for model complexity in the model's function
 ##' form, in addition to the number of free parameters. This index will be
 ##' provided only when the \eqn{\chi^2} value is not scaled. The SIC can be
 ##' computed by
 ##'
 ##' \deqn{ sic = \frac{1}{2}\left(f - \log{\det{I(\hat{\theta})}}\right),}
 ##'
 ##' where \eqn{I(\hat{\theta})} is the information matrix of the parameters.
 ##'
 ##' Hannan-Quinn Information Criterion (hqc; Hannan & Quinn, 1979) is used for
 ##' model selection similar to AIC or BIC.
 ##'
 ##' \deqn{ hqc = f + 2k\log{(\log{N})},}
 ##'
 ##' Note that if Satorra--Bentler or Yuan--Bentler's method is used, the fit
 ##' indices using the scaled \eqn{\chi^2} values are also provided.
 ##'
 ##' See \code{\link{nullRMSEA}} for the further details of the computation of
 ##' RMSEA of the null model.
 ##'
 ##'
 ##' @importFrom lavaan lavInspect
 ##'
 ##' @param object The lavaan model object provided after running the \code{cfa},
 ##' \code{sem}, \code{growth}, or \code{lavaan} functions.
 ##' @param fit.measures Additional fit measures to be calculated. All additional
 ##' fit measures are calculated by default
 ##' @param nPrior The sample size on which prior is based. This argument is used
 ##' to compute BIC*.
 ##' @return \enumerate{
 ##'  \item \code{gammaHat}: Gamma Hat
 ##'  \item \code{adjGammaHat}: Adjusted Gamma Hat
 ##'  \item \code{baseline.rmsea}: RMSEA of the Baseline (Null) Model
 ##'  \item \code{aic.smallN}: Corrected (for small sample size) Akaike Information Criterion
 ##'  \item \code{bic.priorN}: Bayesian Information Criterion with specified prior sample size
 ##'  \item \code{sic}: Stochastic Information Criterion
 ##'  \item \code{hqc}: Hannan-Quinn Information Criterion
 ##'  \item \code{gammaHat.scaled}: Gamma Hat using scaled \eqn{\chi^2}
 ##'  \item \code{adjGammaHat.scaled}: Adjusted Gamma Hat using scaled \eqn{\chi^2}
 ##'  \item \code{baseline.rmsea.scaled}: RMSEA of the Baseline (Null) Model using scaled \eqn{\chi^2}
 ##' }
 ##' @author Sunthud Pornprasertmanit (\email{psunthud@@gmail.com})
 ##'
 ##' Terrence D. Jorgensen (University of Amsterdam; \email{TJorgensen314@@gmail.com})
 ##'
 ##' Aaron Boulton (University of North Carolina, Chapel Hill; \email{aboulton@@email.unc.edu})
 ##'
 ##' Ruben Arslan (Humboldt-University of Berlin, \email{rubenarslan@@gmail.com})
 ##'
 ##' Yves Rosseel (Ghent University; \email{Yves.Rosseel@@UGent.be})
 ##'
 ##' @seealso \itemize{ \item \code{\link{miPowerFit}} For the modification
 ##' indices and their power approach for model fit evaluation \item
 ##' \code{\link{nullRMSEA}} For RMSEA of the null model }
 ##'
 ##' @references Burnham, K., & Anderson, D. (2003). \emph{Model selection and
 ##' multimodel inference: A practical--theoretic approach}. New York, NY:
 ##' Springer--Verlag.
 ##'
 ##' Dudgeon, P. (2004). A note on extending Steiger's (1998) multiple sample
 ##' RMSEA adjustment to other noncentrality parameter-based statistic.
 ##' \emph{Structural Equation Modeling, 11}(3), 305--319.
 ##' \doi{10.1207/s15328007sem1103_1}
 ##'
 ##' Kuha, J. (2004). AIC and BIC: Comparisons of assumptions and performance.
 ##' \emph{Sociological Methods Research, 33}(2), 188--229.
 ##' \doi{10.1177/0049124103262065}
 ##'
 ##' Preacher, K. J. (2006). Quantifying parsimony in structural equation
 ##' modeling. \emph{Multivariate Behavioral Research, 43}(3), 227-259.
 ##' \doi{10.1207/s15327906mbr4103_1}
 ##'
 ##' West, S. G., Taylor, A. B., & Wu, W. (2012). Model fit and model selection
 ##' in structural equation modeling. In R. H. Hoyle (Ed.), \emph{Handbook of
 ##' Structural Equation Modeling} (pp. 209--231). New York, NY: Guilford.
 ##' @examples
 ##'
 ##' HS.model <- ' visual  =~ x1 + x2 + x3
 ##'               textual =~ x4 + x5 + x6
 ##'               speed   =~ x7 + x8 + x9 '
 ##'
 ##' fit <- cfa(HS.model, data = HolzingerSwineford1939)
 ##' moreFitIndices(fit)
 ##'
 ##' fit2 <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "mlr")
 ##' moreFitIndices(fit2)
 ##'
 ##' @export
 moreFitIndices <- function(object, fit.measures = "all", nPrior = 1) {
   ## check for validity of user-specified "fit.measures" argument
   fit.choices <- c("gammaHat","adjGammaHat","baseline.rmsea",
                    "gammaHat.scaled","adjGammaHat.scaled","baseline.rmsea.scaled",
                    "aic.smallN","bic.priorN","hqc","sic")
   flags <- setdiff(fit.measures, c("all", fit.choices))
   if (length(flags)) stop(paste("Argument 'fit.measures' includes invalid options:",
                                 paste(flags, collapse = ", "),
                                 "Please choose 'all' or among the following:",
                                 paste(fit.choices, collapse = ", "), sep = "\n"))
   if ("all" %in% fit.measures) fit.measures <- fit.choices
   
   # Extract fit indices information from lavaan object
   fit <- lavInspect(object, "fit")
   # Get the number of variable
   p <- length(lavaan::lavNames(object, type = "ov", group = 1))
   # Get the number of parameters
   nParam <- fit["npar"]
   
   # Find the number of groups
   ngroup <- lavInspect(object, "ngroups")
   # Get number of observations
   N <- n <- lavInspect(object, "ntotal")
   if (lavInspect(object, "options")$mimic == "EQS") n <- n - ngroup
   
   # Calculate -2*log(likelihood)
   f <- -2 * fit["logl"]
   
   # Compute fit indices
   result <- list()
   if (length(grep("gamma", fit.measures, ignore.case = TRUE))) {
     gammaHat <- p / (p + 2 * ((fit["chisq"] - fit["df"]) / n))
     adjGammaHat <- 1 - (((ngroup * p * (p + 1)) / 2) / fit["df"]) * (1 - gammaHat)
     result["gammaHat"] <- gammaHat
     result["adjGammaHat"] <- adjGammaHat
     if (any(grepl(pattern = "scaled", x = names(fit)))) {
       gammaHatScaled <- p / (p + 2 * ((fit["chisq.scaled"] - fit["df.scaled"]) / n))
       adjGammaHatScaled <- 1 - (((ngroup * p * (p + 1)) / 2) / fit["df.scaled"]) * (1 - gammaHatScaled)
       result["gammaHat.scaled"] <- gammaHatScaled
       result["adjGammaHat.scaled"] <- adjGammaHatScaled
     }
   }
   if (length(grep("rmsea", fit.measures))) {
     result["baseline.rmsea"] <- nullRMSEA(object, silent = TRUE)
     if (any(grepl(pattern = "scaled", x = names(fit)))) {
       result["baseline.rmsea.scaled"] <- nullRMSEA(object, scaled = TRUE, silent = TRUE)
     }
   }
   if (!is.na(f)) {
     if ("aic.smallN" %in% fit.measures) {
       warning('AICc (aic.smallN) was developed for univariate linear models.',
               ' It is probably not appropriate to use AICc to compare SEMs.')
       result["aic.smallN"] <- fit[["aic"]] + (2 * nParam * (nParam + 1)) / (N - nParam - 1)
     }
     if ("bic.priorN" %in% fit.measures) {
       result["bic.priorN"] <- f + log(1 + N/nPrior) * nParam
     }
     if ("hqc" %in% fit.measures) result["hqc"] <- f + 2 * log(log(N)) * nParam
     if ("sic" %in% fit.measures) result["sic"] <- sic(f, object)
   }
   class(result) <- c("lavaan.vector","numeric")
   unlist(result[fit.measures])
 }
 
 
 
 ##' Calculate the RMSEA of the null model
 ##'
 ##' Calculate the RMSEA of the null (baseline) model
 ##'
 ##' RMSEA of the null model is calculated similar to the formula provided in the
 ##' \code{lavaan} package. The standard formula of RMSEA is
 ##'
 ##' \deqn{ RMSEA =\sqrt{\frac{\chi^2}{N \times df} - \frac{1}{N}} \times
 ##' \sqrt{G} }
 ##'
 ##' where \eqn{\chi^2} is the chi-square test statistic value of the target
 ##' model, \eqn{N} is the total sample size, \eqn{df} is the degree of freedom
 ##' of the hypothesized model, \eqn{G} is the number of groups. Kenny proposed
 ##' in his website that
 ##'
 ##' "A reasonable rule of thumb is to examine the RMSEA for the null model and
 ##' make sure that is no smaller than 0.158. An RMSEA for the model of 0.05 and
 ##' a TLI of .90, implies that the RMSEA of the null model is 0.158.  If the
 ##' RMSEA for the null model is less than 0.158, an incremental measure of fit
 ##' may not be that informative."
 ##'
 ##' See also \url{http://davidakenny.net/cm/fit.htm}
 ##'
 ##'
 ##' @importFrom lavaan lavInspect
 ##'
 ##' @param object The lavaan model object provided after running the \code{cfa},
 ##' \code{sem}, \code{growth}, or \code{lavaan} functions.
 ##' @param scaled If \code{TRUE}, the scaled (or robust, if available) RMSEA
 ##'   is returned. Ignored if a robust test statistic was not requested.
 ##' @param silent If \code{TRUE}, do not print anything on the screen.
 ##'
 ##' @return A value of RMSEA of the null model (a \code{numeric} vector)
 ##'   returned invisibly.
 ##'
 ##' @author
 ##' Ruben Arslan (Humboldt-University of Berlin, \email{rubenarslan@@gmail.com})
 ##'
 ##' Terrence D. Jorgensen (University of Amsterdam; \email{TJorgensen314@@gmail.com})
 ##'
 ##' @seealso
 ##' \itemize{
 ##'   \item \code{\link{miPowerFit}} For the modification indices and their
 ##'      power approach for model fit evaluation
 ##'   \item \code{\link{moreFitIndices}} For other fit indices
 ##' }
 ##'
 ##' @references Kenny, D. A., Kaniskan, B., & McCoach, D. B. (2015). The
 ##' performance of RMSEA in models with small degrees of freedom.
 ##' \emph{Sociological Methods Research, 44}(3), 486--507.
 ##' \doi{10.1177/0049124114543236}
 ##'
 ##' @examples
 ##'
 ##' HS.model <- ' visual  =~ x1 + x2 + x3
 ##'               textual =~ x4 + x5 + x6
 ##'               speed   =~ x7 + x8 + x9 '
 ##'
 ##' fit <- cfa(HS.model, data = HolzingerSwineford1939)
 ##' nullRMSEA(fit)
 ##'
 ##' @export
 nullRMSEA <- function(object, scaled = FALSE, silent = FALSE) {
   fit <- lavaan::update(object, model = lavaan::lav_partable_independence(object))
   fits <- lavaan::fitMeasures(fit, fit.measures = c("rmsea","rmsea.scaled",
                                                     "rmsea.robust"))
   if (scaled) {
     RMSEA <- fits["rmsea.robust"]
     if (is.na(RMSEA)) RMSEA <- fits["rmsea.scaled"]
     if (is.na(RMSEA)) RMSEA <- fits["rmsea"]
   } else RMSEA <- fits["rmsea"]
   
   if (!silent) {
     cat("The baseline model's RMSEA =", RMSEA, "\n\n")
     if (RMSEA < 0.158 ) {
       cat("CFI, TLI, and other incremental fit indices may not be very",
           "informative because the baseline model's RMSEA < 0.158",
           "(Kenny, Kaniskan, & McCoach, 2015). \n")
     }
   }
   invisible(RMSEA)
 }
 
 
 
 ## Stochastic Information Criterion
 ## f = minimized discrepancy function
 ## lresults = lavaan sem output object
 #TODO: update to extract f from lresults. Make public?
 sic <- function(f, lresults = NULL) {
   ## p. 596 of doi:10.1007/s10519-004-5587-0 says to use observed Fisher information
   E.inv <- lavaan::lavTech(lresults, "inverted.information.observed")
   if (inherits(E.inv, "try-error")) {
     return(as.numeric(NA))
   }
   E <- MASS::ginv(E.inv) * lavaan::nobs(lresults)
   
   eigvals <- eigen(E, symmetric = TRUE, only.values = TRUE)$values
   # only positive ones
   eigvals <- eigvals[ eigvals > sqrt(.Machine$double.eps)]
   
   DET <- prod(eigvals)
   
   ## check singular
   if (DET <= 0) return(NA)
   
   ## return SIC
   f + log(DET)
 }
 
 
 
 ##' Small-\emph{N} correction for \eqn{chi^2} test statistic
 ##'
 ##' Calculate small-\emph{N} corrections for \eqn{chi^2} model-fit test
 ##' statistic to adjust for small sample size (relative to model size).
 ##'
 ##' Four finite-sample adjustments to the chi-squared statistic are currently
 ##' available, all of which are described in Shi et al. (2018). These all
 ##' assume normally distributed data, and may not work well with severely
 ##' nonnormal data. Deng et al. (2018, section 4) review proposed small-\emph{N}
 ##' adjustments that do not assume normality, which rarely show promise, so
 ##' they are not implemented here. This function currently will apply
 ##' small-\emph{N} adjustments to scaled test statistics with a warning that
 ##' they do not perform well (Deng et al., 2018).
 ##'
 ##' @importFrom lavaan lavInspect lavNames
 ##' @importFrom stats pchisq
 ##' @importFrom methods getMethod
 ##'
 ##' @param fit0,fit1 \linkS4class{lavaan} object(s) provided after running the
 ##'   \code{cfa}, \code{sem}, \code{growth}, or \code{lavaan} functions.
 ##'   \linkS4class{lavaan.mi} object(s) also accepted.
 ##' @param smallN.method \code{character} indicating the small-\emph{N}
 ##'   correction method to use. Multiple may be chosen (all of which assume
 ##'   normality), as described in Shi et al. (2018):
 ##'   \code{c("swain","yuan.2015","yuan.2005","bartlett")}. Users may also
 ##'   simply select \code{"all"}.
 ##' @param \dots Additional arguments to the \code{\link[lavaan]{lavTestLRT}} or
 ##'   \code{\link{lavTestLRT.mi}} functions. Ignored when \code{is.null(fit1)}.
 ##' @param omit.imps \code{character} vector specifying criteria for omitting
 ##'   imputations from pooled results. Ignored unless \code{fit0} (and
 ##'   optionally \code{fit1}) is a \linkS4class{lavaan.mi} object. See
 ##'   \code{\link{lavTestLRT.mi}} for a description of options and defaults.
 ##'
 ##' @return A \code{list} of \code{numeric} vectors: one for the originally
 ##'   requested statistic(s), along with one per requested \code{smallN.method}.
 ##'   All include the the (un)adjusted test statistic, its \emph{df}, and the
 ##'   \emph{p} value for the test under the null hypothesis that the model fits
 ##'   perfectly (or that the 2 models have equivalent fit).
 ##'   The adjusted chi-squared statistic(s) also include(s) the scaling factor
 ##'   for the small-\emph{N} adjustment.
 ##'
 ##' @author
 ##'   Terrence D. Jorgensen (University of Amsterdam; \email{TJorgensen314@@gmail.com})
 ##'
 ##' @references
 ##'   Deng, L., Yang, M., & Marcoulides, K. M. (2018). Structural equation
 ##'   modeling with many variables: A systematic review of issues and
 ##'   developments. \emph{Frontiers in Psychology, 9}, 580.
 ##'   \doi{10.3389/fpsyg.2018.00580}
 ##'
 ##'   Shi, D., Lee, T., & Terry, R. A. (2018). Revisiting the model
 ##'   size effect in structural equation modeling.
 ##'   \emph{Structural Equation Modeling, 25}(1), 21--40.
 ##'   \doi{10.1080/10705511.2017.1369088}
 ##'
 ##' @examples
 ##'
 ##' HS.model <- '
 ##'     visual  =~ x1 + b1*x2 + x3
 ##'     textual =~ x4 + b2*x5 + x6
 ##'     speed   =~ x7 + b3*x8 + x9
 ##' '
 ##' fit1 <- cfa(HS.model, data = HolzingerSwineford1939[1:50,])
 ##' ## test a single model (implicitly compared to a saturated model)
 ##' chisqSmallN(fit1)
 ##'
 ##' ## fit a more constrained model
 ##' fit0 <- cfa(HS.model, data = HolzingerSwineford1939[1:50,],
 ##'             orthogonal = TRUE)
 ##' ## compare 2 models
 ##' chisqSmallN(fit1, fit0)
 ##'
 ##' @export
 chisqSmallN <- function(fit0, fit1 = NULL,
                         smallN.method = if (is.null(fit1)) c("swain","yuan.2015") else "yuan.2005",
                         ..., omit.imps = c("no.conv","no.se")) {
   if ("all" %in% smallN.method) smallN.method <- c("swain","yuan.2015",
                                                    "yuan.2005","bartlett")
   smallN.method <- intersect(tolower(smallN.method),
                              c("swain","yuan.2015","yuan.2005","bartlett"))
   if (!any(smallN.method %in% c("swain","yuan.2015","yuan.2005","bartlett")))
     stop('No recognized options for "smallN.method" argument')
   
   ## check class
   if (!inherits(fit0, what = c("lavaan","lavaanList")))
     stop("this function is only applicable to fitted lavaan models")
   
   ## if there are 2 models...
   if (!is.null(fit1)) {
     
     ## check classes
     if (!inherits(fit1, what = c("lavaan","lavaanList")))
       stop("this function is only applicable to fitted lavaan models")
     modClass <- unique(sapply(list(fit0, fit1), class))
     if (length(modClass) > 1L) stop('All models must be of the same class (e.g.,',
                                     ' cannot compare lavaan objects to lavaan.mi)')
     
     ## check order of DF
     suppressMessages(DF0 <- getMethod("fitMeasures", class(fit0))(fit0, fit.measures = "df",
                                                                   omit.imps = omit.imps)[1])
     suppressMessages(DF1 <- getMethod("fitMeasures", class(fit1))(fit1, fit.measures = "df",
                                                                   omit.imps = omit.imps)[1])
     if (DF0 == DF1) stop("Models have the same degrees of freedom.")
     parent <- which.min(c(DF0, DF1))
     if (parent == 1L) {
       parent <- fit0
       fit0 <- fit1
       fit1 <- parent
       
       parent <- DF0
       DF0 <- DF1
       DF1 <- parent
     }
     if (min(c(DF0, DF1)) == 0L) {
       message('Less restricted model has df=0, so chi-squared difference ',
               'not needed to compare models. Using only the restricted ',
               "model's chi-squared statistic.")
       fit1 <- NULL
     }
   }
   
   ## check whether methods can be used
   if (!is.null(fit1)) {
     
     if (any(smallN.method %in% c("yuan.2015","swain"))) {
       message('Swain(1975) and Yuan (2015) corrections depend on the number ',
               'of free parameters, so it is unavailable for model comparison.')
       smallN.method <- smallN.method[-which(smallN.method %in% c("yuan.2015","swain"))]
     }
     
     if (!length(smallN.method)) {
       stop('No valid options for "smallN.method" argument')
     } else warning('Small-N corrections developed for single models, not for ',
                    'model comparison. Experimentally applying correction to ',
                    'chi-squared difference statistic, which might be invalid.')
   }
   
   ## save quantities relevant across correction methods
   N <- lavInspect(fit0, "ntotal")
   Ng <- lavInspect(fit0, "ngroups")
   if (!lavInspect(fit0, "options")$sample.cov.rescale) N <- N - Ng
   P <- length(lavNames(fit0))
   K <- length(lavNames(fit0, type = "lv")) # count latent factors
   
   if (is.null(fit1)) {
     FIT <- getMethod("fitMeasures", class(fit0))(fit0,
                                                  ## lavaan.mi arguments ignored
                                                  ## for lavaan objects
                                                  omit.imps = omit.imps,
                                                  asymptotic = TRUE,
                                                  fit.measures = c("npar","chisq",
                                                                   "df","pvalue"))
     scaled <- any(grepl(pattern = "scaled", x = names(FIT)))
     if (scaled) warning('Small-N corrections developed assuming normality, but',
                         ' a scaled test was requested. Applying correction(s) ',
                         'to the scaled test statistic, but this has not ',
                         'performed well in past simulations.')
     NPAR <- FIT[["npar"]]
     chi <- FIT[[if (scaled) "chisq.scaled" else "chisq"]]
     DF  <- FIT[[if (scaled) "df.scaled" else "df"]]
     PV  <- FIT[[if (scaled) "pvalue.scaled" else "pvalue"]]
     
   } else {
     ## Compare to a second model. Check matching stats.
     N1 <- lavInspect(fit1, "ntotal")
     Ng1 <- lavInspect(fit1, "ngroups")
     if (!lavInspect(fit1, "options")$sample.cov.rescale) N1 <- N1 - Ng1
     if (N != N1) stop("Unequal sample sizes")
     if (P != length(lavNames(fit1))) stop("Unequal number of observed variables")
     K1 <- length(lavNames(fit1, type = "lv"))
     if (K != K1 && any(smallN.method %in% c("yuan.2005","bartlett"))) {
       warning("Unequal number of latent variables (k). Unclear how to apply ",
               "Yuan (2005) or Bartlett (2015) corrections when comparing ",
               "models with different k. Experimentally using the larger ",
               "model's k, but there is no evidence this is valid.")
       K <- max(K, K1)
     }
     
     try(AOV <- compareFit(fit0, fit1, argsLRT = list(...), indices = FALSE),
         silent = TRUE)
     if (inherits(AOV, "try-error")) stop('Model comparison failed. Try using ',
                                          'lavTestLRT() to investigate why.')
     
     if (inherits(fit0, "lavaan")) {
       if (grepl("scaled", attr(AOV@nested, "heading"), ignore.case = TRUE))
         warning('Small-N corrections developed assuming normality, but scaled ',
                 'tests were requested. Applying correction(s) to the scaled test',
                 ' statistic, but this has not performed well in past simulations.')
       chi <- AOV@nested[["Chisq diff"]][2]
       DF  <- AOV@nested[["Df diff"]][2]
       PV  <- AOV@nested[["Pr(>Chisq)"]][2]
       
     } else if (inherits(fit0, "lavaan.mi")) {
       if (any(grepl("scaled", colnames(AOV@nested), ignore.case = TRUE)))
         warning('Small-N corrections developed assuming normality, but scaled ',
                 'tests were requested. Applying correction(s) to the scaled test',
                 ' statistic, but this has not performed well in past simulations.')
       chi <- AOV@nested[1, 1]
       DF  <- AOV@nested[1, 2]
       PV  <- AOV@nested[1, 3]
     }
     
   }
   
   
   ## empty list to store correction(s)
   out <- list()
   out[[ lavInspect(fit0, "options")$test ]] <- c(chisq = chi, df = DF,
                                                  pvalue = PV)
   class(out[[1]]) <- c("lavaan.vector","numeric")
   
   ## calculate Swain's (1975) correction
   ## (undefined for model comparison)
   if ("swain" %in% smallN.method) {
     Q <- (sqrt(1 + 8*NPAR) - 1) / 2
     num <- P*(2*P^2 + 3*P - 1) - Q*(2*Q^2 + 3*Q - 1)
     SC <- 1 - num / (12*DF*N)
     out[["swain"]] <- c(chisq = chi*SC, df = DF,
                         pvalue = pchisq(chi*SC, DF, lower.tail = FALSE),
                         smallN.factor = SC)
     class(out[["swain"]]) <- c("lavaan.vector","numeric")
   }
   
   ## calculate Yuan's (2015) correction
   ## (undefined for model comparison)
   if ("yuan.2015" %in% smallN.method) {
     ## numerator uses actual N regardless of sample.cov.rescale
     SC <- (lavInspect(fit0, "ntotal") - (2.381 + .361*P + .006*NPAR)) / N
     out[["yuan.2015"]] <- c(chisq = chi*SC, df = DF,
                             pvalue = pchisq(chi*SC, DF, lower.tail = FALSE),
                             smallN.factor = SC)
     class(out[["yuan.2015"]]) <- c("lavaan.vector","numeric")
   }
   
   ## calculate Yuan's (2005) correction
   if ("yuan.2005" %in% smallN.method) {
     SC <- 1 - ((2*P + 2*K + 7) / (6*N))
     out[["yuan.2005"]] <- c(chisq = chi*SC, df = DF,
                             pvalue = pchisq(chi*SC, DF, lower.tail = FALSE),
                             smallN.factor = SC)
     class(out[["yuan.2005"]]) <- c("lavaan.vector","numeric")
   }
   
   ## calculate Bartlett's (1950) k-factor correction (ONLY appropriate for EFA)
   if ("bartlett" %in% smallN.method) {
     message('Bartlett\'s k-factor correction was developed for EFA models, ',
             'not for general SEMs.')
     SC <- 1 - ((2*P + 4*K + 5) / (6*N))
     out[["bartlett"]] <- c(chisq = chi*SC, df = DF,
                            pvalue = pchisq(chi*SC, DF, lower.tail = FALSE),
                            smallN.factor = SC)
     class(out[["bartlett"]]) <- c("lavaan.vector","numeric")
   }
   
   out[c(lavInspect(fit0, "options")$test, smallN.method)]
 }

 ### Sunthud Pornprasertmanit, Terrence D. Jorgensen, Yves Rosseel
 ### Last updated: 10 January 2021
 
 
 ## -------------
 ## reliability()
 ## -------------
 
 
 ##' Calculate reliability values of factors
 ##'
 ##' Calculate reliability values of factors by coefficient omega
 ##'
 ##' The coefficient alpha (Cronbach, 1951) can be calculated by
 ##'
 ##' \deqn{ \alpha = \frac{k}{k - 1}\left[ 1 - \frac{\sum^{k}_{i = 1}
 ##' \sigma_{ii}}{\sum^{k}_{i = 1} \sigma_{ii} + 2\sum_{i < j} \sigma_{ij}}
 ##' \right],}
 ##'
 ##' where \eqn{k} is the number of items in a factor, \eqn{\sigma_{ii}} is the
 ##' item \emph{i} observed variances, \eqn{\sigma_{ij}} is the observed
 ##' covariance of items \emph{i} and \emph{j}.
 ##'
 ##' The coefficient omega (Bollen, 1980; see also Raykov, 2001) can be
 ##' calculated by
 ##'
 ##' \deqn{ \omega_1 =\frac{\left( \sum^{k}_{i = 1} \lambda_i \right)^{2}
 ##' Var\left( \psi \right)}{\left( \sum^{k}_{i = 1} \lambda_i \right)^{2}
 ##' Var\left( \psi \right) + \sum^{k}_{i = 1} \theta_{ii} + 2\sum_{i < j}
 ##' \theta_{ij} }, }
 ##'
 ##' where \eqn{\lambda_i} is the factor loading of item \emph{i}, \eqn{\psi} is
 ##' the factor variance, \eqn{\theta_{ii}} is the variance of measurement errors
 ##' of item \emph{i}, and \eqn{\theta_{ij}} is the covariance of measurement
 ##' errors from item \emph{i} and \emph{j}.
 ##'
 ##' The second coefficient omega (Bentler, 1972, 2009) can be calculated by
 ##'
 ##' \deqn{ \omega_2 = \frac{\left( \sum^{k}_{i = 1} \lambda_i \right)^{2}
 ##' Var\left( \psi \right)}{\bold{1}^\prime \hat{\Sigma} \bold{1}}, }
 ##'
 ##' where \eqn{\hat{\Sigma}} is the model-implied covariance matrix, and
 ##' \eqn{\bold{1}} is the \eqn{k}-dimensional vector of 1. The first and the
 ##' second coefficients omega will have the same value when the model has simple
 ##' structure, but different values when there are (for example) cross-loadings
 ##' or method factors. The first coefficient omega can be viewed as the
 ##' reliability controlling for the other factors (like \eqn{\eta^2_{partial}} in
 ##' ANOVA). The second coefficient omega can be viewed as the unconditional
 ##' reliability (like \eqn{\eta^2} in ANOVA).
 ##'
 ##' The third coefficient omega (McDonald, 1999), which is sometimes referred to
 ##' hierarchical omega, can be calculated by
 ##'
 ##' \deqn{ \omega_3 =\frac{\left( \sum^{k}_{i = 1} \lambda_i \right)^{2}
 ##' Var\left( \psi \right)}{\bold{1}^\prime \Sigma \bold{1}}, }
 ##'
 ##' where \eqn{\Sigma} is the observed covariance matrix. If the model fits the
 ##' data well, the third coefficient omega will be similar to the
 ##' \eqn{\omega_2}. Note that if there is a directional effect in the model, all
 ##' coefficients omega will use the total factor variances, which is calculated
 ##' by \code{\link[lavaan]{lavInspect}(object, "cov.lv")}.
 ##'
 ##' In conclusion, \eqn{\omega_1}, \eqn{\omega_2}, and \eqn{\omega_3} are
 ##' different in the denominator. The denominator of the first formula assumes
 ##' that a model is congeneric factor model where measurement errors are not
 ##' correlated. The second formula accounts for correlated measurement errors.
 ##' However, these two formulas assume that the model-implied covariance matrix
 ##' explains item relationships perfectly. The residuals are subject to sampling
 ##' error. The third formula use observed covariance matrix instead of
 ##' model-implied covariance matrix to calculate the observed total variance.
 ##' This formula is the most conservative method in calculating coefficient
 ##' omega.
 ##'
 ##' The average variance extracted (AVE) can be calculated by
 ##'
 ##' \deqn{ AVE = \frac{\bold{1}^\prime
 ##' \textrm{diag}\left(\Lambda\Psi\Lambda^\prime\right)\bold{1}}{\bold{1}^\prime
 ##' \textrm{diag}\left(\hat{\Sigma}\right) \bold{1}}, }
 ##'
 ##' Note that this formula is modified from Fornell & Larcker (1981) in the case
 ##' that factor variances are not 1. The proposed formula from Fornell & Larcker
 ##' (1981) assumes that the factor variances are 1. Note that AVE will not be
 ##' provided for factors consisting of items with dual loadings. AVE is the
 ##' property of items but not the property of factors.
 ##'
 ##' Regarding categorical indicators, coefficient alpha and AVE are calculated
 ##' based on polychoric correlations. The coefficient alpha from this function
 ##' differs from the standard alpha calculation, which does not assume items are
 ##' continuous, so numerically weighted categories can be treated as numeric.
 ##' Researchers may check the \code{alpha} function in the \code{psych} package
 ##' for the standard coefficient alpha calculation.
 ##'
 ##' Item thresholds are not accounted for. Coefficient omega for categorical
 ##' items, however, is calculated by accounting for both item covariances and
 ##' item thresholds using Green and Yang's (2009, formula 21) approach. Three
 ##' types of coefficient omega indicate different methods to calculate item
 ##' total variances. The original formula from Green and Yang is equivalent to
 ##' \eqn{\omega_3} in this function. Green and Yang did not propose a method for
 ##' calculating reliability with a mixture of categorical and continuous
 ##' indicators, and we are currently unaware of an appropriate method.
 ##' Therefore, when \code{reliability} detects both categorical and continuous
 ##' indicators in the model, an error is returned. If the categorical indicators
 ##' load on a different factor(s) than continuous indicators, then reliability
 ##' can be calculated separately for those scales by fitting separate models and
 ##' submitting each to the \code{reliability} function.
 ##'
 ##'
 ##' @importFrom lavaan lavInspect lavNames
 ##' @importFrom methods getMethod
 ##'
 ##' @param object A \code{\linkS4class{lavaan}} or
 ##'   \code{\linkS4class{lavaan.mi}} object, expected to contain only
 ##'   exogenous common factors (i.e., a CFA model).
 ##' @param return.total \code{logical} indicating whether to return a final
 ##'   column containing the reliability of a composite of all items. Ignored
 ##'   in 1-factor models, and should only be set \code{TRUE} if all factors
 ##'   represent scale dimensions that could nonetheless be collapsed to a
 ##'   single scale composite (scale sum or scale mean).
 ##' @param dropSingle \code{logical} indicating whether to exclude factors
 ##'   defined by a single indicator from the returned results. If \code{TRUE}
 ##'   (default), single indicators will still be included in the \code{total}
 ##'   column when \code{return.total = TRUE}.
 ##' @param omit.imps \code{character} vector specifying criteria for omitting
 ##'   imputations from pooled results.  Can include any of
 ##'   \code{c("no.conv", "no.se", "no.npd")}, the first 2 of which are the
 ##'   default setting, which excludes any imputations that did not
 ##'   converge or for which standard errors could not be computed.  The
 ##'   last option (\code{"no.npd"}) would exclude any imputations which
 ##'   yielded a nonpositive definite covariance matrix for observed or
 ##'   latent variables, which would include any "improper solutions" such
 ##'   as Heywood cases.  NPD solutions are not excluded by default because
 ##'   they are likely to occur due to sampling error, especially in small
 ##'   samples.  However, gross model misspecification could also cause
 ##'   NPD solutions, users can compare pooled results with and without
 ##'   this setting as a sensitivity analysis to see whether some
 ##'   imputations warrant further investigation.
 ##'
 ##' @return Reliability values (coefficient alpha, coefficients omega, average
 ##'   variance extracted) of each factor in each group. If there are multiple
 ##'   factors, a \code{total} column can optionally be included.
 ##'
 ##' @author Sunthud Pornprasertmanit (\email{psunthud@@gmail.com})
 ##'
 ##'   Yves Rosseel (Ghent University; \email{Yves.Rosseel@@UGent.be})
 ##'
 ##'   Terrence D. Jorgensen (University of Amsterdam; \email{TJorgensen314@@gmail.com})
 ##'
 ##' @seealso \code{\link{reliabilityL2}} for reliability value of a desired
 ##' second-order factor, \code{\link{maximalRelia}} for the maximal reliability
 ##' of weighted composite
 ##'
 ##' @references
 ##' Bollen, K. A. (1980). Issues in the comparative measurement of
 ##' political democracy. \emph{American Sociological Review, 45}(3), 370--390.
 ##' \doi{10.2307/2095172}
 ##'
 ##' Bentler, P. M. (1972). A lower-bound method for the dimension-free
 ##' measurement of internal consistency. \emph{Social Science Research, 1}(4),
 ##' 343--357. \doi{10.1016/0049-089X(72)90082-8}
 ##'
 ##' Bentler, P. M. (2009). Alpha, dimension-free, and model-based internal
 ##' consistency reliability. \emph{Psychometrika, 74}(1), 137--143.
 ##' \doi{10.1007/s11336-008-9100-1}
 ##'
 ##' Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
 ##' tests. \emph{Psychometrika, 16}(3), 297--334. \doi{10.1007/BF02310555}
 ##'
 ##' Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models
 ##' with unobservable variables and measurement errors. \emph{Journal of
 ##' Marketing Research, 18}(1), 39--50. \doi{10.2307/3151312}
 ##'
 ##' Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using
 ##' structural equation modeling: An alternative to coefficient alpha.
 ##' \emph{Psychometrika, 74}(1), 155--167. \doi{10.1007/s11336-008-9099-3}
 ##'
 ##' McDonald, R. P. (1999). \emph{Test theory: A unified treatment}. Mahwah, NJ:
 ##' Erlbaum.
 ##'
 ##' Raykov, T. (2001). Estimation of congeneric scale reliability using
 ##' covariance structure analysis with nonlinear constraints \emph{British
 ##' Journal of Mathematical and Statistical Psychology, 54}(2), 315--323.
 ##' \doi{10.1348/000711001159582}
 ##'
 ##' @examples
 ##'
 ##' HS.model <- ' visual  =~ x1 + x2 + x3
 ##'               textual =~ x4 + x5 + x6
 ##'               speed   =~ x7 + x8 + x9 '
 ##'
 ##' fit <- cfa(HS.model, data = HolzingerSwineford1939)
 ##' reliability(fit)
 ##' reliability(fit, return.total = TRUE)
 ##'
 ##' @export
 reliability <- function(object, return.total = FALSE, dropSingle = TRUE,
                         omit.imps = c("no.conv","no.se")) {
   
   ngroups <- lavInspect(object, "ngroups") #TODO: adapt to multiple levels
   nlevels <- lavInspect(object, "nlevels")
   nblocks <- ngroups*nlevels #FIXME: always true?
   group.label <- if (ngroups > 1L) lavInspect(object, "group.label") else NULL
   #FIXME? lavInspect(object, "level.labels")
   clus.label <- if (nlevels > 1L) c("within", lavInspect(object, "cluster")) else NULL
   if (nblocks > 1L) {
     block.label <- paste(rep(group.label, each = nlevels), clus.label,
                          sep = if (ngroups > 1L && nlevels > 1L) "_" else "")
   }
   
   ## parameters in GLIST format (not flat, need block-level list)
   if (inherits(object, "lavaan")) {
     param <- lavInspect(object, "est")
     ve <- lavInspect(object, "cov.lv") # model-implied latent covariance matrix
     S <- object@h1$implied$cov # observed sample covariance matrix (already a list)
     
     if (nblocks == 1L) {
       param <- list(param)
       ve <- list(ve)
     }
     
   } else if (inherits(object, "lavaan.mi")) {
     useImps <- rep(TRUE, length(object@DataList))
     if ("no.conv" %in% omit.imps) useImps <- sapply(object@convergence, "[[", i = "converged")
     if ("no.se" %in% omit.imps) useImps <- useImps & sapply(object@convergence, "[[", i = "SE")
     if ("no.npd" %in% omit.imps) {
       Heywood.lv <- sapply(object@convergence, "[[", i = "Heywood.lv")
       Heywood.ov <- sapply(object@convergence, "[[", i = "Heywood.ov")
       useImps <- useImps & !(Heywood.lv | Heywood.ov)
     }
     m <- sum(useImps)
     if (m == 0L) stop('No imputations meet "omit.imps" criteria.')
     useImps <- which(useImps)
     
     param <- object@coefList[[ useImps[1] ]] # first admissible as template
     coefList <- object@coefList[useImps]
     phiList <- object@phiList[useImps]
     ## add block-level list per imputation?
     if (nblocks == 1L) {
       param <- list(param)
       for (i in 1:m) {
         coefList[[i]] <- list(coefList[[i]])
         phiList[[i]] <- list(phiList[[i]])
       }
     }
     S <- vector("list", nblocks) # pooled observed covariance matrix
     ve <- vector("list", nblocks)
     ## loop over blocks
     for (b in 1:nblocks) {
       
       ## param:  loop over GLIST elements
       for (mat in names(param[[b]])) {
         matList <- lapply(coefList, function(i) i[[b]][[mat]])
         param[[b]][[mat]] <- Reduce("+", matList) / length(matList)
       } # mat
       
       ## pooled observed covariance matrix
       covList <- lapply(object@h1List[useImps], function(i) i$implied$cov[[b]])
       S[[b]] <- Reduce("+", covList) / m
       
       ## pooled model-implied latent covariance matrix
       ve[[b]] <- Reduce("+", lapply(phiList, "[[", i = b) ) / m
       
     } # b
     
   }
   
   if (nblocks == 1L) {
     SigmaHat <- getMethod("fitted", class(object))(object)["cov"] # retain list format
   } else {
     SigmaHat <- sapply(getMethod("fitted", class(object))(object),
                        "[[", "cov", simplify = FALSE)
   }
   
   ly <- lapply(param, "[[", "lambda")
   te <- lapply(param, "[[", "theta")
   
   anyCategorical <- lavInspect(object, "categorical")
   threshold <- if (anyCategorical) getThreshold(object) else NULL
   latScales <- if (anyCategorical) getScales(object) else NULL
   
   result <- list()
   warnHigher <- FALSE
   ## loop over i blocks (groups/levels)
   for (i in 1:nblocks) {
     ## extract factor and indicator names
     indNames <- rownames(ly[[i]])
     facNames <- colnames(ly[[i]])
     ## distinguish between categorical, continuous, and latent indicators
     nameArgs <- list(object = object)
     if (nblocks > 1L) nameArgs$block <- i
     ordNames <- do.call(lavNames, c(nameArgs, list(type = "ov.ord")))
     latInds  <- do.call(lavNames, c(nameArgs, list(type = "lv.ind")))
     higher   <- setdiff(facNames, latInds)
     ## keep track of factor indices, to drop higher-order factors,
     ## and optionally single-indicator factors
     idx.drop <- numeric(0)
     
     ## relevant quantities
     common <- (apply(ly[[i]], 2, sum)^2) * diag(ve[[i]])
     truevar <- ly[[i]] %*% ve[[i]] %*% t(ly[[i]])
     ## vectors to store results for each factor
     error <- rep(NA, length(common))
     alpha <- rep(NA, length(common))
     total <- rep(NA, length(common))
     omega1 <- omega2 <- omega3 <- rep(NA, length(common))
     impliedTotal <- rep(NA, length(common))
     avevar <- rep(NA, length(common))
     
     ## loop over j factors
     for (j in 1:length(common)) {
       index <- which(ly[[i]][,j] != 0)
       ## check for categorical (or mixed) indicators
       categorical <-      any(indNames[index] %in% ordNames)
       if (categorical && !all(indNames[index] %in% ordNames)) {
         stop('Reliability cannot be computed for factors with combinations ',
              'of categorical and continuous (including latent) indicators.')
       }
       ## check for latent indicators
       if (length(latInds) && facNames[j] %in% higher) {
         warnHigher <- TRUE
         idx.drop <- c(idx.drop, j)
         next
       }
       if (dropSingle && length(index) == 1L) {
         idx.drop <- c(idx.drop, j)
         next
       }
       
       sigma <- S[[i]][index, index, drop = FALSE]
       alpha[j] <- computeAlpha(sigma)
       faccontrib <- ly[[i]][,j, drop = FALSE] %*% ve[[i]][j,j, drop = FALSE] %*% t(ly[[i]][,j, drop = FALSE])
       truefac <- diag(faccontrib[index, index, drop = FALSE])
       trueitem <- diag(truevar[index, index, drop = FALSE])
       erritem <- diag(te[[i]][index, index, drop = FALSE])
       if (sum(abs(trueitem - truefac)) < 0.00001) {
         avevar[j] <- sum(trueitem) / sum(trueitem + erritem)
       } else {
         avevar[j] <- NA
       }
       if (categorical) {
         omega1[j] <- omegaCat(truevar = faccontrib[index, index, drop = FALSE],
                               threshold = threshold[[i]][index],
                               scales = latScales[[i]][index],
                               denom = faccontrib[index, index, drop = FALSE] + te[[i]][index, index, drop = FALSE])
         omega2[j] <- omegaCat(truevar = faccontrib[index, index, drop = FALSE],
                               threshold = threshold[[i]][index],
                               scales = latScales[[i]][index],
                               denom = SigmaHat[[i]][index, index, drop = FALSE])
         omega3[j] <- omegaCat(truevar = faccontrib[index, index, drop = FALSE],
                               threshold = threshold[[i]][index],
                               scales = latScales[[i]][index],
                               denom = sigma)
       } else {
         commonfac <- sum(faccontrib[index, index, drop = FALSE])
         error[j] <- sum(te[[i]][index, index, drop = FALSE])
         impliedTotal[j] <- sum(SigmaHat[[i]][index, index, drop = FALSE])
         total[j] <- sum(sigma)
         
         omega1[j] <- commonfac / (commonfac + error[j])
         omega2[j] <- commonfac / impliedTotal[j]
         omega3[j] <- commonfac / total[j]
       }
       ## end loop over j factors
     }
     
     if (return.total & length(facNames) > 1L) {
       alpha <- c(alpha, total = computeAlpha(S[[i]]))
       #FIXME: necessary?    names(alpha) <- c(names(common), "total")
       if (categorical) {
         omega1 <- c(omega1, total = omegaCat(truevar = truevar,
                                              threshold = threshold[[i]],
                                              scales = latScales[[i]],
                                              denom = truevar + te[[i]]))
         omega2 <- c(omega2, total = omegaCat(truevar = truevar,
                                              threshold = threshold[[i]],
                                              scales = latScales[[i]],
                                              denom = SigmaHat[[i]]))
         omega3 <- c(omega3, total = omegaCat(truevar = truevar,
                                              threshold = threshold[[i]],
                                              scales = latScales[[i]],
                                              denom = S[[i]]))
       } else {
         omega1 <- c(omega1, total = sum(truevar) / (sum(truevar) + sum(te[[i]])))
         omega2 <- c(omega2, total = sum(truevar) / (sum(SigmaHat[[i]])))
         omega3 <- c(omega3, total = sum(truevar) / (sum(S[[i]])))
       }
       avevar <- c(avevar,
                   total = sum(diag(truevar)) / sum((diag(truevar) + diag(te[[i]]))))
     }
     
     result[[i]] <- rbind(alpha = alpha, omega = omega1, omega2 = omega2,
                          omega3 = omega3, avevar = avevar)
     colnames(result[[i]])[1:length(facNames)] <- facNames
     if (return.total & length(facNames) > 1L) {
       colnames(result[[i]])[ ncol(result[[i]]) ] <- "total"
     }
     if (length(idx.drop)) {
       result[[i]] <- result[[i]][ , -idx.drop]
       ## reset indices for next block (could have different model/variables)
       idx.drop <- numeric(0)
     }
     ## end loop over blocks
   }
   
   if (anyCategorical) message("For constructs with categorical indicators, ",
                               "the alpha and the average variance extracted ",
                               "are calculated from polychoric (polyserial) ",
                               "correlations, not from Pearson correlations.\n")
   if (warnHigher) message('Higher-order factors were ignored.\n')
   
   ## drop list structure?
   if (nblocks == 1L) {
     result <- result[[1]]
   } else names(result) <- block.label
   
   result
 }
 
 
 
 ## ---------------
 ## reliabilityL2()
 ## ---------------
 
 ##' Calculate the reliability values of a second-order factor
 ##'
 ##' Calculate the reliability values (coefficient omega) of a second-order
 ##' factor
 ##'
 ##' The first formula of the coefficient omega (in the
 ##' \code{\link{reliability}}) will be mainly used in the calculation. The
 ##' model-implied covariance matrix of a second-order factor model can be
 ##' separated into three sources: the second-order common-factor variance,
 ##' the residual variance of the first-order common factors (i.e., not
 ##' accounted for by the second-order factor), and the measurement error of
 ##' observed indicators:
 ##'
 ##' \deqn{ \hat{\Sigma} = \Lambda \bold{B} \Phi_2 \bold{B}^{\prime}
 ##' \Lambda^{\prime} + \Lambda \Psi_{u} \Lambda^{\prime} + \Theta, }
 ##'
 ##' where \eqn{\hat{\Sigma}} is the model-implied covariance matrix,
 ##' \eqn{\Lambda} contains first-order factor loadings, \eqn{\bold{B}} contains
 ##' second-order factor loadings, \eqn{\Phi_2} is the covariance matrix of the
 ##' second-order factor(s), \eqn{\Psi_{u}} is the covariance matrix of residuals
 ##' from first-order factors, and \eqn{\Theta} is the covariance matrix of the
 ##' measurement errors from observed indicators. Thus, we can calculate the
 ##' proportion of variance of a composite score calculated from the observed
 ##' indicators (e.g., a total score or scale mean) that is attributable to the
 ##' second-order factor, i.e. coefficient omega at Level 1:
 ##'
 ##' \deqn{ \omega_{L1} = \frac{\bold{1}^{\prime} \Lambda \bold{B} \Phi_2
 ##' \bold{B}^{\prime} \Lambda^{\prime} \bold{1}}{\bold{1}^{\prime} \Lambda
 ##' \bold{B} \Phi_2 \bold{B} ^{\prime} \Lambda^{\prime} \bold{1} +
 ##' \bold{1}^{\prime} \Lambda \Psi_{u} \Lambda^{\prime} \bold{1} +
 ##' \bold{1}^{\prime} \Theta \bold{1}}, }
 ##'
 ##' where \eqn{\bold{1}} is the \emph{k}-dimensional vector of 1 and \emph{k} is
 ##' the number of observed variables.
 ##'
 ##' The model-implied covariance matrix among first-order factors (\eqn{\Phi_1})
 ##' can be calculated as:
 ##'
 ##' \deqn{ \Phi_1 = \bold{B} \Phi_2 \bold{B}^{\prime} + \Psi_{u}, }
 ##'
 ##' Thus, the proportion of variance among first-order common factors that is
 ##' attributable to the second-order factor (i.e., coefficient omega at Level 2)
 ##' can be calculated as:
 ##'
 ##' \deqn{ \omega_{L2} = \frac{\bold{1_F}^{\prime} \bold{B} \Phi_2
 ##' \bold{B}^{\prime} \bold{1_F}}{\bold{1_F}^{\prime} \bold{B} \Phi_2
 ##' \bold{B}^{\prime} \bold{1_F} + \bold{1_F}^{\prime} \Psi_{u} \bold{1_F}}, }
 ##'
 ##' where \eqn{\bold{1_F}} is the \emph{F}-dimensional vector of 1 and \emph{F}
 ##' is the number of first-order factors. This Level-2 omega can be interpreted
 ##' as an estimate of the reliability of a hypothetical composite calculated
 ##' from error-free observable variables representing the first-order common
 ##' factors. This might only be meaningful as a thought experiment.
 ##'
 ##' An additional thought experiment is possible: If the observed indicators
 ##' contained only the second-order common-factor variance and unsystematic
 ##' measurement error, then there would be no first-order common factors because
 ##' their unique variances would be excluded from the observed measures. An
 ##' estimate of this hypothetical composite reliability can be calculated as the
 ##' partial coefficient omega at Level 1, or the proportion of observed
 ##' variance explained by the second-order factor after partialling out the
 ##' uniqueness from the first-order factors:
 ##'
 ##' \deqn{ \omega_{L1} = \frac{\bold{1}^{\prime} \Lambda \bold{B} \Phi_2
 ##' \bold{B}^{\prime} \Lambda^{\prime} \bold{1}}{\bold{1}^{\prime} \Lambda
 ##' \bold{B} \Phi_2 \bold{B}^{\prime} \Lambda^{\prime} \bold{1} +
 ##' \bold{1}^{\prime} \Theta \bold{1}}, }
 ##'
 ##' Note that if the second-order factor has a direct factor loading on some
 ##' observed variables, the observed variables will be counted as first-order
 ##' factors, which might not be desirable.
 ##'
 ##'
 ##' @importFrom lavaan lavInspect
 ##'
 ##' @param object A \code{\linkS4class{lavaan}} or
 ##'   \code{\linkS4class{lavaan.mi}} object, expected to contain a least one
 ##'   exogenous higher-order common factor.
 ##' @param secondFactor The name of a single second-order factor in the
 ##'   model fitted in \code{object}. The function must be called multiple
 ##'   times to estimate reliability for each higher-order factor.
 ##' @param omit.imps \code{character} vector specifying criteria for omitting
 ##'        imputations from pooled results.  Can include any of
 ##'        \code{c("no.conv", "no.se", "no.npd")}, the first 2 of which are the
 ##'        default setting, which excludes any imputations that did not
 ##'        converge or for which standard errors could not be computed.  The
 ##'        last option (\code{"no.npd"}) would exclude any imputations which
 ##'        yielded a nonpositive definite covariance matrix for observed or
 ##'        latent variables, which would include any "improper solutions" such
 ##'        as Heywood cases.  NPD solutions are not excluded by default because
 ##'        they are likely to occur due to sampling error, especially in small
 ##'        samples.  However, gross model misspecification could also cause
 ##'        NPD solutions, users can compare pooled results with and without
 ##'        this setting as a sensitivity analysis to see whether some
 ##'        imputations warrant further investigation.
 ##'
 ##' @return Reliability values at Levels 1 and 2 of the second-order factor, as
 ##'   well as the partial reliability value at Level 1
 ##'
 ##' @author Sunthud Pornprasertmanit (\email{psunthud@@gmail.com})
 ##'
 ##' @seealso
 ##'   \code{\link{reliability}} for the reliability of the first-order factors.
 ##'
 ##' @examples
 ##'
 ##' HS.model3 <- ' visual  =~ x1 + x2 + x3
 ##'                textual =~ x4 + x5 + x6
 ##'                speed   =~ x7 + x8 + x9
 ##' 			         higher =~ visual + textual + speed'
 ##'
 ##' fit6 <- cfa(HS.model3, data = HolzingerSwineford1939)
 ##' reliability(fit6) # Should provide a warning for the endogenous variables
 ##' reliabilityL2(fit6, "higher")
 ##'
 ##' @export
 reliabilityL2 <- function(object, secondFactor,
                           omit.imps = c("no.conv","no.se")) {
   secondFactor <- as.character(secondFactor)[1] # only one at a time
   
   ngroups <- lavInspect(object, "ngroups") #TODO: adapt to multiple levels
   nlevels <- lavInspect(object, "nlevels")
   nblocks <- ngroups*nlevels #FIXME: always true?
   group.label <- if (ngroups > 1L) lavInspect(object, "group.label") else NULL
   #FIXME? lavInspect(object, "level.labels")
   clus.label <- if (nlevels > 1L) c("within", lavInspect(object, "cluster")) else NULL
   if (nblocks > 1L) {
     block.label <- paste(rep(group.label, each = nlevels), clus.label,
                          sep = if (ngroups > 1L && nlevels > 1L) "_" else "")
   }
   
   ## parameters in GLIST format (not flat, need block-level list)
   if (inherits(object, "lavaan")) {
     param <- lavInspect(object, "est")
     ve <- lavInspect(object, "cov.lv") # model-implied latent covariance matrix
     S <- object@h1$implied$cov # observed sample covariance matrix (already a list)
     
     if (nblocks == 1L) {
       param <- list(param)
       ve <- list(ve)
     }
     
   } else if (inherits(object, "lavaan.mi")) {
     useImps <- rep(TRUE, length(object@DataList))
     if ("no.conv" %in% omit.imps) useImps <- sapply(object@convergence, "[[", i = "converged")
     if ("no.se" %in% omit.imps) useImps <- useImps & sapply(object@convergence, "[[", i = "SE")
     if ("no.npd" %in% omit.imps) {
       Heywood.lv <- sapply(object@convergence, "[[", i = "Heywood.lv")
       Heywood.ov <- sapply(object@convergence, "[[", i = "Heywood.ov")
       useImps <- useImps & !(Heywood.lv | Heywood.ov)
     }
     m <- sum(useImps)
     if (m == 0L) stop('No imputations meet "omit.imps" criteria.')
     useImps <- which(useImps)
     
     param <- object@coefList[[ useImps[1] ]] # first admissible as template
     coefList <- object@coefList[useImps]
     phiList <- object@phiList[useImps]
     ## add block-level list per imputation?
     if (nblocks == 1L) {
       param <- list(param)
       for (i in 1:m) {
         coefList[[i]] <- list(coefList[[i]])
         phiList[[i]] <- list(phiList[[i]])
       }
     }
     
     S <- vector("list", nblocks) # pooled observed covariance matrix
     ve <- vector("list", nblocks)
     ## loop over blocks
     for (b in 1:nblocks) {
       
       ## param:  loop over GLIST elements
       for (mat in names(param[[b]])) {
         matList <- lapply(coefList, function(i) i[[b]][[mat]])
         param[[b]][[mat]] <- Reduce("+", matList) / length(matList)
       } # mat
       
       ## pooled observed covariance matrix
       covList <- lapply(object@h1List[useImps], function(i) i$implied$cov[[b]])
       S[[b]] <- Reduce("+", covList) / m
       
       ## pooled model-implied latent covariance matrix
       ve[[b]] <- Reduce("+", lapply(phiList, "[[", i = b) ) / m
       
     } # b
     
   }
   
   if (nblocks == 1L) {
     SigmaHat <- getMethod("fitted", class(object))(object)["cov"] # retain list format
   } else {
     SigmaHat <- sapply(getMethod("fitted", class(object))(object),
                        "[[", "cov", simplify = FALSE)
   }
   
   ly <- lapply(param, "[[", "lambda")
   te <- lapply(param, "[[", "theta")
   ps <- lapply(param, "[[", "psi")
   be <- lapply(param, "[[", "beta")
   
   result <- list()
   for (i in 1:nblocks) {
     
     # Prepare for higher-order reliability
     l2var <- ve[[i]][secondFactor, secondFactor, drop = FALSE]
     l2load <- be[[1]][,secondFactor]
     indexl2 <- which(l2load != 0)
     commonl2 <- (sum(l2load)^2) * l2var
     errorl2 <- sum(ps[[i]][indexl2, indexl2, drop = FALSE])
     
     # Prepare for lower-order reliability
     indexl1 <- which(apply(ly[[i]][,indexl2, drop = FALSE], 1, function(x) sum(x != 0)) > 0)
     l1load <- ly[[i]][,indexl2] %*% as.matrix(be[[1]][indexl2, secondFactor, drop = FALSE])
     commonl1 <- (sum(l1load)^2) * l2var
     errorl1 <- sum(te[[i]][indexl1, indexl1, drop = FALSE])
     uniquel1 <- 0
     for (j in seq_along(indexl2)) {
       uniquel1 <- uniquel1 + (sum(ly[[i]][,indexl2[j]])^2) * ps[[i]][indexl2[j], indexl2[j], drop = FALSE]
     }
     
     # Adjustment for direct loading from L2 to observed variables
     if (any(ly[[i]][,secondFactor] != 0)) {
       indexind <- which(ly[[i]][,secondFactor] != 0)
       if (length(intersect(indexind, indexl1)) > 0)
         stop("Direct and indirect loadings of higher-order factor to observed",
              " variables are specified at the same time.")
       commonl2 <- sum(c(ly[[i]][,secondFactor], l2load))^2 * l2var
       errorl2 <- errorl2 + sum(te[[i]][indexind, indexind, drop = FALSE])
       commonl1 <- sum(c(ly[[i]][,secondFactor], l1load))^2 * l2var
       errorl1 <- errorl1 + sum(te[[i]][indexind, indexind, drop = FALSE])
     }
     
     # Calculate Reliability
     omegaL1 <- commonl1 / (commonl1 + uniquel1 + errorl1)
     omegaL2 <- commonl2 / (commonl2 + errorl2)
     partialOmegaL1 <- commonl1 / (commonl1 + errorl1)
     result[[i]] <- c(omegaL1 = omegaL1, omegaL2 = omegaL2, partialOmegaL1 = partialOmegaL1)
   }
   
   if (nblocks == 1L) {
     result <- result[[1]]
   } else names(result) <- block.label
   
   result
 }
 
 
 
 ## --------------
 ## maximalRelia()
 ## --------------
 
 ##' Calculate maximal reliability
 ##'
 ##' Calculate maximal reliability of a scale
 ##'
 ##' Given that a composite score (\eqn{W}) is a weighted sum of item scores:
 ##'
 ##' \deqn{ W = \bold{w}^\prime \bold{x} ,}
 ##'
 ##' where \eqn{\bold{x}} is a \eqn{k \times 1} vector of the scores of each
 ##' item, \eqn{\bold{w}} is a \eqn{k \times 1} weight vector of each item, and
 ##' \eqn{k} represents the number of items. Then, maximal reliability is
 ##' obtained by finding \eqn{\bold{w}} such that reliability attains its maximum
 ##' (Li, 1997; Raykov, 2012). Note that the reliability can be obtained by
 ##'
 ##' \deqn{ \rho = \frac{\bold{w}^\prime \bold{S}_T \bold{w}}{\bold{w}^\prime
 ##' \bold{S}_X \bold{w}}}
 ##'
 ##' where \eqn{\bold{S}_T} is the covariance matrix explained by true scores and
 ##' \eqn{\bold{S}_X} is the observed covariance matrix. Numerical method is used
 ##' to find \eqn{\bold{w}} in this function.
 ##'
 ##' For continuous items, \eqn{\bold{S}_T} can be calculated by
 ##'
 ##' \deqn{ \bold{S}_T = \Lambda \Psi \Lambda^\prime,}
 ##'
 ##' where \eqn{\Lambda} is the factor loading matrix and \eqn{\Psi} is the
 ##' covariance matrix among factors. \eqn{\bold{S}_X} is directly obtained by
 ##' covariance among items.
 ##'
 ##' For categorical items, Green and Yang's (2009) method is used for
 ##' calculating \eqn{\bold{S}_T} and \eqn{\bold{S}_X}. The element \eqn{i} and
 ##' \eqn{j} of \eqn{\bold{S}_T} can be calculated by
 ##'
 ##' \deqn{ \left[\bold{S}_T\right]_{ij} = \sum^{C_i - 1}_{c_i = 1} \sum^{C_j -
 ##' 1}_{c_j - 1} \Phi_2\left( \tau_{x_{c_i}}, \tau_{x_{c_j}}, \left[ \Lambda
 ##' \Psi \Lambda^\prime \right]_{ij} \right) - \sum^{C_i - 1}_{c_i = 1}
 ##' \Phi_1(\tau_{x_{c_i}}) \sum^{C_j - 1}_{c_j - 1} \Phi_1(\tau_{x_{c_j}}),}
 ##'
 ##' where \eqn{C_i} and \eqn{C_j} represents the number of thresholds in Items
 ##' \eqn{i} and \eqn{j}, \eqn{\tau_{x_{c_i}}} represents the threshold \eqn{c_i}
 ##' of Item \eqn{i}, \eqn{\tau_{x_{c_j}}} represents the threshold \eqn{c_i} of
 ##' Item \eqn{j}, \eqn{ \Phi_1(\tau_{x_{c_i}})} is the cumulative probability of
 ##' \eqn{\tau_{x_{c_i}}} given a univariate standard normal cumulative
 ##' distribution and \eqn{\Phi_2\left( \tau_{x_{c_i}}, \tau_{x_{c_j}}, \rho
 ##' \right)} is the joint cumulative probability of \eqn{\tau_{x_{c_i}}} and
 ##' \eqn{\tau_{x_{c_j}}} given a bivariate standard normal cumulative
 ##' distribution with a correlation of \eqn{\rho}
 ##'
 ##' Each element of \eqn{\bold{S}_X} can be calculated by
 ##'
 ##' \deqn{ \left[\bold{S}_T\right]_{ij} = \sum^{C_i - 1}_{c_i = 1} \sum^{C_j -
 ##' 1}_{c_j - 1} \Phi_2\left( \tau_{V_{c_i}}, \tau_{V_{c_j}}, \rho^*_{ij}
 ##' \right) - \sum^{C_i - 1}_{c_i = 1} \Phi_1(\tau_{V_{c_i}}) \sum^{C_j -
 ##' 1}_{c_j - 1} \Phi_1(\tau_{V_{c_j}}),}
 ##'
 ##' where \eqn{\rho^*_{ij}} is a polychoric correlation between Items \eqn{i}
 ##' and \eqn{j}.
 ##'
 ##'
 ##' @importFrom lavaan lavInspect lavNames
 ##'
 ##' @param object A \code{\linkS4class{lavaan}} or
 ##'   \code{\linkS4class{lavaan.mi}} object, expected to contain only
 ##'   exogenous common factors (i.e., a CFA model).
 ##' @param omit.imps \code{character} vector specifying criteria for omitting
 ##'        imputations from pooled results.  Can include any of
 ##'        \code{c("no.conv", "no.se", "no.npd")}, the first 2 of which are the
 ##'        default setting, which excludes any imputations that did not
 ##'        converge or for which standard errors could not be computed.  The
 ##'        last option (\code{"no.npd"}) would exclude any imputations which
 ##'        yielded a nonpositive definite covariance matrix for observed or
 ##'        latent variables, which would include any "improper solutions" such
 ##'        as Heywood cases.  NPD solutions are not excluded by default because
 ##'        they are likely to occur due to sampling error, especially in small
 ##'        samples.  However, gross model misspecification could also cause
 ##'        NPD solutions, users can compare pooled results with and without
 ##'        this setting as a sensitivity analysis to see whether some
 ##'        imputations warrant further investigation.
 ##'
 ##' @return Maximal reliability values of each group. The maximal-reliability
 ##'   weights are also provided. Users may extracted the weighted by the
 ##'   \code{attr} function (see example below).
 ##'
 ##' @author Sunthud Pornprasertmanit (\email{psunthud@@gmail.com})
 ##'
 ##' @seealso \code{\link{reliability}} for reliability of an unweighted
 ##'   composite score
 ##'
 ##' @references
 ##' Li, H. (1997). A unifying expression for the maximal reliability of a linear
 ##' composite. \emph{Psychometrika, 62}(2), 245--249. \doi{10.1007/BF02295278}
 ##'
 ##' Raykov, T. (2012). Scale construction and development using structural
 ##' equation modeling. In R. H. Hoyle (Ed.), \emph{Handbook of structural
 ##' equation modeling} (pp. 472--494). New York, NY: Guilford.
 ##'
 ##' @examples
 ##'
 ##' total <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
 ##' fit <- cfa(total, data = HolzingerSwineford1939)
 ##' maximalRelia(fit)
 ##'
 ##' # Extract the weight
 ##' mr <- maximalRelia(fit)
 ##' attr(mr, "weight")
 ##'
 ##' @export
 maximalRelia <- function(object, omit.imps = c("no.conv","no.se")) {
   ngroups <- lavInspect(object, "ngroups") #TODO: adapt to multiple levels
   nlevels <- lavInspect(object, "nlevels")
   nblocks <- ngroups*nlevels #FIXME: always true?
   group.label <- if (ngroups > 1L) lavInspect(object, "group.label") else NULL
   #FIXME? lavInspect(object, "level.labels")
   clus.label <- if (nlevels > 1L) c("within", lavInspect(object, "cluster")) else NULL
   if (nblocks > 1L) {
     block.label <- paste(rep(group.label, each = nlevels), clus.label,
                          sep = if (ngroups > 1L && nlevels > 1L) "_" else "")
   }
   
   ## parameters in GLIST format (not flat, need block-level list)
   if (inherits(object, "lavaan")) {
     param <- lavInspect(object, "est")
     ve <- lavInspect(object, "cov.lv") # model-implied latent covariance matrix
     S <- object@h1$implied$cov # observed sample covariance matrix (already a list)
     
     if (nblocks == 1L) {
       param <- list(param)
       ve <- list(ve)
     }
     
   } else if (inherits(object, "lavaan.mi")) {
     useImps <- rep(TRUE, length(object@DataList))
     if ("no.conv" %in% omit.imps) useImps <- sapply(object@convergence, "[[", i = "converged")
     if ("no.se" %in% omit.imps) useImps <- useImps & sapply(object@convergence, "[[", i = "SE")
     if ("no.npd" %in% omit.imps) {
       Heywood.lv <- sapply(object@convergence, "[[", i = "Heywood.lv")
       Heywood.ov <- sapply(object@convergence, "[[", i = "Heywood.ov")
       useImps <- useImps & !(Heywood.lv | Heywood.ov)
     }
     m <- sum(useImps)
     if (m == 0L) stop('No imputations meet "omit.imps" criteria.')
     useImps <- which(useImps)
     
     param <- object@coefList[[ useImps[1] ]] # first admissible as template
     coefList <- object@coefList[useImps]
     phiList <- object@phiList[useImps]
     ## add block-level list per imputation?
     if (nblocks == 1L) {
       param <- list(param)
       for (i in 1:m) {
         coefList[[i]] <- list(coefList[[i]])
         phiList[[i]] <- list(phiList[[i]])
       }
     }
     S <- vector("list", nblocks) # pooled observed covariance matrix
     ve <- vector("list", nblocks)
     ## loop over blocks
     for (b in 1:nblocks) {
       
       ## param:  loop over GLIST elements
       for (mat in names(param[[b]])) {
         matList <- lapply(coefList, function(i) i[[b]][[mat]])
         param[[b]][[mat]] <- Reduce("+", matList) / length(matList)
       } # mat
       
       ## pooled observed covariance matrix
       covList <- lapply(object@h1List[useImps], function(i) i$implied$cov[[b]])
       S[[b]] <- Reduce("+", covList) / m
       
       ## pooled model-implied latent covariance matrix
       ve[[b]] <- Reduce("+", lapply(phiList, "[[", i = b) ) / m
       
     } # b
     
   }
   
   if (nblocks == 1L) {
     SigmaHat <- getMethod("fitted", class(object))(object)["cov"] # retain list format
   } else {
     SigmaHat <- sapply(getMethod("fitted", class(object))(object),
                        "[[", "cov", simplify = FALSE)
   }
   
   ly <- lapply(param, "[[", "lambda")
   te <- lapply(param, "[[", "theta")
   
   categorical <- lavInspect(object, "categorical")
   threshold <- if (categorical) getThreshold(object) else NULL
   
   result <- list()
   for (i in 1:nblocks) {
     truevar <- ly[[i]] %*% ve[[i]] %*% t(ly[[i]])
     varnames <- colnames(truevar)
     if (categorical) {
       invstdvar <- 1 / sqrt(diag(SigmaHat[[i]]))
       polyr <- diag(invstdvar) %*% truevar %*% diag(invstdvar)
       nitem <- ncol(SigmaHat[[i]])
       result[[i]] <- calcMaximalReliaCat(polyr, threshold[[i]], S[[i]], nitem, varnames)
     } else {
       result[[i]] <- calcMaximalRelia(truevar, S[[i]], varnames)
     }
   }
   
   if (nblocks == 1L) {
     result <- result[[1]]
   } else names(result) <- block.label
   
   result
 }
 
 
 
 ## ----------------
 ## Hidden Functions
 ## ----------------
 
 computeAlpha <- function(S) {
   k <- nrow(S)
   k/(k - 1) * (1.0 - sum(diag(S)) / sum(S))
 }
 
 #' @importFrom stats cov2cor pnorm
 omegaCat <- function(truevar, threshold, scales, denom) {
   ## must be in standardized latent scale
   R <- diag(scales) %*% truevar %*% diag(scales)
   
   ## denom could be model-implied polychoric correlation assuming diagonal theta,
   ##       model-implied polychoric correlation accounting for error covariances,
   ##       or "observed" polychoric correlation matrix.
   ## If parameterization="theta", standardize the polychoric coVARIANCE matrix
   denom <- cov2cor(denom)
   
   nitem <- ncol(denom)
   ## initialize sums of cumulative probabilities
   sumnum <- 0 # numerator
   addden <- 0 # denominator
   ## loop over all pairs of items
   for (j in 1:nitem) {
     for (jp in 1:nitem) {
       ## initialize sums of cumulative probabilities *per item*
       sumprobn2 <- 0
       addprobn2 <- 0
       ## for each pair of items, loop over all their thresholds
       t1 <- threshold[[j]]  * scales[j] # on standardized latent scale
       t2 <- threshold[[jp]] * scales[jp]
       for (c in 1:length(t1)) {
         for (cp in 1:length(t2)) {
           sumprobn2 <- sumprobn2 + p2(t1[c], t2[cp], R[j, jp])
           addprobn2 <- addprobn2 + p2(t1[c], t2[cp], denom[j, jp])
         }
       }
       sumprobn1 <- sum(pnorm(t1))
       sumprobn1p <- sum(pnorm(t2))
       sumnum <- sumnum + (sumprobn2 - sumprobn1 * sumprobn1p)
       addden <- addden + (addprobn2 - sumprobn1 * sumprobn1p)
     }
   }
   reliab <- sumnum / addden
   reliab
 }
 
 
 p2 <- function(t1, t2, r) {
   mnormt::pmnorm(c(t1, t2), c(0,0), matrix(c(1, r, r, 1), 2, 2))
 }
 
 
 # polycorLavaan <- function(object) {
 # 	ngroups <- lavInspect(object, "ngroups")
 # 	coef <- lavInspect(object, "est")
 # 	targettaunames <- NULL
 # 	if (ngroups == 1L) {
 # 		targettaunames <- rownames(coef$tau)
 # 	} else {
 # 		targettaunames <- rownames(coef[[1]]$tau)
 # 	}
 # 	barpos <- sapply(strsplit(targettaunames, ""), function(x) which(x == "|"))
 # 	varnames <- unique(apply(data.frame(targettaunames, barpos - 1), MARGIN = 1,
 # 	                         FUN = function(x) substr(x[1], 1, x[2])))
 # 	if (length(varnames))
 # 	script <- ""
 # 	for (i in 2:length(varnames)) {
 # 		temp <- paste0(varnames[1:(i - 1)], collapse = " + ")
 # 		temp <- paste0(varnames[i], "~~", temp, "\n")
 # 		script <- paste(script, temp)
 # 	}
 # 	newobject <- refit(script, object)
 # 	if (ngroups == 1L) {
 # 		return(lavInspect(newobject, "est")$theta)
 # 	}
 # 	lapply(lavInspect(newobject, "est"), "[[", "theta")
 # }
 
 ##' @importFrom lavaan lavInspect lavNames
 getThreshold <- function(object) {
   ngroups <- lavInspect(object, "ngroups") #TODO: add nlevels when capable
   ordnames <- lavNames(object, "ov.ord")
   EST <- lavInspect(object, "est")
   
   if (ngroups == 1L) {
     thresholds <- EST$tau[,"threshold"]
     result <- lapply(ordnames,
                      function(nn) thresholds[grepl(nn, names(thresholds))])
     names(result) <- ordnames
     ## needs to be within a list when called above within block-loops
     result <- list(result)
     
   } else {
     allThr <- EST[which(names(EST) == "tau")]
     ## convert 1-column matrices to vectors, preserving rownames
     thresholds <- sapply(allThr, "[", j = "threshold", simplify = FALSE)
     result <- list()
     group.label <- lavInspect(object, "group.label")
     
     for (g in 1:ngroups) {
       result[[ group.label[g] ]] <- lapply(ordnames, function(nn) {
         thresholds[[g]][ grepl(nn, names(thresholds[[g]])) ]
       })
       names(result[[ group.label[g] ]]) <- ordnames
     }
     
   }
   
   return(result)
 }
 
 ##' @importFrom lavaan lavInspect lavNames
 getScales <- function(object) {
   ngroups <- lavInspect(object, "ngroups") #TODO: add nlevels when capable
   ordnames <- lavNames(object, "ov.ord") #TODO: use to allow mix of cat/con vars
   EST <- lavInspect(object, "est")
   
   if (ngroups == 1L) {
     result <- list(EST$delta[,"scales"])
   } else {
     result <- lapply(EST[which(names(EST) == "delta")],
                      function(x) x[,"scales"])
     names(result) <- lavInspect(object, "group.label")
   }
   
   return(result)
 }
 
 invGeneralRelia <- function(w, truevar, totalvar) {
   1 - (t(w) %*% truevar %*% w) / (t(w) %*% totalvar %*% w)
 }
 
 #' @importFrom stats pnorm
 invGeneralReliaCat <- function(w, polyr, threshold, denom, nitem) {
   # denom could be polychoric correlation, model-implied correlation, or model-implied without error correlation
   upper <- matrix(NA, nitem, nitem)
   lower <- matrix(NA, nitem, nitem)
   for (j in 1:nitem) {
     for (jp in 1:nitem) {
       sumprobn2 <- 0
       addprobn2 <- 0
       t1 <- threshold[[j]]
       t2 <- threshold[[jp]]
       for (c in 1:length(t1)) {
         for (cp in 1:length(t2)) {
           sumprobn2 <- sumprobn2 + p2(t1[c], t2[cp], polyr[j, jp])
           addprobn2 <- addprobn2 + p2(t1[c], t2[cp], denom[j, jp])
         }
       }
       sumprobn1 <- sum(pnorm(t1))
       sumprobn1p <- sum(pnorm(t2))
       upper[j, jp] <- (sumprobn2 - sumprobn1 * sumprobn1p)
       lower[j, jp] <- (addprobn2 - sumprobn1 * sumprobn1p)
     }
   }
   1 - (t(w) %*% upper %*% w) / (t(w) %*% lower %*% w)
 }
 
 #' @importFrom stats nlminb
 calcMaximalRelia <- function(truevar, totalvar, varnames) {
   start <- rep(1, nrow(truevar))
   out <- nlminb(start, invGeneralRelia, truevar = truevar, totalvar = totalvar)
   if (out$convergence != 0) stop("The numerical method for finding the maximal",
                                  " reliability did not converge.")
   result <- 1 - out$objective
   weight <- out$par / mean(out$par)
   names(weight) <- varnames
   attr(result, "weight") <- weight
   result
 }
 
 #' @importFrom stats nlminb
 calcMaximalReliaCat <- function(polyr, threshold, denom, nitem, varnames) {
   start <- rep(1, nrow(polyr))
   out <- nlminb(start, invGeneralReliaCat, polyr = polyr, threshold = threshold,
                 denom = denom, nitem = nitem)
   if (out$convergence != 0) stop("The numerical method for finding the maximal",
                                  " reliability did not converge.")
   result <- 1 - out$objective
   weight <- out$par / mean(out$par)
   names(weight) <- varnames
   attr(result, "weight") <- weight
   result
 }