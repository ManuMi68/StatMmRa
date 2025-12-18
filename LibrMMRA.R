# Teach Statistical Analysis to Psychologists
# University of Jaen, Spain
# Mars 2012, sept 2023
# 
# Author: Manuel Miguel Ramos Álvarez
###############################################################################

# Loading needed packages
chkPkg <-function(l.of.p){
  new.packages <- l.of.p[!(l.of.p %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(l.of.p, function(x) {
    if(! x %in% tolower((.packages()))) library(x, character.only = TRUE)
  })
  invisible()
}


# Codigo antiguo sustituido
  # if(!require(Rcmdr))
  # {
  # 	print("No tiene instalado el paquete Rcmdr, vamos a intentar instalarlo...")
  # 	install.packages("Rcmdr")		
  # 	library(Rcmdr)
  # }
  # if(!require(modeest))
  # {
  # 	print("No tiene instalado el paquete modeest, vamos a intentar instalarlo...")
  # 	install.packages("modeest")		
  # 	library(modeest)
  # }
  # if(!require(e1071))
  # {
  # 	print("No tiene instalado el paquete e1071, vamos a intentar instalarlo...")
  # 	install.packages("e1071")		
  # 	library(e1071)
  # }

SummUja <-function(DatPas,DPrec=3,Recor=.20) {
  # Example: SummUja(DatPas = c(1,3,5,6,7,8,2,1,2))
  
	#install.packages("Rcmdr",repos="http://cran.es.r-project.org/");
	#install.packages("modeest",repos="http://cran.es.r-project.org/");
	#install.packages("e1071",repos="http://cran.es.r-project.org/");
	#require(Rcmdr);require(modeest); require(e1071);
  chkPkg(c("Rcmdr", "modeest", "e1071"))
  Ntt<-length(DatPas)
	M<-mean(DatPas)
	V<-var(DatPas)
	DT<-sd(DatPas)
	ETM<-DT/sqrt(Ntt)
	CV<-DT/M
	Md<-median(DatPas)
	Q1<-quantile(DatPas,.25)
	Q3<-quantile(DatPas,.75)
	RI<-Q3-Q1
	ASI<-RI/2
	DM<-sum(abs(DatPas-M))/Ntt
	DMd<-sum(abs(DatPas-Md))/Ntt
	Asim<-skewness(DatPas)
	Curt<-kurtosis(DatPas)
	Acot<-mean(DatPas, trim=Recor)
	Geom<-exp(sum(log(DatPas))/Ntt)
	Harm<-1/(sum(1/DatPas)/Ntt)
	# Moda<-mlv(DatPas, method = "mfv")[1]$M
	Moda<-mlv(DatPas, method = "mfv")
	FrVar <- summary(as.factor(DatPas))
	RV<-1-(max(FrVar)/sum(FrVar))
	
	M<-round(M,DPrec);V<-round(V,DPrec);DT<-round(DT,DPrec);
	ETM<-round(ETM,DPrec);CV<-round(CV,DPrec);Md<-round(Md,DPrec);
	Q1<-round(Q1,DPrec);Q3<-round(Q3,DPrec);RI<-round(RI,DPrec);
	ASI<-round(ASI,DPrec);DM<-round(DM,DPrec);DMd<-round(DMd,DPrec);
	Asim<-round(Asim,DPrec);Curt<-round(Curt,DPrec);
	Acot<-round(Acot,DPrec);Geom<-round(Geom,DPrec);Harm<-round(Harm,DPrec);
	Moda<-round(Moda,DPrec);RV<-round(RV,DPrec);
	
	Basicos<-list("Media"=M,"Varianza"=V,"Desv"=DT,"ETM"=ETM,"CoefVar"=CV);
	Robustos<-list("Mediana"=Md,"Cuart3_Perc75"=Q3,"Cuart1_Perc25"=Q1,"RangoInt"=RI,"AmplSemInt"=ASI,"DesvMedia"=DM,"DesvMediana"=DMd);
	Forma<-list("Asim"=Asim,"Curtosis"=Curt);
	Especiales<-list("M.Acotada"=Acot,"M.Armon"=Harm,"M.Geom"=Geom);
	Cualitativas<-list("Moda"=Moda,"RazonVar"=RV,"NumCasos"=Ntt);
	return(c("Basicos"=Basicos,"Robustos"=Robustos,"Forma"=Forma,"Especiales"=Especiales,"Cualitativas"=Cualitativas))
}

rndBlock<-function(Valores,Ciclos) {
  # Example: rndBlock(c("Exp","Control"),10)
  
  # True Random Number Generators 
   # The http://random.org services uses atmospheric noise sample via a radio tuned to 
   # an unused broadcast frequency together with a skew correction originally due to John von Neumann
  # Random Sequences recom for Random Sampling (e.g., drug screening)
  
  
  chkPkg(c("random","foreach"))
  # Basic Option
    # res<-matrix(NA,nrow = length(Valores),ncol = Ciclos)
    # for (i in 1:Ciclos) res[,i]<-Valores[randomSequence(1,length(Valores))]
    # as.data.frame(res)
  #
  foreach(i=1:Ciclos) %do% Valores[randomSequence(1,length(Valores))]
  # write.csv2(res,"ResRandom.csv")
}

rndpseudo<-function(Valores,Ciclos) {
  # Example: rndpseudo(c("Exp","Control"),10)
  
  # Pseudo-Random Number Generators
  # Random Sequences recom for Simulation and Modelling
  
  # Basic Option
    # res<-matrix(NA,nrow = length(Valores),ncol = Ciclos)
    # for (i in 1:Ciclos) res[,i]<-sample(Valores, length(Valores), replace=F)
    # as.data.frame(res)
  #
  foreach(i=1:Ciclos) %do% sample(Valores, length(Valores), replace=F)
  # write.csv2(res,"ResRandomPseudo.csv")
}

rndMuestreo<-function(TamPobl,TamMues) {
  # Example: rndMuestreo(1000,100)
  
  # True Random Number Generators 
  # The http://random.org services uses atmospheric noise sample via a radio tuned to 
  # an unused broadcast frequency together with a skew correction originally due to John von Neumann
  # Random Sequences recom for Random Sampling (e.g., drug screening)
  
  randomSequence(1,TamPobl)[1:TamMues]
  # write.csv2(res,"ResRandomPseudo.csv")
}

rndMuestreoAcot<-function(PunIn, PunFin,TamMues, Restr=0) {
  # Example: rndMuestreoAcot(1000,3000,50,4000)
  
  rndr<-randomNumbers(n=TamMues*100, min=PunIn, max=PunFin, col=1, base=10, check=TRUE)
  Extracc<-NULL
  rndr2<-list()
  cntg<-0
  #i=2
  if (Restr==0) { 
    rndr2 <- list(rndr[1:TamMues])
  }
  if (Restr>0) { 
    for (i in 1:TamMues)  {
      Extracc<-NULL
      cnt<-0
      repeat{
        cnt <-cnt +1
        cntg<-cntg+1
        Extracc[cnt]<-rndr[cntg]
        
        if(sum(Extracc)>Restr){
          ResPrv <- Restr-sum(Extracc[-cnt])
          if (ResPrv<PunIn) {
            Extracc[cnt-1]<- Extracc[cnt-1] + Restr-sum(Extracc[-cnt])
            Extracc<-Extracc[-cnt]
          }
          if (ResPrv>PunIn) Extracc[cnt]<-   Restr-sum(Extracc[-cnt])
          break
        }
      }
      #cntg<-cntg+cnt
      rndr2[[i]]<-Extracc
      #sum(Extracc)
    }
  }
  rndr2
}


#End
