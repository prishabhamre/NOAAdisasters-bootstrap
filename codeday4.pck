codeday4.pck <-
c("codeday4.pck", "my.bootstrap1", "gui.bootstrap1", "my.bootstrap2", 
"gui.bootstrap2", "my.bootstrap3", "gui.bootstrapxy", "my.dat.plot5", 
"my.dat.plot5a")

#following bootstrap function plots the data fit with a smoothing spline with the default cross validation, vs the smoothing spline with 2 degrees of freedom (linear fit).
# and construct Pvalue using linear fit residual bootstrap
# test the difference of fit using the F test 
my.bootstrap1 <-
function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000){

par(mfrow=c(1,2)) # creates 2 split plots
stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F)
F0<-stat.out0$F #the F statistic for matrix mat
resid0<-stat.out0$resid.lin #the residuals of the linear fit  
bootvec<-NULL #initialization of the vector of bootstraps
y0<-predict(stat.out0$smstrlin,mat[,i])$y # predicted value for linear fit
matb<-mat #initialization of bootstrap matrix
for(i1 in 1:nboot){ #repeat following bootstrap step nboot times 
	if(floor(i1/500)==(i1/500)){print(i1)} 
	residb<-sample(resid0,replace=T) #random sample residuals with replacement 
	Yb<-y0+residb # stores the predicted value plus the random sample residuals 
	matb[,j]<-Yb # the bootstrap replaces the disaster column 
	stat.outb<-my.dat.plot5(matb,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T)
	bootvec<-c(bootvec,stat.outb$F) #the F statistic for the bootstrap is appended to the bootstapvector
}
pvalboot<-sum(bootvec>F0)/nboot #calculates the pvalue
boxplot(bootvec) #creates a boxplot 
stat.out0$pvalboot<-pvalboot #the pvalue is added to the output object
stat.out0	# return output object 
}
gui.bootstrap1 <-
function(){
library(tcltk) #returns packcage that allows the gui to appear 
inputs <- function(){
#initializing variables as text 
   x <- tclVar("NOAA")
   y <- tclVar("1")
   z <- tclVar("2")
   w<-tclVar("\"delta temp\"") 
wa<-tclVar("\"disasters\"")
wb<-tclVar("\"Disasters vs warming\"")
zc<-tclVar("2")
qc<-tclVar("F")
rc<-tclVar("10000")
#auto populates the text variables initialized above as parameters for the function my.bootstrap1
   tt <- tktoplevel()
   tkwm.title(tt,"Choose parameters for new function                   ")
   x.entry <- tkentry(tt, textvariable=x)
   y.entry <- tkentry(tt, textvariable=y)
   z.entry <- tkentry(tt, textvariable=z)
   w.entry<-tkentry(tt, textvariable=w)  
wa.entry<-tkentry(tt,textvariable=wa)
wb.entry<-tkentry(tt,textvariable=wb)
zc.entry<-tkentry(tt,textvariable=zc)
qc.entry<-tkentry(tt,textvariable=qc)
rc.entry<-tkentry(tt,textvariable=rc)
   reset <- function() #function which clears the inputs
    {
     tclvalue(x)<-""
     tclvalue(y)<-""
     tclvalue(z)<-""
	tclvalue(w)<-""
tclvalue(wa.entry)<-""
tclvalue(wb.entry)<-""
tclvalue(zc.entry)<-""
tclvalue(qc.entry)<-""
tclvalue(rc.entry)<-""
       }

   reset.but <- tkbutton(tt, text="Reset", command=reset) #creates reset button 

   submit <- function() { #function which initializes the input values to variables to be used to run the my.bootstrap1 function
     x <- tclvalue(x)
     y <- tclvalue(y)
     z <- tclvalue(z)
       w<-tclvalue(w)
	wa<-tclvalue(wa)
	wb<-tclvalue(wb)
	zc<-tclvalue(zc)  
	qc<-tclvalue(qc)
	rc<-tclvalue(rc)
     e <- parent.env(environment()) #assigning the parent environment in which the function will run 
     e$x <- x #initializing variables in the parent enviornment
     e$y <- y
     e$z <- z
e$w<-w
e$wa<-wa
e$wb<-wb
e$zc<-zc
e$qc<-qc
e$rc<-rc
        tkdestroy(tt) #closes the dialog box
   }
  #formatting text, buttons and input entry fields withing the ui dialog box
   submit.but <- tkbutton(tt, text="start", command=submit)
   tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2)
   tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
   tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
   tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)

  tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
   tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
   tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
   tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
   
tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
   tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)

   tkgrid(submit.but, reset.but)

  tkwait.window(tt) #keeps dialog box window open until the submit button is clicked
  return(c(x,y,z,w,wa,wb,zc,qc,rc)) #returns input vector 
}
#Now run the function like:
predictor_para <- inputs() #initializes inputs as predictor parameters
print(predictor_para) #prints the parameters
# evaluates the string and returns an integer which is then initialized
mat<-eval(parse(text=predictor_para[1])) 
ind1<-eval(parse(text=predictor_para[2]))
ind2<-eval(parse(text=predictor_para[3]))
xlab<-eval(parse(text=predictor_para[4]))
ylab<-eval(parse(text=predictor_para[5]))
maintitle<-eval(parse(text=predictor_para[6]))
zcol<-eval(parse(text=predictor_para[7]))
zsqrt<-eval(parse(text=predictor_para[8]))
znboot<-eval(parse(text=predictor_para[9]))
my.bootstrap1(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot)#runs my.bootstrap1 function with the predictor parameters

}
#following bootstrap function builds residuals bootstrap (pivotal or percentile selected by user) confidence intervals (confidence level inputed by user) for the smooth spline model
my.bootstrap2 <-
function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000,pred.bound=T,conf.lev=.95,pivotal=T){
par(mfrow=c(1,1)) #creates one plot
stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F)
resid0<-stat.out0$resid.mod #calculates the residuals for the smoothing spline model 
bootmat<-NULL #initialization of the bootstrap vector
y0<-predict(stat.out0$smstrmod,mat[,i])$y #predicted value for the smoothing spline model
matb<-mat #initialization of bootstrap matrix
for(i1 in 1:nboot){ #repeat following bootstrap step nboot times 
	if(floor(i1/500)==(i1/500)){print(i1)} #print the number of iterations completed by 500s
	residb<-sample(resid0,replace=T) #random sample residuals with replacement 
	Yb<-y0+residb #stores the predicted y value plus the random sample residuals 
	matb[,j]<-Yb # the bootstrap replaces the disaster column
	stat.outb<-my.dat.plot5(matb,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T)
	Ybp<-predict(stat.outb$smstrmod,matb[,i])$y #the predicted y value of the bootstrap smoothing spline model 
	if(pred.bound){ #if pred.bound is true the following lines of code will run 
		if(pivotal){ # if pivotal is true the following lines will run 
			bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp-y0) #combines into a matrix the bootstrap vector and the vector of smoothing spline model bootstrap residuals plus predicted bootstrap (matb) y value minus the predicted y value from mat
		}else{ # the following runs if pivotal is false
			bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp)#combines into a matrix the bootstrap vector and the vector of smoothing spline model bootstrap residuals plus predicted bootstrap (matb) y value
		}
	}else{ #the following lines run if pred.bound is false 
		if(pivotal){ # if pivotal is true the following lines will run
			bootmat<-rbind(bootmat,Ybp-y0) #combines into a matrix the bootstrap vector and the vector of predicted bootstrap (matb) y value minus the predicted y value from mat
		}else{ # the following runs if pivotal is false
			bootmat<-rbind(bootmat,Ybp) #combines into a matrix the bootstrap vector and the vector of predicted bootstrap (matb) y value
		}
	}

}
alpha<-(1-conf.lev)/2 # calculates alpha using the confidence level imputed by user
my.quant<-function(x,a=alpha){quantile(x,c(a,1-a))} #function which takes a vector of values and vector of alpha values and returns the a and 1-a percentiles of the vector of values.
bounds<-apply(bootmat,2,my.quant) # initializes an matrix with the function my.quant performed on the columns
if(pivotal){ # if pivotal is true the following lines will run 
	#following flips the bounds
  bounds[1,]<-y0-bounds[1,] #predicted value minus the bound in row 1 
	bounds[2,]<-y0-bounds[2,] #predicted value minus the bound in row 2 
}
x<-mat[,i] # initialize x as the disaster column 
if(do.sqrt){ #if the do.sqrt is true the following runs
	x<-sqrt(x) #the square root is taken of the disaster column 
}
o1<-order(x) #the x vector is ordered and then that order is stored to be applied in the following lines 
# following plots 2 lines with the ordered x and ordered bounds as vector of coordinates
lines(x[o1],bounds[1,o1],col=zcol+2) 
lines(x[o1],bounds[2,o1],col=zcol+2)
}

gui.bootstrap2 <-
function(){
library(tcltk) #returns package that allows the gui to appear 
 
inputs <- function(){
  #initializing variables as text 
   x <- tclVar("NOAA")
   y <- tclVar("1")
   z <- tclVar("2")
   w<-tclVar("\"delta temp\"") 
wa<-tclVar("\"disasters\"")
wb<-tclVar("\"Disasters vs warming\"")
zc<-tclVar("2")
qc<-tclVar("F")
rc<-tclVar("10000")
za<-tclVar("T")
zb<-tclVar(".95")
wc<-tclVar("T")
#auto populates the text variables initialized above as parameters for the function my.bootstrap2
   tt <- tktoplevel()
   tkwm.title(tt,"Choose parameters for new function                   ")
   x.entry <- tkentry(tt, textvariable=x)
   y.entry <- tkentry(tt, textvariable=y)
   z.entry <- tkentry(tt, textvariable=z)
   w.entry<-tkentry(tt, textvariable=w)  
wa.entry<-tkentry(tt,textvariable=wa)
wb.entry<-tkentry(tt,textvariable=wb)
zc.entry<-tkentry(tt,textvariable=zc)
qc.entry<-tkentry(tt,textvariable=qc)
rc.entry<-tkentry(tt,textvariable=rc)
za.entry<-tkentry(tt,textvariable=za)
zb.entry<-tkentry(tt,textvariable=zb)
wc.entry<-tkentry(tt,textvariable=wc)

   reset <- function() #function which clears the inputs
    {
     tclvalue(x)<-""
     tclvalue(y)<-""
     tclvalue(z)<-""
	tclvalue(w)<-""
tclvalue(wa.entry)<-""
tclvalue(wb.entry)<-""
tclvalue(zc.entry)<-""
tclvalue(qc.entry)<-""
tclvalue(rc.entry)<-""
tclvalue(za.entry)<-""
tclvalue(zb.entry)<-""
tclvalue(wc.entry)<-""

       }

   reset.but <- tkbutton(tt, text="Reset", command=reset) #creates reset button 

   submit <- function() { #function which initializes the input values to variables to be used to run the my.bootstrap2 function
     x <- tclvalue(x)
     y <- tclvalue(y)
     z <- tclvalue(z)
       w<-tclvalue(w)
	wa<-tclvalue(wa)
	wb<-tclvalue(wb)
	zc<-tclvalue(zc)  
	qc<-tclvalue(qc)
	rc<-tclvalue(rc)
	za<-tclvalue(za)
zb<-tclvalue(zb)
wc<-tclvalue(wc)

     e <- parent.env(environment())#assigning the parent environment in which the function will run 
     e$x <- x #initializing variables in the parent environment
     e$y <- y
     e$z <- z
e$w<-w
e$wa<-wa
e$wb<-wb
e$zc<-zc
e$qc<-qc
e$rc<-rc
e$za<-za
e$zb<-zb
e$wc<-wc

        tkdestroy(tt)#closes the dialog box
   }
   #formatting text, buttons and input entry fields withing the ui dialog box
   submit.but <- tkbutton(tt, text="start", command=submit)
   tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2)
   tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
   tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
   tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)

  tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
   tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
   tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
   tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
   
tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
   tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Prediction Bound?"),columnspan=2)
   tkgrid(tklabel(tt,text="Pbound=T"),za.entry, pady=10, padx =30)
tkgrid(tklabel(tt,text="Conf.level"),columnspan=2)
   tkgrid(tklabel(tt,text="Confidence"),zb.entry, pady=10, padx =30)
tkgrid(tklabel(tt,text="Pivotal=T"),columnspan=2)
   tkgrid(tklabel(tt,text="Pivotal"),wc.entry, pady=10, padx =30)


   tkgrid(submit.but, reset.but)

  tkwait.window(tt)#keeps dialog box window open until the submit button is clicked
  return(c(x,y,z,w,wa,wb,zc,qc,rc,za,zb,wc)) #returns input vector 
}
#Now run the function like:
predictor_para <- inputs()#initializes inputs as predictor parameters
print(predictor_para) #prints the parameters
# following evaluates the string and returns an integer which is then initialized
mat<-eval(parse(text=predictor_para[1]))
ind1<-eval(parse(text=predictor_para[2]))
ind2<-eval(parse(text=predictor_para[3]))
xlab<-eval(parse(text=predictor_para[4]))
ylab<-eval(parse(text=predictor_para[5]))
maintitle<-eval(parse(text=predictor_para[6]))
zcol<-eval(parse(text=predictor_para[7]))
zsqrt<-eval(parse(text=predictor_para[8]))
znboot<-eval(parse(text=predictor_para[9]))
zpred<-eval(parse(text=predictor_para[10]))
zconf<-eval(parse(text=predictor_para[11]))
zpivot<-eval(parse(text=predictor_para[12]))

my.bootstrap2(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot,zpred,zconf,zpivot) #runs my.bootstrap2 function with the predictor parameters

}
#following bootstrap function builds XY pair bootstrap (pivotal or percentile selected by user) confidence intervals (confidence level inputed by user) for the smooth spline model
my.bootstrap3 <-
function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,nboot=10000,pred.bound=T,conf.lev=.95,pivotal=T){
par(mfrow=c(1,1))  #creates one plot
stat.out0<-my.dat.plot5(mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=T,in.boot=F)
bootmat<-NULL #initialization of the bootstrap vector
y0<-predict(stat.out0$smstrmod,mat[,i])$y #predicted value for the smoothing spline model
matb<-mat #initialization of bootstrap matrix 
nm<-length(matb[,1]) #calculates length of disaster column 
for(i1 in 1:nboot){ #repeat following bootstrap step nboot times 
	if(floor(i1/500)==(i1/500)){print(i1)}  #print the number of iterations completed by 500s
	zed<-sample(nm,replace=T) #random sample disaster column lengths with replacement resulting in an integer between 1-43 inclusive
	matb<-mat[zed,] # the zedth row of mat(NOAA) is populated into the bootstrap matrix
	stat.outb<-my.dat.plot5a(matb,mat,i,j,zxlab,zylab,zmain,zcol,do.sqrt,do.plot=F,in.boot=T)
	Ybp<-predict(stat.outb$smstrmod,mat[,i])$y #the predicted y value of the bootstrap smoothing spline model 
	if(pred.bound){ #if pred.bound is true the following lines of code will run 
		if(pivotal){ # if pivotal is true the following lines will run 
			bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp-y0) #combines into a matrix the bootstrap vector and the vector of smoothing spline model bootstrap residuals plus predicted bootstrap (matb) y value minus the predicted y value from mat
		}else{ # the following runs if pivotal is false
			bootmat<-rbind(bootmat,stat.outb$resid.mod+Ybp) #combines into a matrix the bootstrap vector and the vector of smoothing spline model bootstrap residuals plus predicted bootstrap (matb) y value
		}
	}else{ #the following lines run if pred.bound is false 
		if(pivotal){ # if pivotal is true the following lines will run
			bootmat<-rbind(bootmat,Ybp-y0) #combines into a matrix the bootstrap vector and the vector of predicted bootstrap (matb) y value minus the predicted y value from mat
		}else{# the following runs if pivotal is false
			bootmat<-rbind(bootmat,Ybp)#combines into a matrix the bootstrap vector and the vector of predicted bootstrap (matb) y value
		}
	}

}
alpha<-(1-conf.lev)/2 # calculates alpha using the confidence level imputed by user
my.quant<-function(x,a=alpha){quantile(x,c(a,1-a))} #function which takes a vector of values and vector of alpha values and returns the a and 1-a percentiles of the vector of values
bounds<-apply(bootmat,2,my.quant)  # initializes an matrix with the function my.quant performed on the columns
if(pivotal){ # if pivotal is true the following lines will run 
  #following flips the bounds
  bounds[1,]<-y0-bounds[1,]
	bounds[2,]<-y0-bounds[2,]
}
x<-mat[,i] # initialize x as the disaster column 
if(do.sqrt){ #if the do.sqrt is true the following runs
	x<-sqrt(x) #the square root is taken of the disaster column
}
o1<-order(x) #the x vector is ordered and then that order is stored to be applied in the following lines 
# following plots 2 lines with the ordered x and ordered bounds as vector of coordinates
lines(x[o1],bounds[1,o1],col=zcol+2)
lines(x[o1],bounds[2,o1],col=zcol+2)
}

gui.bootstrapxy <-
function(){
library(tcltk) #returns package that allows the gui to appear
#,pred.bound=T,conf.lev=.95,pivotal=T
inputs <- function(){
  #initializing variables as text 
   x <- tclVar("NOAA")
   y <- tclVar("1")
   z <- tclVar("2")
   w<-tclVar("\"delta temp\"") 
wa<-tclVar("\"disasters\"")
wb<-tclVar("\"Disasters vs warming\"")
zc<-tclVar("2")
qc<-tclVar("F")
rc<-tclVar("10000")
za<-tclVar("T")
zb<-tclVar(".95")
wc<-tclVar("T")
#auto populates the text variables initialized above as parameters for the function my.bootstrap3
   tt <- tktoplevel()
   tkwm.title(tt,"Choose parameters for new function                   ")
   x.entry <- tkentry(tt, textvariable=x)
   y.entry <- tkentry(tt, textvariable=y)
   z.entry <- tkentry(tt, textvariable=z)
   w.entry<-tkentry(tt, textvariable=w)  
wa.entry<-tkentry(tt,textvariable=wa)
wb.entry<-tkentry(tt,textvariable=wb)
zc.entry<-tkentry(tt,textvariable=zc)
qc.entry<-tkentry(tt,textvariable=qc)
rc.entry<-tkentry(tt,textvariable=rc)
za.entry<-tkentry(tt,textvariable=za)
zb.entry<-tkentry(tt,textvariable=zb)
wc.entry<-tkentry(tt,textvariable=wc)

   reset <- function() #function which clears the inputs
    {
     tclvalue(x)<-""
     tclvalue(y)<-""
     tclvalue(z)<-""
	tclvalue(w)<-""
tclvalue(wa.entry)<-""
tclvalue(wb.entry)<-""
tclvalue(zc.entry)<-""
tclvalue(qc.entry)<-""
tclvalue(rc.entry)<-""
tclvalue(za.entry)<-""
tclvalue(zb.entry)<-""
tclvalue(wc.entry)<-""

       }

   reset.but <- tkbutton(tt, text="Reset", command=reset) #creates reset button

   submit <- function() { #function which initializes the input values to variables to be used to run the my.bootstrap3 function
     x <- tclvalue(x)
     y <- tclvalue(y)
     z <- tclvalue(z)
       w<-tclvalue(w)
	wa<-tclvalue(wa)
	wb<-tclvalue(wb)
	zc<-tclvalue(zc)  
	qc<-tclvalue(qc)
	rc<-tclvalue(rc)
	za<-tclvalue(za)
zb<-tclvalue(zb)
wc<-tclvalue(wc)

     e <- parent.env(environment()) #assigning the parent environment in which the function will run 
     #initializing variables in the parent environment
     e$x <- x
     e$y <- y
     e$z <- z
e$w<-w
e$wa<-wa
e$wb<-wb
e$zc<-zc
e$qc<-qc
e$rc<-rc
e$za<-za
e$zb<-zb
e$wc<-wc

        tkdestroy(tt)#closes the dialog box
   }
   #formatting text, buttons and input entry fields withing the ui dialog box
   submit.but <- tkbutton(tt, text="start", command=submit)
   tkgrid(tklabel(tt,text="Input data matrix"),columnspan=2)
   tkgrid(tklabel(tt,text="data"), x.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input X column"),columnspan=2)
   tkgrid(tklabel(tt,text="i"), y.entry, pady=10, padx =30)


   tkgrid(tklabel(tt,text="Input Y column"),columnspan=2)
   tkgrid(tklabel(tt,text="j"), z.entry, pady=10, padx =30)

  tkgrid(tklabel(tt,text="Input xaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Xaxis"), w.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input yaxis label"),columnspan=2)
   tkgrid(tklabel(tt,text="Yaxis"), wa.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input plot title"),columnspan=2)
   tkgrid(tklabel(tt,text="Plot title"), wb.entry, pady=10, padx =30)

 tkgrid(tklabel(tt,text="Input color code"),columnspan=2)
   tkgrid(tklabel(tt,text="Color code"),zc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Input sqrt indicator"),columnspan=2)
   tkgrid(tklabel(tt,text="Sqrt=T"),qc.entry, pady=10, padx =30)
   
tkgrid(tklabel(tt,text="Input nboot"),columnspan=2)
   tkgrid(tklabel(tt,text="nboot"),rc.entry, pady=10, padx =30)

tkgrid(tklabel(tt,text="Prediction Bound?"),columnspan=2)
   tkgrid(tklabel(tt,text="Pbound=T"),za.entry, pady=10, padx =30)
tkgrid(tklabel(tt,text="Conf.level"),columnspan=2)
   tkgrid(tklabel(tt,text="Confidence"),zb.entry, pady=10, padx =30)
tkgrid(tklabel(tt,text="Pivotal=T"),columnspan=2)
   tkgrid(tklabel(tt,text="Pivotal"),wc.entry, pady=10, padx =30)


   tkgrid(submit.but, reset.but)

  tkwait.window(tt)#keeps dialog box window open until the submit button is clicked
  return(c(x,y,z,w,wa,wb,zc,qc,rc,za,zb,wc))#returns input vector 
}
#Now run the function like:
predictor_para <- inputs() #initializes inputs as predictor parameters
print(predictor_para) #prints the parameters
# following evaluates the string and returns an integer which is then initialized
mat<-eval(parse(text=predictor_para[1]))
ind1<-eval(parse(text=predictor_para[2]))
ind2<-eval(parse(text=predictor_para[3]))
xlab<-eval(parse(text=predictor_para[4]))
ylab<-eval(parse(text=predictor_para[5]))
maintitle<-eval(parse(text=predictor_para[6]))
zcol<-eval(parse(text=predictor_para[7]))
zsqrt<-eval(parse(text=predictor_para[8]))
znboot<-eval(parse(text=predictor_para[9]))
zpred<-eval(parse(text=predictor_para[10]))
zconf<-eval(parse(text=predictor_para[11]))
zpivot<-eval(parse(text=predictor_para[12]))

my.bootstrap3(mat,ind1,ind2,xlab,ylab, maintitle,zcol,zsqrt,znboot,zpred,zconf,zpivot)#runs my.bootstrap2 function with the predictor parameters

}
my.dat.plot5 <-
  function(mat=NOAA,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T){
if(in.boot){
do.sqrt<-F # if in.boot is true then do.sqrt is false
}
if(!do.sqrt){ #if do.sqrt is false the following runs

smstr<-smooth.spline(mat[,i],mat[,j]) # initializes the model calculated by function smooth.spline between disasters and delta temp 
smstr.lin<-smooth.spline(mat[,i],mat[,j],df=2) # initializes the linear fit calculated by function smooth.spline between disasters and delta temp with degrees of freedom equal to 2 
if(do.plot){ #following lines run if do.plot is true
plot(mat[,i],mat[,j],xlab=zxlab,ylab=zylab,main=zmain) #plot is created with parameters requested
lines(smstr,col=zcol) #plots the line representing the smoothing spline model  
lines(smstr.lin,col=(zcol+1)) #plots the line representing the linear fit 

}
resid1<-mat[,j]-predict(smstr,mat[,i])$y # calculates residuals by subtracting the predicted y value (based on the smooth spline model) from the y value from the disasters column 
resid2<-mat[,j]-predict(smstr.lin,mat[,i])$y # calculates residuals by subtracting the predicted y value (based on the linear fit ) from the y value from the disasters column 
}else{ # if do.sqrt is true the following runs (essentially the sqrt is taken of the disaster column )
smstr<-smooth.spline(mat[,i],sqrt(mat[,j]))# initializes the model calculated by function smooth.spline between sqrt of disasters and delta temp 
smstr.lin<-smooth.spline(mat[,i],sqrt(mat[,j]),df=2)# initializes the linear fit calculated by function smooth.spline between sqrt of disasters and delta temp with degrees of freedom equal to 2 
if(do.plot){  #following lines run if do.plot is true
plot(mat[,i],sqrt(mat[,j]),xlab=zxlab,ylab=zylab,main=zmain)  #plot is created with parameters requested
lines(smstr,col=zcol) #plots the line representing the smoothing spline model 
lines(smstr.lin,col=(zcol+1))  #plots the line representing the linear fit 
}
resid1<-sqrt(mat[,j])-predict(smstr,mat[,i])$y # calculates residuals by subtracting the predicted y value (based on the smooth spline model) from the sqrt of the y value from the disasters column 
resid2<-sqrt(mat[,j])-predict(smstr.lin,mat[,i])$y # calculates residuals by subtracting the predicted y value (based on the linear fit ) from the sqrt of the y value from the disasters column 
}

dfmod<-smstr$df #calculates the degrees of freedom of the smoothing spline model 
dflin<-2 # initializes the degrees of freedom of the linear fit at 2 
ssmod<-sum(resid1^2) # calculates the sum of the smoothing spline model residuals squared (RSS of smooth model)
sslin<-sum(resid2^2) # calculates the sum of the linear fit residuals squared (RSS of linear fit)
numss<-sslin-ssmod # stores the RSS of linear fit minus the RSS of smoothing spline model 
n1<-length(mat[,j]) # calculates the number of rows with values 
Fstat<-(numss/(dfmod-dflin))/(ssmod/(n1-dfmod)) #calculates the Fstatistic using formula
pvalue<-1-pf(Fstat,dfmod-dflin,n1-dfmod) #calculates the pvalue using the pf function and subracting the result from 1
stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1) # initializes a list of all output objects
}

#additional parameter (mat0, orginal matrix) used in my.dat.plot5a while mat paramater requests the bootstrap matrix
my.dat.plot5a <-
  function(mat=NOAA,mat0,i=3,j=2,zxlab,zylab,zmain,zcol,do.sqrt=F,do.plot=T,in.boot=T){
if(in.boot){
do.sqrt<-F # if in.boot is true then do.sqrt is false
}
if(!do.sqrt){#if do.sqrt is false the following runs

smstr<-smooth.spline(mat[,i],mat[,j])# initializes the model calculated by function smooth.spline between disasters and delta temp 
pstr<-predict(smstr,mat0[,i]) #predicts the smoothing spline model based of the original matrix (NOAA)
# assigns the predicted smoothing spline model coordinates to the smstr model 
smstr$x<-pstr$x 
smstr$y<-pstr$y
smstr.lin<-smooth.spline(mat[,i],mat[,j],df=2)# initializes the linear fit calculated by function smooth.spline between disasters and delta temp with degrees of freedom equal to 2 
if(do.plot){#following lines run if do.plot is true
plot(mat[,i],mat[,j],xlab=zxlab,ylab=zylab,main=zmain)#plot is created with parameters requested
lines(smstr,col=zcol) #plots the line representing the smoothing spline model  
lines(smstr.lin,col=(zcol+1))#plots the line representing the linear fit 

}
resid1<-mat[,j]-predict(smstr,mat[,i])$y # calculates residuals by subtracting the predicted y value (based on the smooth spline model) from the y value from the disasters column 
resid2<-mat[,j]-predict(smstr.lin,mat[,i])$y # calculates residuals by subtracting the predicted y value (based on the linear fit ) from the y value from the disasters column 
}else{ # if do.sqrt is true the following runs (essentially the sqrt is taken of the disaster column )
smstr<-smooth.spline(mat[,i],sqrt(mat[,j])) # initializes the model calculated by function smooth.spline between sqrt of disasters and delta temp 
pstr<-predict(smstr,mat0[,i]) #predicts the smoothing spline model based of the original matrix (NOAA )
# assigns the predicted smoothing spline model coordinates to the smstr model 
smstr$x<-pstr$x
smstr$y<-pstr$y

smstr.lin<-smooth.spline(mat[,i],sqrt(mat[,j]),df=2)# initializes the linear fit calculated by function smooth.spline between sqrt of disasters and delta temp with degrees of freedom equal to 2 
if(do.plot){  #following lines run if do.plot is true
plot(mat[,i],sqrt(mat[,j]),xlab=zxlab,ylab=zylab,main=zmain) #plot is created with parameters requested
lines(smstr,col=zcol) #plots the line representing the smoothing spline model 
lines(smstr.lin,col=(zcol+1))#plots the line representing the linear fit 
}
resid1<-sqrt(mat[,j])-predict(smstr,mat[,i])$y # calculates residuals by subtracting the predicted y value (based on the smooth spline model) from the sqrt of the y value from the disasters column 
resid2<-sqrt(mat[,j])-predict(smstr.lin,mat[,i])$y # calculates residuals by subtracting the predicted y value (based on the linear fit ) from the sqrt of the y value from the disasters column 
}

dfmod<-smstr$df #calculates the degrees of freedom of the smoothing spline model
dflin<-2 # initializes the degrees of freedom of the linear fit at 2 
ssmod<-sum(resid1^2) # calculates the sum of the smoothing spline model residuals squared (RSS of smooth model)
sslin<-sum(resid2^2) # calculates the sum of the linear fit residuals squared (RSS of linear fit)
numss<-sslin-ssmod # stores the RSS of linear fit minus the RSS of smoothing spline model 
n1<-length(mat[,j]) # calculates the number of rows with values 
Fstat<-(numss/(dfmod-dflin))/(ssmod/(n1-dfmod)) #calculates the Fstatistic using formula
pvalue<-pf(Fstat,dfmod-dflin,n1-dfmod) #calculates the pvalue using the pf function and subracting the result from 1
stat.out<-list(smstrmod=smstr,smstrlin=smstr.lin,resid.mod=resid1,resid.lin=resid2,dfmod=dfmod,dflin=dflin,F=Fstat,P=pvalue,n=n1) # initializes a list of all output objects
}
