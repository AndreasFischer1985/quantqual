## ---- echo=TRUE,message=FALSE,eval=FALSE,results='hide',fig.keep='all'----
#  devtools::install_github("AndreasFischer1985/quantqual")
#  library(quantqual)

## ----web-scraping table, echo=TRUE,message=FALSE,results='hide',fig.keep='all'----
#Get pdf document from an url and save it to the working directory:
url=paste0("http://www.bamf.de/SharedDocs/Anlagen/DE/Publikationen/Flyer/",
"flyer-schluesselzahlen-asyl-halbjahr-2018.pdf?__blob=publicationFile");
fn=paste0(gsub("[/?.:]","",url),".pdf");
doc=getFile(url,fn);

#Extract table:
tab=extractTable(strsplit(doc,"\r\n")[[1]],
reg.up="Staatsangeh.rigkeit",reg.down="Entscheidungen",reg.left="Staatsangeh.rigkeit",
reg.right="$",reg.fix="(^[ ]*[0-9]{1,2}[ ]+|[ ]+[0-9][ ]*$)",correctNotation=T,convert=T)

## ----web-scraping tables, echo=TRUE,message=FALSE,results='hide',fig.keep='all'----
#Get pdf document from an url and save it to the working directory:
url=paste0("http://www.bamf.de/SharedDocs/Anlagen/EN/Publikationen/EMN/Studien/",
"wp73-emn-familiennachzug-drittstaatsangehoerige-deutschland.pdf?__blob=publicationFile")
doc=getFile(url,paste0(gsub("[/?.:]","",url),".pdf"))
doc=strsplit(doc,"\r\n")[[1]]

#Extract tables (skipping dots):
tab2015=extractTable(
doc[(last(which(grepl("Table 4",doc)),1)+4):(last(which(grepl("Table 5",doc)),1)-3)],reg.fix="[.]")

tab2014=extractTable(
doc[(last(which(grepl("Table 5",doc)),1)+5):(last(which(grepl("Table 6",doc)),1)-3)],reg.fix="[.]")

tab2013=extractTable(
doc[(last(which(grepl("Table 6",doc)),1)+4):(last(which(grepl("Table 7",doc)),1)-3)],reg.fix="[.]")

tab2012=extractTable(
doc[(last(which(grepl("Table 7",doc)),1)+5):(last(which(grepl("Table 8",doc)),1)-3)],reg.fix="[.]")

tab2011=extractTable(
doc[(last(which(grepl("Table 8",doc)),1)+4):(last(which(grepl("Table 9",doc)),1)-3)],reg.fix="[.]")

tab2010=extractTable(
doc[(last(which(grepl("Table 9",doc)),1)+5):(last(which(grepl("Table 10",doc)),1)-3)],reg.fix="[.]")


## ----wrangling 1---------------------------------------------------------
dat=tab[c(1:16,19),-c(1,4,6)]
rownames(dat)=c("Afghanistan","Albania","Eritrea","Georgia",
"Iraq","Iran","Kosovo","Macedonia","Nigeria","Pakistan",
"Russ.Fed.","Serbia","Somalia","Syria","Turkey","Unknown","Total")
colnames(dat)=c("2015","2016","2017","1st half of 2018")
dat[is.na(dat)]=0
dat=dat[order(rowSums(dat,na.rm=T),decreasing=T),]
dat

## ----wrangling 2---------------------------------------------------------
#Select first and last column of each table:
t15=tab2015[,c(1,dim(tab2015)[2])]
t14=tab2014[,c(1,dim(tab2014)[2])]
t13=tab2013[,c(1,dim(tab2013)[2])]
t12=tab2012[,c(1,dim(tab2012)[2])]
t11=tab2011[,c(1,dim(tab2011)[2])]
t10=tab2010[,c(1,dim(tab2010)[2])]

#Combine the last columns of all tables to a matrix
dat2=cbind(
"2015"=as.numeric(t15[,2]),
"2014"=as.numeric(t14[match(t15[,1],t14[,1]),2]),
"2013"=as.numeric(t13[match(t15[,1],t13[,1]),2]),
"2012"=as.numeric(t12[match(t15[,1],t12[,1]),2]),
"2011"=as.numeric(t11[match(t15[,1],t11[,1]),2]),
"2010"=as.numeric(t10[match(t15[,1],t10[,1]),2]))
rownames(dat2)=t15[,1]

dat2 #looks like lines 13:15 need a little face-lifting...
dat2=dat2[-c(13,15),];rownames(dat2)[13]="Serbia"
dat2[is.na(dat2)]=0
dat2=dat2[order(rowSums(dat2,na.rm=T),decreasing=T),]
dat2

## ----plot bp, fig.height=7, fig.width=7----------------------------------
options(scipen=5)
b=bp(dat[-1,],beside=F,
main=paste0("First Time Applications from Top-10 Countries of Origin\n",
"between 2015 and the First Half of 2018"),ylim=c(0,700000))
text(b,colSums(dat[-1,]),colSums(dat[-1,]),pos=3,xpd=T)


## ----plotMAT, fig.height=7, fig.width=7----------------------------------
plotMAT(dat[1:4,],"Accumulation of First Time Applications since 2015",show.legend=F)

## ----plot plotMAT2, fig.height=7, fig.width=7----------------------------
plotMAT(dat2[2:21,6:1],paste0("Accumulation of Immigration for Family",
"Reunification Purposes\nfrom Top-20 Countries of Origin"))

## ----plot plotDF, fig.height=7, fig.width=7------------------------------
plotDF(dat[-1,])
plotDF(dat2[2:21,6:1])

## ----plot packedBubbleChart, fig.height=7, fig.width=7-------------------
packedBubbleChart(dat[dat[,"2017"]>0,"2017"],break.names=T,
main="\n\nFirst Time Applications for Asylum\nfrom Top-10 Countries of Origin in 2017",cex=.7)

## ----plot spiderplot, fig.height=7, fig.width=7--------------------------
spiderplot(dat[dat[,"2017"]>0,"2017"][-1],max=50000,main=
"\n\nFirst Time Applications from Top-10 Countries of Origin\nin 2017")

## ----plot correlationplot, fig.height=7, fig.width=7---------------------
dat[dat==0]=NA;
correlationplot(dat[-1,])

## ----plot plotXY, fig.height=7, fig.width=7------------------------------
plotXY(dat[-1,3],dat[-1,4],xlab="2017",ylab="1st half of 2018")

## ----plot outlier.analysis, fig.height=7, fig.width=7--------------------
outlier.analysis(dat[-1,])

## ----wordcloud, fig.height=7, fig.width=7, cache=TRUE---------------------------------------------------------------------------------------------------------------
tdm=vecToTDM(doc,plot=F)
fre=sort(rowSums(tdm))
fre=fre[is.na(match(names(fre),c(0:100,quanteda::stopwords("English"))))]
set.seed(0);wordcloud::wordcloud(names(fre),fre,min.freq=30,random.order=F)

