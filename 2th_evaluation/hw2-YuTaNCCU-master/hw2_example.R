query_func<-function(query_m, i)
{
  if(query_m == "male"){
    which.min(i)
  }
  else if (query_m == "female") {
    which.max(i)
  } else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_yourID.R --target male|female --inputs file1 file2 ... filen --output out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--inputs"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

# read files
names<-c()
weis<-c()
heis<-c()
for(file in files)
{
  name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  weis<-c(weis, d$weight[query_func(query_m, d$weight)])
  heis<-c(heis, d$height[query_func(query_m, d$height)])
  names<-c(names,name)
}
out_data<-data.frame(set=names, wei=weis, hei=heis, stringsAsFactors = F)
index<-sapply(out_data[,c("wei","hei")], query_func, query_m=query_m)

# output file
out_data<-rbind(out_data,c(query_m,names[index]))
write.table(out_data, file=out_f, row.names = F, quote = F)
