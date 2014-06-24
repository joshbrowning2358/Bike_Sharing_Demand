makeOutput = function(preds, call)
{
  #Data quality checks
  if(!is(call,"character"))
    stop("!is(call,'character')")
  if( is.matrix(preds) | is.data.frame(preds) )
    stop("is.matrix(preds) | is.data.frame(preds).  Ensure preds is a numeric vector!")
  if(!"Submissions" %in% list.files() )
    stop("No submissions directory!")
  if(nrow(d)!=17379)
    stop("d should have 17379 rows!")
  if( length(preds)!=17379 & length(preds)!=6493 )
    stop("preds should have 17379 or 6493 values!")

  files = list.files("Submissions")
  files = files[grepl("_raw", files)]
  ids = as.numeric( gsub("_raw.csv", "", files) )
  ids = ids[!is.na(ids)]
  newId = max(ids,0)+1
  
  write.csv(preds, paste0("Submissions/",newId,"_raw.csv"), row.names=F)
  if(length(preds)==17379){
    #Do Cross-validation since you have all predictions
    score = mean( (log(preds+1)-log(d$count+1))^2, na.rm=T )
    write.csv( data.frame(datetime=d$datetime[is.na(d$count)], count=preds[is.na(d$count)] )
          ,file=paste0(newId,"_",score,".csv"), row.names=F)
  } else {
    write.csv( data.frame(datetime=d$datetime[is.na(d$count)], count=preds )
          ,file=paste0(newId,"_NA.csv"), row.names=F)    
  }
  if("desc.csv" %in% list.files("Submissions") ){
    desc = read.csv("Submissions/desc.csv", stringsAsFactors=F)
    desc = rbind( desc, data.frame(newId, call) )
  } else {
    desc = data.frame(newId, call)
  }
  write.csv(desc, file="Submissions/desc.csv")
}
