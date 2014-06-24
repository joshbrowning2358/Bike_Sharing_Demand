makeOutput = function(preds, call)
{
  #Data quality checks
  if(!is(call,"character"))
    stop("!is(call,'character')")
  if( is.matrix(preds) | is.data.frame(preds) )
    stop("is.matrix(preds) | is.data.frame(preds).  Ensure preds is a numeric vector!")
  if(!"Submissions" %in% list.files() )
    stop("No submissions directory!")
  if(nrow(d)!=17379 & nrow(d)!=17379*2)
    stop("d should have 17379 rows!")
  if( length(preds)!=17379 & length(preds)!=6493 & length(preds)!=17379*2 )
    stop("preds should have 17379, 6493, or 17379*2 values!")

  if(nrow(d)==17379){
    actual = d$count
    datetime = d$datetime
  }
  if(nrow(d)==17379*2){
    actual = d$count[d$countType=="registered"]
    datetime = d$datetime[d$countType=="registered"]
    preds = preds[1:17379] + preds[17380:(2*17379)]
  }

  files = list.files("Submissions")
  files = files[grepl("_raw", files)]
  ids = as.numeric( gsub("_raw.csv", "", files) )
  ids = ids[!is.na(ids)]
  newId = max(ids,0)+1
  
  write.csv(preds, paste0("Submissions/",newId,"_raw.csv"), row.names=F)
  if(length(preds)==6493){
    write.csv( data.frame(datetime=datetime[is.na(d$count)], count=preds )
          ,file=paste0("Submissions/",newId,"_NA.csv"), row.names=F)    
  } else {
    #Do Cross-validation since you have all predictions
    score = sqrt( mean( (log(preds+1)-log(actual+1))^2, na.rm=T ) )
    write.csv( data.frame(datetime=datetime[is.na(actual)], count=preds[is.na(actual)] )
          ,file=paste0("Submissions/",newId,"_",score,".csv"), row.names=F)
  }
  if("desc.csv" %in% list.files("Submissions") ){
    desc = read.csv("Submissions/desc.csv", stringsAsFactors=F)
    desc = rbind( desc, data.frame(newId, call) )
  } else {
    desc = data.frame(newId, call)
  }
  write.csv(desc, file="Submissions/desc.csv", row.names=F)
}
