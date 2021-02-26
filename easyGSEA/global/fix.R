# bug in ID conversion in pathview
mol.sum <-
  function(mol.data, id.map, gene.annotpkg="org.Hs.eg.db", sum.method=c("sum","mean", "median", "max", "max.abs", "random")[1]){
    if(is.character(mol.data)){
      gd.names=mol.data
      mol.data=rep(1, length(mol.data))
      names(mol.data)=gd.names
      ng=length(mol.data)
    } else if(!is.null(mol.data)){
      if(length(dim(mol.data))==2){
        gd.names=rownames(mol.data)
        ng=nrow(mol.data)
      } else if(is.numeric(mol.data) & is.null(dim(mol.data))){
        gd.names=names(mol.data)
        ng=length(mol.data)
      } else stop("wrong mol.data format!")
    } else stop("NULL mol.data!")
    
    if(is.character(id.map) & length(id.map)==1){
      id.map=id2eg(gd.names, category=id.map, pkg.name=gene.annotpkg)
    }
    
    sel.idx=id.map[,2]>"" & !is.na(id.map[,2])
    id.map=id.map[sel.idx,,drop=F]
    eff.idx=gd.names %in% id.map[,1]
    mapped.ids=id.map[match(gd.names[eff.idx], id.map[,1]),2]
    if(sum.method %in% c("sum","mean")){
      sum.method=eval(as.name(sum.method))
      mapped.data=apply(cbind(cbind(mol.data)[eff.idx,]),2,function(x){
        sum.res=tapply(x, mapped.ids, sum.method, na.rm=T)
        return(sum.res)
      })
    } else{
      sum.method=eval(as.name(sum.method))
      mol.data=cbind(cbind(mol.data)[eff.idx,])
      if(all(mol.data>=0) | all(mol.data<=0)){
        vars=apply(cbind(mol.data), 1, IQR)
      } else vars=apply(cbind(mol.data), 1, sum, na.rm=T)
      
      sel.rn=tapply(1:sum(eff.idx), mapped.ids, function(x){
        if(length(x)==1) return(x)
        else return(x[which.min(abs(vars[x]-sum.method(vars[x], na.rm=T)))])
      })
      mapped.data=cbind(mol.data[sel.rn,])
      rownames(mapped.data)=names(sel.rn)
    }
    return(mapped.data)
  }

rlang::env_unlock(env = asNamespace('pathview'))
rlang::env_binding_unlock(env = asNamespace('pathview'))
assign('mol.sum', mol.sum, envir = asNamespace('pathview'))
rlang::env_binding_lock(env = asNamespace('pathview'))
rlang::env_lock(asNamespace('pathview'))