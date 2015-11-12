#' @import data.table stringi

#' @export
.datatable.aware <- TRUE

# Borrowed from data.table
.onLoad <- function(libname, pkgname) {

  # Prefix
  prefix = if (!missing(pkgname)) "data.table::" else ""

  # Cbind
  tt <- base::cbind.data.frame
  ss <- body(tt)
  if (class(ss) != "{") ss = as.call(c(as.name("{"), ss))
  if (!length(grep("data.table", ss[[2]]))) {
    ss <- ss[c(1, NA, 2:length(ss))]
    ss[[2]] <- parse(text = paste0("if (!identical(class(..1),'data.frame')) for (x in list(...)) { if (inherits(x,'data.table')) return(",prefix,"data.table(...)) }"))[[1]]

    # Assign the new cbind
    body(tt) <- ss
    (unlockBinding)("cbind.data.frame", baseenv())
    assign("cbind.data.frame", tt, envir=asNamespace("base"), inherits=FALSE)
    lockBinding("cbind.data.frame", baseenv())
  }

  # Rbind
  tt = base::rbind.data.frame
  ss = body(tt)
  if (class(ss) != "{") ss = as.call(c(as.name("{"), ss))
  if (!length(grep("data.table", ss[[2]]))) {
    ss = ss[c(1, NA, 2:length(ss))]
    ss[[2]] = parse(text=paste0("for (x in list(...)) { if (inherits(x,'data.table')) return(",prefix,".rbind.data.table(...)) }"))[[1]]

    # Assign the new rbind
    body(tt) = ss
    (unlockBinding)("rbind.data.frame",baseenv())
    assign("rbind.data.frame",tt,envir=asNamespace("base"),inherits=FALSE)
    lockBinding("rbind.data.frame",baseenv())
  }

}