#~~~~~~~~~~~~~~~~~
## 1. Imports ####
#~~~~~~~~~~~~~~~~~

if (!require(R6        )){install.packages("R6")}
if (!require(zip       )){install.packages("zip")}
if (!require(xml2      )){install.packages("xml2")}
if (!require(data.table)){install.packages("data.table")}

library(R6)
library(zip)
library(xml2)
library(data.table)


#~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2. Defining Classes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~
### 2.1. ODSstyle ####
#~~~~~~~~~~~~~~~~~~~~~

setClass(
  "ODSstyle",
  slots = c(
    font = "character",
    size = "integer",
    color= "character",
    bold = "logical",
    italic="logical",
    underline="logical",
    cellcolor="character",
    vAlign="character",
    hAlign="character", 
    margin="numeric",
    wrap="logical", 
    rotate="integer",
    minDigits="integer",
    maxDigits="integer",
    .color="character",
    .cellcolor="character",
    .key="character"
  ),
  
  prototype = list(
    font = NA_character_,
    size = NA_integer_,
    color= NA_character_,
    bold = NA,
    italic=NA,
    underline=NA,
    cellcolor=NA_character_,
    vAlign=NA_character_,
    hAlign=NA_character_, 
    margin=NA_real_,
    wrap=NA, 
    rotate=NA_integer_,
    minDigits=NA_integer_,
    maxDigits=NA_integer_,
    .color=NA_character_,
    .cellcolor=NA_character_,
    .key=NA_character_
  ),
  
  validity = function(object) {
    slots_to_check <- slotNames(object)
    for (s in slots_to_check) {
      if (length(slot(object, s))!=1){
        return(paste0("slot '", s, "' must be a single value"))
      }
    }
    
    ## alternative way:
    ## if (length(object@font) != 1){return("slot 'font' must be a single string")}
    
    return(TRUE)
  }
)

setMethod(
  "show",
  "ODSstyle",
  function(object){
    cat("ODSstyle\n")
    slots_to_check <- slotNames(object)
    for (s in slots_to_check){
      if (substr(s,1,1)=="."){next}
      value=slot(object, s)
      if (!is.na(value)){cat("  ",s,":", value, "\n")}
    }
  }
)


ODS_createStyle <- function(font=NULL, size=NULL, color=NULL, bold=NULL, italic=NULL, 
                            underline=NULL, cellcolor=NULL, vAlign=NULL, hAlign=NULL, 
                            margin=NULL, wrap=NULL, rotate=NULL, minDigits=NULL, 
                            maxDigits=NULL){
  
  g <- function(a,type){
    if (is.na(a)||is.null(a)){
      if (type=="character"){return(NA_character_)}
      if (type=="integer"){return(NA_integer_)}
      if (type=="numeric"){return(NA_real_)}
      if (type=="logical"){return(NA)}
    }
    if (type=="integer"){return(as.integer(a))}
    return(a)
  }
  
  font       =g(font,"character")
  size       =g(size,"integer")
  color      =g(color,"character")
  bold       =g(bold,"logical")
  italic     =g(italic,"logical")
  underline  =g(underline,"logical")
  cellcolor  =g(cellcolor,"character")
  vAlign     =g(vAlign,"character")
  hAlign     =g(hAlign,"character")
  margin     =g(margin,"numeric")
  wrap       =g(wrap,"logical")
  rotate     =g(rotate,"integer")
  minDigits  =g(minDigits,"integer")
  maxDigits  =g(maxDigits,"integer")
  
  ## computing internal variables
  
  arg_names <- names(formals(sys.function())) ## the argument names of the function
  vals <- mget(arg_names, inherits = FALSE)   ## the values now inside this function
  .key=paste(vals, collapse = "|")
  
  if (is.na(color)){
    .color=NA_character_
  } else {
    rgb <- col2rgb(color)
    .color=sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
  }
  
  if (is.na(cellcolor)){
    .cellcolor=NA_character_
  } else {
    rgb <- col2rgb(cellcolor)
    .cellcolor=sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
  }
  
  
  
  new("ODSstyle",
      font=font,
      size=size,
      color=color,
      bold=bold,
      italic=italic,
      underline=underline,
      cellcolor=cellcolor,
      vAlign=vAlign,
      hAlign=hAlign, 
      margin=margin,
      wrap=wrap, 
      rotate=rotate,
      minDigits=minDigits,
      maxDigits=maxDigits,
      .color=.color,
      .cellcolor=.cellcolor,
      .key=.key)
}


#~~~~~~~~~~~~~~~~~~~~~
### 2.2. ODSsheet ####
#~~~~~~~~~~~~~~~~~~~~~

SHEET <- R6Class("ODSsheet",
                 public = list(
                   sheetName=NA,
                   cellsContent = data.table(
                     row = integer(),
                     column = integer(),
                     type = character(),
                     data_string = character(),
                     data_float = numeric(),
                     styleNumber = integer()
                   ),
                   mergedCells = matrix(
                     nrow = 0, ncol = 4,
                     dimnames = list(NULL, c("fromRow", "toRow", "fromColumn", "toColumn"))
                   ),
                   colWidths=c(),
                   rowHeights=c(),
                   styles = list(),
                   
                   
                   
                   cleanup = function(){
                     ## fixes cells, that were accessed repeatedly (hence overwritten)
                     ## we could also fix styles that aren't used, but this seems tricky
                     if (nrow(self$cellsContent)<=1){return()}
                     cell_id <- self$cellsContent[, paste(row, column)]
                     keep <- !duplicated(cell_id, fromLast = TRUE)
                     self$cellsContent <- self$cellsContent[keep,]
                     #invisible(self)
                   },
                   
                   
                   print = function(...) {
                     self$cleanup()
                     bold <- function(x) paste0("\033[1m", x, "\033[0m")
                     cat(bold("class:"),"       ODSsheet\n")
                     cat(bold("sheetName:   "),self$sheetName,"\n")
                     
                     cat(bold("cellsContent:\n"))
                     if (nrow(self$cellsContent)!=0){
                       print(self$cellsContent)
                       cat("\n")
                     }
                     
                     cat(bold("mergedCells:\n"))
                     if (nrow(self$mergedCells)!=0){
                       print(self$mergedCells)
                     }
                     
                     cat(bold("colWidths:   "),self$colWidths,"\n")
                     cat(bold("rowHeights:  "),self$rowHeights,"\n")
                     cat(bold("styles:"),"\n")
                     print(self$styles)
                   },
                   
                   
                   nrow = function(...){
                     return(max(0,sheet$cellsContent$row))
                   },
                   
                   ncol = function(...){
                     return(max(0,sheet$cellsContent$column))
                   },
                   
                   
                   View = function(...){
                     self$cleanup()
                     ## this needs to be adapted to the number-type
                     sheet = matrix(NA_character_,nrow=self$nrow(),ncol=self$ncol())
                     text=self$cellsContent$data_string
                     text[is.na(text)]=self$cellsContent$data_float[is.na(text)]
                     sheet[cbind(self$cellsContent$row,self$cellsContent$column)]=text
                     sheet=data.table(sheet)
                     NAMES=LETTERS
                     while(self$ncol()>length(NAMES)){
                       NAMES=as.vector(outer(c("",LETTERS),NAMES,paste0))
                       NAMES=unique(NAMES)
                       NAMES=NAMES[order(nchar(NAMES),NAMES)]
                     }
                     colnames(sheet)=NAMES[1:ncol(sheet)]
                     View(sheet)
                     return(invisible(sheet))
                   }
                 )
)


ODS_createSheet = function(sheetName=NULL){
  if (is.null(sheetName)){sheetName=NA}
  SHEET = SHEET$new()
  SHEET$sheetName=sheetName
  return(SHEET)
}


dim.ODSsheet <- function(sheet){c(sheet$nrow(),sheet$ncol())}

preview <- function(sheet){return(invisible(sheet$View()))}


#~~~~~~~~~~~~~~~~~~~
## 3. Functions ####
#~~~~~~~~~~~~~~~~~~~

ODS_writeCell <- function(sheet, data, row, col,  style){
  if (missing(style)){style=ODS_createStyle()}
  if (class(style)!="ODSstyle"){stop("'style' must be a style!")}
  
  ## Q1: does this style already exist?
  keys <- vapply(sheet$styles, function(st) slot(st, ".key"), character(1))
  
  if (any(keys==style@.key)){ ## style already exists:
    styleNumber=which(keys==style@.key)[1]
  } else {
    sheet$styles <- append(sheet$styles, list(style))
    styleNumber=length(sheet$styles)
  }
  
  if (class(data)=="numeric"){
    sheet$cellsContent <- rbind(sheet$cellsContent, list(row,col,"float",NA,data,styleNumber))
  } else {
    sheet$cellsContent <- rbind(sheet$cellsContent, list(row,col,"string",as.character(data),NA,styleNumber))
  }
  
  invisible(sheet)
}




ODS_mergeCells <- function(sheet, rows, cols, mergeCols=TRUE, mergeRows=TRUE){
  if (min(cols)==max(cols) & min(rows)==max(rows)){stop("You cannot merge a single cell; you are stupid...")}
  if (!mergeCols & min(rows)==max(rows)){stop("You are about to merge ... nothing")}
  if (!mergeRows & min(cols)==max(cols)){stop("You are about to merge ... nothing")}
  
  overlapps <- function(mergedCells,news){
    for (i in seq_len(nrow(mergedCells))){
      for (j in seq_len(nrow(news))){
        new=news[j,]
        merged=mergedCells[i,]
        ## we calculate a possible intersection point. If it does not intersect, we are safe!
        
        INTERSECTROW=max(new["fromRow"],merged["fromRow"])
        INTERSECTCOL=max(new["fromColumn"],merged["fromColumn"])
        if (INTERSECTROW<=min(new["toRow"],merged["toRow"]) & 
            INTERSECTCOL<=min(new["toColumn"],merged["toColumn"])){return(c(INTERSECTROW,INTERSECTCOL))} 
      }
    }
    return(FALSE)
  }
  
  if (!mergeCols & !mergeRows){
    stop("You are about to merge ... nothing")
  }
  if (mergeCols & mergeCols){
    news=cbind(fromRow=min(rows),toRow=max(rows),fromColumn=min(cols),toColumn=max(cols))
  }
  if (mergeCols & !mergeRows){
    news=cbind(fromRow=rows,toRow=rows,fromColumn=min(cols),toColumn=max(cols))
  }
  if (!mergeCols & mergeRows){
    news=cbind(fromRow=min(rows),toRow=max(rows):max(rows),fromColumn=cols,toColumn=cols)
  }
  
  result=overlapps(sheet$mergedCells,news)
  if (length(result)!=1){
    stop(paste0("Mergeconflict: Cell at row=",result[1]," and column=",result[2], " is already merged"))
  }
  
  sheet$mergedCells=rbind(sheet$mergedCells,news)
  invisible(sheet)
}


ODS_setColWidths <- function(sheet,cols,width="1.7cm"){
  if (is.numeric(width)){heigt=paste0(width,"cm")}
  m=max(cols)
  c=length(sheet$colWidths)
  if (c<m){sheet$colWidths=c(sheet$colWidths,rep(NA,m-c))}
  sheet$colWidths[cols]=width
}



ODS_setRowHeights <- function(sheet,rows,height="15pt"){
  if (is.numeric(height)){height=paste0(height,"pt")}
  m=max(rows)
  c=length(sheet$rowHeights)
  if (c<m){sheet$rowHeights=c(sheet$rowHeights,rep(NA,m-c))}
  sheet$rowHeights[rows]=height
}


#~~~~~~~~~~~~~~~~~
## 4. Writing ####
#~~~~~~~~~~~~~~~~~


ODS_write <- function(sheet, file="file.ods"){
  if (!"ODSsheet" %in% class(sheet)){stop("'sheet' must be an ODSsheet")}
  sheet$cleanup()
  DEFAULTS=c(colWidth="1.7cm",
             rowHeight="15pt",
             font="Calibri",
             size=11,
             color="black",
             bold=FALSE,
             italic=FALSE,
             underline=FALSE,
             cellcolor="transparent",
             vAlign="bottom",
             hAlign="left",
             margin=0,
             wrap=FALSE,
             rotate=0,
             minDigits=0,
             maxDigits=6,
             .color="#000000",
             .cellcolor="transparent")
  
  # 0.1 NA style slots  ####
  
  # We will create the tabel "AA_stylesTable
  AA_stylesTable=matrix(nrow=length(sheet$styles), ncol=length(slotNames("ODSstyle")), dimnames=list(NULL, slotNames("ODSstyle")))
  AA_stylesTable=cbind(styleName=paste0(rep("style", length(sheet$styles)), seq_along(sheet$styles)),AA_stylesTable)
  for (i in seq_along(sheet$styles)){
    Style=sheet$styles[[i]]
    for (slotName in slotNames("ODSstyle")){
      AA_stylesTable[i,slotName]=slot(Style,slotName)
    }
  }
  
  # Replace missing with default values:
  for (attribute in colnames(AA_stylesTable)){
    AA_stylesTable[is.na(AA_stylesTable[,attribute]),attribute]=DEFAULTS[attribute]
  }
  AA_stylesTable[,"size"  ]=paste0(AA_stylesTable[,"size"  ],"pt")
  AA_stylesTable[,"margin"]=paste0(AA_stylesTable[,"margin"],"cm")
  if (any(as.double(AA_stylesTable[,"minDigits"])>as.double(AA_stylesTable[,"maxDigits"]))){stop("We must have minDigits<=maxDigits")}
  
  # 0.2 cells information ####
  AA_cellsContent=copy(sheet$cellsContent)
  AA_cellsContent[,styleName:=paste0("style",styleNumber)]
  
  # 0.3 col/row styles ####
  
  ## defining row and column styles
  colWidths=c(sheet$colWidths,DEFAULTS["colWidth"]) #not elegant, but works well
  colWidths[is.na(colWidths)]=DEFAULTS["colWidth"]
  AA_colStylesDef=matrix(nrow=length(unique(colWidths)),ncol=2,dimnames=list(NULL, c("colStyleName", "width")))
  AA_colStylesDef[,"colStyleName"]=paste0("co",seq_len(nrow(AA_colStylesDef)))
  AA_colStylesDef[,"width"]=unique(colWidths)
  lookup <- setNames(AA_colStylesDef[, "colStyleName"], AA_colStylesDef[, "width"])
  AA_colStyle <- unname(lookup[colWidths])
  
  rowHeights=c(sheet$rowHeights,DEFAULTS["rowHeight"]) #not elegant, but works well
  rowHeights[is.na(rowHeights)]=DEFAULTS["rowHeight"]
  AA_rowStylesDef=matrix(nrow=length(unique(rowHeights)),ncol=2,dimnames=list(NULL, c("rowStyleName", "height")))
  AA_rowStylesDef[,"rowStyleName"]=paste0("ro",seq_len(nrow(AA_rowStylesDef)))
  AA_rowStylesDef[,"height"]=unique(rowHeights)
  lookup <- setNames(AA_rowStylesDef[, "rowStyleName"], AA_rowStylesDef[, "height"])
  AA_rowStyle <- unname(lookup[rowHeights])
  
  
  # 0.4 special Cells  ####
  
  AA_specialCells= data.table(
    row = integer(),
    column = integer(),
    type = character(), ## currently "mergeStart" or "covered"
    par1 = integer(),
    par2 = integer()
  )
  
  ## 0.4.1: merged Cells ####
  
  mc=data.table(copy(sheet$mergedCells))
  mc[,row:=fromRow]
  mc[,column:=fromColumn]
  mc[,type:="mergeStart"]
  mc[,par1:=toRow-fromRow+1]       ## height
  mc[,par2:=toColumn-fromColumn+1] ## width
  mc[, c("fromRow", "toRow", "fromColumn", "toColumn") := NULL]
  AA_specialCells=rbind(AA_specialCells,mc,fill=T)
  
  ## 0.4.2: covered Cells ####
  
  mc=sheet$mergedCells
  for (i in seq_len(nrow(mc))){
    r=mc[i,"fromRow"]:mc[i,"toRow"]
    c=mc[i,"fromColumn"]:mc[i,"toColumn"]
    
    tmp=data.table(
      row=rep(r,each=length(c)),
      column=rep(c,times=length(r)),
      type="covered"
    )
    tmp=tmp[-1,]
    AA_specialCells=rbind(AA_specialCells,tmp,fill=TRUE)
  }
  
  
  if (FALSE){## debug
    AA_cellsContent<<-AA_cellsContent
    AA_specialCells<<-AA_specialCells
    AA_stylesTable <<-AA_stylesTable
    AA_rowStyle<<-AA_rowStyle
    AA_colStyle<<-AA_colStyle
  }
  
  
  # 1.1 mimetype  ####
  {
    LOCAL_MIMETYPE="application/vnd.oasis.opendocument.spreadsheet"
  }
  
  
  # 1.2 manifest  ####
  {
    doc <- xml_new_root(
      "manifest:manifest",
      `xmlns:manifest` =
        "urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"
    )
    
    xml_add_child(
      doc,
      "manifest:file-entry",
      `manifest:full-path` = "/",
      `manifest:media-type` =
        "application/vnd.oasis.opendocument.spreadsheet"
    )
    
    xml_add_child(
      doc,
      "manifest:file-entry",
      `manifest:full-path` = "styles.xml",
      `manifest:media-type` = "text/xml"
    )
    
    xml_add_child(
      doc,
      "manifest:file-entry",
      `manifest:full-path` = "content.xml",
      `manifest:media-type` = "text/xml"
    )
    
    xml_add_child(
      doc,
      "manifest:file-entry",
      `manifest:full-path` = "meta.xml",
      `manifest:media-type` = "text/xml"
    )
    
    LOCAL_MANIFEST=doc
  }
  
  # 1.3 meta  ####
  {
    doc <- xml_new_root(
      "office:document-meta",
      `xmlns:office` = "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
      `xmlns:meta`   = "urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
    )
    
    xml_add_child(doc, "office:meta")
    
    LOCAL_META=doc
  }
  
  # 1.4 styles  ####
  ## currently, we only define the number styles
  {
    doc <- xml_new_root(
      "office:document-styles",
      `xmlns:table`  = "urn:oasis:names:tc:opendocument:xmlns:table:1.0",
      `xmlns:office` = "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
      `xmlns:text`   = "urn:oasis:names:tc:opendocument:xmlns:text:1.0",
      `xmlns:style`  = "urn:oasis:names:tc:opendocument:xmlns:style:1.0",
      `xmlns:draw`   = "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0",
      `xmlns:fo`     = "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0",
      `xmlns:xlink`  = "http://www.w3.org/1999/xlink",
      `xmlns:dc`     = "http://purl.org/dc/elements/1.1/",
      `xmlns:number` = "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0",
      `xmlns:svg`    = "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0",
      `xmlns:of`     = "urn:oasis:names:tc:opendocument:xmlns:of:1.2",
      `office:version` = "1.4"
    )
    
    
    ffd <- xml_add_child(doc, "office:font-face-decls")
    xml_add_child(
      ffd,
      "style:font-face",
      `style:name` = "Arial",
      `svg:font-family` = "Arial"
    )
    
    
    styles <- xml_add_child(doc, "office:styles")
    
    # Number style N0
    ns0 <- xml_add_child(
      styles,
      "number:number-style",
      `style:name` = "N0"
    )
    xml_add_child(
      ns0,
      "number:number",
      `number:min-integer-digits` = "1"
    )
    
    ## for each style, add the number style:
    for (i in seq_len(nrow(AA_stylesTable))){
      ns <- xml_add_child(
        styles,
        "number:number-style",
        `style:name` = paste0("N",i)
      )
      xml_add_child(
        ns,
        "number:number",
        `number:min-decimal-places` = AA_stylesTable[i,"minDigits"],
        `number:decimal-places` = AA_stylesTable[i,"maxDigits"],
        `number:min-integer-digits` = "1"
      )
    }
    
    
    # Default table-cell style
    default_style <- xml_add_child(
      styles,
      "style:style",
      `style:name` = "Default",
      `style:family` = "table-cell",
      `style:data-style-name` = "N0"
    )
    
    xml_add_child(
      default_style,
      "style:table-cell-properties",
      `style:vertical-align` = "automatic",
      `fo:background-color` = "transparent"
    )
    
    xml_add_child(
      default_style,
      "style:text-properties",
      `fo:color`               =DEFAULTS[".color"],
      `style:font-name`        =DEFAULTS["font"],
      `style:font-name-asian`  =DEFAULTS["font"],
      `style:font-name-complex`=DEFAULTS["font"],
      `fo:font-size`           =paste0(DEFAULTS["size"],"pt"),
      `style:font-size-asian`  =paste0(DEFAULTS["size"],"pt"),
      `style:font-size-complex`=paste0(DEFAULTS["size"],"pt")
    )
    
    # automatic-styles
    auto <- xml_add_child(doc, "office:automatic-styles")
    
    pm1 <- xml_add_child(
      auto,
      "style:page-layout",
      `style:name` = "pm1"
    )
    
    xml_add_child(
      pm1,
      "style:page-layout-properties",
      `fo:margin-top` = "0.3in",
      `fo:margin-bottom` = "0.3in",
      `fo:margin-left` = "0.7in",
      `fo:margin-right` = "0.7in",
      `style:table-centering` = "none",
      `style:print` = "objects charts drawings"
    )
    
    hs <- xml_add_child(pm1, "style:header-style")
    xml_add_child(
      hs,
      "style:header-footer-properties",
      `fo:min-height` = "0.45in",
      `fo:margin-left` = "0.7in",
      `fo:margin-right` = "0.7in",
      `fo:margin-bottom` = "0in"
    )
    
    fs <- xml_add_child(pm1, "style:footer-style")
    xml_add_child(
      fs,
      "style:header-footer-properties",
      `fo:min-height` = "0.45in",
      `fo:margin-left` = "0.7in",
      `fo:margin-right` = "0.7in",
      `fo:margin-top` = "0in"
    )
    
    # master-styles
    masters <- xml_add_child(doc, "office:master-styles")
    
    mp1 <- xml_add_child(
      masters,
      "style:master-page",
      `style:name` = "mp1",
      `style:page-layout-name` = "pm1"
    )
    
    xml_add_child(mp1, "style:header")
    xml_add_child(mp1, "style:header-left", `style:display` = "false")
    xml_add_child(mp1, "style:header-first")
    xml_add_child(mp1, "style:footer")
    xml_add_child(mp1, "style:footer-left", `style:display` = "false")
    xml_add_child(mp1, "style:footer-first")
    LOCAL_STYLES=doc
  }
  
  # 1.5 content  ####
  {
    doc <- xml_new_root(
      "office:document-content",
      `xmlns:table`  = "urn:oasis:names:tc:opendocument:xmlns:table:1.0",
      `xmlns:office` = "urn:oasis:names:tc:opendocument:xmlns:office:1.0",
      `xmlns:text`   = "urn:oasis:names:tc:opendocument:xmlns:text:1.0",
      `xmlns:style`  = "urn:oasis:names:tc:opendocument:xmlns:style:1.0",
      `xmlns:draw`   = "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0",
      `xmlns:fo`     = "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0",
      `xmlns:xlink`  = "http://www.w3.org/1999/xlink",
      `xmlns:dc`     = "http://purl.org/dc/elements/1.1/",
      `xmlns:number` = "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0",
      `xmlns:svg`    = "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0",
      `xmlns:of`     = "urn:oasis:names:tc:opendocument:xmlns:of:1.2",
      `office:version` = "1.4"
    )
    
    # <office:font-face-decls>
    ff <- xml_add_child(doc, "office:font-face-decls")
    xml_add_child(ff, "style:font-face",
                  `style:name` = "Calibri",
                  `svg:font-family` = "Calibri"
    )
    
    # <office:automatic-styles>
    as <- xml_add_child(doc, "office:automatic-styles")
    
    # ce1 (table-cell)
    xml_add_child(as, "style:style",
                  `style:name` = "ce1",
                  `style:family` = "table-cell",
                  `style:parent-style-name` = "Default",
                  `style:data-style-name` = "N0"
    )
    
    
    # COLUMN STYLES ARE DEFINED:
    co0 <- xml_add_child(as, "style:style",
                         `style:name` = "co0",
                         `style:family` = "table-column")
    xml_add_child(co0, "style:table-column-properties",
                  `fo:break-before` = "auto",
                  `style:column-width` = DEFAULTS["colWidth"])
    
    for (i in seq_len(nrow(AA_colStylesDef))){
      col <- xml_add_child(as, "style:style",
                           `style:name` = AA_colStylesDef[i,"colStyleName"],
                           `style:family` = "table-column")
      xml_add_child(col, "style:table-column-properties",
                    `fo:break-before` = "auto",
                    `style:column-width` = AA_colStylesDef[i,"width"]) 
    }
    
    
    
    # ROW STYLES ARE DEFINED:
    ro0 <- xml_add_child(as, "style:style",
                         `style:name` = "ro0",
                         `style:family` = "table-row"
    )
    xml_add_child(ro0, "style:table-row-properties",
                  `fo:break-before` = "auto",
                  `style:row-height` = DEFAULTS["rowHeight"],
                  `style:use-optimal-row-height` = "true"
    )
    
    for (i in seq_len(nrow(AA_rowStylesDef))){
      row <- xml_add_child(as, "style:style",
                           `style:name` = AA_rowStylesDef[i,"rowStyleName"],
                           `style:family` = "table-row")
      node <- xml_add_child(row, "style:table-row-properties",
                            `fo:break-before` = "auto",
                            `style:row-height` = AA_rowStylesDef[i,"height"])
      ## xml_set_attr(node, "style:use-optimal-row-height","true") ## this is just an experiment
      ## there exists the attribute 'style:use-optimal-row-height="true"' ... is this relevant?
      ## so far, I didn't find a difference
    }
    
    
    ## MAIN MODIFICATION: ADDING STYLES
    for (i in seq_len(nrow(AA_stylesTable))){
      xxx<- xml_add_child(as, "style:style",
                          `style:name` = AA_stylesTable[i,"styleName"],
                          `style:family` = "table-cell",
                          `style:parent-style-name` = "Default",
                          `style:data-style-name` = paste0("N",i)
      )
      
      ## this node is not always necessary... lets see if i can just include it always...
      ## also, there would be style:repeat-content="false" when we deal with alignments... I dont know what that is
      node <-xml_add_child(xxx, "style:table-cell-properties",
                           `style:vertical-align` = AA_stylesTable[i,"vAlign"])
      
      if (AA_stylesTable[i,".cellcolor"]!="transparent"){xml_set_attr(node, "fo:background-color",AA_stylesTable[i,".cellcolor"])}
      if (AA_stylesTable[i,"wrap"]     =="TRUE"       ){xml_set_attr(node, "fo:wrap-option","wrap")}
      if (AA_stylesTable[i,"rotate"]   !="0"          ){xml_set_attr(node, "style:rotation-angle",AA_stylesTable[i,"rotate"])}
      
      ## this node is not always necessary... 
      if (AA_stylesTable[i,"hAlign"]=="middle"){
        node <-xml_add_child(xxx, "style:paragraph-properties",
                             `fo:text-align` = "center")}
      if (AA_stylesTable[i,"hAlign"]=="left"){
        node <-xml_add_child(xxx, "style:paragraph-properties",
                             `fo:text-align` = "start",
                             `fo:margin-left`= AA_stylesTable[i,"margin"])}
      if (AA_stylesTable[i,"hAlign"]=="right"){
        node <-xml_add_child(xxx, "style:paragraph-properties",
                             `fo:text-align`  ="end",
                             `fo:margin-right`=AA_stylesTable[i,"margin"])}
      
      
      node <-xml_add_child(xxx, "style:text-properties",
                           `fo:color` = AA_stylesTable[i,".color"],
                           `style:font-name`         = AA_stylesTable[i,"font"],
                           `style:font-name-asian`   = AA_stylesTable[i,"font"],
                           `style:font-name-complex` = AA_stylesTable[i,"font"],
                           `style:font-size`         = AA_stylesTable[i,"size"],
                           `style:font-size-asian`   = AA_stylesTable[i,"size"],
                           `style:font-size-complex` = AA_stylesTable[i,"size"]
      )
      
      if (AA_stylesTable[i,"bold"]=="TRUE"){
        xml_set_attr(node, "fo:font-weight",           "bold")
        xml_set_attr(node, "style:font-weight-asian",  "bold")
        xml_set_attr(node, "style:font-weight-complex","bold")
      }
      if (AA_stylesTable[i,"italic"]=="TRUE"){
        xml_set_attr(node, "fo:font-style",           "italic")
        xml_set_attr(node, "style:font-style-asian",  "italic")
        xml_set_attr(node, "style:font-style-complex","italic")
      }
      if (AA_stylesTable[i,"underline"]=="TRUE"){
        xml_set_attr(node, "style:text-underline-style", "solid")
        xml_set_attr(node, "style:text-underline-type", "single")
      }
    }
    
    
    
    
    # ta1 (table) + its properties
    ta1 <- xml_add_child(as, "style:style",
                         `style:name` = "ta1",
                         `style:family` = "table",
                         `style:master-page-name` = "mp1"
    )
    xml_add_child(ta1, "style:table-properties",
                  `table:display` = "true",
                  `style:writing-mode` = "lr-tb"
    )
    
    
    body <- xml_add_child(doc, "office:body")
    ss <- xml_add_child(body, "office:spreadsheet")
    
    # calculation-settings
    xml_add_child(ss, "table:calculation-settings",
                  `table:case-sensitive` = "false",
                  `table:search-criteria-must-apply-to-whole-cell` = "true",
                  `table:use-wildcards` = "true",
                  `table:use-regular-expressions` = "false",
                  `table:automatic-find-labels` = "false"
    )
    
    # <table:table table:name="Tabelle1" table:style-name="ta1">
    tbl <- xml_add_child(ss, "table:table",
                         `table:name` = ifelse(is.na(sheet$sheetName),"Tabellle1",sheet$sheetName),
                         `table:style-name` = "ta1"
    )
    
    # Define all columns (very inefficient for now)
    for (col in seq_along(AA_colStyle)){
      xml_add_child(tbl, "table:table-column",
                    `table:style-name` = AA_colStyle[col],
                    `table:default-cell-style-name` = "ce1")
    }
    xml_add_child(tbl, "table:table-column",
                  `table:style-name` = "co0",
                  `table:number-columns-repeated` = 2^14-length(AA_colStyle),
                  `table:default-cell-style-name` = "ce1")
    
    
    # Here we fill every cell:
    maxROW=max(AA_specialCells[,row],AA_cellsContent[,row],length(AA_rowStyle),0)
    
    for (rowNr in seq_len(maxROW)){
      row <- xml_add_child(tbl, "table:table-row",`table:style-name` = AA_rowStyle[rowNr])
      
      maxCOL=max(AA_specialCells[row==rowNr,column],AA_cellsContent[row==rowNr,column],0)
      
      for (colNr in seq_len(maxCOL)){
        special    =AA_specialCells[row==rowNr & column==colNr,]
        cellContent=AA_cellsContent[row==rowNr & column==colNr,]
        
        if (nrow(special)==1){
          if (special$type=="covered"){ ## covered cell:
            xml_add_child(row, "table:covered-table-cell",
                          `table:number-columns-repeated` = "1")
            next
          }
        }
        
        
        if (nrow(cellContent)==0){ ## add empty cell
          cell <- xml_add_child(row, "table:table-cell",
                                 `table:style-name` = "ce1")
        } else {
          cell <- xml_add_child(row, "table:table-cell",
                                 `office:value-type`= cellContent[,type],
                                 `table:style-name` = cellContent[,styleName])
          if (cellContent[,type]=="float"){
            xml_set_attr(cell, "office:value", as.character(cellContent[,data_float]))
            p <- xml_add_child(cell, "text:p")
            xml_set_text(p, as.character(cellContent[,data_float]))
          }
          if (cellContent[,type]=="string"){
            p <- xml_add_child(cell, "text:p")
            xml_set_text(p, cellContent[,data_string])
          }
          
          
        }
        
        if (nrow(special)==1){
          if (special$type=="mergeStart"){
            xml_set_attr(cell, "table:number-rows-spanned"   ,special[,par1])
            xml_set_attr(cell, "table:number-columns-spanned",special[,par2])
          }
        }
        
      }
      
      # finish the row:
      xml_add_child(row, "table:table-cell",`table:number-columns-repeated` = 16384-maxCOL)
      
    }
    
    
    # Remaining rows as empty
    row2 <- xml_add_child(tbl, "table:table-row",
                          `table:number-rows-repeated` = 2^20-maxROW,
                          `table:style-name` = "ro0"
    )
    xml_add_child(row2, "table:table-cell",
                  `table:number-columns-repeated` = 2^14
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    LOCAL_CONTENT=doc
    ## DEBUG:
    if (FALSE){cat("\n\n\n",as.character(LOCAL_STYLES, options = "format"))}
    if (FALSE){cat("\n\n\n",as.character(LOCAL_CONTENT, options = "format"))}
  }
  
  
  # 2.1 writing  ####
  {
    if (!dir.exists("TEMP")){dir.create("TEMP")}
    if (!dir.exists("TEMP/META-INF")){dir.create("TEMP/META-INF")}
    
    write_xml(LOCAL_MANIFEST, "TEMP/META-INF/manifest.xml")
    
    write_xml(LOCAL_CONTENT, "TEMP/content.xml")
    write_xml(LOCAL_META, "TEMP/meta.xml")
    write(LOCAL_MIMETYPE,"TEMP/minetype")
    write_xml(LOCAL_STYLES, "TEMP/styles.xml")
    
    
    wd=getwd()
    setwd("./TEMP")
    files <- list.files(recursive = T)
    result=try({
      zip::zip(zipfile = paste0("../",file), files = files)
    },silent = T)
    setwd(wd)
    unlink("TEMP", recursive = TRUE)
    
    if (inherits(result, "try-error")){
      stop(paste0("I am very sorry, but it seems that ",file," is currently open:("))
    }
  }
}


#~~~~~~~~~~~~~~~~~
## Bugs/Todos ####
#~~~~~~~~~~~~~~~~~

# rotate does not rotate more than 90 degrees
# multiple sheets
# complete number formats
# vectorize input
# change from "matrix" to "data.table", for instance AA_stylesTable

#~~~~~~~~~~~~~~~~~~~~~~~~~~
## Maybe look into ... ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~

# even "text only" styles have a digits attribute... 

