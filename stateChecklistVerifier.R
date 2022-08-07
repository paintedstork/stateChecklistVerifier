library (xlsx)
library (dplyr)

#----------------DO NOT MODIFY----------------------------------
mandatoryColumns <- c ("Sl.No", 
                       "Order", 
                       "Family", 
                       "Species", 
                       "English.Name", 
                       "Scientific.Name", 
                       "Authority")
IndiaCheckListMajorVersion <- 6
IndiaCheckListMinorVersion <- 1
indiaChecklist <- paste0("India-Checklist_v",
                   IndiaCheckListMajorVersion,"_",
                   IndiaCheckListMinorVersion,
                   ".xlsx")
base <- read.xlsx(indiaChecklist, sheetIndex = 2)

#------------------------------------------
# Configurations for each State
stateName <- 'Karnataka'
stateChecklistMajorVersion <- 4
stateChecklistMinorVersion <- 0
stateChecklistName <- paste0(stateName,"_Checklist_v",
                             stateChecklistMajorVersion,"_",
                             stateChecklistMinorVersion)
stateChecklist <- paste0(stateChecklistName,
                         ".xlsx")
exceptColumns <- c("Rarity")
outputfile <- paste0(stateChecklistName,
                     "_logs",
                     ".txt")
#--------------------------

verifySheetNames <- function (checklistName)
{
  ret <- TRUE
  wb <- loadWorkbook (paste0(checklistName,".xlsx"))
  sheets <- getSheets(wb)

  printFile(paste("Number of worksheets:", length(sheets)))
  
  if("ReadMe" != names(sheets)[1])
  {
    printFile("First sheet should be named ReadMe")
    ret = FALSE
  }

  if(checklistName != names(sheets)[2])
  {
    printFile(paste("Second sheet should be named", checklistName, "and not", names(sheets)[2]))
    ret = FALSE
  }
  
  if("Additional Bibliography" != names(sheets)[length(sheets)])
  {
    printFile("Third sheet should be named Additional Bibliography")
    ret = FALSE
  }
  
  return (ret)
}

verifyColumn <- function (column, dat)
{
  if ( column %in% colnames(dat) )
  {
    return (TRUE)       
  }
  else
  {
    write(paste("Missing mandatory column:", column,"in second spreadsheet"), outputfile, append = TRUE)
    return (FALSE)
  }
}

verifySlNo <- function (number, dat)
{
  if(which(dat$Sl.No == number) == number)
  {
    return (TRUE)
  }
  else
  {
    write(paste("Sl No mismatch at row",number), outputfile, append = TRUE)
    return (FALSE)
  }
}

verifyEntries <- function (column, dat)
{
  diff <- anti_join (dat, base, by = c(column))
  if(nrow (diff) > 0)
  {
    write(paste ("Unknown", column), outputfile, append = TRUE)
    write.table(diff[column], outputfile, append = TRUE)
    return (FALSE)
  }
  return (TRUE)
}

verifyCrossEntries <- function (column, dat)
{
  if(column %in% colnames(base))
  {
    diff <- anti_join (dat [c("Scientific.Name", column)], base [c("Scientific.Name", column)], by = c("Scientific.Name", column))
    if(nrow (diff) > 0)
    {
      write(paste ("Mismatch in", column), outputfile, append = TRUE)
      write.table(diff, outputfile, append = TRUE, row.names = FALSE)
      return (FALSE)
    }
  }
  return (TRUE)
  
}

printFile <- function (str)
{
  print(str)
  write (str, outputfile, append = TRUE)
}

write (paste("Logs of execution on",timestamp()), outputfile)

printFile("Verifying Sheet Names")
ret <- verifySheetNames (stateChecklistName)

test <- read.xlsx(stateChecklist, sheetIndex = 2)

printFile("Verifying Mandatory Column Names")

ret <- any (FALSE == lapply(mandatoryColumns, verifyColumn, dat = test))

printFile("Verifying Sl No continuity")

ret <- any (FALSE == lapply(1:nrow(test), verifySlNo, dat = test)) || ret

printFile("Verifying all names")
ret <- any (FALSE == lapply(mandatoryColumns[4:6], verifyEntries, dat = test)) || ret

print("Verifying mandatory columns aganist Scientific Name")
ret <- any (FALSE == lapply(mandatoryColumns[2:5], verifyCrossEntries, dat = test)) || ret

printFile ("Verifying other columns aganist Scientific Name")
otherColumns <- setdiff (colnames(test), union (mandatoryColumns, exceptColumns))
ret <- any (FALSE == lapply(otherColumns, verifyCrossEntries, dat = test)) || ret

if (ret == TRUE)
{
  printFile("Please fix errors")
}

printFile ("Verification Complete")
