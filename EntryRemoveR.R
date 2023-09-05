#!/usr/bin/env Rscript

#Childhood Cancer Data Initiative - EntryRemoveR.R

#This script will take a CCDI metadata manifest file and template and remove entries based on the TSV of entries given.

##################
#
# USAGE
#
##################

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CCDI-EntryRemoveR.R --help


##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("readr","dplyr","tidyr","openxlsx","stringi","readxl","janitor","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(readr,verbose = F))
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(tidyr,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(openxlsx, verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))


#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file (.xlsx, .tsv, .csv)", metavar="character"),
  make_option(c("-e", "--entry"), type="character", default=NULL, 
              help="tsv file, no header, of [node]_id that are to be removed from the submission manifest.", metavar="character"),
  make_option(c("-t", "--template"), type="character", default=NULL, 
              help="dataset template file, CCDI_submission_metadata_template.xlsx", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nEntryRemoveR v1.0.0")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)&is.null(opt$template)){
  print_help(opt_parser)
  cat("Please supply both the input file (-f) and template file (-t), CCDI_submission_metadata_template.xlsx.\n\n")
  suppressMessages(stop(call.=FALSE))
}


#Data file pathway
file_path=file_path_as_absolute(opt$file)

#Template file pathway
template_path=file_path_as_absolute(opt$template)

#Template file pathway
entry_path=file_path_as_absolute(opt$entry)

###########
#
# File name rework
#
###########

#Rework the file path to obtain a file extension.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))
path=paste(dirname(file_path),"/",sep = "")

#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_EntRemove",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")


##############
#
# Pull Dictionary Page to create node pulls
#
##############

#Read in Dictionary page to obtain the required properties.
df_dict=suppressMessages(read.xlsx(xlsxFile = template_path,sheet = "Dictionary"))
df_dict=remove_empty(df_dict,c('rows','cols'))

#Look for all entries that have a value
all_properties=unique(df_dict$Property)[grep(pattern = '.',unique(df_dict$Property))]
#Remove all entries that are all spaces
all_properties=all_properties[!grepl(pattern = " ",x = all_properties)]
#Pull out nodes to read in respective tabs
dict_nodes=unique(df_dict$Node)


##############
#
# Read in each tab and apply to a data frame list
#
##############

# A bank of NA terms to make sure NAs are brought in correctly
NA_bank=c("NA","na","N/A","n/a")

#Establish the list
workbook_list=list()
incomplete_node=c()

#create a list of all node pages with data
for (node in dict_nodes){
  #read the sheet
  df=suppressMessages(read_xlsx(path = file_path, trim_ws = TRUE, na=NA_bank, sheet = node, guess_max = 1000000, col_types = "text"))
  #df=readWorkbook(xlsxFile = file_path,sheet = node, na.strings = NA_bank)
  #create an emptier version that removes the type and makes everything a character
  df_empty_test=df%>%
    select(-type)%>%
    mutate(across(everything(), as.character))
  #remove empty rows and columns
  df_empty_test=remove_empty(df_empty_test,c("rows","cols"))
  
  #if there are at least one row in the resulting data frame, add it
  if (dim(df_empty_test)[1]>0){
    #if the only columns in the resulting data frame are only linking properties (node.node_id), do not add it.
    if (any(!grepl(pattern = "\\.",x = colnames(df_empty_test)))){
      #add the data frame to the workbook
      workbook_list=append(x = workbook_list,values = list(df))
      names(workbook_list)[length(workbook_list)]<-node
    }else{
      incomplete_node=c(incomplete_node,node)
    }
  }
}

nodes_present=names(workbook_list)


#############
#
# Entry manipulation 
#
#############

entries_df=suppressMessages(read_tsv(file = entry_path, col_names = FALSE))

entries=entries_df$X1


#############
#
# Data frame manipulation
#
#############

#Start message
cat("\nThe entry remover process has begun.\n")

#start sink to capture changes made
sink(file = paste(path,output_file,"_log.txt",sep = ""))
cat("This output describes the entries that are to be deleted followed by any entries that might be linked to that entry.\nThe linked entries are then added to the original list of entries and are then deleted as well.")

cat("\n\nEntry list provided: \n\t", paste(entries,collapse = "\n\t"), sep="")
cat("\n\n##############################################\n")
sink()

#Based on the list of entries we want removed, while the list has values present, it should continue to loop through all nodes looking for the [node]_id entry and if the [node]_id entry is found in a [node].[node]_id column, then the [node]_id that it is related to, should also be added to the list. Then as these entries are slowly removed from the data set, they are removed from the list and this continues until the list is empty.

#create a list that notes what node the entries, that were deleted, are found on.
entry_list=list()

for (node in nodes_present){
  entry_list=append(x = entry_list,values = list(c()))
  names(entry_list)[length(entry_list)]<-node
}

while (length(entries)>0){
  entry=entries[1]
  
  sink(file = paste(path,output_file,"_log.txt",sep = ""),append = TRUE)
  cat(entry,"\n----------\n", sep = "")
  sink()
  
  for (node in nodes_present){
    
    #create df
    df=workbook_list[node][[1]]
    
    #create node column
    node_id=paste(node,"_id",sep = "")
    #find hits
    node_id_hits=grep(pattern = TRUE, x = df[node_id][[1]] %in% entry)
    #if there are hits
    if (length(node_id_hits)>0){
      
      entry_list[node][[1]]=c(entry_list[node][[1]], entry)
      
      sink(file = paste(path,output_file,"_log.txt",sep = ""),append = TRUE)
      cat("\t",entry,": Removed from ", node_id, " in the ", node, " node.\n", sep = "")
      sink()
    }
    
    if (node!='study'){
      #find columns
      node_cols= colnames(df)
      #only dot linking columns
      node_cols=node_cols[grep(pattern = "\\.", node_cols)]
      #remove the ".id" columns
      node_node_ids=node_cols[-grep(pattern = "\\.id", node_cols)]
      
      for (node_node_id in node_node_ids){
        node_node_id_hits=grep(pattern = TRUE, x = df[node_node_id][[1]] %in% entry)
        if (length(node_node_id_hits)>0){
          for (node_node_id_hit in node_node_id_hits){
            
            #find the [node]_id that is related to the entry that is being deleted
            entry_add=df[node_node_id_hit,node_id][[1]]
            #add this entry to the list as it and the children need to be deleted as well
            entries=c(entries, entry_add)

            sink(file = paste(path,output_file,"_log.txt",sep = ""),append = TRUE)
            cat("\t",entry_add,": Is a child entry of ", entry ," from the ", node_id, " in the ", node, " node.\n", sep = "")
            sink()
          }
        }
      }
      
    }
  }
  entries=entries[-1]
}


sink(file = paste(path,output_file,"_log.txt",sep = ""),append = TRUE)

cat("\n##############################################\nThe following nodes had these entries deleted from them:\n-------\n")

for (node in names(entry_list)){
  cat(node,"\n--------\n\t")
  cat(paste(entry_list[node][[1]], collapse = "\n\t"))
  cat("\n\n")
  
  node_id=paste(node,"_id",sep = "")
  df=workbook_list[node][[1]]
  #write back to workbook
  for (entry in entry_list[node][[1]]){
    entry_loc=grep(pattern = TRUE, x = df[node_id][[1]] %in% entry)
    df=df[-entry_loc,]
  }
  workbook_list[node][[1]]=df
}

sink()


###############
#
# Write out
#
###############

#Write out file

wb=openxlsx::loadWorkbook(file = file_path)

cat("\n\nWriting out the EntRemoveR file.\n")

#progress bar
pb=txtProgressBar(min=0,max=length(nodes_present),style = 3)
x=0

for (node in nodes_present){
  x=x+1
  setTxtProgressBar(pb,x)
  
  df_size=suppressMessages(read_xlsx(path = file_path, trim_ws = TRUE, na=NA_bank, sheet = node, guess_max = 1000000, col_types = "text"))
  
  df=workbook_list[node][[1]]
  openxlsx::deleteData(wb, sheet = node,rows = 1:(dim(df_size)[1]+1),cols=1:(dim(df_size)[2]+1),gridExpand = TRUE)
  openxlsx::writeData(wb=wb, sheet=node, df)
  openxlsx::saveWorkbook(wb = wb,file = paste(path,output_file,".xlsx",sep = ""), overwrite = T)
}



cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")) 
