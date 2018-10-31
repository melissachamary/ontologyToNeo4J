library("RNeo4j")
library("RCurl")
library("redland") #check imports
library("jsonlite")#check imports
library("stringr")

source(paste(getwd(),sep="/function/","importOntology.R"), verbose = TRUE)

graphFolderPath<-"/Users/mimi/Documents/HCL_work/Neo4J-Databases/testNeo4J/"
url_server<-"http://localhost:7474/"
neo4jversion <-"3.4.7"
user<-"RNeo4J"
pwd<-"RNeo4J"
graph<-initNeo4JGraph(graphFolderPath,neo4jversion,url_server, user, pwd)

fileTBox<-"/Users/mimi/Documents/HCL_work/Neo4J-Databases/testNeo4J/installation-3.4.7/import/testNeo4J.owl" #fichier au format RDF/XML
fileABox<-"/Users/mimi/Documents/HCL_work/Neo4J-Databases/testNeo4J/installation-3.4.7/import/testNeo4JINST.owl" #fichier au format RDF/XML


#----- IMPORT + Check----
#Import TBox
data<-importOntologyFile(graph,fileTBox)
  #-- Set prefix and shortname (not in V1)
  data$ontology.prefix<-paste(data$ontology.uri,sep="","#")
  data$ontology.shortName<-"cepi"
  manageOntologyPrefixe(data, graph)
  graph<-assignElementToOntologyNode(graph, data$ontology.uri)
  #--Clean class 
  graph<-trimClassSingleRestrictionPattern(graph)
  #---Clean property 
  graph<-cleanPropertyHierarchies(graph)
  #--- Clean individual
  graph<- trimInstanceAnnotationOnAxiom(graph)
  
#Import ABox
data2<-importOntologyFile(graph,fileABox)
data2$ontology.prefix<-paste(data2$ontology.uri,sep="","#")
data2$ontology.shortName<-"depi"
manageOntologyPrefixe(data2, graph)
graph<-assignElementToOntologyNode(graph, data$ontology.uri)
#--Clean class 
graph<-trimClassSingleRestrictionPattern(graph)
#---Clean property 
graph<-cleanPropertyHierarchies(graph)
#--- Clean individual
graph<- trimInstanceAnnotationOnAxiom(graph)

#---Assign label to class
assignIndividualsToClass(graph)