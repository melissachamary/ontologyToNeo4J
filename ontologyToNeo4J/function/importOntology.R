

####### Initialisation Operation ######
#'@name  intiNeo4JGraph
#'@description initialization of Graph Object belongs to specific Neo4J database, clean database and return graph object
#'@details If There isn't Neo4J database system file in graphFolderPath then the programm will install by default Neo4J in the folder
#'@param graphFolderPath physical location of Neo4J Database
#'@param neo4jversion version of neo4J
#'@param url_server server adress (http://localhost:7474/)
#'@param user user
#'@param pwd password
#'@return graph
initNeo4JGraph<-function(graphFolderPath, neo4jversion ,url_server,user,pwd ){
  pluginCheck<-c("neosemantics-3.4.0.2.jar","apoc-3.4.0.3-all.jar")
  
  neo4Jinstallation<-paste(graphFolderPath, sep="/installation-",neo4jversion)
  ip_server<-paste(url_server, sep="db/data","")#/db/data accès normalisé au données du graph
  
  #### Initialisation du graph  ####
  ## @Check Neo4J.plugin : neosemantics-3.4.0.2.jar apoc
  if(!dir.exists(neo4Jinstallation)){
    stop("Neo4J installation folder does not exist") #A changer pour V1
  }else{
  }
  if(sum(file.exists(paste(neo4Jinstallation,sep="/plugins/",pluginCheck))) 
     < length(file.exists(paste(neo4Jinstallation,sep="/plugins/",pluginCheck)))){
    warning("Neo4J plugin does not exist")
  }else{
  }
  ##----Démarrer Graph--##
  manualLaunchCommand<-paste(neo4Jinstallation,sep="/bin/neo4j ","start")
  #--lancement manuel du serveur si iln'est pas lancé
  if(!url.exists(url_server)){
    warning("[Neo4J Server] Manual launch, wait 2mn for server start")
    
    system(manualLaunchCommand, intern = FALSE, ignore.stderr = FALSE)
    Sys.sleep(240)
  }else{}
  tryCatch(graph <-startGraph(ip_server, username=user, password=pwd),
           # error = function(e),
           finally = print("graph launch server") #Serveur doit être opérationnel
  )
  clear(graph, input=F)
  
  #Ajout de l'indexe sur les URI
  cypher(graph, "CREATE INDEX ON :Resource(uri)")#Tant que l'ensemble des elt n'a pas été importé
 
  return(graph) 
  
}

#'@name  importOntologySet
#'@description Ordonate and import set of ontology files in 'RDF/XML' format
#'@details Not implemented yet
#'@param 
#'@param 

#'@name  importOntologyFile
#'@description import Ontology files in 'RDF/XML' format and check import
#'@details In version2 implement thing to keep shortName prefix
#'@param graph
#'@param file
#'@return a list of element about ontology ontology.uri, ontology.prefix = NULL, ontology.shortName=NULL,namespace.dataframe)
importOntologyFile<-function(graph,file){
  if(!file.exists(file)){
    stop(paste("[Neo4J owl import] : file does not exist ",file, ""))
  }else{
  }
  qImportTBox<-"CALL semantics.importRDF('<<FILE>>','RDF/XML',{typesToLabels: true, shortenUrls : true})"
  importData<-cypher(graph, 
                     query = gsub("<<FILE>>", paste("file://",sep="",file),qImportTBox,fixed=TRUE)
  )
  ###Check imports
  
  #-- Vérification status
  if(!importData$terminationStatus == "OK"|| importData$triplesLoaded == 0){
    stop("[Neo4J owl import] : data not imported ")
  }else{ 
    ontoURI<-checkNeo4JContent(graph,ontoFile = file,tripletCount=importData$triplesLoaded)
  }
  
  namespace.dataframe<-as.data.frame(unlist(importData$namespaces))
  namespace.dataframe<-cbind(namespace.dataframe,row.names(namespace.dataframe))
  print(dim(namespace.dataframe))
  colnames(namespace.dataframe)<-c('prefix','uri')
  row.names(namespace.dataframe)<-NULL
  return(list(ontology.uri=ontoURI, ontology.prefix = NULL, ontology.shortName=NULL,namespace.dataframe=namespace.dataframe))
}

####### Check OWL-Graph Operation ######

#'@name  checkOntology
#'@description check if node owl__Ontology into the graph (stop else), and warn if multiple owl__Ontology nodes
#'@param graph object neo4J graph (library RNeo4J)
#'@return cypher query result (ontoURI)
checkOntology<-function(graph){
  queryOntologyID <- "MATCH (onto:owl__Ontology) WHERE NOT EXISTS((onto)<-[:intoOntology]-()) RETURN DISTINCT onto.uri"
  ontoURI<-cypher(graph,queryOntologyID)
  if(dim(ontoURI)[1]==0){
    stop("[Neo4J owl import] There are no Ontology node element in the model")
  }else if(dim(ontoURI)[1] >1){
    warning("[Neo4J owl import] Mutliple Ontology node element were imported \n 
            Further ontology trimming steps may raised error (Class, ObjectProperty, etc.) ")
  }else{
    
  }
return(ontoURI)}

#'@name  checkOntologyElements
#'@description check by comparing éléments (triple, class, properties) count into ontology file and graph.
#'@param graph object neo4J graph (library RNeo4J)
#'@param ontoFile ontology filePath
#'@param tripletCount number of counted triplet during import, if NA means no tripletCount check required 
#'@return void 

checkOntologyElements<-function(graph,ontoFile,tripletCount){
  #--- Initialisation des objets rdfland
  world <- new("World")
  storage <- new("Storage", world, "hashes", name="", options="hash-type='memory'")
  model <- new("Model", world, storage, options="")
  parser <- new("Parser", world)
  parseFileIntoModel(parser, world, ontoFile, model)
  #-- Nombre de triplet
  tripletOntoQuery <- 'PREFIX owl:<http://www.w3.org/2002/07/owl#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  SELECT (?a AS ?triplet) WHERE { ?a ?p ?c . }'
  queryOnto <- new("Query", world, tripletOntoQuery, base_uri=NULL, query_language="sparql", query_uri=NULL)
  tripletOntoCount <- dim(as.data.frame(fromJSON(getResults(queryOnto, model,"json"))[[2]]))[1]#Récupérer le nombre de triplet
  if(!is.na(tripletCount) && tripletCount != tripletOntoCount){
    stop("[Neo4J owl import] Missing triplet")
  }else{
    
  }
  queryNeo<-'Match (n:owl__<<ELT>>) RETURN (count(n))'
  classOntoQuery<-'PREFIX owl:<http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  SELECT (?a AS ?Class) WHERE {?a a owl:<<ELT>>}'
  checkElementTable<-c("Class", "ObjectProperty", "DataProperty","AnnotationProperty")#Ajout individus
  for(i in checkElementTable){
    resultNeo<-cypher(graph,gsub("<<ELT>>",i,queryNeo))[[1]]
    
    
    queryOnto <- new("Query", world, gsub("<<ELT>>",i,classOntoQuery), base_uri=NULL, query_language="sparql", query_uri=NULL)
    rawResult<-getResults(queryOnto, model,"json")
    if(!is.null(rawResult)){
      resultOnto <- dim(as.data.frame(fromJSON(rawResult)[[2]]))[1]#Récupérer le nombre de triplet
    }else{
      resultOnto <-0
    }
    if(resultOnto>resultNeo){
      stop(gsub("<<ELT>>",i,"[Neo4J owl import] Missing <<ELT>> or wrong <<ELT>> assignation label in Neo4J"))
    }else{}
  }
}

#'@name  checkNeo4JContent
#'@description combination of checkOntology and checkOntologyElements
#'@seealso checkOntology
#'@seealso checkOntologyElements
#'@param graph object neo4J graph (library RNeo4J)
#'@param ontoFile ontology filePath
#'@param triplet specify is triple ount should be matter or not in the ontology Check
#'@return ontoURI (return of checkOntology) 
checkNeo4JContent<-function(graph,ontoFile,tripletCount=NA){
  ontoURI<-checkOntology(graph)
  checkOntologyElements(graph,ontoFile,tripletCount)
return(ontoURI)}

######### Graph Trimming Operation #####
#'@name  manageOntologyPrefixe
#'@description Create nodes labeled :Prefix corresponding to prefixed used in ontology (included the ontology prefix) and assign them to owl__Ontology node corresponding 
#'@details Also manage label rename (ns0) according true prefix
#'@param graph object neo4J graph (library RNeo4J)
#'@param data a list of namespace from importation of owl file, ontology URI, ontology prefix and ontology shortname (importData$namespaces) 
#'@return graph
manageOntologyPrefixe<-function(data, graph){
  #tous les autres préfixes
  queryNamSpaceCreation<-"MATCH (o:owl__Ontology{uri:'<<ONTO_URI>>'}) WITH o MERGE (n:Prefix{uri:'<<URI>>'})  ON CREATE SET n.prefix='<<PREFIX>>' MERGE (o)-[:usePrefix]->(n) RETURN n.prefix"
  queryLabelGet<-"CALL db.labels() YIELD label"
  queryLabelRename<-"call apoc.refactor.rename.label('<<OLD>>', '<<NEW>>')"
  #Init query avec <<ONTO_URI Param>>
  queryNamSpaceCreation<-gsub("<<ONTO_URI>>",data$ontology.uri, queryNamSpaceCreation)
  #Pour chaque namespace dans le tableau : crée le node :Prefix (si il n'existe pas) et l'assigne à l'ontologie courante
  for(i in 1:dim(data$namespace.dataframe)[1]){
    prefix<-cypher(graph,
                   gsub("<<PREFIX>>",data$namespace.dataframe[i,"prefix"],
                        gsub("<<URI>>", data$namespace.dataframe[i,"uri"],
                             queryNamSpaceCreation)))
    print(c(prefix,data$namespace.dataframe[i,"prefix"]))
    #Si le node existe déjà et mais que le préfixe est différent de celui identifié dans le tableau de départ alors changer les labels correspondants
    if(prefix!=data$namespace.dataframe[i,"prefix"]){
      labels<-cypher(graph,queryLabelGet)
      changeLabelDF<-data.frame(oldLabel=labels$label,toChange=rep(NA, times = length(labels)),newLabel=rep(NA, times = length(labels)))
      changeLabelDF$toChange<- str_detect(c(changeLabelDF$oldLabel),pattern=paste(data$namespace.dataframe[i,"prefix"],sep="","__"))
      changeLabelDF<-changeLabelDF[which(changeLabelDF$toChange>0),]
      #Pour chaque label à changer (correspondant au préfixe) => le changer
      for(j in 1:dim(changeLabelDF)[1]){
        changeLabelDF$newLabel[j]<-str_replace(pattern=paste(data$namespace.dataframe[i,"prefix"]),
                                               replacement=paste(prefix,sep="__"),string=changeLabelDF$oldLabel[j])
        cypher(graph,
               gsub("<<OLD>>",changeLabelDF$oldLabel[j],
                    gsub("<<NEW>>",changeLabelDF$newLabel[j],
                         queryLabelRename)))
      }
      print(paste("[manageOnotlogyPrefixe] Nombre de label changé :", dim(changeLabelDF)[1]))
    }
  }
  #-- prefixe de l'ontologie
  queryOntoPrefixCreation<-"MATCH (o:owl__Ontology{uri:'<<ONTO_URI>>'}) WITH o MERGE (n:Prefix{uri:'<<ONTO_PREFIX>>'})  ON CREATE SET n.prefix='<<ONTO_SN>>' MERGE (o)-[:hasPrefix]->(n)"
  cypher(graph ,
         gsub("<<ONTO_PREFIX>>",data$ontology.prefix,
              gsub("<<ONTO_URI>>", data$ontology.uri,
                   gsub("<<ONTO_SN>>",data$ontology.shortName,
                        queryOntoPrefixCreation)))
  )
  
}


#'@name  assignElementToOntologyNode
#'@description Create Top Element (class, property) nodes;
#'   Link them to :owl__Ontology (corresponding to ontoURI) using :intoOntology relation 
#'   and assign Element {e | ! (e -[rdfs__subClassOf relationship]->)} to Top Element (:rdfs__subClassOf)
#'@param graph object neo4J graph (library RNeo4J)
#'@param ontoURI as the result of checkNeo4JContent function
#'@return graph 
assignElementToOntologyNode<-function(graph, ontoURI){
  metanode<-data.frame(label=c("owl__Class", "owl__ObjectProperty", "owl__DataTypeProperty", "owl__AnnotationProperty"),
                       rdfs__label = c("Top Concept", "Top Object Property", "Top Data Property","Top Annotation Property"),
                       nodeID = c(NA,NA,NA,NA),
                       relType = c("rdfs__subClassOf",rep(c("rdfs__subPropertyOf"),3)))
  queryCreationNode<-"CREATE (a:<<LABEL>>{rdfs__label : '<<NAME>>'})
  RETURN id(a)"
  queryCreateOntoLink<-'MATCH (elt), (onto:owl__Ontology)
  WHERE (ID(elt)= <<NODEID>> ) AND onto.uri = "<<ONTOURI>>"
  CREATE (elt) -[r:intoOntology]->(onto)
  RETURN ID(r)'
  queryCreateLinkElementToNode<-"MATCH (e:<<LABEL>>), (topE:<<LABEL>>)
  WHERE NOT (e)-[:<<RELTYPE>>]->(:<<LABEL>>) AND NOT ID(e) = <<NODEID>> AND ID(topE)=<<NODEID>> 
  WITH e,topE
  CREATE (e)-[r:<<RELTYPE>>]->(topE)
  RETURN ID(r)"
  #création des concepts + assignation a l'ontologie + assigner les éléments de l'ontologie correspondants
  for(i in 1:dim(metanode)[1]){
    nodeID<-cypher(graph,
                   gsub( "<<NAME>>", metanode$rdfs__label[i],
                         gsub("<<LABEL>>", metanode$label[i],queryCreationNode,fixed=TRUE)
                         ,fixed = TRUE) )
    metanode$nodeID[i]<-nodeID[1,]
    #Lier à l'ontologie courante #RQ cas vide déjà traité en levé d'erreur
    if(dim(ontoURI)[1]>1){
      warnMessage<-"[Neo4J Top Level element assignation] <<LABEL>> element will assign to only one ontology (<<ONTOURI>>) "
      warning(gsub("<<LABEL>>",metanode$label[i], gsub("<<ONTOURI>>",ontoURI$onto.uri[1],warnMessage)))
    }
    rel<-cypher(graph, 
                gsub("<<NODEID>>",nodeID[1,],
                     gsub("<<ONTOURI>>",ontoURI$onto.uri[1], queryCreateOntoLink, fixed = TRUE),fixed = TRUE))
    #Assigner les concepts aux éléments existants
    assignElementRes<-cypher(graph ,
                             gsub("<<LABEL>>",metanode$label[i],
                                  gsub("<<NODEID>>",nodeID[1,],
                                       gsub("<<RELTYPE>>",metanode$relType[i],queryCreateLinkElementToNode, fixed=TRUE)
                                       ,fixed=TRUE),
                                  fixed=TRUE))
  }

return(graph)}

#'@name  trimClassSingleRestrictionPattern
#'@description Trim single restriction pattern found for node owl__Class and delete corresponding :Restriction nodes
#'@param graph object neo4J graph (library RNeo4J)
#'@return graph 
trimClassSingleRestrictionPattern<-function(graph){
  queryTrimClassSingleRestriction<-
    "MATCH (domain:owl__Class)-[x:<<TYPERELATION>>]->(r:owl__Restriction)-[z]->(b) 
  WITH domain, r, z, b
  Match (r)-[:owl__onProperty]->(prop)
  WITH domain, r, z, b, prop 
  Match (r)-[z]->(range:owl__Class)
  WITH domain, prop.rdfs__label AS relationshipType, range ,r AS toDelNode, count(r) AS restrictionCount
  CALL apoc.create.relationship(domain,relationshipType,{}, range) yield rel
  DETACH DELETE toDelNode
  RETURN count(rel) AS relationCreated, restrictionCount"
  typeRelation<-c("rdfs__subClassOf","owl__equivalentClass")
  for(rel in typeRelation){
    result<-cypher(graph,
                   gsub("<<TYPERELATION>>",rel,queryTrimClassSingleRestriction, fixed=TRUE))
  }
  return(graph)
}


#'@name  cleanPropertyHierarchy
#'@description clean ObjectPropertyHierarchy by deletion of relation between :ObjectProperty node that does not correspond to 
#'@param graph object neo4J graph (library RNeo4J)
#'@return graph 

cleanPropertyHierarchies<-function(graph){
  queryPropertyTypeClean<-"MATCH (op:<<PROPERTYTYPE>>)-[r:<<TYPEREL>>]->(op1)
DELETE r"
  propertyType<-c("owl__ObjectProperty","owl__DatatypeProperty", "owl__AnnotationProperty")
  typeRelation<-c("rdfs__domain","rdfs__range","owl__propertyChainAxiom",
                  "owl__inverseOf", "owl__propertyDisjointWith")
  for(pt in propertyType){
    for(rel in typeRelation){
      result<-cypher(graph, 
                     gsub("<<PROPERTYTYPE>>",pt,
                          gsub("<<TYPEREL>>",rel,queryPropertyTypeClean, fixed = TRUE),
                          fixed=TRUE)
      )
      print(result)
    }
  }
  return(graph)
}

#'@name  trimInstanceAnnotationOnAxiom
#'@description trim annotation on axiom pattern found for node owl__Instance and delete corresponding :Resource nodes
#'@param graph object neo4J graph (library RNeo4J)
#'@return graph 
trimInstanceAnnotationOnAxiom<-function(graph){
  queryRelationAnnotationType<-"Match (c:owl__Axiom)-[:owl__annotatedSource]->(dom:owl__NamedIndividual)-[d]->(range:owl__NamedIndividual)
WITH c,d,dom,range
  MATCH (c)-[:owl__annotatedTarget]->(range)
  RETURN DISTINCT type(d) AS TypeRelation , keys(c) AS AxiomProperties"
  
  queryMergeAnnotationAsRelationProperty<-
    'MATCH  (ax:owl__Axiom)-[:owl__annotatedSource]->(dom:owl__NamedIndividual)-[d:<<RELATION>>]->(range:owl__NamedIndividual)
  WITH ax,d,dom,range
  MATCH (ax)-[:owl__annotatedTarget]->(range)
  WHERE exists(ax.<<PROPERTY>>)
  WITH ax,d,dom,range
  SET d.<<PROPERTY>> = ax.<<PROPERTY>>
  WITH  ax
  DETACH DELETE ax
  RETURN count(ax)
  '
  q<-cypher(graph, queryRelationAnnotationType)
  if(!is.null(q)){
    for(i in 1:length(q$TypeRelation)){
      relation<-q$TypeRelation[i]
      property<-unlist(q$AxiomProperties[[i]])
      property<-property[which(property != "uri")]
      res<-cypher(graph ,
                  gsub("<<RELATION>>",relation,
                       gsub("<<PROPERTY>>",property,
                            queryMergeAnnotationAsRelationProperty, fixed =TRUE),
                       fixed=TRUE))
    }
  }
  
return(graph)
}

#'@name  assignIndividualsToClass
#'@description use label to set rdf:type relationship between individuals and class nodes
#'@param graph object neo4J graph (library RNeo4J)
#'@return graph 
assignIndividualsToClass<-function(graph){
  queryIndividualLabelsGet<-"match (c:owl__NamedIndividual) return(labels(c))"
  queryGetPrefixByName<-"match (c:Prefix) WHERE c.prefix='<<NAME>>' return c.uri"
  queryAssignIndToClass<-"match(ind:<<LABEL_CLASS>>), (class:owl__Class{uri: '<<CLASS_URI>>'})
  MERGE (ind)-[:rdf__type]->(class)
  REMOVE ind:<<LABEL_CLASS>>
  RETURN ind.uri"
  labelInd<-cypher(graph, queryIndividualLabelsGet)
  labelInd<-unique(c(unlist(labelInd)))
  labelInd<-labelInd[which(!labelInd%in% c("Resource","owl__NamedIndividual"))]
  dataAssignation<-data.frame(label = labelInd,  matrix(unlist(str_split(labelInd,pattern="__")),
                                                        nrow = length(labelInd),ncol=2,byrow=TRUE))
  colnames(dataAssignation)<-c("label","prefix","classID")                            
  dataAssignation<-cbind(dataAssignation, classURI=NA)
  prefix<-unique(dataAssignation$prefix)
  for(p in prefix){
    print(p)
    selP<-which(dataAssignation$prefix == p)
    uri<-cypher(graph, gsub("<<NAME>>", p, queryGetPrefixByName))
    print(unlist(uri))
    dataAssignation$classURI[selP]<-paste(unlist(uri), sep="", dataAssignation$classID[selP])
    
  }
  for(l in 1:dim(dataAssignation)[1]){
    cypher(graph,
           gsub("<<LABEL_CLASS>>",dataAssignation$label[l],
                gsub("<<CLASS_URI>>", dataAssignation$classURI[l],queryAssignIndToClass)))
  }
  
  
  return(graph)
}
