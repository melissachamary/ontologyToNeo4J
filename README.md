# ontologyToNeo4J
## Purpose
R package, that aim to rebuild ontology in a neo4J graph. We assum that some ontology semantics will be lost in conversion in order to facilitate graph traversal.
### dependancies
#### Neo4J 3.47
neosemantics-3.4.0.2.jar
apoc-3.4.0.3-all.jar
#### R Cran
RNeo4j
RCurl
redland 
jsonlite
stringr
redland
### Version 1 description (current)
First version, contains function enables to import multiple ontologies (A-Box and T-Box). Each T-Box __Element__ is represented as a node in the graph (Concept, ObjectProperty, DataTypeProperty, AnnotationProperty). __Property__ (ObjectProperty, DataTypeProperty, AnnotationProperty) are considered as __Graph Relation and Property__ when there are used to describe concept or individuals. 
* Import ontology file and check graph created
* Manage ontology prefix and specify which ontology contains node Element 
* Clean unecessary information about Property Nodes as domain,range etc.
* Simplify simple restriction pattern for Concept Nodes __/!\  semantic lost(rdfs:subClassOf, owl:equivalentClass)__
* Simplify Annotation on relationship between individuals

### Upcomming version
Functionality to implement in further version
* Ontology set import (with ontology dependancy management)
* Clean extension (on Element Node)
* Custom label node assignation
* Simplify complex restriction pattern for Concept Nodes __/!\ semantic lost (Union, Disjointness, Intersection, Restriction Group)__

