<?xml version="1.0"?>


<!DOCTYPE rdf:RDF [
    <!ENTITY owl "http://www.w3.org/2002/07/owl#" >
    <!ENTITY xsd "http://www.w3.org/2001/XMLSchema#" >
    <!ENTITY owl2xml "http://www.w3.org/2006/12/owl2-xml#" >
    <!ENTITY rdfs "http://www.w3.org/2000/01/rdf-schema#" >
    <!ENTITY rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#" >
    <!ENTITY Ontology1335829483816 "http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#" >
]>


<rdf:RDF xmlns="http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#"
     xml:base="http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
     xmlns:owl2xml="http://www.w3.org/2006/12/owl2-xml#"
     xmlns:owl="http://www.w3.org/2002/07/owl#"
     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     xmlns:Ontology1335829483816="http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#">
    <owl:Ontology rdf:about=""/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Object Properties
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#hasColor -->

    <owl:ObjectProperty rdf:about="#hasColor"/>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#hasSugar -->

    <owl:ObjectProperty rdf:about="#hasSugar"/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Classes
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#Color -->

    <owl:Class rdf:about="#Color"/>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#DrySugar -->

    <owl:Class rdf:about="#DrySugar">
        <rdfs:subClassOf rdf:resource="#Sugar"/>
    </owl:Class>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#DryWine -->

    <owl:Class rdf:about="#DryWine">
        <owl:equivalentClass>
            <owl:Restriction>
                <owl:onProperty rdf:resource="#hasSugar"/>
                <owl:someValuesFrom rdf:resource="#DrySugar"/>
            </owl:Restriction>
        </owl:equivalentClass>
        <rdfs:subClassOf rdf:resource="#Wine"/>
    </owl:Class>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#Grape -->

    <owl:Class rdf:about="#Grape"/>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#Riesling -->

    <owl:Class rdf:about="#Riesling">
        <owl:equivalentClass>
            <owl:Restriction>
                <owl:onProperty rdf:resource="#hasColor"/>
                <owl:someValuesFrom rdf:resource="#White"/>
            </owl:Restriction>
        </owl:equivalentClass>
        <owl:equivalentClass>
            <owl:Restriction>
                <owl:onProperty rdf:resource="#hasSugar"/>
                <owl:allValuesFrom rdf:resource="#RieslingSugar"/>
            </owl:Restriction>
        </owl:equivalentClass>
        <rdfs:subClassOf rdf:resource="#Wine"/>
    </owl:Class>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#RieslingGrape -->

    <owl:Class rdf:about="#RieslingGrape">
        <rdfs:subClassOf rdf:resource="#Grape"/>
    </owl:Class>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#RieslingSugar -->

    <owl:Class rdf:about="#RieslingSugar">
        <rdfs:subClassOf rdf:resource="#DrySugar"/>
    </owl:Class>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#Sugar -->

    <owl:Class rdf:about="#Sugar"/>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#White -->

    <owl:Class rdf:about="#White">
        <rdfs:subClassOf rdf:resource="#Color"/>
    </owl:Class>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#WhiteWine -->

    <owl:Class rdf:about="#WhiteWine">
        <owl:equivalentClass>
            <owl:Restriction>
                <owl:onProperty rdf:resource="#hasColor"/>
                <owl:someValuesFrom rdf:resource="#White"/>
            </owl:Restriction>
        </owl:equivalentClass>
        <rdfs:subClassOf rdf:resource="#Wine"/>
    </owl:Class>
    


    <!-- http://www.semanticweb.org/ontologies/2012/3/Ontology1335829483816.owl#Wine -->

    <owl:Class rdf:about="#Wine">
        <owl:equivalentClass>
            <owl:Restriction>
                <owl:onProperty rdf:resource="#hasSugar"/>
                <owl:someValuesFrom rdf:resource="#Sugar"/>
            </owl:Restriction>
        </owl:equivalentClass>
    </owl:Class>
</rdf:RDF>



<!-- Generated by the OWL API (version 2.2.1.1138) http://owlapi.sourceforge.net -->

