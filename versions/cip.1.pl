% Author:  Bruce Spencer
% Date: 4/7/2012 started
% 4/18/2012 structural transformation working
% cip is condor in prolog, a very rough prototype to gain experience with the condor system
%:- cd('p:/cip').

% Thea is used to load OWL ontologies so we can handle them as a set of axioms.
file_search_path(library, 'p:/thea').
file_search_path(library, '/Volumes/spencerb$/thea').
:- use_module(library('thea2/owl2_io')). %Assumes thea2 is on the library path
:- use_module(library('thea2/owl2_model')).

verboseOutput:-fail.

run(OWLFileIn) :-
   owl2_model:retract_all_axioms,
   owl2_model:
   load_axioms(OWLFileIn, owl),
   (verboseOutput ->
      aggregate_all(count, owl2_model:subClassOf(_,_), Owl2SubClassCount),
      writef('OWL2 SubClass Axoims count: %w\n', [Owl2SubClassCount]),
      aggregate_all(count, owl2_model:equivalentClasses(_), Owl2EquivalentCount),
      writef('OWL2 Equivalent Axoims count: %w\n', [Owl2EquivalentCount]),
      aggregate_all(count, owl2_model:disjointClasses(_), Owl2DisjointCount),
      writef('OWL2 Disjoint Axoims count: %w\n', [Owl2DisjointCount]),
      aggregate_all(count, owl2_model:subPropertyOf(_,_), Owl2SubPropertyCount),
      writef('OWL2 SubProperty Axioms count: %w\n', [Owl2SubPropertyCount])
      ; true),
   unlistAxioms,
   replaceNegUniversalOccurrences,
   (checkNegUniversalOccurrences -> true ;
      writef('Negative Universal Occurrence Check failed\n', []), fail),
     (verboseOutput ->
        aggregate_all(count, rnuSubClassOf(_,_), RNUSubClassCount),
        writef('Removed Neg Universal SubClass Axoims count: %w\n', [RNUSubClassCount])
        ; true),
   stTrans,
   (verboseOutput ->
      aggregate_all(count, structTransSubClassOf(_,_), STTransSubClassCount),
      writef('Struct Trans SubClass Axoims count: %w\n', [STTransSubClassCount])
      ; true),
   normalize,
   (normalizedCheck -> true; 
      writef('Normalized Axoims Check failed.\n', []), fail),
   (verboseOutput ->
      aggregate_all(count, normSubClassOf(_,_), NormSubClassCount),
      writef('Normalized SubClass Axoims count: %w\n', [NormSubClassCount])
      ; true),
   true.

%unlistAxioms changes the union and intersection lists into formulae

unlistAxioms :-
   retractall(cipSubClassOf(_,_)),
   owl2_model:subClassOf(C, D),
   convertUnionsIntersections(C, C1),
   convertUnionsIntersections(D, D1),
   assert(cipSubClassOf(C1, D1)),
   fail.
unlistAxioms :-
   owl2_model:equivalentClasses(L), %Thea's equivalentClasses are always pairs
   append(_, [C | L1], L), member(D, L1),
   convertUnionsIntersections(C, C1),
   convertUnionsIntersections(D, D1),
   assert(cipSubClassOf(C1, D1)),
   assert(cipSubClassOf(D1, C1)),
   fail.
unlistAxioms:-
   owl2_model:disjointClasses(L),
   append(_, [C | L1], L), member(D, L1),
   convertUnionsIntersections(C, C1),
   convertUnionsIntersections(D, D1),
   assert(cipSubClassOf(intersectionOf(C1, D1), 'owl:Nothing')),
   fail.
unlistAxioms.

convertUnionsIntersections(Formula, FC):-
    Formula = intersectionOf(Cs),
    convertIntersection(Cs, FC).
convertUnionsIntersections(Formula, FC):-
    Formula = unionOf(Cs),
    convertUnion(Cs, FC).
convertUnionsIntersections(Formula, FC):-
    Formula = someValuesFrom(R, C),
    convertUnionsIntersections(C, C1),
    FC = someValuesFrom(R, C1).
convertUnionsIntersections(Formula, FC):-
    Formula = allValuesFrom(R, C),
    convertUnionsIntersections(C, C1),
    FC = allValuesFrom(R, C1).
convertUnionsIntersections(Formula, FC):-
    Formula = complementOf(C),
    convertUnionsIntersections(C, C1),
    FC = complementOf(C1).
convertUnionsIntersections(C, C):-
    owl2_model:class(C).

convertUnion([C1, C2], unionOf(C1C, C2C)):-
   convertUnionsIntersections(C1, C1C),
   convertUnionsIntersections(C2, C2C).
convertUnion([C1, C2, C3 | CR], unionOf(C1C, CRU)):-
   convertUnionsIntersections(C1, C1C),
   convertUnion([C2, C3 | CR], CRU).

convertIntersection([C1, C2], intersectionOf(C1C, C2C)):-
   convertUnionsIntersections(C1, C1C),
   convertUnionsIntersections(C2, C2C).
convertIntersection([C1, C2, C3 | CR], intersectionOf(C1C, CRU)):-
   convertUnionsIntersections(C1, C1C),
   convertIntersection([C2, C3 | CR], CRU).

complement(complementOf(C), C):- !.
complement(C, complementOf(C)).
   
complementarySign(pos, neg).
complementarySign(neg, pos).

% Polarity of Occurrence
polarityOfOccurrence(C, C, pos) :-
   classFormula(C).
polarityOfOccurrence(C, Formula, Sign) :-
   (
    Formula = intersectionOf(C1, _);
    Formula = intersectionOf(_, C1);
    Formula = unionOf(C1, _);
    Formula = unionOf(_, C1);
    Formula = someValuesFrom(_, C1);
    Formula = allValuesFrom(_, C1);
    Formula = subClassOf(_, C1)
   ),
   polarityOfOccurrence(C, C1, Sign).
polarityOfOccurrence(C, Formula, Sign) :-
   (
    Formula = subClassOf(C1, _D);
    Formula = complementOf(C1)
    ),
   polarityOfOccurrence(C, C1, OtherSign),
   complementarySign(Sign, OtherSign).
   
classFormula(Formula) :-
    owl2_model:class(Formula);
    Formula = complementOf(_);
    Formula = intersectionOf(_, _);
    Formula = unionOf(_, _);
    Formula = someValuesFrom(_, _);
    Formula = allValuesFrom(_, _).
    
polarityCheck(Class, subClassOf(C, D), Sign) :-
   cioSubClassOf(C, D),
   polarityOfOccurrence(Class, subClassOf(C, D), Sign).

% Replace Negative Universal Occurrences
replaceNegUniversalOccurrences:-
   retractall(rnuSubClassOf(_,_)),
   cipSubClassOf(C, D),
   replaceNegUniversalOccurrences(subClassOf(C, D), pos, subClassOf(C1, D1)),
   assert(rnuSubClassOf(C1, D1)),
   fail.
replaceNegUniversalOccurrences.
   
   
% Replace Occurrence
%replaceNegUniversalOccurrence(Formula, Sign, ResultFormula)
%replace any negtively occurring allValuesFrom(R, C) in Formula, by the logically equivalent complementOf(someValuesFrom(R, complement(C1)))
% resulting in ResultFormula.   The Sign is the polarity of the occurence of Formula in the axiom where it occurs.
% Note that C1 is the logical equivalent of C, but with its inner negative universals replaced.

replaceNegUniversalOccurrences(NamedClass, _Sign, NamedClass):-
    owl2_model:class(NamedClass).
    
replaceNegUniversalOccurrences(
   allValuesFrom(R, C),
   Sign, Result) :-
   replaceNegUniversalOccurrences(C, Sign, CResult),
   (Sign = neg ->
      Result = complementOf(someValuesFrom(R, complementOf(CResult)))
   ;
      Result = allValuesFrom(R, CResult)
   ).

replaceNegUniversalOccurrences(
    intersectionOf(C1, D1),
       Sign, ResultFormula) :-
    replaceNegUniversalOccurrences(C1, Sign, C1Result),
    replaceNegUniversalOccurrences(D1, Sign, D1Result),
    ResultFormula = intersectionOf(C1Result, D1Result).

replaceNegUniversalOccurrences(
    unionOf(C1, D1),
       Sign, ResultFormula) :-
    replaceNegUniversalOccurrences(C1, Sign, C1Result),
    replaceNegUniversalOccurrences(D1, Sign, D1Result),
    ResultFormula = unionOf(C1Result, D1Result).

replaceNegUniversalOccurrences(
    someValuesFrom(R, C1),
       Sign, ResultFormula) :-
    replaceNegUniversalOccurrences(C1, Sign, C1Result),
    ResultFormula = someValuesFrom(R, C1Result).

replaceNegUniversalOccurrences(
    subClassOf(C1, D1),
        Sign, ResultFormula) :-
    complementarySign(Sign, OtherSign),
    replaceNegUniversalOccurrences(C1, OtherSign, C1Result),
    replaceNegUniversalOccurrences(D1, Sign, D1Result),
    ResultFormula = subClassOf(C1Result, D1Result).

replaceNegUniversalOccurrences(
    complementOf(C1),
        Sign, ResultFormula) :-
   complementarySign(Sign, OtherSign),
   replaceNegUniversalOccurrences(C1, OtherSign, C1Result),
   ResultFormula = complementOf(C1Result).

%checkOccurrenceUniversal is a test procedure that allows to check the
% Sign of any universal restriction in an rnuSubClassOf axiom.
checkNegUniversalOccurrences :-
   \+universalOccurrence(_, _, neg).
universalOccurrence(X, subClassOf(C, D), Sign) :-
   rnuSubClassOf(C, D),
   X = allValuesFrom(_, _),
   polarityOfOccurrence(X, subClassOf(C, D), Sign).

occurrence(Class, Sign) :-
	rnuSubClassOf(C, D), polarityOfOccurrence(Class, subClassOf(C, D), Sign).
   
   
%Structure Transformation
stTrans :-
   retractall(structTransSubClassOf(_,_)),
   setof(occurrence(Class, Sign), occurrence(Class, Sign), Occurrences),
   length(Occurrences, Length), write(Occurrences), nl, write(Length), nl,
   member(occurrence(Class, Sign), Occurrences),
   (owl2_model:class(Class) ->
       STClass = Class;
    Class = 'owl:Thing' ->
       STClass = st('owl:Thing', 'owl:Thing');
    Class = 'owl:Nothing' ->
       STClass = st('owl:Nothing', 'owl:Nothing');
    Class = complementOf(CompFormula) ->
       with_output_to(atom(CompFormulaName), write(CompFormula)),
       STClass = complementOf(st(CompFormulaName, CompFormula));
    Class = intersectionOf(C1, D1) ->
       with_output_to(atom(C1Name), write(C1)),
       with_output_to(atom(D1Name), write(D1)),
       STClass = intersectionOf(st(C1Name, C1), st(D1Name, D1));
    Class = unionOf(C1, D1) ->
       with_output_to(atom(C1Name), write(C1)),
       with_output_to(atom(D1Name), write(D1)),
       STClass = unionOf(st(C1Name, C1), st(D1Name, D1));
    Class = someValuesFrom(R, C1) ->
       with_output_to(atom(C1Name), write(C1)),
       STClass = someValuesFrom(R, st(C1Name, C1));
    Class = allValuesFrom(R, C1) ->
       with_output_to(atom(C1Name), write(C1)),
       STClass = allValuesFrom(R, st(C1Name, C1));
    %else raise an alert
       writef('CIP Structural Transformation: Unknown structure %w\n', [Class])
       ),
    with_output_to(atom(ClassName), write(Class)),
    StClassName = st(ClassName, Class),
    (Sign = pos ->
       (ClassName \= STClass ->
          assert(structTransSubClassOf(StClassName, STClass))
       ;
          true
       )
    ;
     %Sign = neg
       (ClassName \= STClass ->
           assert(structTransSubClassOf(STClass, StClassName))
       ;
          true
       )
     ),
     fail.
stTrans :-
    rnuSubClassOf(C, D),
    with_output_to(atom(CName), write(C)),
    with_output_to(atom(DName), write(D)),
    (CName \= DName ->
        assert(structTransSubClassOf(st(CName, C), st(DName, D)));
        true),
    fail.
stTrans.


%Normalization
normalize :-
   retractall(normSubClassOf(_,_)),
   structTransSubClassOf(C, D),
   (D = intersectionOf(D1, D2) ->
      assert(normSubClassOf(C, D1)),
      assert(normSubClassOf(C, D2));
   C = unionOf(C1, C2) ->
      assert(normSubClassOf(C1, D)),
      assert(normSubClassOf(C2, D));
    C = normSubClassOf(st(_, 'owl:Thing'), 'owl:Thing') ->
       true;
    C = normSubClassOf('owl:Nothing', st(_, 'owl:Nothing')) ->
       true;
    C = st(_, complementOf(CC)), D = complementOf(st(CCName, CC)) ->
      assert(normSubClassOf(intersectionOf(C, st(CCName, CC)), 'owl:Nothing'));
    C = complementOf(st(CCName, CC)), D = st(_, complementOf(CC)) ->
      assert(normSubClassOf('owl.Thing', unionOf(D, st(CCName, CC))));
    %else
      assert(normSubClassOf(C, D))
   ),
   fail.
normalize.

portray(st(Name, _)) :-
   write('$'), write(Name), write('$').

findNotNormalized(subClassOf(C, D)) :-
   normSubClassOf(C, D),
   \+normalized(subClassOf(C, D)), 
   write('Not Normalized: '), write(subClassOf(C, D)), nl.
normalizedCheck :-
   \+ findNotNormalized(_).

normalized(subClassOf(C, D)) :-
   intersectionOfAtoms(C), unionOfAtoms(D).
normalized(subClassOf(C, D)) :-
   D = someValuesFrom(_R, B), cipAtom(C), cipAtom(B).
normalized(subClassOf(C, D)) :-
   C = someValuesFrom(_R, B), cipAtom(D), cipAtom(B).
normalized(subClassOf(C, D)) :-
   D = allValuesFrom(_R, B), cipAtom(C), cipAtom(B).
   % or R subProperty S, ignoring this for now.

cipAtom(A):-atom(A).
cipAtom(st(_, _)).
cipAtom('owl:Thing').
cipAtom('owl:Nothing').

intersectionOfAtoms(intersectionOf(C, D)) :-
   intersectionOfAtoms(C),
   intersectionOfAtoms(D).
intersectionOfAtoms(A) :-
   cipAtom(A).
unionOfAtoms(unionOf(C, D)) :-
   unionOfAtoms(C),
   unionOfAtoms(D).
unionOfAtoms(A) :-
   cipAtom(A).

% RA

%hornSaturate(ToDo, H, Processed) 
% runs the horn ALCH rules from Simancik, Kazakov and Horrocks paper: Consequence-Based Reasoning beyong Horn Ontologies
% accesses the normSubClassOf axioms as side conditions, processes axioms in ToDo, creates the list Processed, in the context of H
% all axioms in the 

/* done */
hornSaturate([], _H, Processed).

/* R^N_sqcap */
hornSaturate([Ax |
hornSaturate([Ax1 | ToDo], H, Processed) :-
	normSubClassOf(intersectionOf(As), C),






