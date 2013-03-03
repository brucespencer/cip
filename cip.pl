% Author:  Bruce Spencer
% Date: 4/7/2012 started
% 4/18/2012 structural transformation working
% 5/6/2012 normalization working
% 5/13/2012 classification working
% cip is condor in prolog, a very rough prototype to gain experience with the condor system
%:- cd('p:/cip').

% Thea is used to load OWL ontologies so we can handle them as a set of axioms.
file_search_path(library, 'p:/thea').
file_search_path(library, '/Volumes/spencerb$/thea').
:- use_module(library('thea2/owl2_io')). %Assumes thea2 is on the library path
:- use_module(library('thea2/owl2_model')).


verboseOutput :- fail.

run(OWLFileIn) :-
   owl2_model:retract_all_axioms,
   owl2_model:load_axioms(OWLFileIn, owl),
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
   (checkNegUniversalOccurrences -> true; 
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
      ; true)
   ;true.
   %interactive.



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
   %% length(Occurrences, Length), write(Occurrences), nl, write(Length), nl,
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

portray(st(Name, _)) :- write(Name).

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

%hornSaturate = hs(SaturateIn, SaturateOut) 
% runs the horn ALCH rules from Simantik, Kazakov and Horrocks paper: Consequence-Based Reasoning beyong Horn Ontologies
% accesses the normSubClassOf axioms as side conditions, processes axioms in Saturate, selects one, tries a rule, 
%if the axiom to be added is not a new axiom, dont add it, try something else. Otherwise add it and start again.
%We take the context H as the initial saturate, thus implementing R_A.
% We assume that each member S of Saturate is an axiom of the form H subClassOf S.

%Initialize
% Also R_A 


% hs(H, S) means H is an intersection of atoms, providing the context and S is the list of all superconcepts of H.
/* R_A */
hs(H, S) :-
        insertConjunctsIntoSaturate(H, H, [], Todo),
        hs(Todo, Todo, S).
% hs(Todo, S1, S0) means for the list of subclass axioms Todo which have yet to be processed and the list S1 which is the saturate in
% the list S0 which is the saturate out
/* R^N_sqcap and R^N_\exists */
hs([subs(H, A) | Todo], S1, S0) :- 
        cipAtom(A),
        findall(subs(H, C),
                ontologyAxiomType1or2InContext(H, S1, C), Subs),
        setsubtract(Subs, S1, New),
        union(New, Todo, Todo1),
        append(New, S1, S2),
        hs(Todo1, S2, S0).
hs([subs(H, someValuesFrom(R, K)) | Todo], S1, S0) :-
        % R^-_\exist
        insertConjunctsIntoSaturate(K, K, [], KS1),
        union(KS1, S1, S2),
        hs(KS1, S2, S3),
        findall(subs(H, B),
                ontologyAxiomType3InContext(K, S3, R, B), Subs1),
        setsubtract(Subs1, S3, New),
        union(New, Todo, Todo1),
        append(New, S3, S4),
        % R^\bot_\exist
        (intersectionInSaturateOfContext('owl.Nothing', K, KS1) ->
           union(subs(H, 'owl.Nothing'), S4, S5)
           ;
           S4 = S5
        ),
        
        % R_\forall
        findall(subs(H, someValuesFrom(R2, Con)),
              (ontologyAxiomType4InContext(H, S5, R2, B2), intersectionOfIntersections(K, B2, Con)),
              Subs2),
        
        setsubtract(Subs2, S5, New2),
        union(New2, Todo1, Todo2),
        append(New2, S5, S6),
        hs(Todo2, S6, S0).

        
hs([], Out, Out).

%An ontology axiom with atom conditions in the context of H with conclusion C which is an atom, Top, Bottom, or existsRB where B is an atom
%Type1 ontology axioms are intersection of A subclass B
%Type2 ontology axioms are A subclass exist R B
ontologyAxiomType1or2InContext(H, Saturate, C):- 
        normSubClassOf(Intersect, C),
        cipAtom(C),
        intersectionInSaturateOfContext(Intersect, H, Saturate).
ontologyAxiomType1or2InContext(H, Saturate, C):- 
        normSubClassOf(A, C),
        cipAtom(A),
        C=someValuesFrom(_R, B), cipAtom(B),
        intersectionInSaturateOfContext(A, H, Saturate).
%Type3 ontology axioms are exists R.A subclass B
ontologyAxiomType3InContext(K, Saturate, R, B):- 
        normSubClassOf(someValuesFrom(S, A), B),
        cipAtom(A),
        cipAtom(B),
        subRoleOf(R, S),
        intersectionInSaturateOfContext(A, K, Saturate).
%Type4 ontology axioms are A subclass all S.B
ontologyAxiomType4InContext(H, Saturate, R, B):- 
        normSubClassOf(A, allValuesFrom(S, B)),
        cipAtom(A),
        cipAtom(B),
        subRoleOf(R, S),
        intersectionInSaturateOfContext(A, H, Saturate).

%intersectionSubsetInContext(Intesection, H, List) is true if each member of the intersection is represented by a member subs(H, C)
intersectionInSaturateOfContext(Atom, H, S):-
        cipAtom(Atom),
        member(subs(H1, Atom), S),
        subContext(H1, H).
intersectionSubsetInContext(intersectionOf(I1, I2), H, S):-
        intersectionSubsetInContext(I1, H, S),
        intersectionSubsetInContext(I2, H, S).
subContext(C, Atom) :-
        cipAtom(Atom),
        intersectionMember(Atom, C).
subContext(C, intersectionOf(A, B)):-
        subContext(C, A),
        subContext(C, B).
intersectionMember(M, M).
intersectionMember(M, intersectionOf(I1, I2)) :-
        intersectionMember(M, I1);
        intersectionMember(M, I2).

% subRole is not implemented
subRoleOf(R, R).



insertConjunctsIntoSaturate(A, H, L, [subs(H, A) | L]):-
        cipAtom(A).
insertConjunctsIntoSaturate(intersectionOf(E1, E2), H, L1, L0 ):-
        insertConjunctsIntoSaturate(E1, H, L1, L2),
        insertConjunctsIntoSaturate(E2, H, L2, L0).

union([], X, X).
union([X | L1], L2, [X | L3]):-
        \+member(X, L2),!,
        union(L1, L2, L3).
union([_X | L1], L2, L3):-
        %member(X, L2),
        union(L1, L2, L3).
intersectionOfIntersections(X, Y, Z):-
        cipAtom(X),
        (memberOfIntersection(X, Y) ->
         Z = Y;
         Z = intersectionOf(X, Y)).
intersectionOfIntersections(intersectionOf(X1, X2), Y1, Y0):-
        intersectionOfIntersections(X2, Y1, Y2),
        intersectionOfIntersections(X1, Y2, Y0).
memberOfIntersection(X, X).
memberOfIntersection(X, intersection(I1, I2)) :-
        memberOfIntersection(X, I1);
        memberOfIntersection(X, I2).

setsubtract([], _S, []).
setsubtract([E | S1], S2, S3) :-
        member(E, S2),!,
        setsubtract(S1, S2, S3).
setsubtract([E | S1], S2, [E | S3]):-
        %\+member(E, S2),
        setsubtract(S1, S2, S3).

printAxioms([]) :- nl.
printAxioms([subs(Left, Right) | Axioms]) :-
        cipAtom(Left),
        cipAtom(Right),
        format('~w subClassOf ~w~n', [Left, Right]),
        printAxioms(Axioms).
printAxioms([_| Axioms]) :-
        printAxioms(Axioms).

% Interactive Queries
interactive :-
        format('l (list classes), f (new file),  q (quit) or class name: '),flush_output,
        current_input(Stream), read_line_to_codes(Stream, Line),
        append(Line, [39], Line1), 
        read_from_chars([39 | Line1], Choice),
        takeAction(Choice).
takeAction(l) :- 
        foreach(owl2_model:class(X), format('~w~n', [X]) ), interactive.
takeAction(q).
takeAction(f) :-
        format('Enter file name: '), flush_output,
        current_input(Stream), read_line_to_codes(Stream, Line),
        append(Line, [39], Line1),
        read_from_chars([39 | Line1], FileName), 
        succeedOnce(run(FileName)),
        interactive.
takeAction(ClassName) :-
        % with_output_to(atom(ClassName), write(Choice)),
        owl2_model:class(ClassName),!,
        STC = st(ClassName, ClassName),
        format('Superclasses of ~p:~n', [ClassName]),
        (hs(STC, Saturate) -> 
            foreach((member(subs(STC, S), Saturate), S = st(_SName, SOrig), cipAtom(SOrig)),
            format('~p~n', [S])),
            interactive;
            %else
            format('Saturation failed. Sorry. Complain to Bruce. ~n'), interactive
        ).

takeAction(_) :-
        format('Not a known class.~n'), 
        interactive.

succeedOnce(X) :- X, !.
succeedOnce(_).
