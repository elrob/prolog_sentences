:-use_module(library(lists)).

noun(grass).
noun(cow).
noun(girl).
noun(boy).
noun(apple).
noun(song).

article(the).
article(a).
article(an).

verb(eats).
verb(sings).
verb(chews).
verb(kicks).

adverb(slowly).
adverb(deliberately).
adverb(merrily).
adverb(sweetly).

vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).


first_letter(X,N) :-
	noun(N),
	atom_chars(N,[X|_]).

starts_with_vowel(N) :-
	noun(N),
	first_letter(X,N),
	vowel(X).

noun_phrase_better([A,N]) :-
	article(A),
	A \= a,
	noun(N),
	starts_with_vowel(N).

noun_phrase_better([A,N]) :-
	article(A),
	A \= an, %is 'a grass' ok?!?
	noun(N),
	\+starts_with_vowel(N).

sublist(L,SL) :-
	subseq0(L,SL),
	SL \= [].

adverb_list(AL) :-
	findall(X,adverb(X),L),
	sublist(L,SL),
	permutation(SL,AL).

add_commas([X],[X]).
add_commas([A,B],[A,B]).
add_commas([H|[A,B|T]],X) :-
	add_commas([A,B|T],SL),
	append([H],[','],HC),
	append(HC,SL,X).
	
add_and([X],[X]).
add_and([A,B],[A,and,B]).
add_and([H|[A,B|T]],X) :-
	add_and([A,B|T],Z),
	append([H],Z,X).

conjunction_of_adverbs(X) :-
	adverb_list(AL),
	add_commas(AL,C),
	add_and(C,X).

verb_phrase([X]) :-
	verb(X).

verb_phrase([V|NP]) :-
	verb(V),
	noun_phrase_better(NP).

verb_phrase(X) :-
	conjunction_of_adverbs(CA),
	verb(V),
	noun_phrase_better(NP),
	append(CA,[V],T),
	append(T,NP,X).

sentence(X) :-
	noun_phrase_better(NP),
	verb_phrase(VP),
	append(NP,VP,X).

followedBy( A, B, [A,B|_] ).
followedBy( A, B, [_|T] ) :- followedBy( A, B, T ).

actions(Actor,Text,As) :-
	findall(Verb,
		(followedBy(Actor,Verb,Text),verb(Verb)),
		As).
	