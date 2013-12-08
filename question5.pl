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

adverb_list(AL) :-
	findall(X,adverb(X),L),
	subseq0(L,SL),
	permutation(SL,AL).

add_commas([],[]).
add_commas([X],[X]).
add_commas([A,B],[A,B]).
add_commas([H|[A,B|T]],X) :-
	add_commas([A,B|T],SL),
	append([H],[','],HC),
	append(HC,SL,X).

add_and([],[]).
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

next_verb( [Verb|_], Verb ) :-
	verb(Verb).
next_verb( [H|T], Verb ) :-
	\+verb(H),
	next_verb(T,Verb).

words_before_next_verb( [Verb|_], [], Verb ) :-
	verb(Verb).
words_before_next_verb( [H|T], L , Verb) :-
	\+verb(H),
	words_before_next_verb( T, SL, Verb ),
	append([H],SL,L).

verb_and_adverb_list( Actor,[Actor|T], Verb, AL ) :-
	words_before_next_verb( T, L, Verb ),
	conjunction_of_adverbs( L ),
	delete(L,',',NC),
	delete(NC,and,AL).
				
verb_and_adverb_list( Actor, [_|T], Verb, AL ) :-
	verb_and_adverb_list( Actor, T, Verb, AL ).

actions(Actor,Text,As) :-
	setof(Verb,
	      (followedBy(Actor,Verb,Text),verb(Verb)),
	      As).
	
actions_and_adverbs(Actor,Text,As) :-
	setof((Verb, AL),
	      verb_and_adverb_list(Actor,Text,Verb, AL),
	      As).