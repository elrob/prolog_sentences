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

noun_phrase_better(X) :-
	article(A),
	A \= a,
	noun(N),
	starts_with_vowel(N),
	append([A],[N],X).

noun_phrase_better(X) :-
	article(A),
	A \= an, %is 'a grass' ok?!?
	noun(N),
	\+starts_with_vowel(N),
	append([A],[N],X).



%%% noun_phrase(X) :-
%%% 	article(A),
%%% 	noun(N),
%%% 	append([A],[N],X).

verb_phrase([X]) :-
	verb(X).

verb_phrase(X) :-
	verb(V),
	noun_phrase_better(NP),
	append([V],NP,X).

sentence(X) :-
	noun_phrase_better(NP),
	verb_phrase(VP),
	append(NP,VP,X).