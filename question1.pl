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

noun_phrase(X) :-
	article(A),
	noun(N),
	append([A],[N],X).

verb_phrase([X]) :-
	verb(X).

verb_phrase(X) :-
	verb(V),
	noun_phrase(NP),
	append([V],NP,X).

sentence(X) :-
	noun_phrase(NP),
	verb_phrase(VP),
	append(NP,VP,X).