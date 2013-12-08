%%%%     Title:  Sentences Prolog Coursework      %%%%
%%%%     Author: Robert Speller.                  %%%%
%%%%     Date:   10/12/2012                       %%%%

/*
**********************************************************
     This program uses a basic lexicon to determine
     whether lists of words are valid sentences.
**********************************************************
*/

:-use_module(library(lists)).

/*
**********************************************************
                    The lexicon:
**********************************************************
*/

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



/*
**********************************************************
            Parts 1, 2 and 3: Sentences
**********************************************************
*/

/* A sentence is a noun phrase followed by a verb phrase.
*/
sentence(X) :-
	noun_phrase_better(NP),
	verb_phrase(VP),
	append(NP,VP,X).


/* A noun phrase is an article followed by a noun.
*/
noun_phrase([A,N]) :-
	article(A),
	noun(N).


/* A better noun phrase uses the articles 'a' and'an' correctly.
*/
noun_phrase_better([A,N]) :-
	article(A),
	A \= a,
	noun(N),
	starts_with_vowel(N).

noun_phrase_better([A,N]) :-
	article(A),
	A \= an, 
	noun(N),
	\+starts_with_vowel(N).


starts_with_vowel(W) :-
	first_character(W,X),
	vowel(X).


/* X is the first character of the atom W.
*/
first_character(W,X) :-
	atom_chars(W,[X|_]).


/* A verb phrase can be just a verb.
*/
verb_phrase([X]) :-
	verb(X).

/* A verb phrase can be a conjunction of adverbs,
   followed by a verb, followed by a noun phrase.
*/
verb_phrase(X) :-
	conjunction_of_adverbs(CA),
	verb(V),
	noun_phrase_better(NP),
	append(CA,[V],T),
	append(T,NP,X).


/* A conjunction of adverbs is a list of zero or
   more adverbs that may require commas and an 'and'.
*/
conjunction_of_adverbs(X) :-
	adverb_list(AL),
	add_commas(AL,C),
	add_and(C,X).


/* An adverb list is any permutation of any subset (including empty)
   of a list of all the adverbs in the lexicon. subseq0 and permutation
   are from the lists library.
*/
adverb_list(AL) :-
	findall(X,adverb(X),L),
	subseq0(L,SL),
	permutation(SL,AL).


/* Commas must be between all adverbs except the last pair.
*/
add_commas([],[]).
add_commas([X],[X]).
add_commas([A,B],[A,B]).
add_commas([H|[A,B|T]],X) :-
	add_commas([A,B|T],SL),
	append([H],[','],HC),
	append(HC,SL,X).


/* 'and' must be between the last pair of adverbs.
*/
add_and([],[]).
add_and([X],[X]).
add_and([A,B],[A,and,B]).
add_and([H|[A,B|T]],X) :-
	add_and([A,B|T],Z),
	append([H],Z,X).



/*
**********************************************************
                  Part 4: Actions
**********************************************************
*/

/* 'As' is a list of actions (verbs) that an actor does
   in a list of 'Text' in which there are no adverbs.
*/
actions(Actor,Text,As) :-
	setof(Verb,
	      (noun(Actor),verb(Verb),followedBy(Actor,Verb,Text)),
	      As).


/* A is followed directly by B in a list.
*/
followedBy( A, B, [A,B|_] ).
followedBy( A, B, [_|T] ) :- followedBy( A, B, T ).



/*
**********************************************************
            Part 5: Actions and Adverbs
**********************************************************
*/

/* 'As' is a list of tuples, each containing actions (verbs)
   and a list of the corresponding adverbs that match
   what an actor (noun) does.
*/
actions_and_adverbs(Actor,Text,As) :-
	setof((Verb, AL),
	      (noun(Actor),verb_and_adverb_list(Actor,Text,Verb, AL)),
	      As).


/* 'Verb' is what an 'Actor' does and AL is the list of adverbs
   describing this action. The adverbs must follow the Actor (noun).
*/
verb_and_adverb_list( Actor, [Actor|T], Verb, AL ) :-
	words_before_next_verb( T, L, Verb ),
	conjunction_of_adverbs( L ),
	delete(L,',',NC),
	delete(NC,and,AL).


/* Recurse along list if the head of the list isn't an the Actor
*/
verb_and_adverb_list( Actor, [_|T], Verb, AL ) :-
	verb_and_adverb_list( Actor, T, Verb, AL ).


/* 'L' is the list of words that proceed the next verb, 'Verb'
*/
words_before_next_verb( [Verb|_], [], Verb ) :-
	verb(Verb).

words_before_next_verb( [H|T], L , Verb) :-
	\+verb(H),
	words_before_next_verb( T, SL, Verb ),
	append([H],SL,L).


/*
**********************************************************
                  END OF FILE
**********************************************************
*/
