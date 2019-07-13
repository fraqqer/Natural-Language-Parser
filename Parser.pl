adj(avid).
adj(good).
adj(long).
adj(old).
adj(racing).
adj(social).
adj(sprightly).
adj(teenage).
adj(young).
det(a).
det(an).
noun(book).
noun(boy).
noun(car).
noun(chat).
noun(father).
noun(grandfather).
noun(guitar).
noun(horses).
noun(person).
noun(petrolhead).
noun(student).
noun(walk).
verb(likes).
verb(loves).

recommend(likes, book, 'He joins the book club.').
recommend(loves, horses, 'They join a riding club.').
recommend(loves, walk, 'He joins a rambling club.').
recommend(likes, chat, 'They join a social club.').
recommend(likes, guitar, 'They should join a band.').
recommend(loves, car, 'They should go to the races.').

agent :-
    perceive(Percepts),
    action(Percepts).

perceive(ParsedOutput) :-
    read(X),
    sentence(X, ParsedOutput),

    write('sentence'), nl,
    tab(6), write('|'), nl,
    tab(6), write('|- '), write(X), nl,
    tab(6), write('|'), nl,
    tab(6), write('|- '), write('produces the recommendation that'), nl,
    tab(6), write('|'), nl.

action(X) :-
    % extract verb and noun from sentences. noun requires degree of specificity depending on the structure of the verb phrase
    X = sentence(_, vp(VP)), VP = vp(verb(Verb), _),
    X = sentence(_, vp(VP2)), (VP2 = vp(_, np2(noun(Noun))) ; VP2 = vp(_, np(_, np2(noun(Noun)))) ; VP2 = vp(_, np(_, np2(_, np2(noun(Noun)))))),

    % use verb/noun variables to produce an output for the bot
    recommend(Verb, Noun, Recommendation),
    tab(6), write('|- '), write('\''), write(Recommendation), write('\'').

%                                             GRAMMAR PARSER

% sentence -> noun phrase + verb phrase
% (first arg: input // second arg: output)
sentence(Sentence, sentence(np(Noun_Phrase), vp(Verb_Phrase))) :-
    write('sentence'), tab(1), write('|'), nl,
    tab(9), write('|'), nl,
    tab(9), write('|--- noun phrase'), nl,
    np(Sentence, Noun_Phrase, Rem),
    tab(9), write('|'), nl,
    tab(9), write('|'), nl,
    tab(9), write('|--- verb phrase'), nl,
    vp(Rem, Verb_Phrase).

% sentence -> verb phrase
sentence(Sentence, sentence(vp(Verb_Phrase))) :-
    write('sentence'), tab(1), write('|'), nl,
    tab(9), write('|'), nl,
    tab(9), write('|'), nl,
    tab(9), write('|--- verb phrase'), nl,
    vp(Sentence, Verb_Phrase).

% noun phrase -> determiner/article + noun phrase
np([X|T], np(det(X), NP2), Rem) :-
    det(X),
    tab(14), write('|- determiner ('), write(X), write(')'), nl,
    tab(19), write('-> [call] np2 ('), write(T), write(')'), nl,
    np2(T, NP2, Rem).

% noun phrase -> noun phrase 2
np(Sentence, Parse, Rem) :-
    tab(14), write('|- np2'), nl,
    np2(Sentence, Parse, Rem).

% noun phrase -> noun phrase + prepositional phrase
np(Sentence, np(NP, PP), Rem) :-
    tab(14), write('|- np'), nl,
    np(Sentence, NP, Rem1),
    pp(Rem1, PP, Rem).

% noun phrase -> noun
np2([H|T], np2(noun(H)), T) :-
    noun(H),
    tab(14), write('|- noun ('), write(H), write(')'), nl.

% noun phrase -> adjective
np2([H|T], np2(adj(H), Rest), Rem) :-
    tab(14), write('|- adjective ('), write(H), write(')'), nl,
    adj(H),
    tab(19), write('-> [call] np2 ('), write(T), write(')'), nl,
    np2(T, Rest, Rem).

% noun phrase -> preposition
pp([H|T], pp(prep(H), Parse), Rem) :-
    tab(14), write('|- preposition'), nl,
    prep(H),
    tab(19), write('-> [call] np ('), write(T), write(')'), nl,
    np(T, Parse, Rem).

% verb phrase -> verb
vp([H|[]], verb(H)) :-
    tab(14), write('|- verb ('), write(H), write(')'), nl,
    verb(H).

% verb phrase -> verb + noun phrase
vp([H|Rest], vp(verb(H), RestParsed)) :-
    tab(14), write('|- verb ('), write(H), write(')'), nl,
    verb(H),
    tab(19), write('-> [call] np ('), write(Rest), write(')'), nl,
    np(Rest, RestParsed, _).

% verb phrase -> verb phrase + preposition
vp([H|Rest], vp(verb(H), RestParsed)) :-
    tab(14), write('|- verb'), nl,
    verb(H),
    tab(19), write('-> [call] pp ('), write(Rest), write(')'), nl,
    pp(_, Rest, RestParsed).
