:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).


filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_set4(A):-list_to_set(A,A).
my_odd5(A):-1 is A mod 2.
my_len6(A,B):-length(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_list_to_set8(A,B):-list_to_set(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_last10(A,B):-last(A,B).
my_toupper11(A,B):-upcase_atom(A,B).
my_tolower12(A,B):-downcase_atom(A,B).
my_element13(A,B):-member(B,A).
my_flatten14(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_set4/1).
prim(my_odd5/1).
prim(my_len6/2).
prim(my_double7/2).
prim(my_list_to_set8/2).
prim(my_succ9/2).
prim(my_last10/2).
prim(my_toupper11/2).
prim(my_tolower12/2).
prim(my_element13/2).
prim(my_flatten14/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['Y','E','I'],['x','u','a','P']],[['Y','E'],['x','u','a']]).
p([['H','F','d'],['r','P','q','i'],['Y','A','M'],['i','U','Z']],[['H','F'],['r','P','q'],['Y','A'],['i','U']]).
p([['a','S','o'],['v','w','k','y']],[['a','S'],['v','w','k']]).
p([['y','n','C','p'],['J','U','X','A']],[['y','n','C'],['J','U','X']]).
p([['I','E','j'],['T','P','f'],['Z','x','c','z']],[['I','E'],['T','P'],['Z','x','c']]).
q([['o','S','v'],['H','a','P','U'],['W','x','A'],['w','t','s']],[['o','S','v'],['H','a','P'],['W','x','A'],['w','t','s']]).
q([['H','U','j','e'],['A','i','E'],['S','s','J','a'],['y','t','d']],[['H','U','j','e'],['A','i'],['S','s','J'],['y','t','d']]).
q([['K','v','n'],['a','h','P','g'],['V','d','O','V'],['b','g','x','K']],[['K','v','n'],['a','h','P','g'],['V','d','O'],['b','g','x','K']]).
q([['z','y','Y'],['j','k','A','E'],['A','n','U'],['x','t','N']],[['z','y','Y'],['j','k','A','E'],['A','n'],['x','t','N']]).
q([['Y','f','h'],['s','R','B','c']],[['Y','f'],['s','R','B','c']]).
