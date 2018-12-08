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

my_last4(A,B):-last(A,B).
my_odd5(A):-1 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_msort7(A,B):-msort(A,B).
my_len8(A,B):-length(A,B).
my_element9(A,B):-member(B,A).
my_max_list10(A,B):-max_list(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_head13([H|_],H).
my_lowercase14(A):-downcase_atom(A,A).
my_toupper15(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_last4/2).
prim(my_odd5/1).
prim(my_set6/1).
prim(my_msort7/2).
prim(my_len8/2).
prim(my_element9/2).
prim(my_max_list10/2).
prim(my_tolower11/2).
prim(my_succ12/2).
prim(my_head13/2).
prim(my_lowercase14/1).
prim(my_toupper15/2).
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
p([['f','n','c'],['A','b','D','X'],['r','g','W'],['t','W','W','C']],[['f','n'],['A','b','D'],['r','g'],['t','W','W']]).
p([['e','j','n'],['B','K','N'],['A','y','S'],['o','O','A','F']],[['e','j'],['B','K'],['A','y'],['o','O','A']]).
p([['t','X','G'],['e','t','Y']],[['t','X'],['e','t']]).
p([['F','K','t','C'],['N','s','Q','D'],['T','R','O'],['U','r','t']],[['F','K','t'],['N','s','Q'],['T','R'],['U','r']]).
p([['B','Q','W'],['t','s','m']],[['B','Q'],['t','s']]).
q([['k','o','V','z'],['j','m','T','s'],['K','b','N'],['L','L','x','p']],[['k','o','V','z'],['j','m','T'],['K','b','N'],['L','L','x']]).
q([['G','q','U','C'],['J','C','t'],['j','y','G'],['C','P','D','H']],[['G','q','U','C'],['J','C'],['j','y','G'],['C','P','D']]).
q([['k','x','X'],['F','w','u','v'],['a','C','o'],['k','z','H','z']],[['k','x','X'],['F','w','u'],['a','C','o'],['k','z','H']]).
q([['t','V','U','k'],['v','b','W'],['S','l','v','o']],[['t','V','U','k'],['v','b','W'],['S','l','v']]).
q([['w','m','w','g'],['v','x','h','a']],[['w','m','w','g'],['v','x','h']]).
