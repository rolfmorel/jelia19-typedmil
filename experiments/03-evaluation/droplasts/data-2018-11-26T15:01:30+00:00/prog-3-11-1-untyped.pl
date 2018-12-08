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

my_min_list3(A,B):-min_list(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_list_to_set6(A,B):-list_to_set(A,B).
my_flatten7(A,B):-flatten(A,B).
my_set8(A):-list_to_set(A,A).
my_even9(A):-0 is A mod 2.
my_max_list10(A,B):-max_list(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_toupper12(A,B):-upcase_atom(A,B).
my_last13(A,B):-last(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_min_list3/2).
prim(my_sumlist4/2).
prim(my_lowercase5/1).
prim(my_list_to_set6/2).
prim(my_flatten7/2).
prim(my_set8/1).
prim(my_even9/1).
prim(my_max_list10/2).
prim(my_tolower11/2).
prim(my_toupper12/2).
prim(my_last13/2).
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
p([['O','h','M'],['f','K','m','Y'],['A','J','g']],[['O','h'],['f','K','m'],['A','J']]).
p([['Q','W','Q','y'],['i','e','v'],['b','m','t','a'],['D','H','L']],[['Q','W','Q'],['i','e'],['b','m','t'],['D','H']]).
p([['v','f','u','a'],['I','q','s','B'],['m','p','P'],['A','t','V']],[['v','f','u'],['I','q','s'],['m','p'],['A','t']]).
p([['C','Z','F'],['H','v','j','v'],['S','a','r','i']],[['C','Z'],['H','v','j'],['S','a','r']]).
p([['G','x','j','v'],['o','j','u'],['E','d','b','S'],['X','d','Z']],[['G','x','j'],['o','j'],['E','d','b'],['X','d']]).
q([['N','a','f','X'],['W','b','r','Y']],[['N','a','f'],['W','b','r','Y']]).
q([['w','d','P'],['W','q','a','c'],['I','Y','V','x'],['l','y','I','o']],[['w','d'],['W','q','a'],['I','Y','V','x'],['l','y','I','o']]).
q([['c','M','U'],['i','E','L'],['B','D','r']],[['c','M','U'],['i','E'],['B','D','r']]).
q([['p','i','M','O'],['D','S','F'],['g','f','H','w']],[['p','i','M','O'],['D','S'],['g','f','H','w']]).
q([['h','n','V','j'],['D','I','p']],[['h','n','V','j'],['D','I']]).
