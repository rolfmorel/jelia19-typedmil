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

my_uppercase3(A):-upcase_atom(A,A).
my_flatten4(A,B):-flatten(A,B).
my_element5(A,B):-member(B,A).
my_even6(A):-0 is A mod 2.
my_len7(A,B):-length(A,B).
my_head8([H|_],H).
my_last9(A,B):-last(A,B).
my_set10(A):-list_to_set(A,A).
my_min_list11(A,B):-min_list(A,B).
my_msort12(A,B):-msort(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_max_list14(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_uppercase3/1).
prim(my_flatten4/2).
prim(my_element5/2).
prim(my_even6/1).
prim(my_len7/2).
prim(my_head8/2).
prim(my_last9/2).
prim(my_set10/1).
prim(my_min_list11/2).
prim(my_msort12/2).
prim(my_pred13/2).
prim(my_max_list14/2).
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
p([['s','b','r','W'],['w','c','f'],['l','X','M']],[['s','b','r'],['w','c'],['l','X']]).
p([['q','A','p'],['R','P','o']],[['q','A'],['R','P']]).
p([['L','U','V'],['J','r','y','w'],['q','q','s','X'],['k','V','G','u']],[['L','U'],['J','r','y'],['q','q','s'],['k','V','G']]).
p([['O','K','i','S'],['K','y','B','A'],['Y','r','g']],[['O','K','i'],['K','y','B'],['Y','r']]).
p([['S','j','Q','s'],['n','s','P','n'],['J','K','H','I'],['L','j','S','b']],[['S','j','Q'],['n','s','P'],['J','K','H'],['L','j','S']]).
q([['R','W','S'],['x','T','Y','C']],[['R','W'],['x','T','Y','C']]).
q([['z','K','L'],['N','M','u','D'],['p','p','f','K']],[['z','K'],['N','M','u','D'],['p','p','f','K']]).
q([['f','D','y'],['O','j','B','D'],['Y','g','S','t'],['c','H','Y']],[['f','D','y'],['O','j','B','D'],['Y','g','S'],['c','H','Y']]).
q([['V','P','d'],['s','D','F'],['F','j','Z','g']],[['V','P','d'],['s','D','F'],['F','j','Z']]).
q([['u','e','a'],['R','J','s'],['V','a','t']],[['u','e','a'],['R','J'],['V','a','t']]).
