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

my_even3(A):-0 is A mod 2.
my_head4([H|_],H).
my_set5(A):-list_to_set(A,A).
my_last6(A,B):-last(A,B).
my_min_list7(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_even3/1).
prim(my_head4/2).
prim(my_set5/1).
prim(my_last6/2).
prim(my_min_list7/2).
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
p([['i','Y','r','p'],['J','c','S'],['U','h','s','b']],[['i','Y','r'],['J','c'],['U','h','s']]).
p([['w','E','R'],['t','O','d','E']],[['w','E'],['t','O','d']]).
p([['J','Y','B','a'],['D','D','n'],['u','f','J','z']],[['J','Y','B'],['D','D'],['u','f','J']]).
p([['p','L','G'],['d','T','t','M'],['d','C','O']],[['p','L'],['d','T','t'],['d','C']]).
p([['c','Z','E'],['i','J','L'],['Z','g','l','Q']],[['c','Z'],['i','J'],['Z','g','l']]).
q([['t','H','J','Q'],['i','D','L'],['Z','y','G','l']],[['t','H','J'],['i','D','L'],['Z','y','G','l']]).
q([['D','M','R','z'],['D','z','z'],['X','U','Q'],['U','d','Z']],[['D','M','R','z'],['D','z'],['X','U'],['U','d','Z']]).
q([['p','R','Q'],['V','M','r','a']],[['p','R','Q'],['V','M','r']]).
q([['Z','Y','t','D'],['H','E','I'],['j','K','D','H'],['d','S','D']],[['Z','Y','t','D'],['H','E','I'],['j','K','D','H'],['d','S']]).
q([['f','J','o'],['i','R','e']],[['f','J'],['i','R','e']]).
