:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
%metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_element2(A,B):-member(B,A).
my_flatten3(A,B):-flatten(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_set5(A):-list_to_set(A,A).
my_min_list6(A,B):-min_list(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_lowercase4/1).
prim(my_set5/1).
prim(my_min_list6/2).
prim(my_pred7/2).
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
p(['D','s','F','o','H','n','g','H','s','N'],'s').
p(['D','W','J','J','z','a','t','S','a'],'a').
p(['B','f','w','f','Z'],'f').
p(['l','f','n','R','l'],'l').
p(['v','D','T','j','j'],'j').
q(['k','W','q','W','r','T','U','O','Q'],'O').
q(['C','k','u','r','X','I','U','G','k'],'u').
q(['V','L','V','C','h','O','S','R'],'C').
q(['l','Z','Z','S','b','X','G','u','H','P'],'l').
q(['f','S','f','K','p','G','d','y','R','g','f'],'R').
