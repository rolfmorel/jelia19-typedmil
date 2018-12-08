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
my_reverse3(A,B):-reverse(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_flatten5(A,B):-flatten(A,B).
my_even6(A):-0 is A mod 2.
my_odd7(A):-1 is A mod 2.
my_succ8(A,B):-succ(A,B),B =< 10.
my_len9(A,B):-length(A,B).
my_last10(A,B):-last(A,B).
my_msort11(A,B):-msort(A,B).
my_max_list12(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_sumlist4/2).
prim(my_flatten5/2).
prim(my_even6/1).
prim(my_odd7/1).
prim(my_succ8/2).
prim(my_len9/2).
prim(my_last10/2).
prim(my_msort11/2).
prim(my_max_list12/2).
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
p(['c','M','d','r','M','c','H','l'],'M').
p(['J','o','t','n','j','t','A'],'t').
p(['z','d','d','w','Z','z','k','z','b'],'d').
p(['l','r','j','d','r','B','D','e'],'r').
p(['B','H','I','d','d','c','L','Q','Q','j'],'Q').
q(['a','u','y','Y','r','y'],'Y').
q(['u','f','E','O','f','d','h','U','y','f'],'h').
q(['i','O','Z','R','U','p','W','R'],'i').
q(['s','T','j','T','a','B'],'s').
q(['z','t','z','p','k','t','q','f','d','W'],'q').
