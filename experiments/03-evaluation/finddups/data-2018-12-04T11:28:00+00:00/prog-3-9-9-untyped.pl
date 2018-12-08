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
my_min_list3(A,B):-min_list(A,B).
my_set4(A):-list_to_set(A,A).
my_succ5(A,B):-succ(A,B),B =< 10.
my_toupper6(A,B):-upcase_atom(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_last10(A,B):-last(A,B).
my_even11(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_set4/1).
prim(my_succ5/2).
prim(my_toupper6/2).
prim(my_sumlist7/2).
prim(my_len8/2).
prim(my_pred9/2).
prim(my_last10/2).
prim(my_even11/1).
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
p(['H','A','H','y','L','F'],'H').
p(['e','T','M','v','X','d','e','U','q'],'e').
p(['f','C','T','y','q','y','R','k','e'],'y').
p(['W','c','U','P','b','R','X','N','W','W'],'W').
p(['X','f','f','e','W','J','L','h'],'f').
q(['D','c','D','u','j','u','m','C'],'j').
q(['n','D','w','I','i','E','m','i','Y','S','m'],'Y').
q(['b','x','j','E','E','c','v','c','I','E','j'],'x').
q(['I','h','Z','X','I','l','P'],'h').
q(['i','E','d','G','a','u','G','P','f','O','V'],'E').
