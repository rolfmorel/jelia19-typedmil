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
my_len3(A,B):-length(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_set7(A):-list_to_set(A,A).
my_lowercase8(A):-downcase_atom(A,A).
my_flatten9(A,B):-flatten(A,B).
my_toupper10(A,B):-upcase_atom(A,B).
my_odd11(A):-1 is A mod 2.
my_reverse12(A,B):-reverse(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_succ4/2).
prim(my_last5/2).
prim(my_min_list6/2).
prim(my_set7/1).
prim(my_lowercase8/1).
prim(my_flatten9/2).
prim(my_toupper10/2).
prim(my_odd11/1).
prim(my_reverse12/2).
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
p(['B','H','d','y','g','W','U','M','U'],'U').
p(['U','t','I','E','U','U'],'U').
p(['S','m','U','c','z','S'],'S').
p(['o','m','p','C','r','G','C'],'C').
p(['X','S','O','d','d','T'],'d').
q(['N','r','R','s','i','R','d'],'N').
q(['v','v','c','B','V','G','s'],'c').
q(['g','C','X','X','Q','c','l','k'],'C').
q(['n','w','o','o','q','p'],'n').
q(['Q','U','H','C','u','Z','a','C','C','d','n'],'H').
