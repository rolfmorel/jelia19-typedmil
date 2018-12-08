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
my_pred3(A,B):-succ(B,A),A > 0.
my_len4(A,B):-length(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_flatten7(A,B):-flatten(A,B).
my_max_list8(A,B):-max_list(A,B).
my_set9(A):-list_to_set(A,A).
my_reverse10(A,B):-reverse(A,B).
my_last11(A,B):-last(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_min_list13(A,B):-min_list(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_tolower15(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_pred3/2).
prim(my_len4/2).
prim(my_sumlist5/2).
prim(my_list_to_set6/2).
prim(my_flatten7/2).
prim(my_max_list8/2).
prim(my_set9/1).
prim(my_reverse10/2).
prim(my_last11/2).
prim(my_succ12/2).
prim(my_min_list13/2).
prim(my_lowercase14/1).
prim(my_tolower15/2).
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
p(['A','r','n','n','z','s','b','M'],'n').
p(['h','q','s','I','h','Z','u','H'],'h').
p(['y','y','R','D','b'],'y').
p(['C','z','R','E','P','J','P','s'],'P').
p(['C','M','g','M','V'],'M').
q(['C','k','I','K','q','E','O','k','I'],'K').
q(['u','K','K','F','g','X'],'X').
q(['D','D','z','u','P','G','j','P'],'j').
q(['n','H','A','B','h','H','x','g','T'],'T').
q(['S','j','a','c','s','q','m','F','b','j','h'],'c').
