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
my_toupper3(A,B):-upcase_atom(A,B).
my_max_list4(A,B):-max_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_reverse6(A,B):-reverse(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_succ8(A,B):-succ(A,B),B =< 10.
my_len9(A,B):-length(A,B).
my_flatten10(A,B):-flatten(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_msort12(A,B):-msort(A,B).
my_set13(A):-list_to_set(A,A).
my_list_to_set14(A,B):-list_to_set(A,B).
my_tolower15(A,B):-downcase_atom(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_uppercase17(A):-upcase_atom(A,A).
my_pred18(A,B):-succ(B,A),A > 0.
my_last19(A,B):-last(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_toupper3/2).
prim(my_max_list4/2).
prim(my_min_list5/2).
prim(my_reverse6/2).
prim(my_lowercase7/1).
prim(my_succ8/2).
prim(my_len9/2).
prim(my_flatten10/2).
prim(my_double11/2).
prim(my_msort12/2).
prim(my_set13/1).
prim(my_list_to_set14/2).
prim(my_tolower15/2).
prim(my_sumlist16/2).
prim(my_uppercase17/1).
prim(my_pred18/2).
prim(my_last19/2).
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
p(['Z','M','V','b','x','x','l','D'],'x').
p(['r','Q','Q','u','R','D'],'Q').
p(['o','j','S','t','o','o','b','o','R','y'],'o').
p(['L','d','L','d','R'],'d').
p(['A','W','p','p','C','z','y','u','e','H'],'p').
q(['M','m','r','X','k','E','{','r'],'{').
q(['j','C','T','H','X','c','a','b','J','E','C'],'H').
q(['x','E','Z','I','Z','n','v'],'n').
q(['p','O','E','R','M','V','C','O','r','F','k'],'F').
q(['V','R','b','h','b','q'],'h').
