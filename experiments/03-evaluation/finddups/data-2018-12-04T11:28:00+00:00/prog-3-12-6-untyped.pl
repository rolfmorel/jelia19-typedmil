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
my_reverse4(A,B):-reverse(A,B).
my_len5(A,B):-length(A,B).
my_msort6(A,B):-msort(A,B).
my_odd7(A):-1 is A mod 2.
my_even8(A):-0 is A mod 2.
my_last9(A,B):-last(A,B).
my_tolower10(A,B):-downcase_atom(A,B).
my_uppercase11(A):-upcase_atom(A,A).
my_min_list12(A,B):-min_list(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_max_list14(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_reverse4/2).
prim(my_len5/2).
prim(my_msort6/2).
prim(my_odd7/1).
prim(my_even8/1).
prim(my_last9/2).
prim(my_tolower10/2).
prim(my_uppercase11/1).
prim(my_min_list12/2).
prim(my_list_to_set13/2).
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
p(['F','L','c','G','I','K','X','c','v'],'c').
p(['o','I','O','l','J','C','Z','C'],'C').
p(['l','k','L','H','n','P','J','L','J','x'],'L').
p(['K','h','I','T','T','a'],'T').
p(['V','y','V','o','h','K'],'V').
q(['V','{','V','h','u','G'],'{').
q(['L','O','n','H','v','a','v'],'H').
q(['c','F','w','z','l','s','J','F'],'c').
q(['Y','s','v','E','E','m','U','o'],'v').
q(['F','h','W','F','o','B','F'],'o').
