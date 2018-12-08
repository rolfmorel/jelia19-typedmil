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
my_max_list3(A,B):-max_list(A,B).
my_flatten4(A,B):-flatten(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_min_list7(A,B):-min_list(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_last9(A,B):-last(A,B).
my_toupper10(A,B):-upcase_atom(A,B).
my_reverse11(A,B):-reverse(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_uppercase13(A):-upcase_atom(A,A).
my_len14(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_max_list3/2).
prim(my_flatten4/2).
prim(my_list_to_set5/2).
prim(my_lowercase6/1).
prim(my_min_list7/2).
prim(my_pred8/2).
prim(my_last9/2).
prim(my_toupper10/2).
prim(my_reverse11/2).
prim(my_succ12/2).
prim(my_uppercase13/1).
prim(my_len14/2).
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
p(['D','x','N','i','V','f','W','D','D','s'],'D').
p(['y','M','s','f','p','M','m','l','Q','s'],'M').
p(['p','d','l','x','V','p','V'],'V').
p(['R','R','J','u','F','J','L','v','W'],'R').
p(['s','s','A','Z','c','S','J'],'s').
q(['X','t','i','X','z','j'],'t').
q(['t','j','e','v','d','N','e','I'],'I').
q(['F','e','q','R','T','j','j'],'R').
q(['u','s','X','u','i','I'],'X').
q(['Y','n','Y','R','F','H','c','E'],'F').
