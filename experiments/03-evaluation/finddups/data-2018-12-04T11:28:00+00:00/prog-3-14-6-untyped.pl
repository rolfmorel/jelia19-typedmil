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
my_list_to_set4(A,B):-list_to_set(A,B).
my_max_list5(A,B):-max_list(A,B).
my_even6(A):-0 is A mod 2.
my_set7(A):-list_to_set(A,A).
my_pred8(A,B):-succ(B,A),A > 0.
my_uppercase9(A):-upcase_atom(A,A).
my_last10(A,B):-last(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_msort12(A,B):-msort(A,B).
my_lowercase13(A):-downcase_atom(A,A).
my_odd14(A):-1 is A mod 2.
my_flatten15(A,B):-flatten(A,B).
my_min_list16(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_list_to_set4/2).
prim(my_max_list5/2).
prim(my_even6/1).
prim(my_set7/1).
prim(my_pred8/2).
prim(my_uppercase9/1).
prim(my_last10/2).
prim(my_tolower11/2).
prim(my_msort12/2).
prim(my_lowercase13/1).
prim(my_odd14/1).
prim(my_flatten15/2).
prim(my_min_list16/2).
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
p(['G','I','g','I','I','a','B','W'],'I').
p(['x','w','Z','w','p','e'],'w').
p(['Q','p','p','G','R','m','x'],'p').
p(['h','i','c','i','v','O','s','a'],'i').
p(['v','z','v','g','I'],'v').
q(['E','D','E','N','n','P','u','Q','r','i'],'N').
q(['M','M','x','O','E','{','i'],'{').
q(['n','u','L','{','n','j','C'],'{').
q(['k','Q','d','Z','Z','M'],'k').
q(['D','m','v','m','I','E','D','a','M','S'],'a').
