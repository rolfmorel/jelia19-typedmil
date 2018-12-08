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
my_list_to_set4(A,B):-list_to_set(A,B).
my_last5(A,B):-last(A,B).
my_even6(A):-0 is A mod 2.
my_set7(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_pred3/2).
prim(my_list_to_set4/2).
prim(my_last5/2).
prim(my_even6/1).
prim(my_set7/1).
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
p(['n','P','b','b','s','o','y'],'b').
p(['g','M','k','i','D','D'],'D').
p(['N','q','c','j','N','W'],'N').
p(['T','W','l','L','w','T','a'],'T').
p(['G','Z','Z','b','E','y'],'Z').
q(['y','p','r','X','r','K','q','Z','E'],'y').
q(['c','g','Y','R','g','C'],'c').
q(['b','I','p','W','l','Y','p','F'],'Y').
q(['h','W','B','n','u','u'],'n').
q(['W','w','W','r','W','R','p','O'],'O').
