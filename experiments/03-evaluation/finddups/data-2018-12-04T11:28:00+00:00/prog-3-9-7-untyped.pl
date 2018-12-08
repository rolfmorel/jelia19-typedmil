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
my_sumlist3(A,B):-sumlist(A,B).
my_flatten4(A,B):-flatten(A,B).
my_min_list5(A,B):-min_list(A,B).
my_max_list6(A,B):-max_list(A,B).
my_even7(A):-0 is A mod 2.
my_set8(A):-list_to_set(A,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_reverse10(A,B):-reverse(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_sumlist3/2).
prim(my_flatten4/2).
prim(my_min_list5/2).
prim(my_max_list6/2).
prim(my_even7/1).
prim(my_set8/1).
prim(my_pred9/2).
prim(my_reverse10/2).
prim(my_list_to_set11/2).
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
p(['h','j','E','H','h','z','S','g','g','N'],'h').
p(['T','v','m','r','v','t','J','B','U','e'],'v').
p(['n','W','R','Z','O','n','R'],'n').
p(['H','M','o','Q','M','J','f'],'M').
p(['X','v','q','o','w','v','O'],'v').
q(['w','a','K','B','u','o','w','p'],'a').
q(['J','y','r','I','P','Q','D','J','N','k'],'y').
q(['G','D','A','Q','Q','Q','y'],'D').
q(['A','A','w','S','h','r'],'h').
q(['s','C','t','r','o','s','E','X','K','l'],'r').
