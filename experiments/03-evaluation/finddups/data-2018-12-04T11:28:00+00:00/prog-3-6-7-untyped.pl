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
my_reverse7(A,B):-reverse(A,B).
my_msort8(A,B):-msort(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_list_to_set4/2).
prim(my_max_list5/2).
prim(my_even6/1).
prim(my_reverse7/2).
prim(my_msort8/2).
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
p(['K','V','C','L','K'],'K').
p(['I','S','F','v','E','I'],'I').
p(['s','s','e','T','L','j','W','o','y'],'s').
p(['N','X','N','y','K','p','I','X','X'],'X').
p(['Y','E','Q','K','Y','j','J','H','k','h'],'Y').
q(['I','w','I','B','h','U','y','t','Y'],'Y').
q(['v','O','Y','c','g','r','v','Z','U'],'r').
q(['T','S','D','n','m','i','D','B','D'],'i').
q(['n','k','[','L','a','z','n','k','o','n'],'[').
q(['l','L','b','v','n','S','l'],'v').
