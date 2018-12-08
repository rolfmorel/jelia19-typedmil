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
my_reverse3(A,B):-reverse(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_odd7(A):-1 is A mod 2.
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_max_list4/2).
prim(my_len5/2).
prim(my_uppercase6/1).
prim(my_odd7/1).
prim(my_sumlist8/2).
prim(my_last9/2).
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
p(['y','n','T','g','L','z','y','S'],'y').
p(['L','A','Q','r','r','A','J','E','O','Z'],'A').
p(['B','a','N','k','a','I'],'a').
p(['Y','E','H','D','U','L','v','E','C','c'],'E').
p(['q','h','q','e','t','N','h'],'h').
q(['[','R','R','g','V','v'],'[').
q(['X','L','l','[','w','o','r','T','w','F','y'],'[').
q(['m','W','n','s','l','j','l','d'],'s').
q(['b','Z','w','a','l','z','B','P','n','a','b'],'w').
q(['T','s','x','Q','C','I','l','x','k','y','P'],'I').
