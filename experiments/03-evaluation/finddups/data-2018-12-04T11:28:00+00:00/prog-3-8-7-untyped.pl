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
my_uppercase4(A):-upcase_atom(A,A).
my_msort5(A,B):-msort(A,B).
my_odd6(A):-1 is A mod 2.
my_last7(A,B):-last(A,B).
my_max_list8(A,B):-max_list(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_len10(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_uppercase4/1).
prim(my_msort5/2).
prim(my_odd6/1).
prim(my_last7/2).
prim(my_max_list8/2).
prim(my_pred9/2).
prim(my_len10/2).
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
p(['y','m','g','v','m','s','P','j','m'],'m').
p(['b','m','l','m','r','H','k','n'],'m').
p(['p','C','D','l','L','D','Q','K'],'D').
p(['r','Q','B','R','r','Q','n','L','J'],'r').
p(['n','g','T','B','P','g','P','s'],'g').
q(['r','H','G','y','j','h','m','G'],'j').
q(['i','d','F','z','j','P','X','d','p','Q'],'i').
q(['X','T','e','T','H','O','H','h'],'h').
q(['w','x','c','w','w','s','z','j'],'x').
q(['f','k','L','M','V','k','q'],'M').
