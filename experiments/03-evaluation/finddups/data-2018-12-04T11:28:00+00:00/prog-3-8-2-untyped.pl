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
my_odd4(A):-1 is A mod 2.
my_sumlist5(A,B):-sumlist(A,B).
my_msort6(A,B):-msort(A,B).
my_uppercase7(A):-upcase_atom(A,A).
my_pred8(A,B):-succ(B,A),A > 0.
my_list_to_set9(A,B):-list_to_set(A,B).
my_even10(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_odd4/1).
prim(my_sumlist5/2).
prim(my_msort6/2).
prim(my_uppercase7/1).
prim(my_pred8/2).
prim(my_list_to_set9/2).
prim(my_even10/1).
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
p(['x','c','G','y','y','U','l'],'y').
p(['c','D','R','S','A','R','P','k','k'],'k').
p(['O','X','O','f','w','u','W'],'O').
p(['p','U','D','I','f','p'],'p').
p(['j','B','j','p','C','V','i','l','N','f'],'j').
q(['T','J','e','w','J','J','o','Z'],'Z').
q(['z','f','P','f','Y','R','G','Y','w','v'],'R').
q(['y','Y','P','a','O','C','P'],'a').
q(['G','q','G','W','q','l','u','H','w','h','L'],'u').
q(['g','H','f','a','l','g'],'f').
