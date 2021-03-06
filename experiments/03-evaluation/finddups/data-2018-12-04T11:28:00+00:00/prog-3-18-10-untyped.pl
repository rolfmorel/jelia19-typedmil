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
my_reverse4(A,B):-reverse(A,B).
my_last5(A,B):-last(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_odd7(A):-1 is A mod 2.
my_max_list8(A,B):-max_list(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_set10(A):-list_to_set(A,A).
my_msort11(A,B):-msort(A,B).
my_flatten12(A,B):-flatten(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_len15(A,B):-length(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_uppercase17(A):-upcase_atom(A,A).
my_tolower18(A,B):-downcase_atom(A,B).
my_list_to_set19(A,B):-list_to_set(A,B).
my_min_list20(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_pred3/2).
prim(my_reverse4/2).
prim(my_last5/2).
prim(my_double6/2).
prim(my_odd7/1).
prim(my_max_list8/2).
prim(my_succ9/2).
prim(my_set10/1).
prim(my_msort11/2).
prim(my_flatten12/2).
prim(my_toupper13/2).
prim(my_lowercase14/1).
prim(my_len15/2).
prim(my_sumlist16/2).
prim(my_uppercase17/1).
prim(my_tolower18/2).
prim(my_list_to_set19/2).
prim(my_min_list20/2).
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
p(['G','I','x','i','d','I','v','t','w','b'],'I').
p(['r','N','b','y','w','S','w'],'w').
p(['X','J','e','J','X','v','G','X'],'X').
p(['m','j','g','k','j','w','d'],'j').
p(['J','p','R','L','m','x','x','R','W','M'],'x').
q(['x','k','l','S','K','S','x'],'K').
q(['e','x','I','w','N','t','e','l','P','e'],'t').
q(['F','R','V','C','H','C','p','E'],'E').
q(['M','U','P','P','N','O','N','G','F'],'O').
q(['Y','U','g','L','w','g'],'w').
