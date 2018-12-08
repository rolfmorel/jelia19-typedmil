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
my_reverse4(A,B):-reverse(A,B).
my_odd5(A):-1 is A mod 2.
my_list_to_set6(A,B):-list_to_set(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_msort10(A,B):-msort(A,B).
my_min_list11(A,B):-min_list(A,B).
my_uppercase12(A):-upcase_atom(A,A).
my_max_list13(A,B):-max_list(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_set15(A):-list_to_set(A,A).
my_double16(N,M):-M is 2*N,M =< 10.
my_last17(A,B):-last(A,B).
my_tolower18(A,B):-downcase_atom(A,B).
my_flatten19(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_reverse4/2).
prim(my_odd5/1).
prim(my_list_to_set6/2).
prim(my_toupper7/2).
prim(my_sumlist8/2).
prim(my_pred9/2).
prim(my_msort10/2).
prim(my_min_list11/2).
prim(my_uppercase12/1).
prim(my_max_list13/2).
prim(my_lowercase14/1).
prim(my_set15/1).
prim(my_double16/2).
prim(my_last17/2).
prim(my_tolower18/2).
prim(my_flatten19/2).
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
p(['t','m','t','f','H','I','s'],'t').
p(['s','F','O','h','Y','C','Y','i','j','c'],'Y').
p(['M','l','M','x','J','U'],'M').
p(['n','z','X','T','n','q','Q','j'],'n').
p(['p','p','i','V','d','X','g','s','r'],'p').
q(['p','a','d','T','w','a','K','M','I','X'],'p').
q(['g','R','S','Z','p','X','p','J','Q','U'],'X').
q(['u','q','J','C','m','z','C','l','p','J','c'],'l').
q(['g','c','K','u','v','u'],'v').
q(['E','I','z','e','x','x','X','h'],'h').
