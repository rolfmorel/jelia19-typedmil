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
my_last3(A,B):-last(A,B).
my_min_list4(A,B):-min_list(A,B).
my_set5(A):-list_to_set(A,A).
my_uppercase6(A):-upcase_atom(A,A).
my_max_list7(A,B):-max_list(A,B).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_pred10(A,B):-succ(B,A),A > 0.
my_odd11(A):-1 is A mod 2.
my_tolower12(A,B):-downcase_atom(A,B).
my_flatten13(A,B):-flatten(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_list_to_set15(A,B):-list_to_set(A,B).
my_toupper16(A,B):-upcase_atom(A,B).
my_msort17(A,B):-msort(A,B).
my_reverse18(A,B):-reverse(A,B).
my_even19(A):-0 is A mod 2.
my_lowercase20(A):-downcase_atom(A,A).
my_sumlist21(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_last3/2).
prim(my_min_list4/2).
prim(my_set5/1).
prim(my_uppercase6/1).
prim(my_max_list7/2).
prim(my_len8/2).
prim(my_succ9/2).
prim(my_pred10/2).
prim(my_odd11/1).
prim(my_tolower12/2).
prim(my_flatten13/2).
prim(my_double14/2).
prim(my_list_to_set15/2).
prim(my_toupper16/2).
prim(my_msort17/2).
prim(my_reverse18/2).
prim(my_even19/1).
prim(my_lowercase20/1).
prim(my_sumlist21/2).
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
p(['l','f','W','g','l','i','P'],'l').
p(['P','q','f','E','q','N'],'q').
p(['M','g','e','s','g','P','e'],'e').
p(['f','z','k','z','b','w','a'],'z').
p(['y','c','L','d','B','H','c','m'],'c').
q(['c','N','l','d','g','X','P','D','I','S','l'],'I').
q(['I','v','w','H','B','G','B'],'v').
q(['d','G','i','q','W','E','N','b','g','E','V'],'i').
q(['X','i','D','I','m','s','s','b'],'I').
q(['X','Y','x','{','v','x','X','I','B'],'{').
