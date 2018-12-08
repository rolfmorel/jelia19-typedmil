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
my_toupper3(A,B):-upcase_atom(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_min_list5(A,B):-min_list(A,B).
my_set6(A):-list_to_set(A,A).
my_lowercase7(A):-downcase_atom(A,A).
my_tolower8(A,B):-downcase_atom(A,B).
my_len9(A,B):-length(A,B).
my_flatten10(A,B):-flatten(A,B).
my_even11(A):-0 is A mod 2.
my_succ12(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_toupper3/2).
prim(my_double4/2).
prim(my_min_list5/2).
prim(my_set6/1).
prim(my_lowercase7/1).
prim(my_tolower8/2).
prim(my_len9/2).
prim(my_flatten10/2).
prim(my_even11/1).
prim(my_succ12/2).
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
p(['z','t','g','g','H','S','s'],'g').
p(['e','j','z','X','z','A','M','W','Z','l'],'z').
p(['c','h','S','A','l','w','h','Z','U','U'],'h').
p(['D','c','m','w','d','K','K','U','d'],'K').
p(['v','i','T','Y','X','c','c'],'c').
q(['o','d','B','b','c','y','F','d','D'],'c').
q(['X','k','q','W','g','O','o','M','o','Q','t'],'t').
q(['X','W','h','A','t','p','A','d','l','k','l'],'k').
q(['a','A','G','v','A','i','w'],'a').
q(['K','H','K','c','Z','H','S','d'],'S').
