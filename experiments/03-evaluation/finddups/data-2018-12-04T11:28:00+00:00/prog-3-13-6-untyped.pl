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
my_uppercase3(A):-upcase_atom(A,A).
my_len4(A,B):-length(A,B).
my_even5(A):-0 is A mod 2.
my_tolower6(A,B):-downcase_atom(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_reverse8(A,B):-reverse(A,B).
my_min_list9(A,B):-min_list(A,B).
my_odd10(A):-1 is A mod 2.
my_succ11(A,B):-succ(A,B),B =< 10.
my_lowercase12(A):-downcase_atom(A,A).
my_msort13(A,B):-msort(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_max_list15(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_uppercase3/1).
prim(my_len4/2).
prim(my_even5/1).
prim(my_tolower6/2).
prim(my_toupper7/2).
prim(my_reverse8/2).
prim(my_min_list9/2).
prim(my_odd10/1).
prim(my_succ11/2).
prim(my_lowercase12/1).
prim(my_msort13/2).
prim(my_pred14/2).
prim(my_max_list15/2).
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
p(['Z','G','R','O','v','A','O','u'],'O').
p(['W','W','I','h','J','N'],'W').
p(['I','j','b','k','K','e','c','v','v','R'],'v').
p(['i','w','w','v','X','H','V','n'],'w').
p(['A','s','e','S','a','a','V','d','u','V'],'a').
q(['D','Y','D','C','E','X','O','v'],'O').
q(['h','n','d','u','H','d','c','L','F'],'c').
q(['G','U','q','Z','L','q','r','b'],'L').
q(['R','j','u','F','p','x','I','n','I','R','C'],'x').
q(['b','T','[','N','f','B','T','V','Q','u'],'[').
