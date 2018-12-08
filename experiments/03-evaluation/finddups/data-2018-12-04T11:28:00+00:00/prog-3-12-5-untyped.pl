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
my_msort3(A,B):-msort(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_flatten8(A,B):-flatten(A,B).
my_len9(A,B):-length(A,B).
my_reverse10(A,B):-reverse(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_succ13(A,B):-succ(A,B),B =< 10.
my_toupper14(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_msort3/2).
prim(my_lowercase4/1).
prim(my_last5/2).
prim(my_min_list6/2).
prim(my_sumlist7/2).
prim(my_flatten8/2).
prim(my_len9/2).
prim(my_reverse10/2).
prim(my_tolower11/2).
prim(my_pred12/2).
prim(my_succ13/2).
prim(my_toupper14/2).
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
p(['e','s','E','m','P','E'],'E').
p(['K','k','u','U','f','K','X','G','t','R'],'K').
p(['k','y','Z','E','E'],'E').
p(['X','t','X','W','f'],'X').
p(['x','Z','A','Z','s','w','V','d','R'],'Z').
q(['m','y','u','P','w','w','f','b','p'],'u').
q(['m','x','C','E','C','j','p','J','f','h'],'m').
q(['z','y','c','e','g','[','c','u','B','m','A'],'[').
q(['j','a','M','a','q','q','u','a','I'],'u').
q(['[','p','T','T','f','G','i'],'[').
