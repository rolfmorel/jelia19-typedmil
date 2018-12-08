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
my_flatten3(A,B):-flatten(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_len5(A,B):-length(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
my_even9(A):-0 is A mod 2.
my_pred10(A,B):-succ(B,A),A > 0.
my_uppercase11(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_sumlist4/2).
prim(my_len5/2).
prim(my_lowercase6/1).
prim(my_reverse7/2).
prim(my_last8/2).
prim(my_even9/1).
prim(my_pred10/2).
prim(my_uppercase11/1).
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
p(['n','h','i','g','X','h','K','a','X','g'],'X').
p(['m','q','Z','m','R','C','h','j','D','D'],'D').
p(['k','Z','b','k','V','k'],'k').
p(['o','Q','o','T','E','I'],'o').
p(['E','Z','k','v','v','s'],'v').
q(['w','i','v','q','h','q','K','w','t'],'i').
q(['u','u','N','U','c','Z','K'],'K').
q(['I','i','Y','N','b','U','T','Q','g','t','i'],'t').
q(['S','t','O','b','K','O','W','p','Q','j','S'],'b').
q(['Q','C','R','P','P','o','d','Q','d'],'R').
