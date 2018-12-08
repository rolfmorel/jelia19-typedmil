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
my_toupper4(A,B):-upcase_atom(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_min_list6(A,B):-min_list(A,B).
my_even7(A):-0 is A mod 2.
my_len8(A,B):-length(A,B).
my_reverse9(A,B):-reverse(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_tolower11(A,B):-downcase_atom(A,B).
my_lowercase12(A):-downcase_atom(A,A).
my_last13(A,B):-last(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_msort15(A,B):-msort(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_uppercase3/1).
prim(my_toupper4/2).
prim(my_list_to_set5/2).
prim(my_min_list6/2).
prim(my_even7/1).
prim(my_len8/2).
prim(my_reverse9/2).
prim(my_pred10/2).
prim(my_tolower11/2).
prim(my_lowercase12/1).
prim(my_last13/2).
prim(my_double14/2).
prim(my_msort15/2).
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
p(['R','h','R','a','T'],'R').
p(['Z','y','w','t','t','g','d'],'t').
p(['w','a','w','x','a','h','m','C'],'w').
p(['a','T','p','p','a','F','Z','j'],'p').
p(['j','q','z','m','K','r','z'],'z').
q(['u','D','i','v','T','D','k','m','l'],'m').
q(['s','E','U','g','E','c','B','U','e','H'],'e').
q(['a','a','f','P','d','b','J','p','c','X','Q'],'c').
q(['r','c','W','j','j','C','p','W','z','[','a'],'[').
q(['T','P','y','G','v','E','z','G','J','D','v'],'z').
