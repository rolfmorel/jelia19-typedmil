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
my_lowercase4(A):-downcase_atom(A,A).
my_even5(A):-0 is A mod 2.
my_len6(A,B):-length(A,B).
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_list_to_set9(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_lowercase4/1).
prim(my_even5/1).
prim(my_len6/2).
prim(my_last7/2).
prim(my_pred8/2).
prim(my_list_to_set9/2).
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
p(['R','e','v','j','L','B','P','Y','L'],'L').
p(['f','m','W','G','W','z','C','D'],'W').
p(['l','b','l','l','Q'],'l').
p(['s','r','i','f','q','r','u'],'r').
p(['a','p','c','w','x','c','O','j','v','a'],'c').
q(['u','K','K','K','n','m','L','Z','F','u'],'L').
q(['V','T','H','o','A','J','n','J','f','x'],'V').
q(['F','V','n','F','u','I'],'u').
q(['X','w','f','n','P','Q','x','X','S','e','B'],'S').
q(['t','t','d','[','o','Y','b','s','f','u'],'[').
