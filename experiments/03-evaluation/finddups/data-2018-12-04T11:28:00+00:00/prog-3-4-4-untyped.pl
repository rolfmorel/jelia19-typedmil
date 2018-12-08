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
my_min_list3(A,B):-min_list(A,B).
my_len4(A,B):-length(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_last6(A,B):-last(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_len4/2).
prim(my_list_to_set5/2).
prim(my_last6/2).
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
p(['L','b','r','O','N','r','N','q','H','N'],'N').
p(['y','y','P','E','q','p'],'y').
p(['d','S','E','b','d','E','E','z'],'E').
p(['i','Z','s','A','Z','G','a'],'Z').
p(['c','n','n','N','D'],'n').
q(['f','A','L','I','f','l','n','f','e','S'],'S').
q(['m','B','v','K','m','P'],'P').
q(['W','p','p','l','n','r','t'],'t').
q(['T','i','w','i','v','h','h','o'],'v').
q(['I','l','x','S','E','Q','S','p','P'],'x').
