:- use_module('metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).

tail([_|T],T).

prim(tail,[list(T),list(T)]).
prim(reverse,[list(T),list(T)]).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
metarule(tohigherorder,[P:[list(S),list(T)],Q:[list(S),list(T),[S,T]],F:[S,T]],([P,A,B]:[list(S),list(T)] :- [[Q,A,B,F]:[list(S),list(T),[S,T]]])).
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_pred2(A,B):-succ(B,A).
my_min_list3(A,B):-min_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_pred5(A,B):-succ(B,A).
my_max_list6(A,B):-max_list(A,B).
my_len7(A,B):-length(A,B).
my_reverse8(A,B):-reverse(A,B).
prim(my_tail0,[list(T),T]).
prim(my_head1,[list(T),T]).
prim(my_pred2,[int,int]).
prim(my_min_list3,[list(int),int]).
prim(my_max_list4,[list(int),int]).
prim(my_pred5,[int,int]).
prim(my_max_list6,[list(int),int]).
prim(my_len7,[list(T),int]).
prim(my_reverse8,[list(T),T]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,[],[list(list(char)),list(list(char))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([['q','s','a','c'],['u','f','d','w'],['w','j','q','j']],[['q','s','a'],['u','f','d'],['w','j','q']]).
p([['t','v','d','n'],['m','j','p'],['o','e','v','s'],['e','x','o','o']],[['t','v','d'],['m','j'],['o','e','v'],['e','x','o']]).
