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
my_succ0(A,B):-succ(A,B).
my_len1(A,B):-length(A,B).
my_max_list2(A,B):-max_list(A,B).
my_tail3([_|TL],TL).
my_min_list4(A,B):-min_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_head8([H|_],H).
my_reverse9(A,B):-reverse(A,B).
my_head10([H|_],H).
my_head11([H|_],H).
my_succ12(A,B):-succ(A,B).
my_max_list13(A,B):-max_list(A,B).
my_last14(A,B):-last(A,B).
my_len15(A,B):-length(A,B).
my_head16([H|_],H).
my_len17(A,B):-length(A,B).
prim(my_succ0,[int,int]).
prim(my_len1,[list(T),int]).
prim(my_max_list2,[list(int),int]).
prim(my_tail3,[list(T),T]).
prim(my_min_list4,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_sumlist6,[list(int),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_head8,[list(T),T]).
prim(my_reverse9,[list(T),T]).
prim(my_head10,[list(T),T]).
prim(my_head11,[list(T),T]).
prim(my_succ12,[int,int]).
prim(my_max_list13,[list(int),int]).
prim(my_last14,[list(T),T]).
prim(my_len15,[list(T),int]).
prim(my_head16,[list(T),T]).
prim(my_len17,[list(T),int]).
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
p([['x','v','k','o'],['m','y','m','i'],['p','k','u'],['i','j','h']],[['x','v','k'],['m','y','m'],['p','k'],['i','j']]).
p([['v','e','g'],['u','j','w','m'],['m','m','l','j'],['r','p','h','v']],[['v','e'],['u','j','w'],['m','m','l'],['r','p','h']]).
