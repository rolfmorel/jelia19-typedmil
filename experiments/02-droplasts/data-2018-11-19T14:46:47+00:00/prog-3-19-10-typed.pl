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
my_sumlist0(A,B):-sumlist(A,B).
my_max_list1(A,B):-max_list(A,B).
my_tail2([_|TL],TL).
my_sumlist3(A,B):-sumlist(A,B).
my_len4(A,B):-length(A,B).
my_succ5(A,B):-succ(A,B).
my_last6(A,B):-last(A,B).
my_succ7(A,B):-succ(A,B).
my_succ8(A,B):-succ(A,B).
my_max_list9(A,B):-max_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_head11([H|_],H).
my_succ12(A,B):-succ(A,B).
my_reverse13(A,B):-reverse(A,B).
my_reverse14(A,B):-reverse(A,B).
my_reverse15(A,B):-reverse(A,B).
my_min_list16(A,B):-min_list(A,B).
my_succ17(A,B):-succ(A,B).
my_reverse18(A,B):-reverse(A,B).
prim(my_sumlist0,[list(int),int]).
prim(my_max_list1,[list(int),int]).
prim(my_tail2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_len4,[list(T),int]).
prim(my_succ5,[int,int]).
prim(my_last6,[list(T),T]).
prim(my_succ7,[int,int]).
prim(my_succ8,[int,int]).
prim(my_max_list9,[list(int),int]).
prim(my_reverse10,[list(T),T]).
prim(my_head11,[list(T),T]).
prim(my_succ12,[int,int]).
prim(my_reverse13,[list(T),T]).
prim(my_reverse14,[list(T),T]).
prim(my_reverse15,[list(T),T]).
prim(my_min_list16,[list(int),int]).
prim(my_succ17,[int,int]).
prim(my_reverse18,[list(T),T]).
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
p([['v','d','w'],['u','a','b'],['i','l','d'],['f','i','o','h']],[['v','d'],['u','a'],['i','l'],['f','i','o']]).
p([['j','f','m','m'],['p','u','e','u']],[['j','f','m'],['p','u','e']]).
