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
my_tail1([_|TL],TL).
my_len2(A,B):-length(A,B).
my_last3(A,B):-last(A,B).
my_last4(A,B):-last(A,B).
my_min_list5(A,B):-min_list(A,B).
my_pred6(A,B):-succ(B,A).
my_max_list7(A,B):-max_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_max_list9(A,B):-max_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_succ11(A,B):-succ(A,B).
my_tail12([_|TL],TL).
my_sumlist13(A,B):-sumlist(A,B).
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_head16([H|_],H).
my_reverse17(A,B):-reverse(A,B).
my_reverse18(A,B):-reverse(A,B).
my_max_list19(A,B):-max_list(A,B).
my_max_list20(A,B):-max_list(A,B).
my_len21(A,B):-length(A,B).
my_pred22(A,B):-succ(B,A).
my_reverse23(A,B):-reverse(A,B).
my_succ24(A,B):-succ(A,B).
my_min_list25(A,B):-min_list(A,B).
prim(my_succ0,[int,int]).
prim(my_tail1,[list(T),T]).
prim(my_len2,[list(T),int]).
prim(my_last3,[list(T),T]).
prim(my_last4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_max_list7,[list(int),int]).
prim(my_min_list8,[list(int),int]).
prim(my_max_list9,[list(int),int]).
prim(my_max_list10,[list(int),int]).
prim(my_succ11,[int,int]).
prim(my_tail12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_len14,[list(T),int]).
prim(my_last15,[list(T),T]).
prim(my_head16,[list(T),T]).
prim(my_reverse17,[list(T),T]).
prim(my_reverse18,[list(T),T]).
prim(my_max_list19,[list(int),int]).
prim(my_max_list20,[list(int),int]).
prim(my_len21,[list(T),int]).
prim(my_pred22,[int,int]).
prim(my_reverse23,[list(T),T]).
prim(my_succ24,[int,int]).
prim(my_min_list25,[list(int),int]).
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
p([['v','j','j'],['p','a','a'],['g','o','n']],[['v','j'],['p','a'],['g','o']]).
p([['i','k','g','k'],['v','u','l']],[['i','k','g'],['v','u']]).
