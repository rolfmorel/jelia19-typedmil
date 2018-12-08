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
my_succ1(A,B):-succ(A,B).
my_head2([H|_],H).
my_pred3(A,B):-succ(B,A).
my_min_list4(A,B):-min_list(A,B).
my_reverse5(A,B):-reverse(A,B).
my_max_list6(A,B):-max_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B).
my_max_list9(A,B):-max_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_pred11(A,B):-succ(B,A).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_last15(A,B):-last(A,B).
my_succ16(A,B):-succ(A,B).
my_max_list17(A,B):-max_list(A,B).
my_head18([H|_],H).
my_head19([H|_],H).
my_tail20([_|TL],TL).
my_head21([H|_],H).
my_sumlist22(A,B):-sumlist(A,B).
my_reverse23(A,B):-reverse(A,B).
my_max_list24(A,B):-max_list(A,B).
my_reverse25(A,B):-reverse(A,B).
prim(my_succ0,[int,int]).
prim(my_succ1,[int,int]).
prim(my_head2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_min_list4,[list(int),int]).
prim(my_reverse5,[list(T),T]).
prim(my_max_list6,[list(int),int]).
prim(my_reverse7,[list(T),T]).
prim(my_succ8,[int,int]).
prim(my_max_list9,[list(int),int]).
prim(my_reverse10,[list(T),T]).
prim(my_pred11,[int,int]).
prim(my_pred12,[int,int]).
prim(my_max_list13,[list(int),int]).
prim(my_sumlist14,[list(int),int]).
prim(my_last15,[list(T),T]).
prim(my_succ16,[int,int]).
prim(my_max_list17,[list(int),int]).
prim(my_head18,[list(T),T]).
prim(my_head19,[list(T),T]).
prim(my_tail20,[list(T),T]).
prim(my_head21,[list(T),T]).
prim(my_sumlist22,[list(int),int]).
prim(my_reverse23,[list(T),T]).
prim(my_max_list24,[list(int),int]).
prim(my_reverse25,[list(T),T]).
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
p([['e','t','g'],['r','n','y'],['x','m','f','e'],['u','i','p','n']],[['e','t'],['r','n'],['x','m','f'],['u','i','p']]).
p([['d','b','s'],['b','g','d'],['u','d','d','w'],['u','j','a','s']],[['d','b'],['b','g'],['u','d','d'],['u','j','a']]).
