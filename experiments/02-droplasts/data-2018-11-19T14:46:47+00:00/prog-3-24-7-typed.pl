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
my_min_list1(A,B):-min_list(A,B).
my_min_list2(A,B):-min_list(A,B).
my_succ3(A,B):-succ(A,B).
my_max_list4(A,B):-max_list(A,B).
my_tail5([_|TL],TL).
my_len6(A,B):-length(A,B).
my_reverse7(A,B):-reverse(A,B).
my_min_list8(A,B):-min_list(A,B).
my_min_list9(A,B):-min_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_succ11(A,B):-succ(A,B).
my_reverse12(A,B):-reverse(A,B).
my_min_list13(A,B):-min_list(A,B).
my_last14(A,B):-last(A,B).
my_last15(A,B):-last(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_last17(A,B):-last(A,B).
my_succ18(A,B):-succ(A,B).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
my_last21(A,B):-last(A,B).
my_succ22(A,B):-succ(A,B).
my_max_list23(A,B):-max_list(A,B).
prim(my_tail0,[list(T),T]).
prim(my_min_list1,[list(int),int]).
prim(my_min_list2,[list(int),int]).
prim(my_succ3,[int,int]).
prim(my_max_list4,[list(int),int]).
prim(my_tail5,[list(T),T]).
prim(my_len6,[list(T),int]).
prim(my_reverse7,[list(T),T]).
prim(my_min_list8,[list(int),int]).
prim(my_min_list9,[list(int),int]).
prim(my_max_list10,[list(int),int]).
prim(my_succ11,[int,int]).
prim(my_reverse12,[list(T),T]).
prim(my_min_list13,[list(int),int]).
prim(my_last14,[list(T),T]).
prim(my_last15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_last17,[list(T),T]).
prim(my_succ18,[int,int]).
prim(my_pred19,[int,int]).
prim(my_pred20,[int,int]).
prim(my_last21,[list(T),T]).
prim(my_succ22,[int,int]).
prim(my_max_list23,[list(int),int]).
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
p([['s','h','a'],['g','s','k'],['w','l','e'],['x','k','f','a']],[['s','h'],['g','s'],['w','l'],['x','k','f']]).
p([['n','s','p','q'],['u','y','w']],[['n','s','p'],['u','y']]).
