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
my_min_list0(A,B):-min_list(A,B).
my_reverse1(A,B):-reverse(A,B).
my_last2(A,B):-last(A,B).
my_max_list3(A,B):-max_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_reverse8(A,B):-reverse(A,B).
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A).
my_reverse11(A,B):-reverse(A,B).
my_head12([H|_],H).
my_min_list13(A,B):-min_list(A,B).
my_len14(A,B):-length(A,B).
my_pred15(A,B):-succ(B,A).
my_succ16(A,B):-succ(A,B).
my_succ17(A,B):-succ(A,B).
my_min_list18(A,B):-min_list(A,B).
my_reverse19(A,B):-reverse(A,B).
my_len20(A,B):-length(A,B).
my_tail21([_|TL],TL).
my_len22(A,B):-length(A,B).
my_pred23(A,B):-succ(B,A).
my_len24(A,B):-length(A,B).
my_pred25(A,B):-succ(B,A).
my_reverse26(A,B):-reverse(A,B).
prim(my_min_list0,[list(int),int]).
prim(my_reverse1,[list(T),T]).
prim(my_last2,[list(T),T]).
prim(my_max_list3,[list(int),int]).
prim(my_min_list4,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_len6,[list(T),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_reverse8,[list(T),T]).
prim(my_last9,[list(T),T]).
prim(my_pred10,[int,int]).
prim(my_reverse11,[list(T),T]).
prim(my_head12,[list(T),T]).
prim(my_min_list13,[list(int),int]).
prim(my_len14,[list(T),int]).
prim(my_pred15,[int,int]).
prim(my_succ16,[int,int]).
prim(my_succ17,[int,int]).
prim(my_min_list18,[list(int),int]).
prim(my_reverse19,[list(T),T]).
prim(my_len20,[list(T),int]).
prim(my_tail21,[list(T),T]).
prim(my_len22,[list(T),int]).
prim(my_pred23,[int,int]).
prim(my_len24,[list(T),int]).
prim(my_pred25,[int,int]).
prim(my_reverse26,[list(T),T]).
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
p([['t','p','g','i'],['w','x','y','b']],[['t','p','g'],['w','x','y']]).
p([['x','x','p'],['x','o','r','f'],['h','b','b','d']],[['x','x'],['x','o','r'],['h','b','b']]).
