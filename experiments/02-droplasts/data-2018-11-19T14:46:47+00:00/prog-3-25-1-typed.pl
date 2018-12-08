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
my_sumlist1(A,B):-sumlist(A,B).
my_tail2([_|TL],TL).
my_len3(A,B):-length(A,B).
my_max_list4(A,B):-max_list(A,B).
my_succ5(A,B):-succ(A,B).
my_pred6(A,B):-succ(B,A).
my_reverse7(A,B):-reverse(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_last10(A,B):-last(A,B).
my_max_list11(A,B):-max_list(A,B).
my_tail12([_|TL],TL).
my_sumlist13(A,B):-sumlist(A,B).
my_pred14(A,B):-succ(B,A).
my_sumlist15(A,B):-sumlist(A,B).
my_pred16(A,B):-succ(B,A).
my_head17([H|_],H).
my_reverse18(A,B):-reverse(A,B).
my_last19(A,B):-last(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_max_list22(A,B):-max_list(A,B).
my_last23(A,B):-last(A,B).
my_sumlist24(A,B):-sumlist(A,B).
prim(my_sumlist0,[list(int),int]).
prim(my_sumlist1,[list(int),int]).
prim(my_tail2,[list(T),T]).
prim(my_len3,[list(T),int]).
prim(my_max_list4,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_pred6,[int,int]).
prim(my_reverse7,[list(T),T]).
prim(my_sumlist8,[list(int),int]).
prim(my_last9,[list(T),T]).
prim(my_last10,[list(T),T]).
prim(my_max_list11,[list(int),int]).
prim(my_tail12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_pred14,[int,int]).
prim(my_sumlist15,[list(int),int]).
prim(my_pred16,[int,int]).
prim(my_head17,[list(T),T]).
prim(my_reverse18,[list(T),T]).
prim(my_last19,[list(T),T]).
prim(my_sumlist20,[list(int),int]).
prim(my_sumlist21,[list(int),int]).
prim(my_max_list22,[list(int),int]).
prim(my_last23,[list(T),T]).
prim(my_sumlist24,[list(int),int]).
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
p([['o','n','j','x'],['g','q','i','n'],['k','y','h'],['k','v','r']],[['o','n','j'],['g','q','i'],['k','y'],['k','v']]).
p([['x','s','b'],['k','g','r','t']],[['x','s'],['k','g','r']]).
