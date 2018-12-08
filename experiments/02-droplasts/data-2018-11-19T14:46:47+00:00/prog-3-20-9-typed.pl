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
my_pred2(A,B):-succ(B,A).
my_sumlist3(A,B):-sumlist(A,B).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_succ6(A,B):-succ(A,B).
my_min_list7(A,B):-min_list(A,B).
my_succ8(A,B):-succ(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_min_list10(A,B):-min_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_min_list14(A,B):-min_list(A,B).
my_head15([H|_],H).
my_sumlist16(A,B):-sumlist(A,B).
my_succ17(A,B):-succ(A,B).
my_reverse18(A,B):-reverse(A,B).
my_succ19(A,B):-succ(A,B).
prim(my_succ0,[int,int]).
prim(my_tail1,[list(T),T]).
prim(my_pred2,[int,int]).
prim(my_sumlist3,[list(int),int]).
prim(my_max_list4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_succ6,[int,int]).
prim(my_min_list7,[list(int),int]).
prim(my_succ8,[int,int]).
prim(my_sumlist9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
prim(my_reverse12,[list(T),T]).
prim(my_len13,[list(T),int]).
prim(my_min_list14,[list(int),int]).
prim(my_head15,[list(T),T]).
prim(my_sumlist16,[list(int),int]).
prim(my_succ17,[int,int]).
prim(my_reverse18,[list(T),T]).
prim(my_succ19,[int,int]).
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
p([['q','q','m'],['d','n','j','y'],['f','p','h'],['a','r','r']],[['q','q'],['d','n','j'],['f','p'],['a','r']]).
p([['l','w','s','h'],['o','r','g'],['n','d','x'],['e','v','q']],[['l','w','s'],['o','r'],['n','d'],['e','v']]).
