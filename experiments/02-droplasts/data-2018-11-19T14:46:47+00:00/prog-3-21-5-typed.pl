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
my_max_list0(A,B):-max_list(A,B).
my_reverse1(A,B):-reverse(A,B).
my_head2([H|_],H).
my_len3(A,B):-length(A,B).
my_max_list4(A,B):-max_list(A,B).
my_succ5(A,B):-succ(A,B).
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_len8(A,B):-length(A,B).
my_reverse9(A,B):-reverse(A,B).
my_min_list10(A,B):-min_list(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_len12(A,B):-length(A,B).
my_succ13(A,B):-succ(A,B).
my_len14(A,B):-length(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_reverse16(A,B):-reverse(A,B).
my_succ17(A,B):-succ(A,B).
my_pred18(A,B):-succ(B,A).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
prim(my_max_list0,[list(int),int]).
prim(my_reverse1,[list(T),T]).
prim(my_head2,[list(T),T]).
prim(my_len3,[list(T),int]).
prim(my_max_list4,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_len6,[list(T),int]).
prim(my_max_list7,[list(int),int]).
prim(my_len8,[list(T),int]).
prim(my_reverse9,[list(T),T]).
prim(my_min_list10,[list(int),int]).
prim(my_sumlist11,[list(int),int]).
prim(my_len12,[list(T),int]).
prim(my_succ13,[int,int]).
prim(my_len14,[list(T),int]).
prim(my_sumlist15,[list(int),int]).
prim(my_reverse16,[list(T),T]).
prim(my_succ17,[int,int]).
prim(my_pred18,[int,int]).
prim(my_pred19,[int,int]).
prim(my_pred20,[int,int]).
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
p([['q','x','s'],['f','x','q']],[['q','x'],['f','x']]).
p([['e','w','t'],['p','x','h','y'],['f','q','n','x'],['e','t','o']],[['e','w'],['p','x','h'],['f','q','n'],['e','t']]).
