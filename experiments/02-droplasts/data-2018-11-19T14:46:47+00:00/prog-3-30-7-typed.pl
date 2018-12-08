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
my_head0([H|_],H).
my_last1(A,B):-last(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_len3(A,B):-length(A,B).
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_head6([H|_],H).
my_len7(A,B):-length(A,B).
my_len8(A,B):-length(A,B).
my_last9(A,B):-last(A,B).
my_tail10([_|TL],TL).
my_tail11([_|TL],TL).
my_pred12(A,B):-succ(B,A).
my_tail13([_|TL],TL).
my_max_list14(A,B):-max_list(A,B).
my_tail15([_|TL],TL).
my_head16([H|_],H).
my_tail17([_|TL],TL).
my_head18([H|_],H).
my_len19(A,B):-length(A,B).
my_pred20(A,B):-succ(B,A).
my_len21(A,B):-length(A,B).
my_len22(A,B):-length(A,B).
my_min_list23(A,B):-min_list(A,B).
my_max_list24(A,B):-max_list(A,B).
my_head25([H|_],H).
my_reverse26(A,B):-reverse(A,B).
my_max_list27(A,B):-max_list(A,B).
my_tail28([_|TL],TL).
my_head29([H|_],H).
prim(my_head0,[list(T),T]).
prim(my_last1,[list(T),T]).
prim(my_sumlist2,[list(int),int]).
prim(my_len3,[list(T),int]).
prim(my_min_list4,[list(int),int]).
prim(my_sumlist5,[list(int),int]).
prim(my_head6,[list(T),T]).
prim(my_len7,[list(T),int]).
prim(my_len8,[list(T),int]).
prim(my_last9,[list(T),T]).
prim(my_tail10,[list(T),T]).
prim(my_tail11,[list(T),T]).
prim(my_pred12,[int,int]).
prim(my_tail13,[list(T),T]).
prim(my_max_list14,[list(int),int]).
prim(my_tail15,[list(T),T]).
prim(my_head16,[list(T),T]).
prim(my_tail17,[list(T),T]).
prim(my_head18,[list(T),T]).
prim(my_len19,[list(T),int]).
prim(my_pred20,[int,int]).
prim(my_len21,[list(T),int]).
prim(my_len22,[list(T),int]).
prim(my_min_list23,[list(int),int]).
prim(my_max_list24,[list(int),int]).
prim(my_head25,[list(T),T]).
prim(my_reverse26,[list(T),T]).
prim(my_max_list27,[list(int),int]).
prim(my_tail28,[list(T),T]).
prim(my_head29,[list(T),T]).
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
p([['j','k','w'],['g','i','w','f']],[['j','k'],['g','i','w']]).
p([['p','s','d','d'],['h','j','n','r'],['u','g','p','n'],['c','g','p']],[['p','s','d'],['h','j','n'],['u','g','p'],['c','g']]).
