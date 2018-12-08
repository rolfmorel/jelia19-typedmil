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
my_last0(A,B):-last(A,B).
my_sumlist1(A,B):-sumlist(A,B).
my_pred2(A,B):-succ(B,A).
my_sumlist3(A,B):-sumlist(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_last7(A,B):-last(A,B).
my_head8([H|_],H).
my_len9(A,B):-length(A,B).
my_max_list10(A,B):-max_list(A,B).
my_tail11([_|TL],TL).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_reverse14(A,B):-reverse(A,B).
my_reverse15(A,B):-reverse(A,B).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
my_tail18([_|TL],TL).
my_pred19(A,B):-succ(B,A).
my_sumlist20(A,B):-sumlist(A,B).
my_min_list21(A,B):-min_list(A,B).
my_pred22(A,B):-succ(B,A).
my_succ23(A,B):-succ(A,B).
my_tail24([_|TL],TL).
my_pred25(A,B):-succ(B,A).
my_sumlist26(A,B):-sumlist(A,B).
my_len27(A,B):-length(A,B).
my_reverse28(A,B):-reverse(A,B).
my_succ29(A,B):-succ(A,B).
prim(my_last0,[list(T),T]).
prim(my_sumlist1,[list(int),int]).
prim(my_pred2,[int,int]).
prim(my_sumlist3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_succ5,[int,int]).
prim(my_head6,[list(T),T]).
prim(my_last7,[list(T),T]).
prim(my_head8,[list(T),T]).
prim(my_len9,[list(T),int]).
prim(my_max_list10,[list(int),int]).
prim(my_tail11,[list(T),T]).
prim(my_reverse12,[list(T),T]).
prim(my_len13,[list(T),int]).
prim(my_reverse14,[list(T),T]).
prim(my_reverse15,[list(T),T]).
prim(my_reverse16,[list(T),T]).
prim(my_max_list17,[list(int),int]).
prim(my_tail18,[list(T),T]).
prim(my_pred19,[int,int]).
prim(my_sumlist20,[list(int),int]).
prim(my_min_list21,[list(int),int]).
prim(my_pred22,[int,int]).
prim(my_succ23,[int,int]).
prim(my_tail24,[list(T),T]).
prim(my_pred25,[int,int]).
prim(my_sumlist26,[list(int),int]).
prim(my_len27,[list(T),int]).
prim(my_reverse28,[list(T),T]).
prim(my_succ29,[int,int]).
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
p([['m','q','v'],['e','n','a'],['o','f','l','j'],['f','g','o','p']],[['m','q'],['e','n'],['o','f','l'],['f','g','o']]).
p([['u','p','k','t'],['q','g','p'],['g','e','h','o']],[['u','p','k'],['q','g'],['g','e','h']]).
