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
my_reverse1(A,B):-reverse(A,B).
my_succ2(A,B):-succ(A,B).
my_max_list3(A,B):-max_list(A,B).
my_last4(A,B):-last(A,B).
my_last5(A,B):-last(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_head8([H|_],H).
my_last9(A,B):-last(A,B).
my_len10(A,B):-length(A,B).
my_succ11(A,B):-succ(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_pred16(A,B):-succ(B,A).
my_succ17(A,B):-succ(A,B).
my_tail18([_|TL],TL).
my_min_list19(A,B):-min_list(A,B).
my_min_list20(A,B):-min_list(A,B).
my_max_list21(A,B):-max_list(A,B).
my_reverse22(A,B):-reverse(A,B).
my_max_list23(A,B):-max_list(A,B).
my_len24(A,B):-length(A,B).
my_reverse25(A,B):-reverse(A,B).
my_last26(A,B):-last(A,B).
my_min_list27(A,B):-min_list(A,B).
my_head28([H|_],H).
prim(my_last0,[list(T),T]).
prim(my_reverse1,[list(T),T]).
prim(my_succ2,[int,int]).
prim(my_max_list3,[list(int),int]).
prim(my_last4,[list(T),T]).
prim(my_last5,[list(T),T]).
prim(my_head6,[list(T),T]).
prim(my_tail7,[list(T),T]).
prim(my_head8,[list(T),T]).
prim(my_last9,[list(T),T]).
prim(my_len10,[list(T),int]).
prim(my_succ11,[int,int]).
prim(my_sumlist12,[list(int),int]).
prim(my_sumlist13,[list(int),int]).
prim(my_sumlist14,[list(int),int]).
prim(my_sumlist15,[list(int),int]).
prim(my_pred16,[int,int]).
prim(my_succ17,[int,int]).
prim(my_tail18,[list(T),T]).
prim(my_min_list19,[list(int),int]).
prim(my_min_list20,[list(int),int]).
prim(my_max_list21,[list(int),int]).
prim(my_reverse22,[list(T),T]).
prim(my_max_list23,[list(int),int]).
prim(my_len24,[list(T),int]).
prim(my_reverse25,[list(T),T]).
prim(my_last26,[list(T),T]).
prim(my_min_list27,[list(int),int]).
prim(my_head28,[list(T),T]).
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
p([['y','s','t'],['y','c','f','q'],['l','s','o','p']],[['y','s'],['y','c','f'],['l','s','o']]).
p([['y','g','h','w'],['o','e','o']],[['y','g','h'],['o','e']]).
