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
my_tail1([_|TL],TL).
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_head4([H|_],H).
my_len5(A,B):-length(A,B).
my_head6([H|_],H).
my_min_list7(A,B):-min_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_head9([H|_],H).
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
my_pred12(A,B):-succ(B,A).
my_sumlist13(A,B):-sumlist(A,B).
my_head14([H|_],H).
my_len15(A,B):-length(A,B).
my_max_list16(A,B):-max_list(A,B).
my_succ17(A,B):-succ(A,B).
my_last18(A,B):-last(A,B).
my_min_list19(A,B):-min_list(A,B).
my_reverse20(A,B):-reverse(A,B).
my_succ21(A,B):-succ(A,B).
my_max_list22(A,B):-max_list(A,B).
my_max_list23(A,B):-max_list(A,B).
my_last24(A,B):-last(A,B).
my_min_list25(A,B):-min_list(A,B).
my_succ26(A,B):-succ(A,B).
my_last27(A,B):-last(A,B).
prim(my_sumlist0,[list(int),int]).
prim(my_tail1,[list(T),T]).
prim(my_sumlist2,[list(int),int]).
prim(my_reverse3,[list(T),T]).
prim(my_head4,[list(T),T]).
prim(my_len5,[list(T),int]).
prim(my_head6,[list(T),T]).
prim(my_min_list7,[list(int),int]).
prim(my_reverse8,[list(T),T]).
prim(my_head9,[list(T),T]).
prim(my_sumlist10,[list(int),int]).
prim(my_tail11,[list(T),T]).
prim(my_pred12,[int,int]).
prim(my_sumlist13,[list(int),int]).
prim(my_head14,[list(T),T]).
prim(my_len15,[list(T),int]).
prim(my_max_list16,[list(int),int]).
prim(my_succ17,[int,int]).
prim(my_last18,[list(T),T]).
prim(my_min_list19,[list(int),int]).
prim(my_reverse20,[list(T),T]).
prim(my_succ21,[int,int]).
prim(my_max_list22,[list(int),int]).
prim(my_max_list23,[list(int),int]).
prim(my_last24,[list(T),T]).
prim(my_min_list25,[list(int),int]).
prim(my_succ26,[int,int]).
prim(my_last27,[list(T),T]).
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
p([['x','c','t'],['f','t','a'],['d','c','q','j']],[['x','c'],['f','t'],['d','c','q']]).
p([['s','r','b','q'],['j','v','a'],['m','f','o'],['i','o','f','b']],[['s','r','b'],['j','v'],['m','f'],['i','o','f']]).
