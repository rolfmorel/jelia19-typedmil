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
my_pred0(A,B):-succ(B,A).
my_sumlist1(A,B):-sumlist(A,B).
my_succ2(A,B):-succ(A,B).
my_reverse3(A,B):-reverse(A,B).
my_pred4(A,B):-succ(B,A).
my_head5([H|_],H).
my_pred6(A,B):-succ(B,A).
my_sumlist7(A,B):-sumlist(A,B).
my_reverse8(A,B):-reverse(A,B).
my_min_list9(A,B):-min_list(A,B).
my_tail10([_|TL],TL).
my_min_list11(A,B):-min_list(A,B).
my_min_list12(A,B):-min_list(A,B).
my_max_list13(A,B):-max_list(A,B).
my_head14([H|_],H).
my_succ15(A,B):-succ(A,B).
my_len16(A,B):-length(A,B).
my_head17([H|_],H).
my_last18(A,B):-last(A,B).
my_pred19(A,B):-succ(B,A).
my_succ20(A,B):-succ(A,B).
my_reverse21(A,B):-reverse(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_tail23([_|TL],TL).
my_len24(A,B):-length(A,B).
my_tail25([_|TL],TL).
my_len26(A,B):-length(A,B).
my_sumlist27(A,B):-sumlist(A,B).
prim(my_pred0,[int,int]).
prim(my_sumlist1,[list(int),int]).
prim(my_succ2,[int,int]).
prim(my_reverse3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_head5,[list(T),T]).
prim(my_pred6,[int,int]).
prim(my_sumlist7,[list(int),int]).
prim(my_reverse8,[list(T),T]).
prim(my_min_list9,[list(int),int]).
prim(my_tail10,[list(T),T]).
prim(my_min_list11,[list(int),int]).
prim(my_min_list12,[list(int),int]).
prim(my_max_list13,[list(int),int]).
prim(my_head14,[list(T),T]).
prim(my_succ15,[int,int]).
prim(my_len16,[list(T),int]).
prim(my_head17,[list(T),T]).
prim(my_last18,[list(T),T]).
prim(my_pred19,[int,int]).
prim(my_succ20,[int,int]).
prim(my_reverse21,[list(T),T]).
prim(my_sumlist22,[list(int),int]).
prim(my_tail23,[list(T),T]).
prim(my_len24,[list(T),int]).
prim(my_tail25,[list(T),T]).
prim(my_len26,[list(T),int]).
prim(my_sumlist27,[list(int),int]).
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
p([['u','v','q','u'],['u','v','c','l'],['w','v','m']],[['u','v','q'],['u','v','c'],['w','v']]).
p([['f','k','h'],['j','l','k'],['t','q','v','y'],['a','h','w']],[['f','k'],['j','l'],['t','q','v'],['a','h']]).
