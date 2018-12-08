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
my_head1([H|_],H).
my_reverse2(A,B):-reverse(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_head5([H|_],H).
my_reverse6(A,B):-reverse(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_head8([H|_],H).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_tail11([_|TL],TL).
my_sumlist12(A,B):-sumlist(A,B).
my_len13(A,B):-length(A,B).
my_last14(A,B):-last(A,B).
my_last15(A,B):-last(A,B).
my_head16([H|_],H).
my_reverse17(A,B):-reverse(A,B).
my_reverse18(A,B):-reverse(A,B).
my_min_list19(A,B):-min_list(A,B).
my_min_list20(A,B):-min_list(A,B).
my_max_list21(A,B):-max_list(A,B).
my_min_list22(A,B):-min_list(A,B).
my_len23(A,B):-length(A,B).
my_min_list24(A,B):-min_list(A,B).
my_len25(A,B):-length(A,B).
my_max_list26(A,B):-max_list(A,B).
prim(my_sumlist0,[list(int),int]).
prim(my_head1,[list(T),T]).
prim(my_reverse2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_head5,[list(T),T]).
prim(my_reverse6,[list(T),T]).
prim(my_sumlist7,[list(int),int]).
prim(my_head8,[list(T),T]).
prim(my_min_list9,[list(int),int]).
prim(my_reverse10,[list(T),T]).
prim(my_tail11,[list(T),T]).
prim(my_sumlist12,[list(int),int]).
prim(my_len13,[list(T),int]).
prim(my_last14,[list(T),T]).
prim(my_last15,[list(T),T]).
prim(my_head16,[list(T),T]).
prim(my_reverse17,[list(T),T]).
prim(my_reverse18,[list(T),T]).
prim(my_min_list19,[list(int),int]).
prim(my_min_list20,[list(int),int]).
prim(my_max_list21,[list(int),int]).
prim(my_min_list22,[list(int),int]).
prim(my_len23,[list(T),int]).
prim(my_min_list24,[list(int),int]).
prim(my_len25,[list(T),int]).
prim(my_max_list26,[list(int),int]).
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
p([['j','m','f'],['g','o','m','t'],['k','f','a','b']],[['j','m'],['g','o','m'],['k','f','a']]).
p([['u','m','c','x'],['d','v','k','n'],['o','r','x'],['q','x','c','a']],[['u','m','c'],['d','v','k'],['o','r'],['q','x','c']]).
