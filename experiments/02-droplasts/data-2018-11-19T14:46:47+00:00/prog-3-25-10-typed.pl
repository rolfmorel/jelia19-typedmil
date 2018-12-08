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
my_reverse0(A,B):-reverse(A,B).
my_head1([H|_],H).
my_head2([H|_],H).
my_max_list3(A,B):-max_list(A,B).
my_reverse4(A,B):-reverse(A,B).
my_last5(A,B):-last(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_max_list7(A,B):-max_list(A,B).
my_last8(A,B):-last(A,B).
my_len9(A,B):-length(A,B).
my_min_list10(A,B):-min_list(A,B).
my_max_list11(A,B):-max_list(A,B).
my_succ12(A,B):-succ(A,B).
my_succ13(A,B):-succ(A,B).
my_last14(A,B):-last(A,B).
my_len15(A,B):-length(A,B).
my_min_list16(A,B):-min_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_last18(A,B):-last(A,B).
my_reverse19(A,B):-reverse(A,B).
my_min_list20(A,B):-min_list(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_tail22([_|TL],TL).
my_last23(A,B):-last(A,B).
my_sumlist24(A,B):-sumlist(A,B).
prim(my_reverse0,[list(T),T]).
prim(my_head1,[list(T),T]).
prim(my_head2,[list(T),T]).
prim(my_max_list3,[list(int),int]).
prim(my_reverse4,[list(T),T]).
prim(my_last5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_max_list7,[list(int),int]).
prim(my_last8,[list(T),T]).
prim(my_len9,[list(T),int]).
prim(my_min_list10,[list(int),int]).
prim(my_max_list11,[list(int),int]).
prim(my_succ12,[int,int]).
prim(my_succ13,[int,int]).
prim(my_last14,[list(T),T]).
prim(my_len15,[list(T),int]).
prim(my_min_list16,[list(int),int]).
prim(my_sumlist17,[list(int),int]).
prim(my_last18,[list(T),T]).
prim(my_reverse19,[list(T),T]).
prim(my_min_list20,[list(int),int]).
prim(my_sumlist21,[list(int),int]).
prim(my_tail22,[list(T),T]).
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
p([['f','d','j'],['p','g','h'],['l','v','e'],['f','j','n','a']],[['f','d'],['p','g'],['l','v'],['f','j','n']]).
p([['f','w','s'],['p','m','r'],['t','h','t'],['j','c','j']],[['f','w'],['p','m'],['t','h'],['j','c']]).
