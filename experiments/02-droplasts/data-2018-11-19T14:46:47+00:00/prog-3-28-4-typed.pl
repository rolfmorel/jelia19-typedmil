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
my_reverse1(A,B):-reverse(A,B).
my_head2([H|_],H).
my_head3([H|_],H).
my_sumlist4(A,B):-sumlist(A,B).
my_tail5([_|TL],TL).
my_len6(A,B):-length(A,B).
my_len7(A,B):-length(A,B).
my_pred8(A,B):-succ(B,A).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_pred11(A,B):-succ(B,A).
my_head12([H|_],H).
my_min_list13(A,B):-min_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_succ17(A,B):-succ(A,B).
my_pred18(A,B):-succ(B,A).
my_len19(A,B):-length(A,B).
my_pred20(A,B):-succ(B,A).
my_reverse21(A,B):-reverse(A,B).
my_last22(A,B):-last(A,B).
my_min_list23(A,B):-min_list(A,B).
my_sumlist24(A,B):-sumlist(A,B).
my_pred25(A,B):-succ(B,A).
my_head26([H|_],H).
my_last27(A,B):-last(A,B).
prim(my_pred0,[int,int]).
prim(my_reverse1,[list(T),T]).
prim(my_head2,[list(T),T]).
prim(my_head3,[list(T),T]).
prim(my_sumlist4,[list(int),int]).
prim(my_tail5,[list(T),T]).
prim(my_len6,[list(T),int]).
prim(my_len7,[list(T),int]).
prim(my_pred8,[int,int]).
prim(my_min_list9,[list(int),int]).
prim(my_reverse10,[list(T),T]).
prim(my_pred11,[int,int]).
prim(my_head12,[list(T),T]).
prim(my_min_list13,[list(int),int]).
prim(my_reverse14,[list(T),T]).
prim(my_sumlist15,[list(int),int]).
prim(my_sumlist16,[list(int),int]).
prim(my_succ17,[int,int]).
prim(my_pred18,[int,int]).
prim(my_len19,[list(T),int]).
prim(my_pred20,[int,int]).
prim(my_reverse21,[list(T),T]).
prim(my_last22,[list(T),T]).
prim(my_min_list23,[list(int),int]).
prim(my_sumlist24,[list(int),int]).
prim(my_pred25,[int,int]).
prim(my_head26,[list(T),T]).
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
p([['m','c','s','p'],['o','o','t','u'],['k','p','a']],[['m','c','s'],['o','o','t'],['k','p']]).
p([['t','r','l','w'],['b','x','i','r'],['j','f','l','c'],['g','t','h','s']],[['t','r','l'],['b','x','i'],['j','f','l'],['g','t','h']]).
