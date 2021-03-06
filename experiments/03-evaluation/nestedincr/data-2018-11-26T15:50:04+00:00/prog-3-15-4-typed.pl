:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_flatten2(A,B):-flatten(A,B).
my_max_list3(A,B):-max_list(A,B).
my_last4(A,B):-last(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_msort6(A,B):-msort(A,B).
my_tail7([_|TL],TL).
my_double8(N,M):-M is 2*N,M =< 10.
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
my_element10(A,B):-member(B,A).
my_sumlist11(A,B):-sumlist(A,B).
my_len12(A,B):-length(A,B).
my_lowercase13(A):-downcase_atom(A,A),char_code(A,_).
my_min_list14(A,B):-min_list(A,B).
my_set15(A):-list_to_set(A,A).
my_even16(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_flatten2,[list(list(T)),list(T)]).
prim(my_max_list3,[list(int),int]).
prim(my_last4,[list(T),T]).
prim(my_pred5,[int,int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_tail7,[list(T),list(T)]).
prim(my_double8,[int,int]).
prim(my_tolower9,[char,char]).
prim(my_element10,[list(T),T]).
prim(my_sumlist11,[list(int),int]).
prim(my_len12,[list(_),int]).
prim(my_lowercase13,[char]).
prim(my_min_list14,[list(int),int]).
prim(my_set15,[list(_)]).
prim(my_even16,[int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(int)),list(list(int))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([[7,7,7],[3,5,1]],[[9,9,9],[5,7,3]]).
p([[0,3,7,2],[4,3,6],[4,2,5],[2,1,5,5]],[[2,5,9,4],[6,5,8],[6,4,7],[4,3,7,7]]).
p([[0,0,4],[2,3,1],[0,3,2,2],[2,1,4,2]],[[2,2,6],[4,5,3],[2,5,4,4],[4,3,6,4]]).
p([[4,4,2,6],[1,7,7,3]],[[6,6,4,8],[3,9,9,5]]).
p([[0,5,0],[5,0,3,2],[3,4,2,6]],[[2,7,2],[7,2,5,4],[5,6,4,8]]).
q([[3,4,7,2],[2,3,0,0],[3,0,7]],[[5,6,9,4],[2,3,0,0],[5,2,9]]).
q([[1,4,2,6],[5,0,2,7],[3,5,7],[1,7,2,1]],[[1,4,2,6],[7,2,4,9],[3,5,7],[3,9,4,3]]).
q([[1,1,4,0],[1,5,2,6],[1,6,4],[0,3,6,2]],[[3,3,6,2],[1,5,2,6],[3,8,6],[2,5,8,4]]).
q([[1,4,3,3],[6,7,5,3],[7,4,4],[2,4,6]],[[3,6,5,5],[6,7,5,3],[9,6,6],[4,6,8]]).
q([[1,1,3],[5,7,2],[1,0,6,7],[1,7,6]],[[1,1,3],[7,9,4],[3,2,8,9],[1,7,6]]).
