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
my_tail2([_|TL],TL).
my_min_list3(A,B):-min_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_toupper6(A,B):-upcase_atom(A,B),char_code(A,_).
my_pred7(A,B):-succ(B,A),A > 0.
my_uppercase8(A):-upcase_atom(A,A),char_code(A,_).
my_list_to_set9(A,B):-list_to_set(A,B).
my_lowercase10(A):-downcase_atom(A,A),char_code(A,_).
my_double11(N,M):-M is 2*N,M =< 10.
my_last12(A,B):-last(A,B).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_head14([H|_],H).
my_odd15(A):-1 is A mod 2.
my_flatten16(A,B):-flatten(A,B).
prim(my_succ1,[int,int]).
prim(my_tail2,[list(T),list(T)]).
prim(my_min_list3,[list(int),int]).
prim(my_max_list4,[list(int),int]).
prim(my_sumlist5,[list(int),int]).
prim(my_toupper6,[char,char]).
prim(my_pred7,[int,int]).
prim(my_uppercase8,[char]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_lowercase10,[char]).
prim(my_double11,[int,int]).
prim(my_last12,[list(T),T]).
prim(my_tolower13,[char,char]).
prim(my_head14,[list(T),T]).
prim(my_odd15,[int]).
prim(my_flatten16,[list(list(T)),list(T)]).
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
p([[0,0,3,3],[0,1,3,7],[6,2,4],[3,1,6]],[[2,2,5,5],[2,3,5,9],[8,4,6],[5,3,8]]).
p([[7,0,5,1],[6,1,0],[4,4,0]],[[9,2,7,3],[8,3,2],[6,6,2]]).
p([[2,4,4,5],[6,1,0,4]],[[4,6,6,7],[8,3,2,6]]).
p([[4,6,5],[3,4,2],[6,1,4,0],[2,3,5]],[[6,8,7],[5,6,4],[8,3,6,2],[4,5,7]]).
p([[5,1,6,5],[1,2,7]],[[7,3,8,7],[3,4,9]]).
q([[0,5,0,1],[2,5,4]],[[0,5,0,1],[4,7,6]]).
q([[1,4,5],[6,4,0],[6,7,6,4],[6,6,4]],[[1,4,5],[6,4,0],[8,9,8,6],[8,8,6]]).
q([[4,0,0,0],[1,3,0]],[[6,2,2,2],[1,3,0]]).
q([[1,5,0],[5,5,6,7],[4,3,2,0]],[[3,7,2],[5,5,6,7],[6,5,4,2]]).
q([[7,4,2,6],[5,6,3,5]],[[9,6,4,8],[5,6,3,5]]).
