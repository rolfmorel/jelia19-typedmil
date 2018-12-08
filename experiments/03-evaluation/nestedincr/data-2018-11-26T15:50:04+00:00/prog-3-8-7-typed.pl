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
my_msort2(A,B):-msort(A,B).
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_list_to_set4(A,B):-list_to_set(A,B).
my_flatten5(A,B):-flatten(A,B).
my_tail6([_|TL],TL).
my_element7(A,B):-member(B,A).
my_set8(A):-list_to_set(A,A).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_msort2,[list(int),list(int)]).
prim(my_toupper3,[char,char]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_tail6,[list(T),list(T)]).
prim(my_element7,[list(T),T]).
prim(my_set8,[list(_)]).
prim(my_uppercase9,[char]).
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
p([[4,7,1,7],[5,5,3,4],[5,7,4,2],[7,5,6]],[[6,9,3,9],[7,7,5,6],[7,9,6,4],[9,7,8]]).
p([[7,6,0,3],[6,6,1],[2,3,0],[0,1,0,2]],[[9,8,2,5],[8,8,3],[4,5,2],[2,3,2,4]]).
p([[0,7,5],[6,5,3,3],[5,5,2]],[[2,9,7],[8,7,5,5],[7,7,4]]).
p([[4,4,5,2],[2,2,6,5]],[[6,6,7,4],[4,4,8,7]]).
p([[1,0,1],[1,6,2,4]],[[3,2,3],[3,8,4,6]]).
q([[2,0,0],[1,6,3,5]],[[4,2,2],[1,6,3,5]]).
q([[5,2,7],[4,3,3,7],[1,2,4,4],[2,4,5]],[[7,4,9],[6,5,5,9],[1,2,4,4],[4,6,7]]).
q([[6,0,3],[5,2,4,0],[3,3,3,1]],[[6,0,3],[7,4,6,2],[5,5,5,3]]).
q([[7,7,7,7],[7,3,7],[4,2,5]],[[7,7,7,7],[9,5,9],[6,4,7]]).
q([[3,5,0],[1,3,6,0],[1,4,3],[7,5,6,1]],[[5,7,2],[1,3,6,0],[3,6,5],[9,7,8,3]]).
