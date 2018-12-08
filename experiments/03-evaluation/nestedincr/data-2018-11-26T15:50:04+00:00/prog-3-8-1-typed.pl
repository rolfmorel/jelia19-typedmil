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
my_len2(A,B):-length(A,B).
my_tail3([_|TL],TL).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_list_to_set5(A,B):-list_to_set(A,B).
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_set7(A):-list_to_set(A,A).
my_element8(A,B):-member(B,A).
my_head9([H|_],H).
prim(my_succ1,[int,int]).
prim(my_len2,[list(_),int]).
prim(my_tail3,[list(T),list(T)]).
prim(my_lowercase4,[char]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_uppercase6,[char]).
prim(my_set7,[list(_)]).
prim(my_element8,[list(T),T]).
prim(my_head9,[list(T),T]).
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
p([[4,2,7,2],[5,3,7,3],[4,3,2]],[[6,4,9,4],[7,5,9,5],[6,5,4]]).
p([[1,2,5,6],[1,6,5],[7,6,3]],[[3,4,7,8],[3,8,7],[9,8,5]]).
p([[1,5,0,1],[4,6,4,4],[0,7,1,7]],[[3,7,2,3],[6,8,6,6],[2,9,3,9]]).
p([[0,5,5,0],[4,0,5,1]],[[2,7,7,2],[6,2,7,3]]).
p([[3,0,5,0],[3,1,5,1]],[[5,2,7,2],[5,3,7,3]]).
q([[3,6,3,3],[7,5,1,5],[1,7,0,2]],[[5,8,5,5],[7,5,1,5],[3,9,2,4]]).
q([[5,2,0,6],[2,5,5],[7,4,2,5]],[[7,4,2,8],[4,7,7],[7,4,2,5]]).
q([[4,6,6],[0,1,7],[1,3,7,5],[5,6,1]],[[6,8,8],[2,3,9],[1,3,7,5],[5,6,1]]).
q([[1,7,5,0],[3,3,4]],[[3,9,7,2],[3,3,4]]).
q([[3,4,3],[0,0,3]],[[5,6,5],[0,0,3]]).
