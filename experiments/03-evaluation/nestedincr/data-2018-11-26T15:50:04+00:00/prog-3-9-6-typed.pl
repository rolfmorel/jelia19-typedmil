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
my_tolower2(A,B):-downcase_atom(A,B),char_code(A,_).
my_list_to_set3(A,B):-list_to_set(A,B).
my_flatten4(A,B):-flatten(A,B).
my_element5(A,B):-member(B,A).
my_sumlist6(A,B):-sumlist(A,B).
my_toupper7(A,B):-upcase_atom(A,B),char_code(A,_).
my_set8(A):-list_to_set(A,A).
my_len9(A,B):-length(A,B).
my_odd10(A):-1 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_tolower2,[char,char]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_element5,[list(T),T]).
prim(my_sumlist6,[list(int),int]).
prim(my_toupper7,[char,char]).
prim(my_set8,[list(_)]).
prim(my_len9,[list(_),int]).
prim(my_odd10,[int]).
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
p([[3,1,2,6],[1,0,4,3]],[[5,3,4,8],[3,2,6,5]]).
p([[1,0,6],[7,1,6],[5,2,2,4]],[[3,2,8],[9,3,8],[7,4,4,6]]).
p([[3,2,6],[7,3,4,5],[7,5,1]],[[5,4,8],[9,5,6,7],[9,7,3]]).
p([[3,1,4,4],[0,4,5,4]],[[5,3,6,6],[2,6,7,6]]).
p([[1,0,6],[6,2,3],[2,4,4]],[[3,2,8],[8,4,5],[4,6,6]]).
q([[2,3,3,0],[5,0,0],[2,4,4,2]],[[4,5,5,2],[7,2,2],[2,4,4,2]]).
q([[4,1,6],[3,1,0],[6,2,5],[4,2,7]],[[6,3,8],[5,3,2],[6,2,5],[6,4,9]]).
q([[7,2,6],[4,3,0],[4,6,4,7]],[[7,2,6],[6,5,2],[6,8,6,9]]).
q([[4,5,0,5],[7,5,0],[0,0,0,6],[5,2,7,1]],[[6,7,2,7],[9,7,2],[2,2,2,8],[5,2,7,1]]).
q([[2,2,2,1],[6,6,5,3],[6,4,6,2]],[[4,4,4,3],[8,8,7,5],[6,4,6,2]]).
