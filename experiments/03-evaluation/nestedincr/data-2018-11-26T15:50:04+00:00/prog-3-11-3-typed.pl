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
my_max_list4(A,B):-max_list(A,B).
my_uppercase5(A):-upcase_atom(A,A),char_code(A,_).
my_len6(A,B):-length(A,B).
my_head7([H|_],H).
my_lowercase8(A):-downcase_atom(A,A),char_code(A,_).
my_flatten9(A,B):-flatten(A,B).
my_min_list10(A,B):-min_list(A,B).
my_set11(A):-list_to_set(A,A).
my_element12(A,B):-member(B,A).
prim(my_succ1,[int,int]).
prim(my_tolower2,[char,char]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_max_list4,[list(int),int]).
prim(my_uppercase5,[char]).
prim(my_len6,[list(_),int]).
prim(my_head7,[list(T),T]).
prim(my_lowercase8,[char]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_min_list10,[list(int),int]).
prim(my_set11,[list(_)]).
prim(my_element12,[list(T),T]).
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
p([[1,1,1],[5,3,4]],[[3,3,3],[7,5,6]]).
p([[1,2,3,0],[3,6,1]],[[3,4,5,2],[5,8,3]]).
p([[0,0,4,4],[4,4,6],[0,0,5,0],[5,0,0,7]],[[2,2,6,6],[6,6,8],[2,2,7,2],[7,2,2,9]]).
p([[5,6,5],[5,1,7,2]],[[7,8,7],[7,3,9,4]]).
p([[7,7,2],[5,2,1],[7,0,5]],[[9,9,4],[7,4,3],[9,2,7]]).
q([[5,6,5],[6,3,0]],[[7,8,7],[6,3,0]]).
q([[2,2,1,5],[6,2,1],[4,6,0,6],[3,0,3,4]],[[4,4,3,7],[6,2,1],[6,8,2,8],[3,0,3,4]]).
q([[2,3,3,6],[4,0,4,7]],[[4,5,5,8],[4,0,4,7]]).
q([[4,3,5],[2,0,5,5],[5,4,3,1],[5,4,6]],[[6,5,7],[4,2,7,7],[5,4,3,1],[5,4,6]]).
q([[3,4,2],[1,6,5]],[[3,4,2],[3,8,7]]).
