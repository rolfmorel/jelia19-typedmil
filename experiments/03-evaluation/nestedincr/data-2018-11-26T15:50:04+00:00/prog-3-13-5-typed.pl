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
my_head2([H|_],H).
my_lowercase3(A):-downcase_atom(A,A),char_code(A,_).
my_pred4(A,B):-succ(B,A),A > 0.
my_reverse5(A,B):-reverse(A,B).
my_flatten6(A,B):-flatten(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_set9(A):-list_to_set(A,A).
my_max_list10(A,B):-max_list(A,B).
my_last11(A,B):-last(A,B).
my_uppercase12(A):-upcase_atom(A,A),char_code(A,_).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_min_list14(A,B):-min_list(A,B).
prim(my_succ1,[int,int]).
prim(my_head2,[list(T),T]).
prim(my_lowercase3,[char]).
prim(my_pred4,[int,int]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_sumlist7,[list(int),int]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_set9,[list(_)]).
prim(my_max_list10,[list(int),int]).
prim(my_last11,[list(T),T]).
prim(my_uppercase12,[char]).
prim(my_tolower13,[char,char]).
prim(my_min_list14,[list(int),int]).
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
p([[2,6,2,1],[0,1,2,7],[7,4,3,7]],[[4,8,4,3],[2,3,4,9],[9,6,5,9]]).
p([[5,1,7,0],[2,4,5]],[[7,3,9,2],[4,6,7]]).
p([[2,2,2],[1,7,4,5]],[[4,4,4],[3,9,6,7]]).
p([[6,3,1,2],[5,4,1],[4,0,2,2],[7,2,7]],[[8,5,3,4],[7,6,3],[6,2,4,4],[9,4,9]]).
p([[0,0,5],[5,4,6]],[[2,2,7],[7,6,8]]).
q([[3,2,1,1],[7,7,5,7],[1,3,2]],[[5,4,3,3],[7,7,5,7],[3,5,4]]).
q([[5,1,4,6],[0,7,3],[2,3,2,1]],[[7,3,6,8],[0,7,3],[4,5,4,3]]).
q([[0,1,6,7],[3,4,6,1],[0,6,2]],[[2,3,8,9],[3,4,6,1],[2,8,4]]).
q([[5,2,5,1],[3,0,7],[2,1,3],[6,7,5]],[[7,4,7,3],[3,0,7],[4,3,5],[6,7,5]]).
q([[6,6,1,5],[2,0,7]],[[8,8,3,7],[2,0,7]]).
