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
my_tail3([_|TL],TL).
my_max_list4(A,B):-max_list(A,B).
my_odd5(A):-1 is A mod 2.
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_even7(A):-0 is A mod 2.
my_len8(A,B):-length(A,B).
my_min_list9(A,B):-min_list(A,B).
my_last10(A,B):-last(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_reverse12(A,B):-reverse(A,B).
my_set13(A):-list_to_set(A,A).
my_list_to_set14(A,B):-list_to_set(A,B).
prim(my_succ1,[int,int]).
prim(my_tolower2,[char,char]).
prim(my_tail3,[list(T),list(T)]).
prim(my_max_list4,[list(int),int]).
prim(my_odd5,[int]).
prim(my_uppercase6,[char]).
prim(my_even7,[int]).
prim(my_len8,[list(_),int]).
prim(my_min_list9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_pred11,[int,int]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_set13,[list(_)]).
prim(my_list_to_set14,[list(T),list(T)]).
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
p([[4,6,3,0],[7,5,1]],[[6,8,5,2],[9,7,3]]).
p([[5,2,6,4],[1,4,2,1]],[[7,4,8,6],[3,6,4,3]]).
p([[6,1,6],[3,1,6]],[[8,3,8],[5,3,8]]).
p([[1,3,4],[6,0,0],[5,6,4,7]],[[3,5,6],[8,2,2],[7,8,6,9]]).
p([[4,0,4,0],[4,3,1],[2,2,0,0],[6,4,6,7]],[[6,2,6,2],[6,5,3],[4,4,2,2],[8,6,8,9]]).
q([[4,1,7,2],[3,6,4,5],[3,0,4],[3,7,4,2]],[[6,3,9,4],[5,8,6,7],[5,2,6],[3,7,4,2]]).
q([[2,4,4],[4,7,2,6],[0,6,1]],[[4,6,6],[4,7,2,6],[2,8,3]]).
q([[3,7,5,0],[3,2,7],[4,3,1,7]],[[3,7,5,0],[5,4,9],[6,5,3,9]]).
q([[5,3,6,4],[0,1,2],[4,7,2,7]],[[5,3,6,4],[2,3,4],[6,9,4,9]]).
q([[6,6,2,2],[4,7,2,7],[1,1,4,6],[4,4,5,7]],[[6,6,2,2],[6,9,4,9],[1,1,4,6],[6,6,7,9]]).
