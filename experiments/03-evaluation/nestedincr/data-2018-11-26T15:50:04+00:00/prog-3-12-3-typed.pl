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
my_toupper2(A,B):-upcase_atom(A,B),char_code(A,_).
my_sumlist3(A,B):-sumlist(A,B).
my_tail4([_|TL],TL).
my_odd5(A):-1 is A mod 2.
my_reverse6(A,B):-reverse(A,B).
my_max_list7(A,B):-max_list(A,B).
my_set8(A):-list_to_set(A,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_even10(A):-0 is A mod 2.
my_flatten11(A,B):-flatten(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_min_list13(A,B):-min_list(A,B).
prim(my_succ1,[int,int]).
prim(my_toupper2,[char,char]).
prim(my_sumlist3,[list(int),int]).
prim(my_tail4,[list(T),list(T)]).
prim(my_odd5,[int]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_max_list7,[list(int),int]).
prim(my_set8,[list(_)]).
prim(my_pred9,[int,int]).
prim(my_even10,[int]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_double12,[int,int]).
prim(my_min_list13,[list(int),int]).
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
p([[6,3,2,4],[0,7,5,4]],[[8,5,4,6],[2,9,7,6]]).
p([[1,3,3],[2,1,2],[3,7,4],[4,5,0]],[[3,5,5],[4,3,4],[5,9,6],[6,7,2]]).
p([[0,5,3],[7,4,7],[1,0,4,3]],[[2,7,5],[9,6,9],[3,2,6,5]]).
p([[0,6,6,1],[7,5,0,1],[2,2,3],[1,2,5]],[[2,8,8,3],[9,7,2,3],[4,4,5],[3,4,7]]).
p([[4,2,0],[5,5,1],[2,5,4,2],[0,5,3,4]],[[6,4,2],[7,7,3],[4,7,6,4],[2,7,5,6]]).
q([[0,6,7],[5,4,2,5],[4,5,3]],[[2,8,9],[7,6,4,7],[4,5,3]]).
q([[2,6,5],[6,0,1],[0,1,5,2],[4,3,7]],[[4,8,7],[8,2,3],[0,1,5,2],[4,3,7]]).
q([[6,0,6],[5,6,5,7],[1,2,7]],[[8,2,8],[5,6,5,7],[3,4,9]]).
q([[6,7,7],[7,3,2,3]],[[6,7,7],[9,5,4,5]]).
q([[2,6,1,5],[2,0,6],[5,1,5],[2,7,1]],[[4,8,3,7],[2,0,6],[7,3,7],[4,9,3]]).
