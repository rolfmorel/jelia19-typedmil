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
my_uppercase3(A):-upcase_atom(A,A),char_code(A,_).
my_max_list4(A,B):-max_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_even6(A):-0 is A mod 2.
my_tail7([_|TL],TL).
my_last8(A,B):-last(A,B).
my_odd9(A):-1 is A mod 2.
my_double10(N,M):-M is 2*N,M =< 10.
my_flatten11(A,B):-flatten(A,B).
my_toupper12(A,B):-upcase_atom(A,B),char_code(A,_).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_head14([H|_],H).
my_sumlist15(A,B):-sumlist(A,B).
my_element16(A,B):-member(B,A).
my_set17(A):-list_to_set(A,A).
prim(my_succ1,[int,int]).
prim(my_msort2,[list(int),list(int)]).
prim(my_uppercase3,[char]).
prim(my_max_list4,[list(int),int]).
prim(my_min_list5,[list(int),int]).
prim(my_even6,[int]).
prim(my_tail7,[list(T),list(T)]).
prim(my_last8,[list(T),T]).
prim(my_odd9,[int]).
prim(my_double10,[int,int]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_toupper12,[char,char]).
prim(my_tolower13,[char,char]).
prim(my_head14,[list(T),T]).
prim(my_sumlist15,[list(int),int]).
prim(my_element16,[list(T),T]).
prim(my_set17,[list(_)]).
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
p([[5,4,7],[0,0,4],[7,7,5],[7,4,2]],[[7,6,9],[2,2,6],[9,9,7],[9,6,4]]).
p([[1,5,4],[7,7,6]],[[3,7,6],[9,9,8]]).
p([[5,4,0,1],[7,4,2],[7,2,3,3],[6,0,2]],[[7,6,2,3],[9,6,4],[9,4,5,5],[8,2,4]]).
p([[5,7,4],[7,7,0]],[[7,9,6],[9,9,2]]).
p([[5,5,4],[4,7,4,0]],[[7,7,6],[6,9,6,2]]).
q([[7,7,1],[6,1,2],[5,1,6],[4,4,0]],[[7,7,1],[8,3,4],[7,3,8],[4,4,0]]).
q([[7,1,4],[2,5,6],[3,7,0,0],[4,3,3,1]],[[9,3,6],[4,7,8],[5,9,2,2],[4,3,3,1]]).
q([[1,7,5],[6,3,5],[1,1,5,0],[1,1,5,2]],[[3,9,7],[6,3,5],[1,1,5,0],[3,3,7,4]]).
q([[2,1,2],[1,6,2],[3,2,0]],[[4,3,4],[1,6,2],[5,4,2]]).
q([[0,1,1],[0,1,4],[7,1,3,7],[6,0,6]],[[2,3,3],[2,3,6],[7,1,3,7],[6,0,6]]).
