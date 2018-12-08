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
my_last2(A,B):-last(A,B).
my_element3(A,B):-member(B,A).
my_len4(A,B):-length(A,B).
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).
my_head6([H|_],H).
my_list_to_set7(A,B):-list_to_set(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_msort9(A,B):-msort(A,B).
prim(my_succ1,[int,int]).
prim(my_last2,[list(T),T]).
prim(my_element3,[list(T),T]).
prim(my_len4,[list(_),int]).
prim(my_lowercase5,[char]).
prim(my_head6,[list(T),T]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_double8,[int,int]).
prim(my_msort9,[list(int),list(int)]).
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
p([[1,7,2,3],[6,0,2,4]],[[3,9,4,5],[8,2,4,6]]).
p([[3,3,6,3],[6,3,0,4],[3,1,5,7]],[[5,5,8,5],[8,5,2,6],[5,3,7,9]]).
p([[2,1,3,1],[2,5,1,4],[5,4,4]],[[4,3,5,3],[4,7,3,6],[7,6,6]]).
p([[2,5,5,6],[1,1,1]],[[4,7,7,8],[3,3,3]]).
p([[0,0,4,2],[2,1,5,5]],[[2,2,6,4],[4,3,7,7]]).
q([[1,3,0,1],[1,7,1,6],[1,4,5]],[[3,5,2,3],[3,9,3,8],[1,4,5]]).
q([[5,6,5,2],[4,6,2],[6,3,3,5]],[[7,8,7,4],[6,8,4],[6,3,3,5]]).
q([[5,7,2,7],[5,2,4,4],[1,2,6,4]],[[7,9,4,9],[7,4,6,6],[1,2,6,4]]).
q([[1,4,3],[3,2,6,2]],[[1,4,3],[5,4,8,4]]).
q([[1,7,2,6],[7,2,4]],[[3,9,4,8],[7,2,4]]).
