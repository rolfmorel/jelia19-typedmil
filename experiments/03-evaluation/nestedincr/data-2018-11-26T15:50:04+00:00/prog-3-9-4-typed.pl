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
my_element2(A,B):-member(B,A).
my_msort3(A,B):-msort(A,B).
my_uppercase4(A):-upcase_atom(A,A),char_code(A,_).
my_reverse5(A,B):-reverse(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_set7(A):-list_to_set(A,A).
my_odd8(A):-1 is A mod 2.
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_even10(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_element2,[list(T),T]).
prim(my_msort3,[list(int),list(int)]).
prim(my_uppercase4,[char]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_sumlist6,[list(int),int]).
prim(my_set7,[list(_)]).
prim(my_odd8,[int]).
prim(my_lowercase9,[char]).
prim(my_even10,[int]).
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
p([[7,2,3],[1,2,7,2],[3,6,5,3],[7,4,5,7]],[[9,4,5],[3,4,9,4],[5,8,7,5],[9,6,7,9]]).
p([[7,4,5],[4,6,4],[0,7,5],[5,1,3]],[[9,6,7],[6,8,6],[2,9,7],[7,3,5]]).
p([[2,5,3],[3,5,4,7],[3,1,4,7]],[[4,7,5],[5,7,6,9],[5,3,6,9]]).
p([[6,0,1,7],[3,7,3,5],[2,1,5]],[[8,2,3,9],[5,9,5,7],[4,3,7]]).
p([[5,2,2,2],[6,2,6],[2,6,7],[7,3,5,6]],[[7,4,4,4],[8,4,8],[4,8,9],[9,5,7,8]]).
q([[7,5,4,7],[0,3,7]],[[7,5,4,7],[2,5,9]]).
q([[1,4,0,4],[0,6,0,1]],[[1,4,0,4],[2,8,2,3]]).
q([[7,5,1],[2,3,1,0]],[[9,7,3],[2,3,1,0]]).
q([[0,3,3],[4,4,5,2],[7,7,6,2]],[[2,5,5],[6,6,7,4],[7,7,6,2]]).
q([[6,2,1,6],[4,1,5,5]],[[8,4,3,8],[4,1,5,5]]).
