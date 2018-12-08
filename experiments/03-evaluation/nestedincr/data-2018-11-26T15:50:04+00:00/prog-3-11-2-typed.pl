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
my_pred2(A,B):-succ(B,A),A > 0.
my_tolower3(A,B):-downcase_atom(A,B),char_code(A,_).
my_tail4([_|TL],TL).
my_msort5(A,B):-msort(A,B).
my_head6([H|_],H).
my_uppercase7(A):-upcase_atom(A,A),char_code(A,_).
my_flatten8(A,B):-flatten(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_even10(A):-0 is A mod 2.

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).

my_odd12(A):-1 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_pred2,[int,int]).
prim(my_tolower3,[char,char]).
prim(my_tail4,[list(T),list(T)]).
prim(my_msort5,[list(int),list(int)]).
prim(my_head6,[list(T),T]).
prim(my_uppercase7,[char]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_sumlist9,[list(int),int]).
prim(my_even10,[int]).
prim(my_odd12,[int]).
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
p([[0,4,6,6],[7,4,1],[0,0,1]],[[2,6,8,8],[9,6,3],[2,2,3]]).
p([[5,0,4,7],[5,2,1,3],[0,7,5,4]],[[7,2,6,9],[7,4,3,5],[2,9,7,6]]).
p([[3,1,4,5],[7,2,6,4],[2,7,2,7]],[[5,3,6,7],[9,4,8,6],[4,9,4,9]]).
p([[3,6,7,6],[5,7,5],[7,7,7,5]],[[5,8,9,8],[7,9,7],[9,9,9,7]]).
p([[7,0,1,0],[0,2,0,2],[2,2,2,5]],[[9,2,3,2],[2,4,2,4],[4,4,4,7]]).
q([[1,7,7,7],[7,4,5]],[[1,7,7,7],[9,6,7]]).
q([[1,5,5,1],[7,0,4,5],[6,4,6,0],[1,1,2,2]],[[1,5,5,1],[7,0,4,5],[8,6,8,2],[3,3,4,4]]).
q([[3,4,2],[5,4,7],[3,2,4]],[[5,6,4],[5,4,7],[5,4,6]]).
q([[3,5,4,7],[7,0,3,3],[7,3,2],[0,1,4,5]],[[5,7,6,9],[7,0,3,3],[9,5,4],[0,1,4,5]]).
q([[3,3,4],[2,4,7,4],[4,4,5]],[[5,5,6],[2,4,7,4],[6,6,7]]).
