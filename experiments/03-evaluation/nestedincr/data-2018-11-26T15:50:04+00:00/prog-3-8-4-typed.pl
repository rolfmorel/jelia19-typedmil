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

my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_set7(A):-list_to_set(A,A).
my_last8(A,B):-last(A,B).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_toupper3,[char,char]).
prim(my_lowercase4,[char]).
prim(my_len5,[list(_),int]).
prim(my_min_list6,[list(int),int]).
prim(my_set7,[list(_)]).
prim(my_last8,[list(T),T]).
prim(my_tolower9,[char,char]).
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
p([[1,1,3,2],[1,0,1],[5,3,6,6]],[[3,3,5,4],[3,2,3],[7,5,8,8]]).
p([[3,0,4],[5,7,5],[1,0,3,2]],[[5,2,6],[7,9,7],[3,2,5,4]]).
p([[1,5,0],[4,6,1]],[[3,7,2],[6,8,3]]).
p([[4,2,2],[5,0,6],[2,0,6,1],[6,5,1]],[[6,4,4],[7,2,8],[4,2,8,3],[8,7,3]]).
p([[3,0,0],[3,0,1]],[[5,2,2],[5,2,3]]).
q([[7,4,4,0],[6,6,4,7]],[[7,4,4,0],[8,8,6,9]]).
q([[5,0,1],[0,0,7],[2,7,7,2],[5,7,3,1]],[[7,2,3],[0,0,7],[4,9,9,4],[7,9,5,3]]).
q([[6,3,7,6],[0,1,1,4],[4,3,0],[3,6,5,4]],[[8,5,9,8],[0,1,1,4],[4,3,0],[5,8,7,6]]).
q([[3,3,6,0],[7,4,7,6],[5,7,7,1],[7,4,3,0]],[[3,3,6,0],[7,4,7,6],[7,9,9,3],[9,6,5,2]]).
q([[7,3,1],[3,2,4],[0,6,6]],[[9,5,3],[3,2,4],[2,8,8]]).
