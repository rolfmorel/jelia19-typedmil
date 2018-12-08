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
my_uppercase2(A):-upcase_atom(A,A),char_code(A,_).
my_tail3([_|TL],TL).
my_element4(A,B):-member(B,A).
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).
my_head6([H|_],H).
my_reverse7(A,B):-reverse(A,B).
my_set8(A):-list_to_set(A,A).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).

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

my_list_to_set11(A,B):-list_to_set(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_last13(A,B):-last(A,B).
prim(my_succ1,[int,int]).
prim(my_uppercase2,[char]).
prim(my_tail3,[list(T),list(T)]).
prim(my_element4,[list(T),T]).
prim(my_lowercase5,[char]).
prim(my_head6,[list(T),T]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_set8,[list(_)]).
prim(my_tolower9,[char,char]).
prim(my_list_to_set11,[list(T),list(T)]).
prim(my_double12,[int,int]).
prim(my_last13,[list(T),T]).
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
p([[0,3,0,1],[0,2,7,2]],[[2,5,2,3],[2,4,9,4]]).
p([[5,5,1,2],[6,2,3]],[[7,7,3,4],[8,4,5]]).
p([[7,7,5],[2,5,2,1],[1,3,4]],[[9,9,7],[4,7,4,3],[3,5,6]]).
p([[1,1,7,1],[5,5,7,4],[7,6,5],[3,4,4]],[[3,3,9,3],[7,7,9,6],[9,8,7],[5,6,6]]).
p([[3,2,0],[6,3,0,2],[7,0,3,4],[3,3,1]],[[5,4,2],[8,5,2,4],[9,2,5,6],[5,5,3]]).
q([[2,7,5],[1,2,5],[5,2,0]],[[4,9,7],[3,4,7],[5,2,0]]).
q([[4,7,7,1],[4,5,5,6]],[[4,7,7,1],[6,7,7,8]]).
q([[1,2,0],[3,4,3,3],[7,0,7,0]],[[1,2,0],[5,6,5,5],[9,2,9,2]]).
q([[5,2,0],[5,0,1]],[[7,4,2],[5,0,1]]).
q([[3,6,6],[6,5,4,7]],[[5,8,8],[6,5,4,7]]).
