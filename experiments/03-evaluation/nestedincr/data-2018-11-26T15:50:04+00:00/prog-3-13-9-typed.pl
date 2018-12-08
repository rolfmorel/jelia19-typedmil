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
my_flatten2(A,B):-flatten(A,B).
my_element3(A,B):-member(B,A).
my_msort4(A,B):-msort(A,B).
my_head5([H|_],H).
my_max_list6(A,B):-max_list(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_reverse8(A,B):-reverse(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_tolower11(A,B):-downcase_atom(A,B),char_code(A,_).
my_pred12(A,B):-succ(B,A),A > 0.
my_odd13(A):-1 is A mod 2.

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

prim(my_succ1,[int,int]).
prim(my_flatten2,[list(list(T)),list(T)]).
prim(my_element3,[list(T),T]).
prim(my_msort4,[list(int),list(int)]).
prim(my_head5,[list(T),T]).
prim(my_max_list6,[list(int),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_reverse8,[list(T),list(T)]).
prim(my_double9,[int,int]).
prim(my_toupper10,[char,char]).
prim(my_tolower11,[char,char]).
prim(my_pred12,[int,int]).
prim(my_odd13,[int]).
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
p([[2,4,2],[1,3,2],[7,0,5,6]],[[4,6,4],[3,5,4],[9,2,7,8]]).
p([[7,1,4,3],[6,3,4],[4,4,6,2]],[[9,3,6,5],[8,5,6],[6,6,8,4]]).
p([[0,6,5],[4,3,3,0]],[[2,8,7],[6,5,5,2]]).
p([[5,4,5],[3,3,5,7],[5,5,0,3]],[[7,6,7],[5,5,7,9],[7,7,2,5]]).
p([[6,3,1,4],[6,4,6],[7,0,3,3]],[[8,5,3,6],[8,6,8],[9,2,5,5]]).
q([[1,6,4],[4,2,7]],[[3,8,6],[4,2,7]]).
q([[5,0,2],[3,0,6,2],[4,6,5,5],[0,1,7]],[[5,0,2],[5,2,8,4],[4,6,5,5],[2,3,9]]).
q([[3,5,4,7],[3,5,2]],[[3,5,4,7],[5,7,4]]).
q([[0,7,0,0],[6,5,3],[3,6,4,7]],[[2,9,2,2],[6,5,3],[5,8,6,9]]).
q([[0,3,3],[1,5,2,6],[3,6,7],[4,0,6,0]],[[0,3,3],[3,7,4,8],[5,8,9],[6,2,8,2]]).
