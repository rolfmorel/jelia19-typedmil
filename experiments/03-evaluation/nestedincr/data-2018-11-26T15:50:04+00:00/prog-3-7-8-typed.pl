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
my_len2(A,B):-length(A,B).
my_element3(A,B):-member(B,A).
my_pred4(A,B):-succ(B,A),A > 0.

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

my_reverse6(A,B):-reverse(A,B).
my_lowercase7(A):-downcase_atom(A,A),char_code(A,_).
my_last8(A,B):-last(A,B).
prim(my_succ1,[int,int]).
prim(my_len2,[list(_),int]).
prim(my_element3,[list(T),T]).
prim(my_pred4,[int,int]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_lowercase7,[char]).
prim(my_last8,[list(T),T]).
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
p([[0,6,1,1],[5,4,5]],[[2,8,3,3],[7,6,7]]).
p([[5,4,2],[7,1,3,6],[2,1,1]],[[7,6,4],[9,3,5,8],[4,3,3]]).
p([[7,5,5,7],[4,0,5],[1,0,7],[2,4,1,2]],[[9,7,7,9],[6,2,7],[3,2,9],[4,6,3,4]]).
p([[6,4,5,0],[0,1,2]],[[8,6,7,2],[2,3,4]]).
p([[7,3,6],[2,2,3],[7,0,7,7],[5,2,7]],[[9,5,8],[4,4,5],[9,2,9,9],[7,4,9]]).
q([[5,4,1,3],[7,1,7,2],[7,7,0]],[[7,6,3,5],[9,3,9,4],[7,7,0]]).
q([[0,4,7,5],[1,1,7]],[[2,6,9,7],[1,1,7]]).
q([[2,0,4,3],[7,1,2,7],[4,2,5,6],[6,2,0,7]],[[4,2,6,5],[9,3,4,9],[6,4,7,8],[6,2,0,7]]).
q([[5,5,1],[6,0,3],[5,7,0]],[[7,7,3],[8,2,5],[5,7,0]]).
q([[5,1,2],[7,7,5,4],[3,1,0,3]],[[5,1,2],[9,9,7,6],[5,3,2,5]]).
