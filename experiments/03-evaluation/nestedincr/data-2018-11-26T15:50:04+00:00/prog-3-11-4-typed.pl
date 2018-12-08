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
my_uppercase3(A):-upcase_atom(A,A),char_code(A,_).
my_set4(A):-list_to_set(A,A).
my_last5(A,B):-last(A,B).
my_max_list6(A,B):-max_list(A,B).
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).
my_head8([H|_],H).
my_flatten9(A,B):-flatten(A,B).

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

my_lowercase11(A):-downcase_atom(A,A),char_code(A,_).
my_pred12(A,B):-succ(B,A),A > 0.
prim(my_succ1,[int,int]).
prim(my_element2,[list(T),T]).
prim(my_uppercase3,[char]).
prim(my_set4,[list(_)]).
prim(my_last5,[list(T),T]).
prim(my_max_list6,[list(int),int]).
prim(my_tolower7,[char,char]).
prim(my_head8,[list(T),T]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_lowercase11,[char]).
prim(my_pred12,[int,int]).
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
p([[3,5,7],[7,3,5,0],[0,2,2]],[[5,7,9],[9,5,7,2],[2,4,4]]).
p([[4,1,5,0],[3,4,0],[3,1,4],[2,7,5,1]],[[6,3,7,2],[5,6,2],[5,3,6],[4,9,7,3]]).
p([[2,3,3],[1,5,3],[1,1,7]],[[4,5,5],[3,7,5],[3,3,9]]).
p([[6,6,7],[3,7,3,2],[4,5,2,0],[3,3,1]],[[8,8,9],[5,9,5,4],[6,7,4,2],[5,5,3]]).
p([[4,1,1],[5,7,7,1],[6,6,4,7],[4,5,5,1]],[[6,3,3],[7,9,9,3],[8,8,6,9],[6,7,7,3]]).
q([[3,7,2],[3,3,1,3]],[[3,7,2],[5,5,3,5]]).
q([[3,3,6],[1,7,3],[1,4,5],[5,3,2,4]],[[5,5,8],[3,9,5],[1,4,5],[7,5,4,6]]).
q([[4,4,1,2],[2,0,6],[1,1,4],[3,3,7]],[[6,6,3,4],[2,0,6],[3,3,6],[5,5,9]]).
q([[6,0,5],[1,4,0],[1,1,0]],[[8,2,7],[3,6,2],[1,1,0]]).
q([[2,1,7],[5,5,7],[6,2,1],[7,1,5]],[[4,3,9],[7,7,9],[8,4,3],[7,1,5]]).
