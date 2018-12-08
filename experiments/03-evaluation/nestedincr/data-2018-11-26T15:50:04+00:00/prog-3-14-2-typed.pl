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

my_flatten3(A,B):-flatten(A,B).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_toupper5(A,B):-upcase_atom(A,B),char_code(A,_).
my_head6([H|_],H).
my_list_to_set7(A,B):-list_to_set(A,B).
my_reverse8(A,B):-reverse(A,B).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
my_even10(A):-0 is A mod 2.
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_msort12(A,B):-msort(A,B).
my_set13(A):-list_to_set(A,A).
my_pred14(A,B):-succ(B,A),A > 0.
my_len15(A,B):-length(A,B).
prim(my_succ1,[int,int]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_lowercase4,[char]).
prim(my_toupper5,[char,char]).
prim(my_head6,[list(T),T]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_reverse8,[list(T),list(T)]).
prim(my_tolower9,[char,char]).
prim(my_even10,[int]).
prim(my_uppercase11,[char]).
prim(my_msort12,[list(int),list(int)]).
prim(my_set13,[list(_)]).
prim(my_pred14,[int,int]).
prim(my_len15,[list(_),int]).
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
p([[5,1,6,4],[3,3,7,0],[0,0,0],[2,0,0,0]],[[7,3,8,6],[5,5,9,2],[2,2,2],[4,2,2,2]]).
p([[7,4,6,5],[7,6,2,2],[6,5,1]],[[9,6,8,7],[9,8,4,4],[8,7,3]]).
p([[5,0,6,4],[2,5,1],[7,6,6]],[[7,2,8,6],[4,7,3],[9,8,8]]).
p([[6,0,1],[0,2,6],[3,3,0],[2,1,7]],[[8,2,3],[2,4,8],[5,5,2],[4,3,9]]).
p([[0,1,4],[5,0,4,1],[3,1,3]],[[2,3,6],[7,2,6,3],[5,3,5]]).
q([[1,2,2],[4,1,7,1],[4,5,3]],[[3,4,4],[4,1,7,1],[6,7,5]]).
q([[1,3,7,3],[7,6,7],[7,2,1],[2,7,0,1]],[[3,5,9,5],[7,6,7],[7,2,1],[4,9,2,3]]).
q([[6,1,2],[6,5,6],[3,4,6,0],[2,7,1,0]],[[6,1,2],[8,7,8],[5,6,8,2],[4,9,3,2]]).
q([[5,7,5],[4,0,6,5],[6,7,4]],[[7,9,7],[4,0,6,5],[8,9,6]]).
q([[4,4,4],[6,3,4],[6,2,6]],[[6,6,6],[6,3,4],[8,4,8]]).
