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
my_lowercase2(A):-downcase_atom(A,A),char_code(A,_).
my_element3(A,B):-member(B,A).
my_max_list4(A,B):-max_list(A,B).
my_uppercase5(A):-upcase_atom(A,A),char_code(A,_).
my_pred6(A,B):-succ(B,A),A > 0.
my_msort7(A,B):-msort(A,B).
my_tail8([_|TL],TL).
my_flatten9(A,B):-flatten(A,B).
my_head10([H|_],H).
my_len11(A,B):-length(A,B).

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

my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_lowercase2,[char]).
prim(my_element3,[list(T),T]).
prim(my_max_list4,[list(int),int]).
prim(my_uppercase5,[char]).
prim(my_pred6,[int,int]).
prim(my_msort7,[list(int),list(int)]).
prim(my_tail8,[list(T),list(T)]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_head10,[list(T),T]).
prim(my_len11,[list(_),int]).
prim(my_tolower13,[char,char]).
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
p([[7,7,5],[6,0,1],[3,6,4,4],[7,4,2,7]],[[9,9,7],[8,2,3],[5,8,6,6],[9,6,4,9]]).
p([[7,4,6],[4,1,3],[2,3,3],[1,3,0,7]],[[9,6,8],[6,3,5],[4,5,5],[3,5,2,9]]).
p([[6,6,4,6],[7,5,0]],[[8,8,6,8],[9,7,2]]).
p([[4,5,5,5],[3,0,1],[7,2,2,7],[7,7,7,6]],[[6,7,7,7],[5,2,3],[9,4,4,9],[9,9,9,8]]).
p([[5,5,2,1],[5,1,4],[3,3,0],[7,0,1,7]],[[7,7,4,3],[7,3,6],[5,5,2],[9,2,3,9]]).
q([[4,4,2],[7,1,0],[3,6,4]],[[4,4,2],[9,3,2],[5,8,6]]).
q([[5,0,1,2],[7,5,1,1],[7,3,7,5]],[[7,2,3,4],[7,5,1,1],[9,5,9,7]]).
q([[4,1,0],[6,7,6],[3,0,0,1],[7,1,0,7]],[[6,3,2],[8,9,8],[5,2,2,3],[7,1,0,7]]).
q([[3,7,5],[0,1,6,0],[2,4,7],[7,3,2,1]],[[5,9,7],[2,3,8,2],[4,6,9],[7,3,2,1]]).
q([[1,4,6,6],[1,0,2,1]],[[1,4,6,6],[3,2,4,3]]).
