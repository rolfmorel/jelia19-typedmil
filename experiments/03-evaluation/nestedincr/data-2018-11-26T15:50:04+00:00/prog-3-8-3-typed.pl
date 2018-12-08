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
my_sumlist3(A,B):-sumlist(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).

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
my_tolower7(A,B):-downcase_atom(A,B),char_code(A,_).
my_element8(A,B):-member(B,A).
my_pred9(A,B):-succ(B,A),A > 0.
prim(my_succ1,[int,int]).
prim(my_len2,[list(_),int]).
prim(my_sumlist3,[list(int),int]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_tolower7,[char,char]).
prim(my_element8,[list(T),T]).
prim(my_pred9,[int,int]).
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
p([[6,4,4,4],[7,4,4,4]],[[8,6,6,6],[9,6,6,6]]).
p([[2,4,6],[4,1,4],[3,2,7,6]],[[4,6,8],[6,3,6],[5,4,9,8]]).
p([[0,4,1,4],[2,5,7],[7,0,1],[4,6,6,5]],[[2,6,3,6],[4,7,9],[9,2,3],[6,8,8,7]]).
p([[7,4,6],[3,2,4],[2,7,3,2],[3,2,7,6]],[[9,6,8],[5,4,6],[4,9,5,4],[5,4,9,8]]).
p([[3,7,4],[2,3,3],[7,5,1],[2,5,7,7]],[[5,9,6],[4,5,5],[9,7,3],[4,7,9,9]]).
q([[0,4,6],[1,7,0,2],[1,4,3],[1,7,1,3]],[[0,4,6],[3,9,2,4],[3,6,5],[3,9,3,5]]).
q([[2,5,4,3],[4,7,3,7],[2,4,3],[1,2,7,7]],[[4,7,6,5],[4,7,3,7],[2,4,3],[3,4,9,9]]).
q([[5,1,0],[4,5,2],[5,0,3]],[[7,3,2],[6,7,4],[5,0,3]]).
q([[1,0,6],[0,6,3],[3,4,7],[5,6,2,1]],[[3,2,8],[0,6,3],[5,6,9],[7,8,4,3]]).
q([[0,2,1],[6,2,6,0],[6,3,6,2]],[[2,4,3],[6,2,6,0],[8,5,8,4]]).
