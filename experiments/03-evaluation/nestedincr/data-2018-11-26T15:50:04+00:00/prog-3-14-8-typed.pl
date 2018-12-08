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
my_sumlist2(A,B):-sumlist(A,B).
my_odd3(A):-1 is A mod 2.
my_list_to_set4(A,B):-list_to_set(A,B).
my_flatten5(A,B):-flatten(A,B).
my_toupper6(A,B):-upcase_atom(A,B),char_code(A,_).
my_set7(A):-list_to_set(A,A).
my_element8(A,B):-member(B,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_len10(A,B):-length(A,B).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_msort12(A,B):-msort(A,B).
my_last13(A,B):-last(A,B).
my_reverse14(A,B):-reverse(A,B).

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
prim(my_sumlist2,[list(int),int]).
prim(my_odd3,[int]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_toupper6,[char,char]).
prim(my_set7,[list(_)]).
prim(my_element8,[list(T),T]).
prim(my_pred9,[int,int]).
prim(my_len10,[list(_),int]).
prim(my_uppercase11,[char]).
prim(my_msort12,[list(int),list(int)]).
prim(my_last13,[list(T),T]).
prim(my_reverse14,[list(T),list(T)]).
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
p([[0,0,7],[4,3,1,7],[1,5,1,2],[7,5,2]],[[2,2,9],[6,5,3,9],[3,7,3,4],[9,7,4]]).
p([[6,6,6],[4,0,0,5]],[[8,8,8],[6,2,2,7]]).
p([[4,1,3,7],[7,6,3],[4,3,6,7]],[[6,3,5,9],[9,8,5],[6,5,8,9]]).
p([[7,7,2,1],[4,1,7,2],[3,7,4],[0,0,1]],[[9,9,4,3],[6,3,9,4],[5,9,6],[2,2,3]]).
p([[3,1,1],[2,6,7,5]],[[5,3,3],[4,8,9,7]]).
q([[7,2,5],[5,2,0],[6,6,7,0],[5,7,4]],[[7,2,5],[7,4,2],[8,8,9,2],[7,9,6]]).
q([[7,6,1,1],[2,0,2],[4,2,3,2]],[[7,6,1,1],[4,2,4],[6,4,5,4]]).
q([[7,7,1],[7,0,3],[4,3,4],[1,7,1,2]],[[7,7,1],[9,2,5],[6,5,6],[3,9,3,4]]).
q([[5,2,6,5],[3,3,1],[5,2,6],[4,6,7]],[[5,2,6,5],[3,3,1],[7,4,8],[6,8,9]]).
q([[5,0,5],[7,2,1,3],[5,3,5]],[[7,2,7],[7,2,1,3],[7,5,7]]).
