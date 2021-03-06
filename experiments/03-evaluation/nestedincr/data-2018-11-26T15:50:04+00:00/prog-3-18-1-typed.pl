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
my_reverse3(A,B):-reverse(A,B).
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).
my_msort6(A,B):-msort(A,B).
my_head7([H|_],H).
my_pred8(A,B):-succ(B,A),A > 0.
my_odd9(A):-1 is A mod 2.
my_list_to_set10(A,B):-list_to_set(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_uppercase13(A):-upcase_atom(A,A),char_code(A,_).
my_even14(A):-0 is A mod 2.

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

my_set16(A):-list_to_set(A,A).
my_flatten17(A,B):-flatten(A,B).
my_max_list18(A,B):-max_list(A,B).
my_min_list19(A,B):-min_list(A,B).
prim(my_succ1,[int,int]).
prim(my_element2,[list(T),T]).
prim(my_reverse3,[list(T),list(T)]).
prim(my_lowercase4,[char]).
prim(my_tolower5,[char,char]).
prim(my_msort6,[list(int),list(int)]).
prim(my_head7,[list(T),T]).
prim(my_pred8,[int,int]).
prim(my_odd9,[int]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_sumlist11,[list(int),int]).
prim(my_last12,[list(T),T]).
prim(my_uppercase13,[char]).
prim(my_even14,[int]).
prim(my_set16,[list(_)]).
prim(my_flatten17,[list(list(T)),list(T)]).
prim(my_max_list18,[list(int),int]).
prim(my_min_list19,[list(int),int]).
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
p([[1,5,1],[7,2,3,4],[3,6,0,0],[6,2,3,3]],[[3,7,3],[9,4,5,6],[5,8,2,2],[8,4,5,5]]).
p([[3,2,2,2],[0,5,5,7],[3,5,2],[0,1,6]],[[5,4,4,4],[2,7,7,9],[5,7,4],[2,3,8]]).
p([[0,6,4,0],[6,7,6]],[[2,8,6,2],[8,9,8]]).
p([[3,1,5,5],[7,2,5],[7,2,5]],[[5,3,7,7],[9,4,7],[9,4,7]]).
p([[2,6,6,0],[7,3,3,2],[1,5,7],[6,2,7,0]],[[4,8,8,2],[9,5,5,4],[3,7,9],[8,4,9,2]]).
q([[5,1,5],[0,3,0,5],[1,1,5,4]],[[7,3,7],[0,3,0,5],[3,3,7,6]]).
q([[3,5,5,2],[2,1,0],[1,4,6],[6,7,7]],[[3,5,5,2],[4,3,2],[3,6,8],[8,9,9]]).
q([[7,3,6],[1,2,1,1],[7,3,6]],[[9,5,8],[3,4,3,3],[7,3,6]]).
q([[2,4,3],[4,3,1]],[[2,4,3],[6,5,3]]).
q([[2,0,7,7],[4,4,1,0],[0,0,2,7]],[[4,2,9,9],[4,4,1,0],[2,2,4,9]]).
