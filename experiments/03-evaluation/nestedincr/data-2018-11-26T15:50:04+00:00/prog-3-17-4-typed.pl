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
my_list_to_set3(A,B):-list_to_set(A,B).
my_flatten4(A,B):-flatten(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_last6(A,B):-last(A,B).
my_tail7([_|TL],TL).
my_msort8(A,B):-msort(A,B).
my_max_list9(A,B):-max_list(A,B).
my_even10(A):-0 is A mod 2.
my_sumlist11(A,B):-sumlist(A,B).
my_reverse12(A,B):-reverse(A,B).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_head14([H|_],H).
my_uppercase15(A):-upcase_atom(A,A),char_code(A,_).
my_min_list16(A,B):-min_list(A,B).

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

my_odd18(A):-1 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_element2,[list(T),T]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_double5,[int,int]).
prim(my_last6,[list(T),T]).
prim(my_tail7,[list(T),list(T)]).
prim(my_msort8,[list(int),list(int)]).
prim(my_max_list9,[list(int),int]).
prim(my_even10,[int]).
prim(my_sumlist11,[list(int),int]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_tolower13,[char,char]).
prim(my_head14,[list(T),T]).
prim(my_uppercase15,[char]).
prim(my_min_list16,[list(int),int]).
prim(my_odd18,[int]).
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
p([[1,6,5],[2,5,4,2],[6,0,3],[7,7,3]],[[3,8,7],[4,7,6,4],[8,2,5],[9,9,5]]).
p([[1,3,0,4],[1,1,7],[1,4,5],[6,4,0]],[[3,5,2,6],[3,3,9],[3,6,7],[8,6,2]]).
p([[5,6,2],[3,2,4],[6,1,0,5]],[[7,8,4],[5,4,6],[8,3,2,7]]).
p([[3,3,2,7],[5,4,3,6]],[[5,5,4,9],[7,6,5,8]]).
p([[4,2,3,4],[0,6,7],[2,7,1],[4,5,4,5]],[[6,4,5,6],[2,8,9],[4,9,3],[6,7,6,7]]).
q([[1,0,5,4],[7,4,6,5]],[[1,0,5,4],[9,6,8,7]]).
q([[6,4,2,4],[4,3,2,1],[4,0,3],[6,3,6]],[[8,6,4,6],[6,5,4,3],[6,2,5],[6,3,6]]).
q([[2,7,3],[4,5,0]],[[4,9,5],[4,5,0]]).
q([[5,5,3],[6,2,4]],[[5,5,3],[8,4,6]]).
q([[1,1,7,0],[0,4,3,1],[4,4,7,4]],[[1,1,7,0],[2,6,5,3],[6,6,9,6]]).
