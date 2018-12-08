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
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_flatten4(A,B):-flatten(A,B).
my_max_list5(A,B):-max_list(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_odd7(A):-1 is A mod 2.
my_len8(A,B):-length(A,B).
my_even9(A):-0 is A mod 2.
my_element10(A,B):-member(B,A).
my_head11([H|_],H).
my_toupper12(A,B):-upcase_atom(A,B),char_code(A,_).
my_tail13([_|TL],TL).
my_sumlist14(A,B):-sumlist(A,B).
my_lowercase15(A):-downcase_atom(A,A),char_code(A,_).
my_double16(N,M):-M is 2*N,M =< 10.
my_set17(A):-list_to_set(A,A).

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
prim(my_min_list2,[list(int),int]).
prim(my_last3,[list(T),T]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_max_list5,[list(int),int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_odd7,[int]).
prim(my_len8,[list(_),int]).
prim(my_even9,[int]).
prim(my_element10,[list(T),T]).
prim(my_head11,[list(T),T]).
prim(my_toupper12,[char,char]).
prim(my_tail13,[list(T),list(T)]).
prim(my_sumlist14,[list(int),int]).
prim(my_lowercase15,[char]).
prim(my_double16,[int,int]).
prim(my_set17,[list(_)]).
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
p([[2,0,3,0],[7,5,2,7],[6,6,0]],[[4,2,5,2],[9,7,4,9],[8,8,2]]).
p([[0,6,6],[0,1,3],[4,4,7],[1,1,4]],[[2,8,8],[2,3,5],[6,6,9],[3,3,6]]).
p([[6,7,2],[1,5,6,4]],[[8,9,4],[3,7,8,6]]).
p([[6,3,4],[5,3,3],[3,3,5]],[[8,5,6],[7,5,5],[5,5,7]]).
p([[6,7,2],[2,0,2],[0,6,4]],[[8,9,4],[4,2,4],[2,8,6]]).
q([[0,1,5],[0,0,6]],[[0,1,5],[2,2,8]]).
q([[2,1,5,3],[2,5,7],[1,1,3,3],[5,0,5]],[[4,3,7,5],[2,5,7],[1,1,3,3],[7,2,7]]).
q([[6,5,2],[3,6,2,1],[7,0,1,3]],[[8,7,4],[5,8,4,3],[7,0,1,3]]).
q([[6,7,5],[2,4,2]],[[8,9,7],[2,4,2]]).
q([[5,3,1,3],[2,1,7],[6,3,1,7],[3,6,0,6]],[[7,5,3,5],[4,3,9],[6,3,1,7],[5,8,2,8]]).
