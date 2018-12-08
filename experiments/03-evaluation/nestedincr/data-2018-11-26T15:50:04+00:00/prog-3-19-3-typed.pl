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
my_set3(A):-list_to_set(A,A).

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

my_msort5(A,B):-msort(A,B).
my_reverse6(A,B):-reverse(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_even8(A):-0 is A mod 2.
my_flatten9(A,B):-flatten(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_min_list11(A,B):-min_list(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_odd13(A):-1 is A mod 2.
my_tail14([_|TL],TL).
my_max_list15(A,B):-max_list(A,B).
my_double16(N,M):-M is 2*N,M =< 10.
my_len17(A,B):-length(A,B).
my_toupper18(A,B):-upcase_atom(A,B),char_code(A,_).
my_tolower19(A,B):-downcase_atom(A,B),char_code(A,_).
my_last20(A,B):-last(A,B).
prim(my_succ1,[int,int]).
prim(my_element2,[list(T),T]).
prim(my_set3,[list(_)]).
prim(my_msort5,[list(int),list(int)]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_pred7,[int,int]).
prim(my_even8,[int]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_sumlist10,[list(int),int]).
prim(my_min_list11,[list(int),int]).
prim(my_list_to_set12,[list(T),list(T)]).
prim(my_odd13,[int]).
prim(my_tail14,[list(T),list(T)]).
prim(my_max_list15,[list(int),int]).
prim(my_double16,[int,int]).
prim(my_len17,[list(_),int]).
prim(my_toupper18,[char,char]).
prim(my_tolower19,[char,char]).
prim(my_last20,[list(T),T]).
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
p([[3,3,0,3],[1,0,7]],[[5,5,2,5],[3,2,9]]).
p([[6,7,5,7],[7,2,3,0],[6,2,6]],[[8,9,7,9],[9,4,5,2],[8,4,8]]).
p([[6,1,3],[2,3,3]],[[8,3,5],[4,5,5]]).
p([[7,5,1],[3,4,2],[4,1,2,0]],[[9,7,3],[5,6,4],[6,3,4,2]]).
p([[3,2,0,5],[6,7,6]],[[5,4,2,7],[8,9,8]]).
q([[7,0,0,3],[5,4,1,7],[0,6,5,3],[3,5,6]],[[9,2,2,5],[5,4,1,7],[2,8,7,5],[3,5,6]]).
q([[3,5,2,0],[6,7,5]],[[3,5,2,0],[8,9,7]]).
q([[7,7,7,7],[7,5,7,6],[7,0,0,6],[6,1,2,7]],[[9,9,9,9],[9,7,9,8],[7,0,0,6],[8,3,4,9]]).
q([[4,2,0],[1,5,1,7],[2,0,4],[6,3,3,5]],[[4,2,0],[3,7,3,9],[2,0,4],[8,5,5,7]]).
q([[7,1,4,1],[2,5,0],[3,0,0],[4,3,3]],[[9,3,6,3],[4,7,2],[5,2,2],[4,3,3]]).
