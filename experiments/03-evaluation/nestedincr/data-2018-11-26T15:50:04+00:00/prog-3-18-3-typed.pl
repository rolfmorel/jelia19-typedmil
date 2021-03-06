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
my_double2(N,M):-M is 2*N,M =< 10.
my_flatten3(A,B):-flatten(A,B).
my_last4(A,B):-last(A,B).
my_lowercase5(A):-downcase_atom(A,A),char_code(A,_).
my_uppercase6(A):-upcase_atom(A,A),char_code(A,_).
my_len7(A,B):-length(A,B).
my_min_list8(A,B):-min_list(A,B).
my_element9(A,B):-member(B,A).
my_toupper10(A,B):-upcase_atom(A,B),char_code(A,_).
my_msort11(A,B):-msort(A,B).
my_tolower12(A,B):-downcase_atom(A,B),char_code(A,_).

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

my_odd14(A):-1 is A mod 2.
my_even15(A):-0 is A mod 2.
my_set16(A):-list_to_set(A,A).
my_max_list17(A,B):-max_list(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_tail19([_|TL],TL).
prim(my_succ1,[int,int]).
prim(my_double2,[int,int]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_last4,[list(T),T]).
prim(my_lowercase5,[char]).
prim(my_uppercase6,[char]).
prim(my_len7,[list(_),int]).
prim(my_min_list8,[list(int),int]).
prim(my_element9,[list(T),T]).
prim(my_toupper10,[char,char]).
prim(my_msort11,[list(int),list(int)]).
prim(my_tolower12,[char,char]).
prim(my_odd14,[int]).
prim(my_even15,[int]).
prim(my_set16,[list(_)]).
prim(my_max_list17,[list(int),int]).
prim(my_sumlist18,[list(int),int]).
prim(my_tail19,[list(T),list(T)]).
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
p([[1,7,4,2],[6,0,2,5],[5,4,6,2],[3,4,7]],[[3,9,6,4],[8,2,4,7],[7,6,8,4],[5,6,9]]).
p([[3,2,0],[4,1,7],[6,0,5,1]],[[5,4,2],[6,3,9],[8,2,7,3]]).
p([[0,2,1,7],[6,4,0,7]],[[2,4,3,9],[8,6,2,9]]).
p([[6,7,7],[6,3,6,0],[4,5,4,4]],[[8,9,9],[8,5,8,2],[6,7,6,6]]).
p([[7,2,5],[2,3,5,4],[3,5,6]],[[9,4,7],[4,5,7,6],[5,7,8]]).
q([[2,0,5,3],[4,4,2]],[[4,2,7,5],[4,4,2]]).
q([[2,5,3,1],[3,3,4,0],[0,7,6]],[[2,5,3,1],[5,5,6,2],[2,9,8]]).
q([[0,4,5,2],[6,7,2]],[[0,4,5,2],[8,9,4]]).
q([[2,2,4,3],[6,6,3],[1,6,4,2]],[[4,4,6,5],[6,6,3],[3,8,6,4]]).
q([[6,6,5],[0,5,6]],[[6,6,5],[2,7,8]]).
