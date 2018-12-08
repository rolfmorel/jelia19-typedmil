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
my_max_list2(A,B):-max_list(A,B).
my_even3(A):-0 is A mod 2.
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_set5(A):-list_to_set(A,A).
my_double6(N,M):-M is 2*N,M =< 10.
my_element7(A,B):-member(B,A).
my_flatten8(A,B):-flatten(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_msort10(A,B):-msort(A,B).
my_min_list11(A,B):-min_list(A,B).
my_tolower12(A,B):-downcase_atom(A,B),char_code(A,_).
my_head13([H|_],H).
my_toupper14(A,B):-upcase_atom(A,B),char_code(A,_).

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

my_last16(A,B):-last(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_len18(A,B):-length(A,B).
prim(my_succ1,[int,int]).
prim(my_max_list2,[list(int),int]).
prim(my_even3,[int]).
prim(my_lowercase4,[char]).
prim(my_set5,[list(_)]).
prim(my_double6,[int,int]).
prim(my_element7,[list(T),T]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_pred9,[int,int]).
prim(my_msort10,[list(int),list(int)]).
prim(my_min_list11,[list(int),int]).
prim(my_tolower12,[char,char]).
prim(my_head13,[list(T),T]).
prim(my_toupper14,[char,char]).
prim(my_last16,[list(T),T]).
prim(my_sumlist17,[list(int),int]).
prim(my_len18,[list(_),int]).
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
p([[7,7,3],[7,2,6],[6,4,6]],[[9,9,5],[9,4,8],[8,6,8]]).
p([[0,2,1,3],[3,7,0]],[[2,4,3,5],[5,9,2]]).
p([[4,2,5],[7,0,6,2],[4,7,5]],[[6,4,7],[9,2,8,4],[6,9,7]]).
p([[5,5,4,7],[3,4,4]],[[7,7,6,9],[5,6,6]]).
p([[5,3,1],[1,2,4]],[[7,5,3],[3,4,6]]).
q([[7,6,2],[3,5,2]],[[9,8,4],[3,5,2]]).
q([[0,5,2],[2,4,3],[2,5,2,7],[0,0,3,7]],[[0,5,2],[4,6,5],[4,7,4,9],[2,2,5,9]]).
q([[7,4,6],[2,4,0,4],[7,7,4]],[[9,6,8],[2,4,0,4],[9,9,6]]).
q([[2,1,7],[4,5,6],[2,5,6],[5,7,2]],[[4,3,9],[4,5,6],[4,7,8],[7,9,4]]).
q([[4,7,2],[4,6,5,2],[2,7,7],[4,0,6,0]],[[6,9,4],[6,8,7,4],[2,7,7],[4,0,6,0]]).
