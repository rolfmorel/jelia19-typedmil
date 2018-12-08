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
my_tolower2(A,B):-downcase_atom(A,B),char_code(A,_).
my_toupper3(A,B):-upcase_atom(A,B),char_code(A,_).
my_len4(A,B):-length(A,B).
my_tail5([_|TL],TL).
my_last6(A,B):-last(A,B).

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

my_element8(A,B):-member(B,A).
my_sumlist9(A,B):-sumlist(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_max_list11(A,B):-max_list(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_min_list13(A,B):-min_list(A,B).
my_lowercase14(A):-downcase_atom(A,A),char_code(A,_).
my_set15(A):-list_to_set(A,A).
my_flatten16(A,B):-flatten(A,B).
my_odd17(A):-1 is A mod 2.
my_uppercase18(A):-upcase_atom(A,A),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_tolower2,[char,char]).
prim(my_toupper3,[char,char]).
prim(my_len4,[list(_),int]).
prim(my_tail5,[list(T),list(T)]).
prim(my_last6,[list(T),T]).
prim(my_element8,[list(T),T]).
prim(my_sumlist9,[list(int),int]).
prim(my_double10,[int,int]).
prim(my_max_list11,[list(int),int]).
prim(my_pred12,[int,int]).
prim(my_min_list13,[list(int),int]).
prim(my_lowercase14,[char]).
prim(my_set15,[list(_)]).
prim(my_flatten16,[list(list(T)),list(T)]).
prim(my_odd17,[int]).
prim(my_uppercase18,[char]).
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
p([[7,7,4],[3,1,2]],[[9,9,6],[5,3,4]]).
p([[6,3,2],[1,3,7],[5,6,5],[0,6,5]],[[8,5,4],[3,5,9],[7,8,7],[2,8,7]]).
p([[4,7,4,4],[0,2,0,6],[1,5,1,5]],[[6,9,6,6],[2,4,2,8],[3,7,3,7]]).
p([[3,6,1],[1,4,0],[4,7,4,6]],[[5,8,3],[3,6,2],[6,9,6,8]]).
p([[4,2,1],[1,4,0]],[[6,4,3],[3,6,2]]).
q([[6,3,7],[2,7,4]],[[8,5,9],[2,7,4]]).
q([[1,1,6],[5,4,2]],[[1,1,6],[7,6,4]]).
q([[5,0,4,0],[3,2,0,7],[7,4,7]],[[5,0,4,0],[5,4,2,9],[9,6,9]]).
q([[2,4,4,6],[5,0,4],[5,7,1]],[[4,6,6,8],[5,0,4],[7,9,3]]).
q([[2,2,6,0],[7,0,2,7],[0,6,4],[2,0,3,0]],[[4,4,8,2],[9,2,4,9],[0,6,4],[4,2,5,2]]).
