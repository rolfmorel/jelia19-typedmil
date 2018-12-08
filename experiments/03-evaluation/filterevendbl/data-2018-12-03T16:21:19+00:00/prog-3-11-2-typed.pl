:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).

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


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_even2(A):-0 is A mod 2.
my_double3(N,M):-M is 2*N,M =< 10.
my_pred4(A,B):-succ(B,A),A > 0.
my_uppercase5(A):-upcase_atom(A,A),char_code(A,_).
my_sumlist6(A,B):-sumlist(A,B).
my_element7(A,B):-member(B,A).
my_tolower8(A,B):-downcase_atom(A,B),char_code(A,_).
my_flatten9(A,B):-flatten(A,B).
my_len10(A,B):-length(A,B).
my_toupper11(A,B):-upcase_atom(A,B),char_code(A,_).
my_head12([H|_],H).
my_succ13(A,B):-succ(A,B),B =< 10.
my_list_to_set14(A,B):-list_to_set(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_pred4,[int,int]).
prim(my_uppercase5,[char]).
prim(my_sumlist6,[list(int),int]).
prim(my_element7,[list(T),T]).
prim(my_tolower8,[char,char]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_len10,[list(_),int]).
prim(my_toupper11,[char,char]).
prim(my_head12,[list(T),T]).
prim(my_succ13,[int,int]).
prim(my_list_to_set14,[list(T),list(T)]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(int),list(int)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([0,1,5,5,9],[0]).
p([3,7,7,4],[8]).
p([7,3,0,4,2],[0,8,4]).
p([4,2,1,0,9,4,4,2],[8,4,0,8,8,4]).
p([1,1,2,4],[4,8]).
q([9,0,2,1,3,3,0],[2,0,0,4]).
q([4,1,9,0],[0,8,5]).
q([4,9,4,2],[2,4,8,8]).
q([5,1,3,0,4,9,3,1,1],[0,9,8]).
q([7,2,9,4],[2,8,4]).
