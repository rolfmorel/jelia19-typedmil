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
my_reverse5(A,B):-reverse(A,B).
my_min_list6(A,B):-min_list(A,B).
my_toupper7(A,B):-upcase_atom(A,B),char_code(A,_).
my_last8(A,B):-last(A,B).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_flatten10(A,B):-flatten(A,B).
my_len11(A,B):-length(A,B).
my_max_list12(A,B):-max_list(A,B).
my_lowercase13(A):-downcase_atom(A,A),char_code(A,_).
my_msort14(A,B):-msort(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
my_odd16(A):-1 is A mod 2.
my_sumlist17(A,B):-sumlist(A,B).
my_list_to_set18(A,B):-list_to_set(A,B).
my_head19([H|_],H).
my_set20(A):-list_to_set(A,A).
my_element21(A,B):-member(B,A).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_pred4,[int,int]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_min_list6,[list(int),int]).
prim(my_toupper7,[char,char]).
prim(my_last8,[list(T),T]).
prim(my_uppercase9,[char]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_len11,[list(_),int]).
prim(my_max_list12,[list(int),int]).
prim(my_lowercase13,[char]).
prim(my_msort14,[list(int),list(int)]).
prim(my_succ15,[int,int]).
prim(my_odd16,[int]).
prim(my_sumlist17,[list(int),int]).
prim(my_list_to_set18,[list(T),list(T)]).
prim(my_head19,[list(T),T]).
prim(my_set20,[list(_)]).
prim(my_element21,[list(T),T]).
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
p([7,0,3,0,3,4,0],[0,0,8,0]).
p([0,0,7,0],[0,0,0]).
p([3,2,4,5,4],[4,8,8]).
p([0,4,5,3,1,4,4],[0,8,8,8]).
p([3,9,1,3,2,3,3,3],[4]).
q([2,4,4,4,4,0,1,0],[6,0,8,0,8,4,8,8]).
q([0,7,2,2,2,0,5,4,0],[0,8,4,1,4,0,4,0]).
q([3,0,0,9,4,0,1,0],[0,0,0,0,8,3]).
q([0,7,2,4,0,0,9],[4,8,0,6,0,0]).
q([0,9,4,2,0,1],[0,0,8,4,8]).
