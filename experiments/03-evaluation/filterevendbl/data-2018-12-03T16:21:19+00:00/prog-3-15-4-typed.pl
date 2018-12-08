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
my_odd4(A):-1 is A mod 2.
my_list_to_set5(A,B):-list_to_set(A,B).
my_lowercase6(A):-downcase_atom(A,A),char_code(A,_).
my_head7([H|_],H).
my_sumlist8(A,B):-sumlist(A,B).
my_min_list9(A,B):-min_list(A,B).
my_uppercase10(A):-upcase_atom(A,A),char_code(A,_).
my_max_list11(A,B):-max_list(A,B).
my_toupper12(A,B):-upcase_atom(A,B),char_code(A,_).
my_pred13(A,B):-succ(B,A),A > 0.
my_flatten14(A,B):-flatten(A,B).
my_msort15(A,B):-msort(A,B).
my_last16(A,B):-last(A,B).
my_tail17([_|TL],TL).
my_reverse18(A,B):-reverse(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_odd4,[int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_lowercase6,[char]).
prim(my_head7,[list(T),T]).
prim(my_sumlist8,[list(int),int]).
prim(my_min_list9,[list(int),int]).
prim(my_uppercase10,[char]).
prim(my_max_list11,[list(int),int]).
prim(my_toupper12,[char,char]).
prim(my_pred13,[int,int]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_msort15,[list(int),list(int)]).
prim(my_last16,[list(T),T]).
prim(my_tail17,[list(T),list(T)]).
prim(my_reverse18,[list(T),list(T)]).
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
p([2,3,9,7,1],[4]).
p([4,1,0,1,7,0,3,7,9],[8,0,0]).
p([0,4,5,0,9,0,4,3],[0,8,0,0,8]).
p([1,0,2,7],[0,4]).
p([7,2,4,9,2,0],[4,8,4,0]).
q([1,4,7,0,0,0],[0,9,8,0,0]).
q([2,2,9,2,2,5,2,0],[0,4,0,4,4,4,4]).
q([5,7,2,3],[6,4]).
q([4,3,7,1,7,4,4,9],[8,8,8,9]).
q([2,0,2,4,2,4,7,0],[8,8,9,4,0,4,4,0]).
