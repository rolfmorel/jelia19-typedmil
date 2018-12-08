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
my_list_to_set4(A,B):-list_to_set(A,B).
my_last5(A,B):-last(A,B).
my_msort6(A,B):-msort(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_max_list8(A,B):-max_list(A,B).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_tail10([_|TL],TL).
my_toupper11(A,B):-upcase_atom(A,B),char_code(A,_).
my_element12(A,B):-member(B,A).
my_lowercase13(A):-downcase_atom(A,A),char_code(A,_).
my_succ14(A,B):-succ(A,B),B =< 10.
my_flatten15(A,B):-flatten(A,B).
my_pred16(A,B):-succ(B,A),A > 0.
my_set17(A):-list_to_set(A,A).
my_reverse18(A,B):-reverse(A,B).
my_tolower19(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_last5,[list(T),T]).
prim(my_msort6,[list(int),list(int)]).
prim(my_sumlist7,[list(int),int]).
prim(my_max_list8,[list(int),int]).
prim(my_uppercase9,[char]).
prim(my_tail10,[list(T),list(T)]).
prim(my_toupper11,[char,char]).
prim(my_element12,[list(T),T]).
prim(my_lowercase13,[char]).
prim(my_succ14,[int,int]).
prim(my_flatten15,[list(list(T)),list(T)]).
prim(my_pred16,[int,int]).
prim(my_set17,[list(_)]).
prim(my_reverse18,[list(T),list(T)]).
prim(my_tolower19,[char,char]).
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
p([4,2,9,4,1,2],[8,4,8,4]).
p([9,2,0,7,7],[4,0]).
p([4,4,5,0,1,4],[8,8,0,8]).
p([1,9,2,3,7,9,1,3],[4]).
p([4,2,1,0],[8,4,0]).
q([3,4,9,0],[0,5,8]).
q([2,7,2,2,4,1],[8,2,4,4,4]).
q([0,4,4,2,4,0,3,0,2],[8,6,0,8,4,8,0,0,4]).
q([3,9,9,2,2,4,9],[9,4,4,8]).
q([0,0,0,4,4,2,7,4,0],[8,8,8,0,0,0,4,0,9]).
