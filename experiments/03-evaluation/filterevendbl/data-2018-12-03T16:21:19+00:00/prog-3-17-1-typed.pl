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
my_msort4(A,B):-msort(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_element6(A,B):-member(B,A).
my_lowercase7(A):-downcase_atom(A,A),char_code(A,_).
my_odd8(A):-1 is A mod 2.
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_max_list10(A,B):-max_list(A,B).
my_tolower11(A,B):-downcase_atom(A,B),char_code(A,_).
my_set12(A):-list_to_set(A,A).
my_list_to_set13(A,B):-list_to_set(A,B).
my_head14([H|_],H).
my_len15(A,B):-length(A,B).
my_flatten16(A,B):-flatten(A,B).
my_toupper17(A,B):-upcase_atom(A,B),char_code(A,_).
my_succ18(A,B):-succ(A,B),B =< 10.
my_reverse19(A,B):-reverse(A,B).
my_tail20([_|TL],TL).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_sumlist5,[list(int),int]).
prim(my_element6,[list(T),T]).
prim(my_lowercase7,[char]).
prim(my_odd8,[int]).
prim(my_uppercase9,[char]).
prim(my_max_list10,[list(int),int]).
prim(my_tolower11,[char,char]).
prim(my_set12,[list(_)]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_head14,[list(T),T]).
prim(my_len15,[list(_),int]).
prim(my_flatten16,[list(list(T)),list(T)]).
prim(my_toupper17,[char,char]).
prim(my_succ18,[int,int]).
prim(my_reverse19,[list(T),list(T)]).
prim(my_tail20,[list(T),list(T)]).
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
p([3,0,0,2,7,1],[0,0,4]).
p([0,0,5,5,9,3,1,4],[0,0,8]).
p([4,4,2,1,5],[8,8,4]).
p([2,3,4,7,4,2,2],[4,8,8,4,4]).
p([2,7,4,2,0,4,2,0,0],[4,8,4,0,8,4,0,0]).
q([4,5,9,0],[0,4,8]).
q([2,2,2,2,1,1],[4,4,4,1,4]).
q([2,9,9,0,2,0,0,0],[0,0,4,4,0,5,0]).
q([2,0,1,1,9,9,4,2,5],[0,8,4,4,4]).
q([9,3,0,4,3],[0,8,7]).
