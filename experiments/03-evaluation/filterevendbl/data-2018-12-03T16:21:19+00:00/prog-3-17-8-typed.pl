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
my_lowercase4(A):-downcase_atom(A,A),char_code(A,_).
my_tail5([_|TL],TL).
my_max_list6(A,B):-max_list(A,B).
my_element7(A,B):-member(B,A).
my_last8(A,B):-last(A,B).
my_msort9(A,B):-msort(A,B).
my_succ10(A,B):-succ(A,B),B =< 10.
my_head11([H|_],H).
my_len12(A,B):-length(A,B).
my_flatten13(A,B):-flatten(A,B).
my_list_to_set14(A,B):-list_to_set(A,B).
my_tolower15(A,B):-downcase_atom(A,B),char_code(A,_).
my_sumlist16(A,B):-sumlist(A,B).
my_odd17(A):-1 is A mod 2.
my_reverse18(A,B):-reverse(A,B).
my_set19(A):-list_to_set(A,A).
my_toupper20(A,B):-upcase_atom(A,B),char_code(A,_).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_lowercase4,[char]).
prim(my_tail5,[list(T),list(T)]).
prim(my_max_list6,[list(int),int]).
prim(my_element7,[list(T),T]).
prim(my_last8,[list(T),T]).
prim(my_msort9,[list(int),list(int)]).
prim(my_succ10,[int,int]).
prim(my_head11,[list(T),T]).
prim(my_len12,[list(_),int]).
prim(my_flatten13,[list(list(T)),list(T)]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_tolower15,[char,char]).
prim(my_sumlist16,[list(int),int]).
prim(my_odd17,[int]).
prim(my_reverse18,[list(T),list(T)]).
prim(my_set19,[list(_)]).
prim(my_toupper20,[char,char]).
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
p([5,2,2,4,7,4],[4,4,8,8]).
p([5,7,7,2,7,2],[4,4]).
p([7,4,4,0,4,4,4,2,7],[8,8,0,8,8,8,4]).
p([4,7,7,4,1],[8,8]).
p([1,2,5,0,7,3,3,1],[4,0]).
q([5,9,3,2,7,5,3],[4,2]).
q([1,3,4,7,0,0,9],[0,8,0,8]).
q([3,3,9,7,7,9,7,9,4],[7,8]).
q([4,1,0,7,1],[0,1,8]).
q([4,0,0,3,9,0,4],[9,0,8,8,0,0]).
