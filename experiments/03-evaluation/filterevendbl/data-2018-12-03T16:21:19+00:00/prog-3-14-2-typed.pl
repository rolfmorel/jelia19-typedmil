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
my_len4(A,B):-length(A,B).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_pred7(A,B):-succ(B,A),A > 0.
my_msort8(A,B):-msort(A,B).
my_odd9(A):-1 is A mod 2.
my_sumlist10(A,B):-sumlist(A,B).
my_element11(A,B):-member(B,A).
my_flatten12(A,B):-flatten(A,B).
my_reverse13(A,B):-reverse(A,B).
my_toupper14(A,B):-upcase_atom(A,B),char_code(A,_).
my_max_list15(A,B):-max_list(A,B).
my_tolower16(A,B):-downcase_atom(A,B),char_code(A,_).
my_set17(A):-list_to_set(A,A).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_len4,[list(_),int]).
prim(my_min_list5,[list(int),int]).
prim(my_tail6,[list(T),list(T)]).
prim(my_pred7,[int,int]).
prim(my_msort8,[list(int),list(int)]).
prim(my_odd9,[int]).
prim(my_sumlist10,[list(int),int]).
prim(my_element11,[list(T),T]).
prim(my_flatten12,[list(list(T)),list(T)]).
prim(my_reverse13,[list(T),list(T)]).
prim(my_toupper14,[char,char]).
prim(my_max_list15,[list(int),int]).
prim(my_tolower16,[char,char]).
prim(my_set17,[list(_)]).
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
p([0,4,0,0,3,2,0],[0,8,0,0,4,0]).
p([7,3,5,7,2,1,1],[4]).
p([4,0,0,5,3,9,0],[8,0,0,0]).
p([2,4,9,4,0,4],[4,8,8,0,8]).
p([3,3,5,5,0,1,5],[0]).
q([4,0,1,9],[2,8,0]).
q([0,4,2,4,3,4],[8,0,8,4,8,0]).
q([9,3,9,0,7],[3,0]).
q([1,4,0,1,9,9,2,9],[4,4,8,0]).
q([4,9,1,2,2,4],[4,8,9,8,4]).
