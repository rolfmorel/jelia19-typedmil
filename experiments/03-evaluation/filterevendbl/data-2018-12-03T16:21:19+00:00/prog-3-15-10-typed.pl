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
my_sumlist5(A,B):-sumlist(A,B).
my_element6(A,B):-member(B,A).
my_head7([H|_],H).
my_toupper8(A,B):-upcase_atom(A,B),char_code(A,_).
my_tail9([_|TL],TL).
my_msort10(A,B):-msort(A,B).
my_tolower11(A,B):-downcase_atom(A,B),char_code(A,_).
my_len12(A,B):-length(A,B).
my_last13(A,B):-last(A,B).
my_set14(A):-list_to_set(A,A).
my_flatten15(A,B):-flatten(A,B).
my_reverse16(A,B):-reverse(A,B).
my_odd17(A):-1 is A mod 2.
my_list_to_set18(A,B):-list_to_set(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_pred4,[int,int]).
prim(my_sumlist5,[list(int),int]).
prim(my_element6,[list(T),T]).
prim(my_head7,[list(T),T]).
prim(my_toupper8,[char,char]).
prim(my_tail9,[list(T),list(T)]).
prim(my_msort10,[list(int),list(int)]).
prim(my_tolower11,[char,char]).
prim(my_len12,[list(_),int]).
prim(my_last13,[list(T),T]).
prim(my_set14,[list(_)]).
prim(my_flatten15,[list(list(T)),list(T)]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_odd17,[int]).
prim(my_list_to_set18,[list(T),list(T)]).
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
p([4,1,4,4,2],[8,8,8,4]).
p([9,7,9,2,9,5,3,1,1],[4]).
p([3,7,3,9,9,2,3,7,3],[4]).
p([9,7,4,4,3],[8,8]).
p([2,2,5,0,4,4,0,4],[4,4,0,8,8,0,8]).
q([1,9,0,3,7],[4,0]).
q([5,4,1,9],[8,2]).
q([4,7,5,5,3,5,9],[8,4]).
q([4,4,4,5,0,9,2],[8,8,8,5,4,0]).
q([0,4,9,0,1,3,4,3,0],[7,0,0,8,8,0]).
