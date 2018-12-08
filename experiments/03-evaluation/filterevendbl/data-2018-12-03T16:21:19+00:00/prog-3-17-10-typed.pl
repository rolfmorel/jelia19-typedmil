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
my_head6([H|_],H).
my_pred7(A,B):-succ(B,A),A > 0.
my_last8(A,B):-last(A,B).
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_tolower10(A,B):-downcase_atom(A,B),char_code(A,_).
my_max_list11(A,B):-max_list(A,B).
my_tail12([_|TL],TL).
my_succ13(A,B):-succ(A,B),B =< 10.
my_flatten14(A,B):-flatten(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_reverse16(A,B):-reverse(A,B).
my_msort17(A,B):-msort(A,B).
my_toupper18(A,B):-upcase_atom(A,B),char_code(A,_).
my_len19(A,B):-length(A,B).
my_element20(A,B):-member(B,A).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_odd4,[int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_head6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_last8,[list(T),T]).
prim(my_lowercase9,[char]).
prim(my_tolower10,[char,char]).
prim(my_max_list11,[list(int),int]).
prim(my_tail12,[list(T),list(T)]).
prim(my_succ13,[int,int]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_sumlist15,[list(int),int]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_msort17,[list(int),list(int)]).
prim(my_toupper18,[char,char]).
prim(my_len19,[list(_),int]).
prim(my_element20,[list(T),T]).
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
p([3,4,3,4,1,7],[8,8]).
p([7,1,4,1,3,9,3,3],[8]).
p([5,1,5,4],[8]).
p([9,2,0,0,4,4,0],[4,0,0,8,8,0]).
p([4,9,3,7,7,5,3,5,0],[8,0]).
q([4,3,1,1],[8,0]).
q([9,2,4,9,2,2,5],[8,4,4,8,4]).
q([9,3,0,7,4,3],[3,8,0]).
q([5,9,4,7,7,1],[7,8]).
q([7,2,0,4,4,2],[4,0,8,0,4,8]).
