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
my_flatten5(A,B):-flatten(A,B).
my_toupper6(A,B):-upcase_atom(A,B),char_code(A,_).
my_sumlist7(A,B):-sumlist(A,B).
my_set8(A):-list_to_set(A,A).
my_odd9(A):-1 is A mod 2.
my_reverse10(A,B):-reverse(A,B).
my_succ11(A,B):-succ(A,B),B =< 10.
my_head12([H|_],H).
my_max_list13(A,B):-max_list(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_last15(A,B):-last(A,B).
my_tolower16(A,B):-downcase_atom(A,B),char_code(A,_).
my_element17(A,B):-member(B,A).
my_tail18([_|TL],TL).
my_min_list19(A,B):-min_list(A,B).
my_uppercase20(A):-upcase_atom(A,A),char_code(A,_).
my_msort21(A,B):-msort(A,B).
prim(my_even2,[int]).
prim(my_double3,[int,int]).
prim(my_len4,[list(_),int]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_toupper6,[char,char]).
prim(my_sumlist7,[list(int),int]).
prim(my_set8,[list(_)]).
prim(my_odd9,[int]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_succ11,[int,int]).
prim(my_head12,[list(T),T]).
prim(my_max_list13,[list(int),int]).
prim(my_pred14,[int,int]).
prim(my_last15,[list(T),T]).
prim(my_tolower16,[char,char]).
prim(my_element17,[list(T),T]).
prim(my_tail18,[list(T),list(T)]).
prim(my_min_list19,[list(int),int]).
prim(my_uppercase20,[char]).
prim(my_msort21,[list(int),list(int)]).
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
p([0,4,1,9],[0,8]).
p([3,9,5,0,3,5],[0]).
p([1,3,9,5,4,9,4],[8,8]).
p([4,7,4,0,1,1,4],[8,8,0,8]).
p([3,0,5,3,9,3,7],[0]).
q([1,7,3,5,2,0,9,9],[0,3,4]).
q([2,3,2,4],[4,4,4,8]).
q([2,1,5,5],[4,3]).
q([3,4,0,9,5,4,5,1,2],[4,8,0,6,8]).
q([5,5,9,5,2,5,1,5],[4,0]).
