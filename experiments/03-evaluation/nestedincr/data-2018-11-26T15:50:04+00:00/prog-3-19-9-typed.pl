:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_uppercase2(A):-upcase_atom(A,A),char_code(A,_).
my_flatten3(A,B):-flatten(A,B).

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

my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).
my_sumlist6(A,B):-sumlist(A,B).
my_head7([H|_],H).
my_msort8(A,B):-msort(A,B).
my_tail9([_|TL],TL).
my_even10(A):-0 is A mod 2.
my_pred11(A,B):-succ(B,A),A > 0.
my_reverse12(A,B):-reverse(A,B).
my_double13(N,M):-M is 2*N,M =< 10.
my_len14(A,B):-length(A,B).
my_element15(A,B):-member(B,A).
my_max_list16(A,B):-max_list(A,B).
my_last17(A,B):-last(A,B).
my_toupper18(A,B):-upcase_atom(A,B),char_code(A,_).
my_lowercase19(A):-downcase_atom(A,A),char_code(A,_).
my_min_list20(A,B):-min_list(A,B).
prim(my_succ1,[int,int]).
prim(my_uppercase2,[char]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_tolower5,[char,char]).
prim(my_sumlist6,[list(int),int]).
prim(my_head7,[list(T),T]).
prim(my_msort8,[list(int),list(int)]).
prim(my_tail9,[list(T),list(T)]).
prim(my_even10,[int]).
prim(my_pred11,[int,int]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_double13,[int,int]).
prim(my_len14,[list(_),int]).
prim(my_element15,[list(T),T]).
prim(my_max_list16,[list(int),int]).
prim(my_last17,[list(T),T]).
prim(my_toupper18,[char,char]).
prim(my_lowercase19,[char]).
prim(my_min_list20,[list(int),int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(list(int)),list(list(int))],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p([[6,6,5,4],[2,5,7,6]],[[8,8,7,6],[4,7,9,8]]).
p([[0,0,3],[1,7,6,0],[2,3,7]],[[2,2,5],[3,9,8,2],[4,5,9]]).
p([[7,0,0],[3,0,6],[5,4,3,7],[5,3,5,6]],[[9,2,2],[5,2,8],[7,6,5,9],[7,5,7,8]]).
p([[3,0,2,0],[4,6,2],[7,5,7,5]],[[5,2,4,2],[6,8,4],[9,7,9,7]]).
p([[4,2,3],[3,5,7,4],[2,0,7,6]],[[6,4,5],[5,7,9,6],[4,2,9,8]]).
q([[3,4,1],[5,3,4,7],[5,2,1],[5,6,3]],[[5,6,3],[7,5,6,9],[7,4,3],[5,6,3]]).
q([[3,7,3,7],[6,3,7,7],[0,7,1,3]],[[3,7,3,7],[8,5,9,9],[2,9,3,5]]).
q([[1,2,2],[7,6,3],[1,2,0]],[[3,4,4],[9,8,5],[1,2,0]]).
q([[1,2,5,7],[0,0,1]],[[1,2,5,7],[2,2,3]]).
q([[3,4,0,2],[2,3,3]],[[5,6,2,4],[2,3,3]]).
