:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_succ1(A,B):-succ(A,B),B =< 10.
my_lowercase2(A):-downcase_atom(A,A),char_code(A,_).
my_len3(A,B):-length(A,B).
my_tail4([_|TL],TL).
my_msort5(A,B):-msort(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_double7(N,M):-M is 2*N,M =< 10.
my_toupper8(A,B):-upcase_atom(A,B),char_code(A,_).
my_even9(A):-0 is A mod 2.
my_reverse10(A,B):-reverse(A,B).
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_set12(A):-list_to_set(A,A).
my_head13([H|_],H).
my_flatten14(A,B):-flatten(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_tolower16(A,B):-downcase_atom(A,B),char_code(A,_).
my_list_to_set17(A,B):-list_to_set(A,B).

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

my_element19(A,B):-member(B,A).
my_max_list20(A,B):-max_list(A,B).
my_min_list21(A,B):-min_list(A,B).
my_last22(A,B):-last(A,B).
prim(my_succ1/2).
prim(my_lowercase2/1).
prim(my_len3/2).
prim(my_tail4/2).
prim(my_msort5/2).
prim(my_pred6/2).
prim(my_double7/2).
prim(my_toupper8/2).
prim(my_even9/1).
prim(my_reverse10/2).
prim(my_uppercase11/1).
prim(my_set12/1).
prim(my_head13/2).
prim(my_flatten14/2).
prim(my_sumlist15/2).
prim(my_tolower16/2).
prim(my_list_to_set17/2).
prim(my_element19/2).
prim(my_max_list20/2).
prim(my_min_list21/2).
prim(my_last22/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([[4,3,1],[5,4,1],[6,0,1,4],[0,6,2]],[[6,5,3],[7,6,3],[8,2,3,6],[2,8,4]]).
p([[4,1,7],[2,6,2]],[[6,3,9],[4,8,4]]).
p([[2,3,5],[0,5,6]],[[4,5,7],[2,7,8]]).
p([[5,6,7],[7,1,4,3]],[[7,8,9],[9,3,6,5]]).
p([[5,2,4],[7,3,5,1]],[[7,4,6],[9,5,7,3]]).
q([[5,5,0,3],[3,0,4,6],[4,7,6,4],[2,5,3]],[[5,5,0,3],[5,2,6,8],[6,9,8,6],[2,5,3]]).
q([[6,6,3],[7,1,5,1]],[[8,8,5],[7,1,5,1]]).
q([[5,6,0],[7,3,0,6]],[[7,8,2],[7,3,0,6]]).
q([[0,5,2],[3,4,4]],[[2,7,4],[3,4,4]]).
q([[4,1,2,5],[7,1,5]],[[4,1,2,5],[9,3,7]]).
