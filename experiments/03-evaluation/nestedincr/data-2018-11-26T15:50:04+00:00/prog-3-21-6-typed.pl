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
my_tolower2(A,B):-downcase_atom(A,B),char_code(A,_).
my_uppercase3(A):-upcase_atom(A,A),char_code(A,_).
my_head4([H|_],H).
my_pred5(A,B):-succ(B,A),A > 0.
my_msort6(A,B):-msort(A,B).
my_reverse7(A,B):-reverse(A,B).
my_element8(A,B):-member(B,A).
my_list_to_set9(A,B):-list_to_set(A,B).
my_lowercase10(A):-downcase_atom(A,A),char_code(A,_).
my_flatten11(A,B):-flatten(A,B).
my_min_list12(A,B):-min_list(A,B).
my_tail13([_|TL],TL).
my_len14(A,B):-length(A,B).
my_toupper15(A,B):-upcase_atom(A,B),char_code(A,_).
my_max_list16(A,B):-max_list(A,B).
my_odd17(A):-1 is A mod 2.
my_set18(A):-list_to_set(A,A).

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

my_sumlist20(A,B):-sumlist(A,B).
my_even21(A):-0 is A mod 2.
my_double22(N,M):-M is 2*N,M =< 10.
prim(my_succ1,[int,int]).
prim(my_tolower2,[char,char]).
prim(my_uppercase3,[char]).
prim(my_head4,[list(T),T]).
prim(my_pred5,[int,int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_element8,[list(T),T]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_lowercase10,[char]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_min_list12,[list(int),int]).
prim(my_tail13,[list(T),list(T)]).
prim(my_len14,[list(_),int]).
prim(my_toupper15,[char,char]).
prim(my_max_list16,[list(int),int]).
prim(my_odd17,[int]).
prim(my_set18,[list(_)]).
prim(my_sumlist20,[list(int),int]).
prim(my_even21,[int]).
prim(my_double22,[int,int]).
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
p([[2,6,6,5],[3,1,3,2],[4,2,7]],[[4,8,8,7],[5,3,5,4],[6,4,9]]).
p([[3,4,1,1],[3,7,6],[7,7,2,2],[0,7,3]],[[5,6,3,3],[5,9,8],[9,9,4,4],[2,9,5]]).
p([[6,1,1,1],[5,1,3],[3,5,2,5]],[[8,3,3,3],[7,3,5],[5,7,4,7]]).
p([[2,1,4,5],[6,3,0,2],[6,2,3]],[[4,3,6,7],[8,5,2,4],[8,4,5]]).
p([[4,2,1],[6,5,2,0]],[[6,4,3],[8,7,4,2]]).
q([[5,1,5],[0,1,6,2]],[[5,1,5],[2,3,8,4]]).
q([[2,2,4,7],[6,7,2],[3,0,3],[1,4,5]],[[4,4,6,9],[8,9,4],[5,2,5],[1,4,5]]).
q([[7,0,2],[4,7,2,7],[1,1,5]],[[7,0,2],[6,9,4,9],[3,3,7]]).
q([[6,7,6],[6,0,6],[5,1,7],[2,5,1]],[[6,7,6],[8,2,8],[5,1,7],[4,7,3]]).
q([[5,5,3,2],[2,4,1,6],[5,3,7,1],[1,4,5]],[[5,5,3,2],[4,6,3,8],[7,5,9,3],[1,4,5]]).
