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
my_len2(A,B):-length(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_element4(A,B):-member(B,A).

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

my_list_to_set6(A,B):-list_to_set(A,B).
my_reverse7(A,B):-reverse(A,B).
my_head8([H|_],H).
my_msort9(A,B):-msort(A,B).
my_tolower10(A,B):-downcase_atom(A,B),char_code(A,_).
my_set11(A):-list_to_set(A,A).
my_odd12(A):-1 is A mod 2.
my_pred13(A,B):-succ(B,A),A > 0.
my_even14(A):-0 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_len2,[list(_),int]).
prim(my_sumlist3,[list(int),int]).
prim(my_element4,[list(T),T]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_head8,[list(T),T]).
prim(my_msort9,[list(int),list(int)]).
prim(my_tolower10,[char,char]).
prim(my_set11,[list(_)]).
prim(my_odd12,[int]).
prim(my_pred13,[int,int]).
prim(my_even14,[int]).
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
p([[5,7,1],[5,5,5,6]],[[7,9,3],[7,7,7,8]]).
p([[0,3,3],[1,0,7,3],[1,2,4],[5,6,3]],[[2,5,5],[3,2,9,5],[3,4,6],[7,8,5]]).
p([[2,4,3],[3,0,5,0],[0,0,7]],[[4,6,5],[5,2,7,2],[2,2,9]]).
p([[6,1,1],[5,5,2],[2,1,0,6],[1,2,7,2]],[[8,3,3],[7,7,4],[4,3,2,8],[3,4,9,4]]).
p([[5,1,6,4],[0,3,5]],[[7,3,8,6],[2,5,7]]).
q([[7,0,1],[4,3,7,1]],[[7,0,1],[6,5,9,3]]).
q([[7,3,0],[3,0,6],[0,7,2]],[[7,3,0],[5,2,8],[2,9,4]]).
q([[3,3,5,5],[5,4,1,6]],[[5,5,7,7],[5,4,1,6]]).
q([[6,1,7,4],[5,2,5],[4,2,1,2],[4,5,3]],[[6,1,7,4],[7,4,7],[4,2,1,2],[6,7,5]]).
q([[5,5,4,3],[3,4,4,7],[6,3,0,2],[2,3,4,1]],[[7,7,6,5],[5,6,6,9],[8,5,2,4],[2,3,4,1]]).
