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

my_sumlist3(A,B):-sumlist(A,B).
my_msort4(A,B):-msort(A,B).
my_flatten5(A,B):-flatten(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_lowercase8(A):-downcase_atom(A,A),char_code(A,_).
my_max_list9(A,B):-max_list(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_uppercase11(A):-upcase_atom(A,A),char_code(A,_).
my_even12(A):-0 is A mod 2.
my_len13(A,B):-length(A,B).
my_set14(A):-list_to_set(A,A).
my_min_list15(A,B):-min_list(A,B).
my_tail16([_|TL],TL).
my_tolower17(A,B):-downcase_atom(A,B),char_code(A,_).
my_head18([H|_],H).
prim(my_succ1,[int,int]).
prim(my_sumlist3,[list(int),int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_double7,[int,int]).
prim(my_lowercase8,[char]).
prim(my_max_list9,[list(int),int]).
prim(my_pred10,[int,int]).
prim(my_uppercase11,[char]).
prim(my_even12,[int]).
prim(my_len13,[list(_),int]).
prim(my_set14,[list(_)]).
prim(my_min_list15,[list(int),int]).
prim(my_tail16,[list(T),list(T)]).
prim(my_tolower17,[char,char]).
prim(my_head18,[list(T),T]).
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
p([[7,7,7],[0,2,6],[3,1,3,4],[2,0,4]],[[9,9,9],[2,4,8],[5,3,5,6],[4,2,6]]).
p([[1,4,5,2],[0,4,6,6]],[[3,6,7,4],[2,6,8,8]]).
p([[2,3,5,2],[7,6,3]],[[4,5,7,4],[9,8,5]]).
p([[0,1,2],[1,3,2,4],[6,0,1,1],[3,4,6,1]],[[2,3,4],[3,5,4,6],[8,2,3,3],[5,6,8,3]]).
p([[4,1,2],[4,3,0,4]],[[6,3,4],[6,5,2,6]]).
q([[3,0,5,6],[7,1,1,0],[1,4,0],[6,1,1,2]],[[5,2,7,8],[9,3,3,2],[1,4,0],[6,1,1,2]]).
q([[6,1,6],[3,3,0],[7,0,6]],[[8,3,8],[5,5,2],[7,0,6]]).
q([[2,3,1,6],[2,0,0]],[[2,3,1,6],[4,2,2]]).
q([[0,7,0],[3,3,5,5]],[[2,9,2],[3,3,5,5]]).
q([[3,1,2,4],[5,6,7],[3,1,1]],[[3,1,2,4],[7,8,9],[5,3,3]]).
