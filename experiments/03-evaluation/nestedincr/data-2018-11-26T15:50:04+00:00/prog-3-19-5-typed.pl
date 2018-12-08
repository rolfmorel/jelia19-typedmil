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
my_odd2(A):-1 is A mod 2.
my_min_list3(A,B):-min_list(A,B).
my_pred4(A,B):-succ(B,A),A > 0.
my_sumlist5(A,B):-sumlist(A,B).
my_msort6(A,B):-msort(A,B).

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

my_max_list8(A,B):-max_list(A,B).
my_even9(A):-0 is A mod 2.
my_element10(A,B):-member(B,A).
my_len11(A,B):-length(A,B).
my_head12([H|_],H).
my_toupper13(A,B):-upcase_atom(A,B),char_code(A,_).
my_reverse14(A,B):-reverse(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
my_tolower16(A,B):-downcase_atom(A,B),char_code(A,_).
my_flatten17(A,B):-flatten(A,B).
my_list_to_set18(A,B):-list_to_set(A,B).
my_uppercase19(A):-upcase_atom(A,A),char_code(A,_).
my_tail20([_|TL],TL).
prim(my_succ1,[int,int]).
prim(my_odd2,[int]).
prim(my_min_list3,[list(int),int]).
prim(my_pred4,[int,int]).
prim(my_sumlist5,[list(int),int]).
prim(my_msort6,[list(int),list(int)]).
prim(my_max_list8,[list(int),int]).
prim(my_even9,[int]).
prim(my_element10,[list(T),T]).
prim(my_len11,[list(_),int]).
prim(my_head12,[list(T),T]).
prim(my_toupper13,[char,char]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_double15,[int,int]).
prim(my_tolower16,[char,char]).
prim(my_flatten17,[list(list(T)),list(T)]).
prim(my_list_to_set18,[list(T),list(T)]).
prim(my_uppercase19,[char]).
prim(my_tail20,[list(T),list(T)]).
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
p([[7,0,6,0],[4,5,5],[6,7,4,4]],[[9,2,8,2],[6,7,7],[8,9,6,6]]).
p([[5,6,0],[5,1,3],[2,5,5,0],[6,1,7]],[[7,8,2],[7,3,5],[4,7,7,2],[8,3,9]]).
p([[2,4,5,1],[6,6,4,5],[7,5,7,2]],[[4,6,7,3],[8,8,6,7],[9,7,9,4]]).
p([[3,6,6],[5,2,3],[2,5,6]],[[5,8,8],[7,4,5],[4,7,8]]).
p([[7,5,2,6],[3,3,1,2],[6,7,0],[7,4,5]],[[9,7,4,8],[5,5,3,4],[8,9,2],[9,6,7]]).
q([[2,5,5],[0,6,6],[0,6,5,0]],[[2,5,5],[2,8,8],[2,8,7,2]]).
q([[6,3,6],[5,6,1,4]],[[8,5,8],[5,6,1,4]]).
q([[2,6,4],[1,5,6,5],[3,7,0]],[[4,8,6],[3,7,8,7],[3,7,0]]).
q([[7,0,5],[6,1,2],[1,5,6,2]],[[7,0,5],[8,3,4],[3,7,8,4]]).
q([[1,1,3],[6,2,6],[5,7,7]],[[3,3,5],[8,4,8],[5,7,7]]).
