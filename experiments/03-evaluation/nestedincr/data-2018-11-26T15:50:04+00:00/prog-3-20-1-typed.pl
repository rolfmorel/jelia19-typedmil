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
my_lowercase2(A):-downcase_atom(A,A),char_code(A,_).
my_uppercase3(A):-upcase_atom(A,A),char_code(A,_).
my_set4(A):-list_to_set(A,A).
my_last5(A,B):-last(A,B).
my_odd6(A):-1 is A mod 2.
my_pred7(A,B):-succ(B,A),A > 0.
my_flatten8(A,B):-flatten(A,B).

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

my_len10(A,B):-length(A,B).
my_even11(A):-0 is A mod 2.
my_head12([H|_],H).
my_toupper13(A,B):-upcase_atom(A,B),char_code(A,_).
my_list_to_set14(A,B):-list_to_set(A,B).
my_element15(A,B):-member(B,A).
my_reverse16(A,B):-reverse(A,B).
my_min_list17(A,B):-min_list(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_msort19(A,B):-msort(A,B).
my_max_list20(A,B):-max_list(A,B).
my_double21(N,M):-M is 2*N,M =< 10.
prim(my_succ1,[int,int]).
prim(my_lowercase2,[char]).
prim(my_uppercase3,[char]).
prim(my_set4,[list(_)]).
prim(my_last5,[list(T),T]).
prim(my_odd6,[int]).
prim(my_pred7,[int,int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_len10,[list(_),int]).
prim(my_even11,[int]).
prim(my_head12,[list(T),T]).
prim(my_toupper13,[char,char]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_element15,[list(T),T]).
prim(my_reverse16,[list(T),list(T)]).
prim(my_min_list17,[list(int),int]).
prim(my_sumlist18,[list(int),int]).
prim(my_msort19,[list(int),list(int)]).
prim(my_max_list20,[list(int),int]).
prim(my_double21,[int,int]).
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
p([[2,0,4],[7,0,7],[0,0,4]],[[4,2,6],[9,2,9],[2,2,6]]).
p([[3,4,1],[2,3,7],[0,5,1,3],[7,6,2,3]],[[5,6,3],[4,5,9],[2,7,3,5],[9,8,4,5]]).
p([[3,2,2],[2,3,3],[6,4,1,4],[0,2,2]],[[5,4,4],[4,5,5],[8,6,3,6],[2,4,4]]).
p([[7,2,6],[3,6,3,0],[2,5,7,7]],[[9,4,8],[5,8,5,2],[4,7,9,9]]).
p([[5,1,2],[0,6,7,1],[4,0,3,2]],[[7,3,4],[2,8,9,3],[6,2,5,4]]).
q([[0,7,0,1],[0,1,1,5],[7,0,3]],[[2,9,2,3],[2,3,3,7],[7,0,3]]).
q([[2,6,3,7],[0,3,6,1],[2,7,5],[7,2,1,7]],[[2,6,3,7],[2,5,8,3],[2,7,5],[9,4,3,9]]).
q([[6,4,7,1],[5,6,5,4],[0,1,3]],[[8,6,9,3],[5,6,5,4],[2,3,5]]).
q([[6,6,2],[4,7,3],[6,0,2,0],[7,1,6]],[[6,6,2],[4,7,3],[8,2,4,2],[9,3,8]]).
q([[7,1,5],[4,0,7,1],[0,4,5],[1,0,7,6]],[[9,3,7],[4,0,7,1],[2,6,7],[3,2,9,8]]).
