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
my_sumlist4(A,B):-sumlist(A,B).
my_len5(A,B):-length(A,B).

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

my_tail7([_|TL],TL).
my_uppercase8(A):-upcase_atom(A,A),char_code(A,_).
my_msort9(A,B):-msort(A,B).
my_set10(A):-list_to_set(A,A).
my_last11(A,B):-last(A,B).
my_element12(A,B):-member(B,A).
my_even13(A):-0 is A mod 2.
my_pred14(A,B):-succ(B,A),A > 0.
my_flatten15(A,B):-flatten(A,B).
my_double16(N,M):-M is 2*N,M =< 10.
my_max_list17(A,B):-max_list(A,B).
my_tolower18(A,B):-downcase_atom(A,B),char_code(A,_).
my_reverse19(A,B):-reverse(A,B).
my_lowercase20(A):-downcase_atom(A,A),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_odd2,[int]).
prim(my_min_list3,[list(int),int]).
prim(my_sumlist4,[list(int),int]).
prim(my_len5,[list(_),int]).
prim(my_tail7,[list(T),list(T)]).
prim(my_uppercase8,[char]).
prim(my_msort9,[list(int),list(int)]).
prim(my_set10,[list(_)]).
prim(my_last11,[list(T),T]).
prim(my_element12,[list(T),T]).
prim(my_even13,[int]).
prim(my_pred14,[int,int]).
prim(my_flatten15,[list(list(T)),list(T)]).
prim(my_double16,[int,int]).
prim(my_max_list17,[list(int),int]).
prim(my_tolower18,[char,char]).
prim(my_reverse19,[list(T),list(T)]).
prim(my_lowercase20,[char]).
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
p([[5,0,7,3],[4,6,3,5],[2,1,4,2],[4,7,2]],[[7,2,9,5],[6,8,5,7],[4,3,6,4],[6,9,4]]).
p([[6,7,6,0],[0,1,2],[7,3,5],[5,3,7,5]],[[8,9,8,2],[2,3,4],[9,5,7],[7,5,9,7]]).
p([[6,2,4,1],[6,0,1,4],[1,4,6,5]],[[8,4,6,3],[8,2,3,6],[3,6,8,7]]).
p([[0,1,7],[4,0,5],[2,3,7]],[[2,3,9],[6,2,7],[4,5,9]]).
p([[5,0,4],[2,4,4,0]],[[7,2,6],[4,6,6,2]]).
q([[7,5,2],[4,1,5,7],[0,7,6],[7,1,1,6]],[[7,5,2],[6,3,7,9],[2,9,8],[9,3,3,8]]).
q([[5,0,0,1],[2,3,0,7],[5,4,4,3]],[[5,0,0,1],[4,5,2,9],[7,6,6,5]]).
q([[1,5,3,6],[6,5,0,1],[5,5,3,5]],[[1,5,3,6],[8,7,2,3],[7,7,5,7]]).
q([[3,7,5,5],[1,2,3,2]],[[3,7,5,5],[3,4,5,4]]).
q([[4,6,0,4],[5,6,0],[0,4,7,2]],[[6,8,2,6],[5,6,0],[2,6,9,4]]).
