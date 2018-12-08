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
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_reverse4(A,B):-reverse(A,B).
my_element5(A,B):-member(B,A).
my_lowercase6(A):-downcase_atom(A,A),char_code(A,_).
my_double7(N,M):-M is 2*N,M =< 10.
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_odd10(A):-1 is A mod 2.
my_even11(A):-0 is A mod 2.
my_last12(A,B):-last(A,B).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_msort14(A,B):-msort(A,B).
my_list_to_set15(A,B):-list_to_set(A,B).
my_set16(A):-list_to_set(A,A).
my_pred17(A,B):-succ(B,A),A > 0.
my_toupper18(A,B):-upcase_atom(A,B),char_code(A,_).
my_sumlist19(A,B):-sumlist(A,B).
prim(my_succ1,[int,int]).
prim(my_min_list2,[list(int),int]).
prim(my_tail3,[list(T),list(T)]).
prim(my_reverse4,[list(T),list(T)]).
prim(my_element5,[list(T),T]).
prim(my_lowercase6,[char]).
prim(my_double7,[int,int]).
prim(my_head8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_odd10,[int]).
prim(my_even11,[int]).
prim(my_last12,[list(T),T]).
prim(my_tolower13,[char,char]).
prim(my_msort14,[list(int),list(int)]).
prim(my_list_to_set15,[list(T),list(T)]).
prim(my_set16,[list(_)]).
prim(my_pred17,[int,int]).
prim(my_toupper18,[char,char]).
prim(my_sumlist19,[list(int),int]).
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
p([[0,0,5,3],[1,6,5,7]],[[2,2,7,5],[3,8,7,9]]).
p([[4,3,4],[1,1,6,6]],[[6,5,6],[3,3,8,8]]).
p([[6,3,5],[6,3,2,0],[1,2,4]],[[8,5,7],[8,5,4,2],[3,4,6]]).
p([[7,7,5],[6,0,6,7],[1,6,4],[2,0,4]],[[9,9,7],[8,2,8,9],[3,8,6],[4,2,6]]).
p([[5,6,0],[5,6,4]],[[7,8,2],[7,8,6]]).
q([[3,1,7,4],[2,2,7],[1,2,5,5],[2,7,4,0]],[[3,1,7,4],[4,4,9],[1,2,5,5],[4,9,6,2]]).
q([[5,6,7],[6,6,3],[4,1,0],[5,7,7,4]],[[7,8,9],[6,6,3],[4,1,0],[7,9,9,6]]).
q([[1,6,6,0],[4,0,5,4],[6,5,7],[5,6,0,5]],[[3,8,8,2],[6,2,7,6],[6,5,7],[5,6,0,5]]).
q([[4,0,4,3],[7,3,1,2]],[[4,0,4,3],[9,5,3,4]]).
q([[7,2,1],[2,6,1,5]],[[7,2,1],[4,8,3,7]]).
