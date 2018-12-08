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
my_max_list2(A,B):-max_list(A,B).
my_lowercase3(A):-downcase_atom(A,A),char_code(A,_).
my_double4(N,M):-M is 2*N,M =< 10.
my_pred5(A,B):-succ(B,A),A > 0.
my_len6(A,B):-length(A,B).
my_last7(A,B):-last(A,B).
my_head8([H|_],H).
my_tolower9(A,B):-downcase_atom(A,B),char_code(A,_).
my_msort10(A,B):-msort(A,B).
my_even11(A):-0 is A mod 2.
my_odd12(A):-1 is A mod 2.
my_element13(A,B):-member(B,A).
my_sumlist14(A,B):-sumlist(A,B).
my_reverse15(A,B):-reverse(A,B).
my_toupper16(A,B):-upcase_atom(A,B),char_code(A,_).
my_list_to_set17(A,B):-list_to_set(A,B).
prim(my_succ1,[int,int]).
prim(my_max_list2,[list(int),int]).
prim(my_lowercase3,[char]).
prim(my_double4,[int,int]).
prim(my_pred5,[int,int]).
prim(my_len6,[list(_),int]).
prim(my_last7,[list(T),T]).
prim(my_head8,[list(T),T]).
prim(my_tolower9,[char,char]).
prim(my_msort10,[list(int),list(int)]).
prim(my_even11,[int]).
prim(my_odd12,[int]).
prim(my_element13,[list(T),T]).
prim(my_sumlist14,[list(int),int]).
prim(my_reverse15,[list(T),list(T)]).
prim(my_toupper16,[char,char]).
prim(my_list_to_set17,[list(T),list(T)]).
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
p([[3,5,4,6],[1,4,5],[3,0,1,1]],[[5,7,6,8],[3,6,7],[5,2,3,3]]).
p([[5,5,0],[0,5,3]],[[7,7,2],[2,7,5]]).
p([[4,5,2],[1,3,5,6],[3,4,2],[0,3,1,4]],[[6,7,4],[3,5,7,8],[5,6,4],[2,5,3,6]]).
p([[4,4,1,7],[1,5,1],[0,2,4,1]],[[6,6,3,9],[3,7,3],[2,4,6,3]]).
p([[6,6,3,0],[0,2,1,1],[1,2,3,6]],[[8,8,5,2],[2,4,3,3],[3,4,5,8]]).
q([[5,7,2],[7,0,4,3],[1,2,6,5],[3,6,6]],[[7,9,4],[7,0,4,3],[1,2,6,5],[5,8,8]]).
q([[1,6,3],[6,6,3],[1,0,7,0],[5,5,1]],[[3,8,5],[6,6,3],[3,2,9,2],[5,5,1]]).
q([[2,0,1,7],[4,3,0],[2,2,0,4]],[[4,2,3,9],[6,5,2],[2,2,0,4]]).
q([[5,4,2,0],[0,0,0],[0,2,0,2],[0,0,2,2]],[[7,6,4,2],[0,0,0],[2,4,2,4],[2,2,4,4]]).
q([[0,2,5,5],[1,3,6],[6,7,0,1],[4,0,7,4]],[[2,4,7,7],[3,5,8],[6,7,0,1],[4,0,7,4]]).
