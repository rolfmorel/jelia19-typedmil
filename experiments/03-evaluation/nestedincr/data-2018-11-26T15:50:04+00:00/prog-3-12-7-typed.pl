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
my_toupper2(A,B):-upcase_atom(A,B),char_code(A,_).
my_sumlist3(A,B):-sumlist(A,B).
my_reverse4(A,B):-reverse(A,B).
my_msort5(A,B):-msort(A,B).
my_max_list6(A,B):-max_list(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_tail8([_|TL],TL).
my_lowercase9(A):-downcase_atom(A,A),char_code(A,_).
my_element10(A,B):-member(B,A).
my_min_list11(A,B):-min_list(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
prim(my_succ1,[int,int]).
prim(my_toupper2,[char,char]).
prim(my_sumlist3,[list(int),int]).
prim(my_reverse4,[list(T),list(T)]).
prim(my_msort5,[list(int),list(int)]).
prim(my_max_list6,[list(int),int]).
prim(my_double7,[int,int]).
prim(my_tail8,[list(T),list(T)]).
prim(my_lowercase9,[char]).
prim(my_element10,[list(T),T]).
prim(my_min_list11,[list(int),int]).
prim(my_list_to_set12,[list(T),list(T)]).
prim(my_pred13,[int,int]).
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
p([[1,1,5,4],[2,5,1],[6,6,5,5],[0,6,5,6]],[[3,3,7,6],[4,7,3],[8,8,7,7],[2,8,7,8]]).
p([[3,1,4,4],[5,1,7],[6,0,0,6]],[[5,3,6,6],[7,3,9],[8,2,2,8]]).
p([[1,1,5,5],[7,6,5,6],[5,0,6,5]],[[3,3,7,7],[9,8,7,8],[7,2,8,7]]).
p([[4,1,2],[7,4,5]],[[6,3,4],[9,6,7]]).
p([[4,0,3,5],[2,4,4,5]],[[6,2,5,7],[4,6,6,7]]).
q([[0,5,4],[6,0,2,4],[5,0,3],[5,0,0,0]],[[0,5,4],[6,0,2,4],[7,2,5],[7,2,2,2]]).
q([[1,7,2],[6,4,6,5]],[[3,9,4],[6,4,6,5]]).
q([[3,6,6,2],[5,1,2]],[[5,8,8,4],[5,1,2]]).
q([[0,3,3],[2,5,3],[3,7,2],[0,0,0]],[[2,5,5],[2,5,3],[5,9,4],[2,2,2]]).
q([[1,0,1],[4,2,7,5],[7,4,5]],[[1,0,1],[6,4,9,7],[9,6,7]]).
