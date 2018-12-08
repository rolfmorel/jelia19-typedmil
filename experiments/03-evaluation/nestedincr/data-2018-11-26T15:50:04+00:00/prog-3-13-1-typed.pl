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
my_double2(N,M):-M is 2*N,M =< 10.
my_odd3(A):-1 is A mod 2.
my_pred4(A,B):-succ(B,A),A > 0.
my_tolower5(A,B):-downcase_atom(A,B),char_code(A,_).
my_sumlist6(A,B):-sumlist(A,B).
my_len7(A,B):-length(A,B).
my_lowercase8(A):-downcase_atom(A,A),char_code(A,_).
my_uppercase9(A):-upcase_atom(A,A),char_code(A,_).
my_msort10(A,B):-msort(A,B).
my_tail11([_|TL],TL).
my_set12(A):-list_to_set(A,A).
my_toupper13(A,B):-upcase_atom(A,B),char_code(A,_).
my_reverse14(A,B):-reverse(A,B).
prim(my_succ1,[int,int]).
prim(my_double2,[int,int]).
prim(my_odd3,[int]).
prim(my_pred4,[int,int]).
prim(my_tolower5,[char,char]).
prim(my_sumlist6,[list(int),int]).
prim(my_len7,[list(_),int]).
prim(my_lowercase8,[char]).
prim(my_uppercase9,[char]).
prim(my_msort10,[list(int),list(int)]).
prim(my_tail11,[list(T),list(T)]).
prim(my_set12,[list(_)]).
prim(my_toupper13,[char,char]).
prim(my_reverse14,[list(T),list(T)]).
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
p([[3,6,4,7],[4,6,4,2]],[[5,8,6,9],[6,8,6,4]]).
p([[5,1,5,3],[1,3,2],[7,2,1,1],[1,2,6,1]],[[7,3,7,5],[3,5,4],[9,4,3,3],[3,4,8,3]]).
p([[4,0,1],[6,5,3,6]],[[6,2,3],[8,7,5,8]]).
p([[0,6,1],[5,3,2],[1,5,6,3],[5,6,0,0]],[[2,8,3],[7,5,4],[3,7,8,5],[7,8,2,2]]).
p([[7,3,7,3],[3,3,6],[6,1,1],[7,2,3]],[[9,5,9,5],[5,5,8],[8,3,3],[9,4,5]]).
q([[0,2,7],[0,5,0],[3,5,0],[2,0,0,2]],[[0,2,7],[0,5,0],[5,7,2],[4,2,2,4]]).
q([[4,0,6,6],[1,3,5,3],[3,4,2,6]],[[4,0,6,6],[3,5,7,5],[5,6,4,8]]).
q([[2,6,4],[6,2,5]],[[4,8,6],[6,2,5]]).
q([[7,7,7,3],[0,0,3,5],[0,3,3],[1,0,1]],[[7,7,7,3],[2,2,5,7],[0,3,3],[3,2,3]]).
q([[4,0,3,5],[6,7,4],[4,7,3,2]],[[4,0,3,5],[8,9,6],[6,9,5,4]]).
