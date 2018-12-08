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
my_element2(A,B):-member(B,A).
my_last3(A,B):-last(A,B).
my_head4([H|_],H).
my_min_list5(A,B):-min_list(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_pred7(A,B):-succ(B,A),A > 0.
my_lowercase8(A):-downcase_atom(A,A),char_code(A,_).
my_flatten9(A,B):-flatten(A,B).
my_msort10(A,B):-msort(A,B).
my_toupper11(A,B):-upcase_atom(A,B),char_code(A,_).
my_reverse12(A,B):-reverse(A,B).
my_tolower13(A,B):-downcase_atom(A,B),char_code(A,_).
my_list_to_set14(A,B):-list_to_set(A,B).
my_len15(A,B):-length(A,B).
my_max_list16(A,B):-max_list(A,B).
my_tail17([_|TL],TL).
my_uppercase18(A):-upcase_atom(A,A),char_code(A,_).
my_odd19(A):-1 is A mod 2.
prim(my_succ1,[int,int]).
prim(my_element2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_head4,[list(T),T]).
prim(my_min_list5,[list(int),int]).
prim(my_double6,[int,int]).
prim(my_pred7,[int,int]).
prim(my_lowercase8,[char]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_msort10,[list(int),list(int)]).
prim(my_toupper11,[char,char]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_tolower13,[char,char]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_len15,[list(_),int]).
prim(my_max_list16,[list(int),int]).
prim(my_tail17,[list(T),list(T)]).
prim(my_uppercase18,[char]).
prim(my_odd19,[int]).
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
p([[3,7,1,3],[3,7,5,3],[6,6,5,1]],[[5,9,3,5],[5,9,7,5],[8,8,7,3]]).
p([[2,2,0,1],[0,0,4],[1,2,3,7]],[[4,4,2,3],[2,2,6],[3,4,5,9]]).
p([[3,7,3,1],[0,0,6,4]],[[5,9,5,3],[2,2,8,6]]).
p([[2,4,0,7],[1,1,3,7],[7,6,7]],[[4,6,2,9],[3,3,5,9],[9,8,9]]).
p([[2,4,6,6],[4,4,7,7],[5,6,1,5]],[[4,6,8,8],[6,6,9,9],[7,8,3,7]]).
q([[5,0,3],[7,3,4,6],[0,4,2,0]],[[7,2,5],[9,5,6,8],[0,4,2,0]]).
q([[1,7,7,0],[3,4,5,1]],[[1,7,7,0],[5,6,7,3]]).
q([[5,5,7],[0,1,2,3]],[[5,5,7],[2,3,4,5]]).
q([[6,3,1],[0,5,5],[5,6,3,3],[5,7,1,3]],[[8,5,3],[2,7,7],[7,8,5,5],[5,7,1,3]]).
q([[3,7,2],[5,0,6]],[[3,7,2],[7,2,8]]).
