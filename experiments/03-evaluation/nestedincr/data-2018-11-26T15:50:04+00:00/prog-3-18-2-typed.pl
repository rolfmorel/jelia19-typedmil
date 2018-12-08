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
my_uppercase2(A):-upcase_atom(A,A),char_code(A,_).
my_list_to_set3(A,B):-list_to_set(A,B).
my_reverse4(A,B):-reverse(A,B).
my_min_list5(A,B):-min_list(A,B).
my_last6(A,B):-last(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_odd8(A):-1 is A mod 2.
my_max_list9(A,B):-max_list(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_element11(A,B):-member(B,A).
my_head12([H|_],H).
my_sumlist13(A,B):-sumlist(A,B).
my_toupper14(A,B):-upcase_atom(A,B),char_code(A,_).
my_lowercase15(A):-downcase_atom(A,A),char_code(A,_).
my_len16(A,B):-length(A,B).
my_msort17(A,B):-msort(A,B).

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

my_tolower19(A,B):-downcase_atom(A,B),char_code(A,_).
prim(my_succ1,[int,int]).
prim(my_uppercase2,[char]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_reverse4,[list(T),list(T)]).
prim(my_min_list5,[list(int),int]).
prim(my_last6,[list(T),T]).
prim(my_pred7,[int,int]).
prim(my_odd8,[int]).
prim(my_max_list9,[list(int),int]).
prim(my_double10,[int,int]).
prim(my_element11,[list(T),T]).
prim(my_head12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_toupper14,[char,char]).
prim(my_lowercase15,[char]).
prim(my_len16,[list(_),int]).
prim(my_msort17,[list(int),list(int)]).
prim(my_tolower19,[char,char]).
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
p([[2,6,5,3],[1,6,4],[1,6,3],[3,4,0,6]],[[4,8,7,5],[3,8,6],[3,8,5],[5,6,2,8]]).
p([[6,3,6],[4,4,0,4]],[[8,5,8],[6,6,2,6]]).
p([[1,0,4,7],[5,6,5,5],[1,2,2,4]],[[3,2,6,9],[7,8,7,7],[3,4,4,6]]).
p([[7,7,6],[7,4,0]],[[9,9,8],[9,6,2]]).
p([[2,0,4],[6,4,6]],[[4,2,6],[8,6,8]]).
q([[2,4,7],[0,2,7]],[[4,6,9],[0,2,7]]).
q([[5,6,7],[1,1,3,0],[2,1,6]],[[5,6,7],[3,3,5,2],[4,3,8]]).
q([[3,4,2],[5,1,5,4],[6,1,3],[0,3,2,2]],[[5,6,4],[7,3,7,6],[6,1,3],[2,5,4,4]]).
q([[6,0,0],[4,2,4]],[[8,2,2],[4,2,4]]).
q([[1,3,6],[6,3,1,7],[0,4,2]],[[3,5,8],[6,3,1,7],[2,6,4]]).
