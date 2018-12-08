:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_uppercase0(A):-upcase_atom(A,A).
my_tolower1(A,B):-downcase_atom(A,B).

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


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_set4(A):-list_to_set(A,A).
my_odd5(A):-1 is A mod 2.
my_flatten6(A,B):-flatten(A,B).
my_max_list7(A,B):-max_list(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_double9(N,M):-M is 2*N,M =< 10.
my_element10(A,B):-member(B,A).
my_tail11([_|TL],TL).
my_list_to_set12(A,B):-list_to_set(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_even14(A):-0 is A mod 2.
my_reverse15(A,B):-reverse(A,B).
my_msort16(A,B):-msort(A,B).
my_head17([H|_],H).
my_last18(A,B):-last(A,B).
my_min_list19(A,B):-min_list(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_set4,[list(_)]).
prim(my_odd5,[int]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_max_list7,[list(int),int]).
prim(my_succ8,[int,int]).
prim(my_double9,[int,int]).
prim(my_element10,[list(T),T]).
prim(my_tail11,[list(T),list(T)]).
prim(my_list_to_set12,[list(T),list(T)]).
prim(my_toupper13,[char,char]).
prim(my_even14,[int]).
prim(my_reverse15,[list(T),list(T)]).
prim(my_msort16,[list(int),list(int)]).
prim(my_head17,[list(T),T]).
prim(my_last18,[list(T),T]).
prim(my_min_list19,[list(int),int]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),list(char)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p(['P',v,z,w,z,'Z'],[p,z]).
p([z,c,'O',g,'K',e,m],[o,k]).
p([e,v,r,w],[]).
p(['S','Q',h,c,'L'],[s,q,l]).
p([m,'V','D','H','I','A',u,'E'],[v,d,h,i,a,e]).
q([b,'B',r,'G',t],[v,g,b]).
q(['B',c,c,e,y,'L','J'],['R',j,b,l]).
q([i,'N',y,'C',o],['E',c,n]).
q([s,a,b,p,'C'],[c,j]).
q([d,'I','M','W',i],[w,c,m,i]).
