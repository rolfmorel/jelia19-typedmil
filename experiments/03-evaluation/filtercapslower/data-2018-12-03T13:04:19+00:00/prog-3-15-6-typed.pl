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

my_head4([H|_],H).
my_even5(A):-0 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_odd7(A):-1 is A mod 2.
my_msort8(A,B):-msort(A,B).
my_element9(A,B):-member(B,A).
my_min_list10(A,B):-min_list(A,B).
my_lowercase11(A):-downcase_atom(A,A).
my_sumlist12(A,B):-sumlist(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_list_to_set15(A,B):-list_to_set(A,B).
my_tail16([_|TL],TL).
my_pred17(A,B):-succ(B,A),A > 0.
my_flatten18(A,B):-flatten(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_head4,[list(T),T]).
prim(my_even5,[int]).
prim(my_set6,[list(_)]).
prim(my_odd7,[int]).
prim(my_msort8,[list(int),list(int)]).
prim(my_element9,[list(T),T]).
prim(my_min_list10,[list(int),int]).
prim(my_lowercase11,[char]).
prim(my_sumlist12,[list(int),int]).
prim(my_toupper13,[char,char]).
prim(my_double14,[int,int]).
prim(my_list_to_set15,[list(T),list(T)]).
prim(my_tail16,[list(T),list(T)]).
prim(my_pred17,[int,int]).
prim(my_flatten18,[list(list(T)),list(T)]).
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
p(['U','K','P','L'],[u,k,p,l]).
p(['F',i,u,'J'],[f,j]).
p(['N',j,m,m,v,r,k,k],[n]).
p([j,'Z','O','G'],[z,o,g]).
p(['N','W','R','Y',p,'G',o,'B'],[n,w,r,y,g,b]).
q(['W',h,'F','C',i,'P','U','S',z],['S',w,f,c,u,p,s]).
q(['G','S','B','L',t,b,j,m,i],[s,c,g,b,l]).
q([i,x,'P','F'],[p,f,a]).
q(['L',u,'F','O','K'],[k,p,l,o,f]).
q([r,z,'V',h,v,'X','H'],[v,'G',x,h]).
