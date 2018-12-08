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

my_msort4(A,B):-msort(A,B).
my_min_list5(A,B):-min_list(A,B).
my_element6(A,B):-member(B,A).
my_odd7(A):-1 is A mod 2.
my_reverse8(A,B):-reverse(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_last10(A,B):-last(A,B).
my_len11(A,B):-length(A,B).
my_set12(A):-list_to_set(A,A).
my_succ13(A,B):-succ(A,B),B =< 10.
my_max_list14(A,B):-max_list(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_msort4,[list(int),list(int)]).
prim(my_min_list5,[list(int),int]).
prim(my_element6,[list(T),T]).
prim(my_odd7,[int]).
prim(my_reverse8,[list(T),list(T)]).
prim(my_sumlist9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_len11,[list(_),int]).
prim(my_set12,[list(_)]).
prim(my_succ13,[int,int]).
prim(my_max_list14,[list(int),int]).
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
p(['Q',m,o,'E','X',c,'E','T','N'],[q,e,x,e,t,n]).
p(['L',v,'D',f,'O'],[l,d,o]).
p(['S',d,'Z','F','W'],[s,z,f,w]).
p([o,r,e,'E','B',k],[e,b]).
p(['H','R',z,i,v,'N',f,'G'],[h,r,n,g]).
q([w,'A',p,g],[a,a]).
q([p,'G',p,'D'],[y,g,d]).
q([n,'X','M','A',d,h],[a,r,x,m]).
q(['H','O','D',n,v,'D','E',s,'Z'],['F',e,d,h,z,d,o]).
q([b,b,k,'V',o,p,z,'W','B'],[v,b,'R',w]).
