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

my_pred4(A,B):-succ(B,A),A > 0.
my_succ5(A,B):-succ(A,B),B =< 10.
my_element6(A,B):-member(B,A).
my_even7(A):-0 is A mod 2.
my_lowercase8(A):-downcase_atom(A,A).
my_list_to_set9(A,B):-list_to_set(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_msort11(A,B):-msort(A,B).
my_last12(A,B):-last(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_reverse14(A,B):-reverse(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_pred4,[int,int]).
prim(my_succ5,[int,int]).
prim(my_element6,[list(T),T]).
prim(my_even7,[int]).
prim(my_lowercase8,[char]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_double10,[int,int]).
prim(my_msort11,[list(int),list(int)]).
prim(my_last12,[list(T),T]).
prim(my_sumlist13,[list(int),int]).
prim(my_reverse14,[list(T),list(T)]).
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
p(['G',i,q,b,n,'S',z],[g,s]).
p(['K',c,'N',s],[k,n]).
p(['B',v,n,v,'T',r],[b,t]).
p([v,v,'E',z,'R',y,l,'A'],[e,r,a]).
p(['Y','G',i,'A'],[y,g,a]).
q(['Z',a,w,h,'H','S',w,'Z'],[s,z,z,h,'J']).
q(['G','A',t,'D','O',d,k,'W',d],[i,w,g,o,d,a]).
q(['I','L','Q','W'],['T',w,i,q,l]).
q([p,e,'Q','B',d,c,'E',n],['O',e,q,b]).
q(['O',y,c,'C','A'],[c,a,'U',o]).
