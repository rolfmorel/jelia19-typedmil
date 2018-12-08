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

my_element4(A,B):-member(B,A).
my_tail5([_|TL],TL).
my_min_list6(A,B):-min_list(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_last8(A,B):-last(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_set10(A):-list_to_set(A,A).
my_list_to_set11(A,B):-list_to_set(A,B).
my_max_list12(A,B):-max_list(A,B).
my_double13(N,M):-M is 2*N,M =< 10.
my_odd14(A):-1 is A mod 2.
my_succ15(A,B):-succ(A,B),B =< 10.
my_head16([H|_],H).
my_toupper17(A,B):-upcase_atom(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_element4,[list(T),T]).
prim(my_tail5,[list(T),list(T)]).
prim(my_min_list6,[list(int),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_last8,[list(T),T]).
prim(my_lowercase9,[char]).
prim(my_set10,[list(_)]).
prim(my_list_to_set11,[list(T),list(T)]).
prim(my_max_list12,[list(int),int]).
prim(my_double13,[int,int]).
prim(my_odd14,[int]).
prim(my_succ15,[int,int]).
prim(my_head16,[list(T),T]).
prim(my_toupper17,[char,char]).
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
p([z,w,v,'Y','X','N'],[y,x,n]).
p(['U','N','Q','N','Z',i,'R'],[u,n,q,n,z,r]).
p(['D',r,x,'E','J',d,'B',p],[d,e,j,b]).
p(['J',m,'J',e,d,'W','T',f],[j,j,w,t]).
p([h,'X','X','P'],[x,x,p]).
q(['A','W','G',l],[w,g,a,'A']).
q(['B','P','S','P',i,'I','L'],[l,i,p,s,p,b,l]).
q([l,'L',l,'P',j,c],['C',p,l]).
q(['I',j,'N',s,n],[i,n,h]).
q([c,v,'Q',a,'P','X','T'],[q,p,x,'X',t]).
