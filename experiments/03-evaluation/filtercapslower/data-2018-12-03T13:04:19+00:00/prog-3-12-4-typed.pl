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

my_lowercase4(A):-downcase_atom(A,A).
my_even5(A):-0 is A mod 2.
my_succ6(A,B):-succ(A,B),B =< 10.
my_head7([H|_],H).
my_last8(A,B):-last(A,B).
my_msort9(A,B):-msort(A,B).
my_len10(A,B):-length(A,B).
my_odd11(A):-1 is A mod 2.
my_list_to_set12(A,B):-list_to_set(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_double15(N,M):-M is 2*N,M =< 10.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_lowercase4,[char]).
prim(my_even5,[int]).
prim(my_succ6,[int,int]).
prim(my_head7,[list(T),T]).
prim(my_last8,[list(T),T]).
prim(my_msort9,[list(int),list(int)]).
prim(my_len10,[list(_),int]).
prim(my_odd11,[int]).
prim(my_list_to_set12,[list(T),list(T)]).
prim(my_toupper13,[char,char]).
prim(my_pred14,[int,int]).
prim(my_double15,[int,int]).
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
p(['L','X',j,'F','L','V',k,'A'],[l,x,f,l,v,a]).
p(['S','Y',y,u,n,f,'T',m,'W'],[s,y,t,w]).
p(['G',u,d,j,'H',i,'R'],[g,h,r]).
p(['C','A','C','P'],[c,a,c,p]).
p([q,e,'F',b,'D'],[f,d]).
q([j,'F',i,s,'D','W'],[f,w,q,d]).
q([f,'C','K',n,'E',l,e,y,'H'],[h,k,c,e,w]).
q([g,'H','V','Q','V',z,'P','K','K'],['Q',h,k,p,v,q,v,k]).
q([k,'R',k,n,'R'],[r,'K',r]).
q([i,'O',m,v,'N',h,u],['R',o,n]).
