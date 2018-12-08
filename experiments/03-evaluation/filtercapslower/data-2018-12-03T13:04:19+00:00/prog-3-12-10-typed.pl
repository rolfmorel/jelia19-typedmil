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
my_len5(A,B):-length(A,B).
my_element6(A,B):-member(B,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_last8(A,B):-last(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_head10([H|_],H).
my_tail11([_|TL],TL).
my_odd12(A):-1 is A mod 2.
my_double13(N,M):-M is 2*N,M =< 10.
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_msort4,[list(int),list(int)]).
prim(my_len5,[list(_),int]).
prim(my_element6,[list(T),T]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_last8,[list(T),T]).
prim(my_toupper9,[char,char]).
prim(my_head10,[list(T),T]).
prim(my_tail11,[list(T),list(T)]).
prim(my_odd12,[int]).
prim(my_double13,[int,int]).
prim(my_max_list14,[list(int),int]).
prim(my_sumlist15,[list(int),int]).
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
p(['I','Y',s,a,'U'],[i,y,u]).
p([y,i,'D',w,y,'H',m,'D','E'],[d,h,d,e]).
p([t,l,d,'G','P',x],[g,p]).
p(['C','O','A','A'],[c,o,a,a]).
p(['W','T','Y',j,'U','K','Z','W'],[w,t,y,u,k,z,w]).
q(['T',l,'P',j,k,o,y,q],[p,t,'X']).
q(['A','B',z,'K',q,'L','K','O'],[b,a,k,l,x,o,k]).
q(['B',n,v,'F'],[s,f,b]).
q([l,f,d,'I','O','Z',k],['Z',z,i,o]).
q(['M',m,v,u,b],[m,d]).
