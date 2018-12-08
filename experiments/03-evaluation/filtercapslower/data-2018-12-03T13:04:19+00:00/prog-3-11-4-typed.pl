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

my_even4(A):-0 is A mod 2.
my_set5(A):-list_to_set(A,A).
my_tail6([_|TL],TL).
my_msort7(A,B):-msort(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_min_list9(A,B):-min_list(A,B).
my_last10(A,B):-last(A,B).
my_lowercase11(A):-downcase_atom(A,A).
my_flatten12(A,B):-flatten(A,B).
my_double13(N,M):-M is 2*N,M =< 10.
my_element14(A,B):-member(B,A).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_even4,[int]).
prim(my_set5,[list(_)]).
prim(my_tail6,[list(T),list(T)]).
prim(my_msort7,[list(int),list(int)]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_min_list9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_lowercase11,[char]).
prim(my_flatten12,[list(list(T)),list(T)]).
prim(my_double13,[int,int]).
prim(my_element14,[list(T),T]).
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
p([k,'C','X','I',y],[c,x,i]).
p([n,'L','T','F'],[l,t,f]).
p([y,b,'Z','S','Y'],[z,s,y]).
p([u,i,'J',x,x,a],[j]).
p(['S',j,z,'G',e,'F','Q',s,p],[s,g,f,q]).
q(['N',g,y,'V',y,'O',b],['U',n,o,v]).
q(['J','Y','Z',x,'J','G','G',r],[j,g,y,z,g,j,'T']).
q([o,'F',t,'P',p,'E',z,n,f],[p,f,'J',e]).
q([l,'L','U',y,m,o,p],[j,u,l]).
q([l,q,r,'G',t,'F'],[g,'W',f]).
