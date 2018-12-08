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
my_set5(A):-list_to_set(A,A).
my_min_list6(A,B):-min_list(A,B).
my_element7(A,B):-member(B,A).
my_lowercase8(A):-downcase_atom(A,A).
my_max_list9(A,B):-max_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_even12(A):-0 is A mod 2.
my_flatten13(A,B):-flatten(A,B).
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_double16(N,M):-M is 2*N,M =< 10.
my_succ17(A,B):-succ(A,B),B =< 10.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_msort4,[list(int),list(int)]).
prim(my_set5,[list(_)]).
prim(my_min_list6,[list(int),int]).
prim(my_element7,[list(T),T]).
prim(my_lowercase8,[char]).
prim(my_max_list9,[list(int),int]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_pred11,[int,int]).
prim(my_even12,[int]).
prim(my_flatten13,[list(list(T)),list(T)]).
prim(my_len14,[list(_),int]).
prim(my_last15,[list(T),T]).
prim(my_double16,[int,int]).
prim(my_succ17,[int,int]).
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
p([z,'B',s,k,'F',z,'T','H'],[b,f,t,h]).
p([a,q,'R',t,'F'],[r,f]).
p(['A','A','H','T',u,'Q','Y'],[a,a,h,t,q,y]).
p(['B','Z','N',w],[b,z,n]).
p([z,q,y,'Q',n,'U','B','E'],[q,u,b,e]).
q([v,'K',s,'N',w,'V'],[k,v,o,n]).
q(['J',c,'D',w,h,s,'E'],[d,j,e,'S']).
q(['Q','R','P','N','D',t,'U','G'],[g,u,p,n,'U',r,q,d]).
q([x,'K','X','S','X','B',q,'A'],[x,k,a,'L',b,x,s]).
q(['Y',k,k,c],[y,w]).
