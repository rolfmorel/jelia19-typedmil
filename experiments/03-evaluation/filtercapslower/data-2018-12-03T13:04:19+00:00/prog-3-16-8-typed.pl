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

my_last4(A,B):-last(A,B).
my_max_list5(A,B):-max_list(A,B).
my_even6(A):-0 is A mod 2.
my_double7(N,M):-M is 2*N,M =< 10.
my_flatten8(A,B):-flatten(A,B).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_element11(A,B):-member(B,A).
my_tail12([_|TL],TL).
my_list_to_set13(A,B):-list_to_set(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_set15(A):-list_to_set(A,A).
my_head16([H|_],H).
my_odd17(A):-1 is A mod 2.
my_lowercase18(A):-downcase_atom(A,A).
my_pred19(A,B):-succ(B,A),A > 0.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_last4,[list(T),T]).
prim(my_max_list5,[list(int),int]).
prim(my_even6,[int]).
prim(my_double7,[int,int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_min_list9,[list(int),int]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_element11,[list(T),T]).
prim(my_tail12,[list(T),list(T)]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_sumlist14,[list(int),int]).
prim(my_set15,[list(_)]).
prim(my_head16,[list(T),T]).
prim(my_odd17,[int]).
prim(my_lowercase18,[char]).
prim(my_pred19,[int,int]).
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
p([u,'P','L','S','N','R'],[p,l,s,n,r]).
p(['I',a,x,'V',j,'Z',q,g,'S'],[i,v,z,s]).
p([j,'P','L',r,i,l,'A'],[p,l,a]).
p(['E','J',m,'P',h],[e,j,p]).
p([h,l,r,'V','N','U','F','J','T'],[v,n,u,f,j,t]).
q(['Q',x,m,'S',z],[s,q,i]).
q(['G','I','K','L','O',a],[o,'K',k,i,g,l]).
q([n,'R','T',j,s,e,x],[r,h,t]).
q([j,'Z',y,z],[b,z]).
q([g,'W','E','X','I','X','M'],[s,i,e,w,x,x,m]).
