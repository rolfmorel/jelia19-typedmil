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
my_list_to_set5(A,B):-list_to_set(A,B).
my_odd6(A):-1 is A mod 2.
my_reverse7(A,B):-reverse(A,B).
my_element8(A,B):-member(B,A).
my_tail9([_|TL],TL).
my_min_list10(A,B):-min_list(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_succ12(A,B):-succ(A,B),B =< 10.
my_double13(N,M):-M is 2*N,M =< 10.
my_flatten14(A,B):-flatten(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_max_list16(A,B):-max_list(A,B).
my_even17(A):-0 is A mod 2.
my_toupper18(A,B):-upcase_atom(A,B).
my_head19([H|_],H).
my_msort20(A,B):-msort(A,B).
my_lowercase21(A):-downcase_atom(A,A).
my_set22(A):-list_to_set(A,A).
my_len23(A,B):-length(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_last4,[list(T),T]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_odd6,[int]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_element8,[list(T),T]).
prim(my_tail9,[list(T),list(T)]).
prim(my_min_list10,[list(int),int]).
prim(my_pred11,[int,int]).
prim(my_succ12,[int,int]).
prim(my_double13,[int,int]).
prim(my_flatten14,[list(list(T)),list(T)]).
prim(my_sumlist15,[list(int),int]).
prim(my_max_list16,[list(int),int]).
prim(my_even17,[int]).
prim(my_toupper18,[char,char]).
prim(my_head19,[list(T),T]).
prim(my_msort20,[list(int),list(int)]).
prim(my_lowercase21,[char]).
prim(my_set22,[list(_)]).
prim(my_len23,[list(_),int]).
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
p([g,j,'V',v,q,w,n],[v]).
p(['U','N','V',i,'V','V'],[u,n,v,v,v]).
p(['L','C','N',q,'Y',v,'S',j],[l,c,n,y,s]).
p(['V',l,r,'Y','Y',s,k,i,'H'],[v,y,y,h]).
p([c,j,'J',f,'J','K',s,'C'],[j,j,k,c]).
q([x,'J','G',g,'V'],[g,j,v,'W']).
q(['L',i,'Z','C','U','X','Q'],[u,q,z,c,x,l,d]).
q([y,'S','Z','Y','Y','F',p,'U'],[y,s,f,u,u,y,z]).
q([x,'G','Z','P','Q','K',y,'O','Y'],[k,u,z,y,p,g,o,q]).
q(['K',b,e,'R','P','F','L','X'],[r,f,l,k,j,p,x]).
