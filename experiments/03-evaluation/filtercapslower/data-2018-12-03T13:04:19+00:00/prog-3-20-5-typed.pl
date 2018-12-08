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

my_odd4(A):-1 is A mod 2.
my_toupper5(A,B):-upcase_atom(A,B).
my_last6(A,B):-last(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_lowercase9(A):-downcase_atom(A,A).
my_tail10([_|TL],TL).
my_max_list11(A,B):-max_list(A,B).
my_set12(A):-list_to_set(A,A).
my_element13(A,B):-member(B,A).
my_reverse14(A,B):-reverse(A,B).
my_len15(A,B):-length(A,B).
my_head16([H|_],H).
my_double17(N,M):-M is 2*N,M =< 10.
my_flatten18(A,B):-flatten(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_msort20(A,B):-msort(A,B).
my_min_list21(A,B):-min_list(A,B).
my_succ22(A,B):-succ(A,B),B =< 10.
my_even23(A):-0 is A mod 2.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_odd4,[int]).
prim(my_toupper5,[char,char]).
prim(my_last6,[list(T),T]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_pred8,[int,int]).
prim(my_lowercase9,[char]).
prim(my_tail10,[list(T),list(T)]).
prim(my_max_list11,[list(int),int]).
prim(my_set12,[list(_)]).
prim(my_element13,[list(T),T]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_len15,[list(_),int]).
prim(my_head16,[list(T),T]).
prim(my_double17,[int,int]).
prim(my_flatten18,[list(list(T)),list(T)]).
prim(my_sumlist19,[list(int),int]).
prim(my_msort20,[list(int),list(int)]).
prim(my_min_list21,[list(int),int]).
prim(my_succ22,[int,int]).
prim(my_even23,[int]).
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
p([e,'V',l,a,q,'Y',f],[v,y]).
p([y,'Y','L','Q',q,'C'],[y,l,q,c]).
p([w,'M',h,'L',b],[m,l]).
p(['L','D',m,o,q],[l,d]).
p(['H','Y',p,'Z',w],[h,y,z]).
q(['X','P','H','X','X'],[h,x,x,'T',x,p]).
q([h,u,s,'S','K','S','E',v,'I'],[k,i,s,s,e,q]).
q([p,f,'H',a,d,'H'],[h,h,'Z']).
q(['I','M',a,'Y',q,'T','Q',m],[c,y,m,q,i,t]).
q([c,j,'Z',n],['W',z]).
