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
my_tail5([_|TL],TL).
my_element6(A,B):-member(B,A).
my_double7(N,M):-M is 2*N,M =< 10.
my_sumlist8(A,B):-sumlist(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_odd10(A):-1 is A mod 2.
my_even11(A):-0 is A mod 2.
my_last12(A,B):-last(A,B).
my_set13(A):-list_to_set(A,A).
my_reverse14(A,B):-reverse(A,B).
my_msort15(A,B):-msort(A,B).
my_list_to_set16(A,B):-list_to_set(A,B).
my_min_list17(A,B):-min_list(A,B).
my_toupper18(A,B):-upcase_atom(A,B).
my_succ19(A,B):-succ(A,B),B =< 10.
my_flatten20(A,B):-flatten(A,B).
my_len21(A,B):-length(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_pred4,[int,int]).
prim(my_tail5,[list(T),list(T)]).
prim(my_element6,[list(T),T]).
prim(my_double7,[int,int]).
prim(my_sumlist8,[list(int),int]).
prim(my_lowercase9,[char]).
prim(my_odd10,[int]).
prim(my_even11,[int]).
prim(my_last12,[list(T),T]).
prim(my_set13,[list(_)]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_msort15,[list(int),list(int)]).
prim(my_list_to_set16,[list(T),list(T)]).
prim(my_min_list17,[list(int),int]).
prim(my_toupper18,[char,char]).
prim(my_succ19,[int,int]).
prim(my_flatten20,[list(list(T)),list(T)]).
prim(my_len21,[list(_),int]).
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
p([b,'C','O',x,u,'K','K'],[c,o,k,k]).
p([t,'Y',h,m,v,'Z','F',n,c],[y,z,f]).
p([t,t,j,t,o],[]).
p(['N',j,j,f,'F'],[n,f]).
p(['N','L','R',o,e,'Q','N',u,s],[n,l,r,q,n]).
q([k,z,'E','Q','S'],[q,e,s,'F']).
q([u,s,'Z',l,'E',f,'V',c],[v,'W',e,z]).
q(['Z',j,'Q','W',u,'P',v,'T'],[i,p,q,t,w,z]).
q([c,d,x,'K',t,'B','R'],['A',k,b,r]).
q(['C','J',w,c],[j,'B',c]).
