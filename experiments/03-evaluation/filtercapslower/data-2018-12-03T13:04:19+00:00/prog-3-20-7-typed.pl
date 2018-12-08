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

my_list_to_set4(A,B):-list_to_set(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_len6(A,B):-length(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_max_list8(A,B):-max_list(A,B).
my_last9(A,B):-last(A,B).
my_odd10(A):-1 is A mod 2.
my_pred11(A,B):-succ(B,A),A > 0.
my_head12([H|_],H).
my_succ13(A,B):-succ(A,B),B =< 10.
my_reverse14(A,B):-reverse(A,B).
my_msort15(A,B):-msort(A,B).
my_flatten16(A,B):-flatten(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_element18(A,B):-member(B,A).
my_toupper19(A,B):-upcase_atom(A,B).
my_tail20([_|TL],TL).
my_min_list21(A,B):-min_list(A,B).
my_set22(A):-list_to_set(A,A).
my_even23(A):-0 is A mod 2.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_lowercase5,[char]).
prim(my_len6,[list(_),int]).
prim(my_double7,[int,int]).
prim(my_max_list8,[list(int),int]).
prim(my_last9,[list(T),T]).
prim(my_odd10,[int]).
prim(my_pred11,[int,int]).
prim(my_head12,[list(T),T]).
prim(my_succ13,[int,int]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_msort15,[list(int),list(int)]).
prim(my_flatten16,[list(list(T)),list(T)]).
prim(my_sumlist17,[list(int),int]).
prim(my_element18,[list(T),T]).
prim(my_toupper19,[char,char]).
prim(my_tail20,[list(T),list(T)]).
prim(my_min_list21,[list(int),int]).
prim(my_set22,[list(_)]).
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
p(['H','P','K','N',v,y,b,j],[h,p,k,n]).
p([s,g,f,c,'K'],[k]).
p([x,'A','S',c,u,'C','K','D','R'],[a,s,c,k,d,r]).
p(['L',n,v,'E',o,e,'Q',q,s],[l,e,q]).
p([l,h,t,'C',u,'S',f,'K'],[c,s,k]).
q(['G',k,i,'I',e,'A',o,'Q'],[a,'Y',g,q,i]).
q([k,'D','L','A',f,s],[a,a,l,d]).
q(['D',d,'U','L','K','Y',f],[v,y,u,k,d,l]).
q([b,w,'P',d,'H'],[p,h,c]).
q(['M','U',f,'W','G','Q','M','N'],['B',n,u,m,m,g,q,w]).
