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

my_head4([H|_],H).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_toupper7(A,B):-upcase_atom(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_msort9(A,B):-msort(A,B).
my_last10(A,B):-last(A,B).
my_len11(A,B):-length(A,B).
my_odd12(A):-1 is A mod 2.
my_set13(A):-list_to_set(A,A).
my_even14(A):-0 is A mod 2.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_head4,[list(T),T]).
prim(my_max_list5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_toupper7,[char,char]).
prim(my_sumlist8,[list(int),int]).
prim(my_msort9,[list(int),list(int)]).
prim(my_last10,[list(T),T]).
prim(my_len11,[list(_),int]).
prim(my_odd12,[int]).
prim(my_set13,[list(_)]).
prim(my_even14,[int]).
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
p([x,a,'I','K','P','Y'],[i,k,p,y]).
p(['T',p,'H',d,v,'R',z],[t,h,r]).
p([d,'M','S',g,'D',d,y,'O'],[m,s,d,o]).
p([i,c,w,'D',h,'I','N',s],[d,i,n]).
p(['U',u,s,h,'H','O',m,'Z','I'],[u,h,o,z,i]).
q([b,'U','P','J',f,m,x,n,'R'],[j,r,p,u,d]).
q(['L','D','Q','H'],['G',l,h,d,q]).
q([a,'K',q,p,'B','M',r,n,'F'],[k,y,f,b,m]).
q(['M','U','T','V',s,'Z','F','L'],[l,z,m,t,u,'A',f,v]).
q(['O','S',l,'K','A','C'],[s,'U',o,a,c,k]).
