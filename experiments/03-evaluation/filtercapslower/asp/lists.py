import dlvhex
import sys
import ast
#import re

def pyDbg(N,MSG):
    N, MSG = N.value(), MSG.value()
    ##print(f"debug({N}: {MSG})", file=sys.stderr)
    dlvhex.output(())

def is_type(t):
    if '(' in t:
        if t[:4] == 'list':
            return is_type(t[5:-1])
        if t[1] == '[' and t[-1] == ']':
            return all(is_type(ty) for ty in t[1:-1].split(',')) # FIXME: parsing is too simplistic
        return False
    else:
        return t in ['int','bool','nat','etc']

def pyTy(T):
    T = T.value()
    ##print(f"pyTyp({T})", file=sys.stderr)
    dlvhex.output(()) if T == 'int' else None
    # if is_type(T): dlvhex.output(())

def pyTy():
    ##print(f"pyTyp()", file=sys.stderr)
    dlvhex.output(('int','list(int)','(int,int)','(list(int),list(int)'))
    # if is_type(T): dlvhex.output(())

def pyReverse(L):
    def reverse(l,acc):
        if l == []: return acc
        return reverse(l[1],[l[0],acc])
    ##print("pyReverse({})".format(L.value()), file=sys.stderr)
    L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))

    if type(L) == list:
        dlvhex.output((str(reverse(L,[])).replace('[','(').replace(']',')').replace("'",'"'),))

def pyLength(L):
    try:
        def length(l):
            return 0 if l == [] else (1 + length(l[1]))
        L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))

        ##print(f"pyLength({L})", file=sys.stderr)
        dlvhex.output((length(L),))
    except Exception as e:
        return

def pyMaxlist(L):
    try:
        def maxlist(l):
            return l[0] if l[1] == [] else max(l[0],maxlist(l[1]))
        L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))
        if L == [] or type(L[0]) != int: return

        ##print(f"pyMaxlist({L})", file=sys.stderr)
        dlvhex.output((maxlist(L),))
    except Exception as e:
        return

def pyMinlist(L):
    try:
        def minlist(l):
            return l[0] if l[1] == [] else min(l[0],minlist(l[1]))
        L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))
        if L == [] or type(L[0]) != int: return

        ##print(f"pyMinlist({L})", file=sys.stderr)
        dlvhex.output((minlist(L),))
    except Exception as e:
        return

def pySumlist(L):
    ##print(f"pySumlist({L})", file=sys.stderr)
    try:
        def sumlist(l):
            return 0 if l == [] else (l[0] + sumlist(l[1]))
        L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))
        if type(L[0]) != int: return

        dlvhex.output((sumlist(L),))
    except Exception as e:
        return

def pySucc(X):
    X = X.value()
    ##print(f"pySucc({X})", file=sys.stderr)
    if X.isdigit():
        X = int(X)
        dlvhex.output((str(X+ 1),))

def pyPred(X):
    X = X.value()
    ##print(f"pyPred({X})", file=sys.stderr)
    if X.isdigit():
        X = int(X)
        dlvhex.output((str(X- 1),))

def pyMapDbg(H1,TL1,T,Z):
    H1, TL1, T = H1.value(), TL1.value(), T.value()
    Z = Z.value()
    ##print(f"pyMapDbg({H1}, {TL1}, {T}, {Z})", file=sys.stderr)
    dlvhex.output(())

def pyDestruct(L):
    L = L.value()
    ##print(f"pyDestruct({L})", file=sys.stderr)
    ##print("split", L.split(',', 1), file=sys.stderr)
    if ',' in L:
        (_,_,h), tl = L.split(',', 1)
        dlvhex.output((('"' + h + '"', '"[' + tl),))

table = {'length': pyLength, 'succ': pySucc}
def pyCall(P, X):
    ##print(f"pyCall({P}, {X})", file=sys.stderr)
    ##print(dlvhex.currentInput, file=sys.stderr)
    table.get(P.value(), lambda x: 0)(X)

# def getFirst(car_list):
#         if car_list.value() != '"[]"':
#                 first = re.search('c\((.+?)\)(]|,c)', car_list.value()).group(1)
#                 dlvhex.output(('"[c(' + first + ')]"', ))

# def removeFirst(car_list):
#         if re.search('c\(.+?\)(]|,c)(.*)]', car_list.value()):
#                 rest = re.search('c\(.+?\)(]|,c)(.*)]', car_list.value()).group(2)
#                 dlvhex.output(('"[c(' + rest + ']"', ))
#         elif car_list.value() != '"[]"':
#                 dlvhex.output(('"[]"', ))

# def contains(car_list, word):
#         if re.search(word.value()[1:-1], car_list.value()):
#                 dlvhex.output(())

def pyLast(L):
    try:
        def last(l):
            return l[0] if l[1] == [] else last(l[1])
        L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))

        ##print(f"pyLast({L})", file=sys.stderr)
        if L != []:
            las = last(L)
            if type(las) == str:
                las = '"' + las + '"'
            dlvhex.output((las.replace('[','(').replace(']',')').replace("'",'"'),))
    except Exception as e:
        return

def pyCharcode(L):
    try:
        dlvhex.output((ord(L.value()),))
    except Exception as e:
        return

def pyFlatten(L):
    try:
        L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))

        def concat(l,r):
            if l == []:
                return r
            return [l[0],concat(l[1],r)]

        def flatten(l, acc):
            if l == []:
                return acc
            return flatten(l[1], concat(acc,l[0]))

        if len(L) == 0:
            dlvhex.output(("()",))

        ##print(f"pyReverse({L})", file=sys.stderr)
        if type(L) == list and type(L[0]) == list:
            dlvhex.output((str(flatten(L,[])).replace('[','(').replace(']',')').replace("'",'"'),))
    except:
        pass

def conslist_to_pylist(L):
    if L == []:
        return L
    return [L[0]] + (conslist_to_pylist(L[1]))

def pylist_to_conslist(L):
    if L == []:
        return L
    return [L[0], (pylist_to_conslist(L[1:]))]

def pyListToSet(L):
    try:
        L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))

        def to_set(seq):
            # from stackoverflow: https://stackoverflow.com/questions/480214/how-do-you-remove-duplicates-from-a-list-whilst-preserving-order
            seen = set()
            seen_add = seen.add
            return [x for x in seq if not (x in seen or seen_add(x))]

        ##print(f"pyListToSet({L})", file=sys.stderr)
        if type(L) == list:
            dlvhex.output((str(pylist_to_conslist(to_set(conslist_to_pylist(L)))).replace('[','(').replace(']',')').replace("'",'"'),))
    except:
        pass

def pyElement(L):
    try:
        L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))

        ##print(f"pyElement({L})", file=sys.stderr)
        if type(L) == list:
            dlvhex.output((str(e).replace('[','(').replace(']',')').replace("'",'"') for e in
                conslist_to_pylist(L)))
    except:
        pass

def pySort(L):
    try:
        L = ast.literal_eval(L.value().replace('(','[').replace(')',']'))

        if len(L) == 0:
            dlvhex.output(("()",))

        ##print(f"pyListToSet({L})", file=sys.stderr)
        if type(L) == list and type(L[0]) == int:
            dlvhex.output((str(pylist_to_conslist(sorted(conslist_to_pylist(L)))).replace('[','(').replace(']',')').replace("'",'"'),))
    except:
        pass

def pyToUpper(C):
    try:
        C = ast.literal_eval(C.value().replace('(','[').replace(')',']'))

        ##print(f"pyToUpper({L})", file=sys.stderr)
        if type(C) == str and len(C) == 1:
            dlvhex.output(('"' + C.upper() + '"',))
    except:
        pass

def pyToLower(C):
    try:
        C = ast.literal_eval(C.value().replace('(','[').replace(')',']'))

        ##print(f"pyToLower({L})", file=sys.stderr)
        if type(C) == str and len(C) == 1:
            dlvhex.output(('"' + C.lower() + '"',))
    except:
        pass

def pySet(L):
    try:
        L = ast.literal_eval(C.value().replace('(','[').replace(')',']'))

        def to_set(seq):
            # from stackoverflow: https://stackoverflow.com/questions/480214/how-do-you-remove-duplicates-from-a-list-whilst-preserving-order
            seen = set()
            seen_add = seen.add
            return [x for x in seq if not (x in seen or seen_add(x))]

        if type(L) == list and pylist_to_conslist(to_set(conslist_to_pylist(L))) == L:
            dlvhex.output(())
    except:
        pass

def pyUppercase(C):
    try:
        C = ast.literal_eval(C.value().replace('(','[').replace(')',']'))

        ##print(f"pyToUpper({L})", file=sys.stderr)
        if type(C) == str and len(C) == 1 and C.isupper():
            dlvhex.output(())
    except:
        pass

def pyLowercase(C):
    try:
        C = ast.literal_eval(C.value().replace('(','[').replace(')',']'))

        ##print(f"pyToUpper({L})", file=sys.stderr)
        if type(C) == str and len(C) == 1 and C.islower():
            dlvhex.output(())
    except:
        pass

def register():
	prop = dlvhex.ExtSourceProperties()
	dlvhex.addAtom("pyDbg", (dlvhex.CONSTANT, dlvhex.CONSTANT), 0, prop)

	prop = dlvhex.ExtSourceProperties()
	dlvhex.addAtom("pySet", (dlvhex.CONSTANT,), 0, prop)

	prop = dlvhex.ExtSourceProperties()
	dlvhex.addAtom("pyUppercase", (dlvhex.CONSTANT,), 0, prop)

	prop = dlvhex.ExtSourceProperties()
	dlvhex.addAtom("pyLowercase", (dlvhex.CONSTANT,), 0, prop)

	#prop = dlvhex.ExtSourceProperties()
	#dlvhex.addAtom("pyTy", (dlvhex.CONSTANT,), 0, prop)

	#prop = dlvhex.ExtSourceProperties()
	#dlvhex.addAtom("pyMapDbg", 
        #    (dlvhex.CONSTANT, dlvhex.CONSTANT, dlvhex.CONSTANT, dlvhex.CONSTANT), 0, prop)
	# prop = dlvhex.ExtSourceProperties()
	# dlvhex.addAtom("pyEnumTy", (), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyLength", (dlvhex.CONSTANT, ), 1, prop)
        
	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyMaxlist", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyMinlist", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pySumlist", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyReverse", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pySucc", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyPred", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyLast", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyCharcode", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyFlatten", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyListToSet", (dlvhex.CONSTANT, ), 1, prop)
         
	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pySort", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyElement", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyToUpper", (dlvhex.CONSTANT, ), 1, prop)

	prop = dlvhex.ExtSourceProperties()
	prop.addFiniteOutputDomain(0)
	dlvhex.addAtom("pyToLower", (dlvhex.CONSTANT, ), 1, prop)

	#prop = dlvhex.ExtSourceProperties()
	#prop.addFiniteOutputDomain(0)
	#dlvhex.addAtom("pyDestruct", (dlvhex.CONSTANT, ), 2, prop)

	#prop = dlvhex.ExtSourceProperties()
	#prop.addFiniteOutputDomain(0)
	#dlvhex.addAtom("pyCall", (dlvhex.CONSTANT, dlvhex.CONSTANT), 1, prop)

	#prop = dlvhex.ExtSourceProperties()
	#prop.addFiniteOutputDomain(0)
	#dlvhex.addAtom("getFirst", (dlvhex.CONSTANT, ), 1, prop)

	#prop = dlvhex.ExtSourceProperties()
	#prop.addFiniteOutputDomain(0)
	#dlvhex.addAtom("removeFirst", (dlvhex.CONSTANT, ), 1, prop)

	#prop = dlvhex.ExtSourceProperties()
	#dlvhex.addAtom("contains", (dlvhex.CONSTANT, dlvhex.CONSTANT ), 0, prop)
