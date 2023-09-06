def environ():
   return [[],[]] 

def isnull(x):
    if len(x) == 0:
        return True
    else:
        return False

def car(x):
    if not isnull(x):
        return [x[0]]

def cons(atom, x):
    # list.append() always returns None
    # use list + list_item instead
    # print("consing")
    print(atom, x)
    return atom + x

def cdr(x):
    if not isnull(x):
        return x[1]
    else:
        return []

def ribs(var, val):
    # print("ribsing")
    return cons([var], cons([val], []))

def empty(x):
    if len(x[0]) == 0 and len(x[1]) == 0:
        return True
    else:
        return False

def ext_env(var, val, env):
    # print(var, val, env)
    if empty(env):
        # print("empty")
        temp =  [cons(ribs(var, val), []),environ()]
        # print('temp', temp)
        return temp
    else:
        # print("not empty")
        # print('cdr', cdr(env))
        return cons(env[0], ext_env(var, val, cdr(env)))



print(ext_env(['a','b'], [3,4], environ()))
test_env = ext_env(['a','b'], [3,4], environ())
# print("test_env", test_env)
test_env2 = ext_env(['c','d'], [5,6], test_env)
print(test_env2)

# print(ext_env(('a','b'), (3,4), 
#               (ext_env(('a','b'), (3,4), 
#                        ext_env(('a','b'), (3,4), [])))))
