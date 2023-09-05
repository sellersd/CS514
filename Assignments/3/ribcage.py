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
    # print(atom, x)
    return ([atom] + x)

def cdr(x):
    if not isnull(x):
        return x[1:]
    else:
        return []

def ribs(var, val):
    # print("ribsing")
    return (cons(var, cons(val, [])))

def empty(x):
    if len(x) == 0:
        return True
    else:
        return False

def ext_env(var, val, env):
    # print(var, val, env)
    if empty(env):
        return cons(ribs(var, val), [])
    else:
        cons(ext_env(var, val, cdr(env)), env)

# print(type([]))
#
# test = [1,2,3,4]
# print(cdr(cdr(cdr(test))))
# print("not empty",empty(cdr(cdr(cdr(test)))))
# print("empty",empty(cdr(cdr(cdr(cdr(test))))))
# print("cdr of []",empty(cdr(cdr(cdr(cdr(cdr(cdr(test))))))))
# print()

# print(ribs(3 , 4))
print(ext_env(('a','b'), (3,4), []))
# print(ext_env(('a','b'), (3,4), 
#               (ext_env(('a','b'), (3,4), 
#                        ext_env(('a','b'), (3,4), [])))))
